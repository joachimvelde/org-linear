;;; org-linear-api.el --- Linear GraphQL client for org-linear -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Asynchronous access to the Linear GraphQL API.
;;
;; `org-linear-api-query' sends one GraphQL request.  `org-linear-api-issues'
;; follows Linear's cursor pagination and returns every matching issue.
;; Both take a success callback and an optional error callback; nothing
;; here blocks Emacs while a request is in flight.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup org-linear nil
  "Work with Linear issues in Org mode."
  :group 'org
  :prefix "org-linear-")

(defcustom org-linear-api-url "https://api.linear.app/graphql"
  "Endpoint of the Linear GraphQL API."
  :type 'string)

(defcustom org-linear-api-key nil
  "Linear personal API key, or a function that returns one.
When nil, the key is read with `auth-source' for host
\"api.linear.app\".  Storing the key in an encrypted authinfo file is
preferred over setting it here."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key")
                 (function :tag "Function returning the key")))

(defcustom org-linear-api-page-size 50
  "Number of issues requested per page.
Linear rejects queries above a complexity limit, so large pages with
many nested fields can fail."
  :type 'natnum)

(defcustom org-linear-api-max-issues 1000
  "Stop paginating after this many issues."
  :type 'natnum)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(define-error 'org-linear-api-error "Linear API error")

(defun org-linear-api--key ()
  "Return the Linear API key, or signal a `user-error'."
  (let ((key (cond ((functionp org-linear-api-key) (funcall org-linear-api-key))
                   ((stringp org-linear-api-key) org-linear-api-key)
                   (t (let* ((found (car (condition-case err
                                             (auth-source-search
                                              :host "api.linear.app" :max 1)
                                           ;; e.g. GPG missing for ~/.authinfo.gpg
                                           (error
                                            (user-error "Auth-source failed (%s); \
set `org-linear-api-key' instead"
                                                        (error-message-string err))))))
                             (secret (plist-get found :secret)))
                        (if (functionp secret) (funcall secret) secret))))))
    (if (and (stringp key) (not (string-empty-p key)))
        key
      (user-error "No Linear API key: set `org-linear-api-key' or add \
\"machine api.linear.app password <key>\" to your authinfo file"))))

(defun org-linear-api--default-error (message)
  "Report MESSAGE from a failed request."
  (message "org-linear: %s" message))

(defun org-linear-api--parse-response (status)
  "Parse the HTTP response in the current buffer.
STATUS is the plist `url-retrieve' passes to its callback.  Return
the GraphQL `data' object, or signal `org-linear-api-error'."
  (let ((err (plist-get status :error)))
    (when (and err (not (bound-and-true-p url-http-response-status)))
      (signal 'org-linear-api-error (list (format "Request failed: %S" err)))))
  (goto-char (or (bound-and-true-p url-http-end-of-headers) (point-min)))
  (let* ((body (decode-coding-string
                (buffer-substring-no-properties (point) (point-max)) 'utf-8))
         (code (bound-and-true-p url-http-response-status))
         (json (condition-case nil
                   (json-parse-string body :object-type 'alist :array-type 'list
                                      :null-object nil :false-object nil)
                 (json-parse-error nil)))
         (errors (and (consp json) (alist-get 'errors json))))
    (cond
     (errors
      (signal 'org-linear-api-error
              (list (mapconcat (lambda (e) (or (alist-get 'message e) (format "%S" e)))
                               errors "; "))))
     ((not (consp json))
      (signal 'org-linear-api-error
              (list (format "HTTP %s, response is not JSON" code))))
     ((and code (>= code 400))
      (signal 'org-linear-api-error (list (format "HTTP %s" code))))
     (t (alist-get 'data json)))))

(defun org-linear-api--post (body callback on-error)
  "POST the JSON string BODY to Linear.
Call CALLBACK with the response `data' object, or ON-ERROR with a
message string."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(org-linear-api--key))))
        (url-request-data (encode-coding-string body 'utf-8)))
    (url-retrieve
     org-linear-api-url
     (lambda (status)
       (let ((buffer (current-buffer))
             (result nil)
             (failure nil))
         (unwind-protect
             (condition-case err
                 (setq result (org-linear-api--parse-response status))
               (org-linear-api-error (setq failure (cadr err))))
           (kill-buffer buffer))
         ;; Run the callbacks outside the response buffer, which is gone.
         (if failure
             (funcall on-error failure)
           (funcall callback result))))
     nil t t)))

(defun org-linear-api-query (query variables callback &optional on-error)
  "Run the GraphQL QUERY with VARIABLES, a plist, asynchronously.
Call CALLBACK with the `data' object as an alist.  On failure call
ON-ERROR with a message string; the default shows it in the echo
area."
  (org-linear-api--post
   (json-serialize (list :query query :variables variables)
                   :null-object :null :false-object :false)
   callback
   (or on-error #'org-linear-api--default-error)))

(defconst org-linear-api--issues-query
  "query OrgLinearIssues($first: Int!, $after: String, $filter: IssueFilter) {
  issues(first: $first, after: $after, filter: $filter) {
    nodes {
      id identifier title url priority updatedAt dueDate
      state { name type position }
      team { key name }
      assignee { name }
      project { name }
      labels(first: 10) { nodes { name } }
    }
    pageInfo { hasNextPage endCursor }
  }
}"
  "GraphQL query for one page of issues.")

(defun org-linear-api-issues (filter callback &optional on-error)
  "Fetch every issue that matches FILTER, following pagination.
FILTER is a plist in the shape of Linear's IssueFilter input, or nil
for no filter.  Call CALLBACK with the list of issue alists, or
ON-ERROR with a message string if any page fails.  Stop after
`org-linear-api-max-issues' issues."
  (let ((issues nil))
    (cl-labels
        ((fetch (cursor)
           (org-linear-api-query
            org-linear-api--issues-query
            (list :first org-linear-api-page-size
                  :after (or cursor :null)
                  :filter (or filter :null))
            (lambda (data)
              (let* ((conn (alist-get 'issues data))
                     (page (alist-get 'pageInfo conn)))
                (setq issues (nconc issues (alist-get 'nodes conn)))
                (if (and (alist-get 'hasNextPage page)
                         (< (length issues) org-linear-api-max-issues))
                    (fetch (alist-get 'endCursor page))
                  (funcall callback (take org-linear-api-max-issues issues)))))
            on-error)))
      (fetch nil))))

(defconst org-linear-api--issue-query
  "query OrgLinearIssue($id: String!) {
  issue(id: $id) {
    id identifier title url priority createdAt updatedAt dueDate estimate
    description branchName
    state { name type position }
    team { key name }
    assignee { name }
    creator { name }
    project { name }
    cycle { number name }
    parent { id identifier title url priority state { name type } }
    children(first: 50) {
      nodes { id identifier title url priority updatedAt
              state { name type position } team { key } assignee { name } }
    }
    labels(first: 20) { nodes { name } }
    attachments(first: 25) { nodes { title url } }
    comments(first: 100) {
      nodes { id body createdAt
              user { name } externalUser { name } botActor { name }
              parent { id } }
    }
  }
}"
  "GraphQL query for one issue with its description and comments.")

(defun org-linear-api-issue (id callback &optional on-error)
  "Fetch the issue ID with its description, sub-issues and comments.
ID is an identifier such as \"ENG-12\" or an issue UUID.  Call
CALLBACK with the issue alist, or ON-ERROR with a message string."
  (let ((on-error (or on-error #'org-linear-api--default-error)))
    (org-linear-api-query
     org-linear-api--issue-query (list :id id)
     (lambda (data)
       (if-let* ((issue (alist-get 'issue data)))
           (funcall callback issue)
         (funcall on-error (format "Issue %s not found" id))))
     on-error)))

(defconst org-linear-api--states-query
  "query OrgLinearStates($id: String!) {
  issue(id: $id) {
    team { id key states(first: 100) { nodes { id name type position } } }
  }
}"
  "GraphQL query for the workflow states of an issue's team.")

(defvar org-linear-api--states-cache (make-hash-table :test #'equal)
  "Workflow states per team key, filled by `org-linear-api-team-states'.")

(defun org-linear-api-team-states (id callback &optional on-error team-key)
  "Get the workflow states of the team that owns issue ID.
Call CALLBACK with the list of state alists (id, name, type,
position), or ON-ERROR with a message string.  When TEAM-KEY names
a team whose states are cached, call CALLBACK at once without a
request.  States are cached per team for the session;
`org-linear-api-clear-cache' empties the cache."
  (let ((on-error (or on-error #'org-linear-api--default-error))
        (cached (and team-key (gethash team-key org-linear-api--states-cache))))
    (if cached
        (funcall callback cached)
      (org-linear-api-query
       org-linear-api--states-query (list :id id)
       (lambda (data)
	 (let* ((team (alist-get 'team (alist-get 'issue data)))
		(states (alist-get 'nodes (alist-get 'states team))))
           (if (null states)
               (funcall on-error (format "No workflow states found for %s" id))
             (puthash (alist-get 'key team) states org-linear-api--states-cache)
             (funcall callback states))))
       on-error))))

(defun org-linear-api-clear-cache ()
  "Forget cached workflow states."
  (interactive)
  (clrhash org-linear-api--states-cache))

(defconst org-linear-api--update-mutation
  "mutation OrgLinearUpdate($id: String!, $input: IssueUpdateInput!) {
  issueUpdate(id: $id, input: $input) {
    success
    issue { id identifier priority state { name type } }
  }
}"
  "GraphQL mutation that updates one issue.")

(defun org-linear-api-update-issue (id input callback &optional on-error)
  "Update issue ID with INPUT, a plist in the shape of IssueUpdateInput.
For example (:stateId \"...\") or (:priority 2).  Call CALLBACK with
the updated issue alist, or ON-ERROR with a message string."
  (let ((on-error (or on-error #'org-linear-api--default-error)))
    (org-linear-api-query
     org-linear-api--update-mutation (list :id id :input input)
     (lambda (data)
       (let ((result (alist-get 'issueUpdate data)))
         (if (alist-get 'success result)
             (funcall callback (alist-get 'issue result))
           (funcall on-error (format "Linear did not update %s" id)))))
     on-error)))

(defconst org-linear-api--description-query
  "query OrgLinearDescription($id: String!) {
  issue(id: $id) { id identifier title description }
}"
  "GraphQL query for an issue's current title and description.")

(defun org-linear-api-issue-text (id callback &optional on-error)
  "Fetch the current title and description of issue ID.
Call CALLBACK with an alist with `id', `identifier', `title' and
`description', or ON-ERROR with a message string."
  (let ((on-error (or on-error #'org-linear-api--default-error)))
    (org-linear-api-query
     org-linear-api--description-query (list :id id)
     (lambda (data)
       (if-let* ((issue (alist-get 'issue data)))
           (funcall callback issue)
         (funcall on-error (format "Issue %s not found" id))))
     on-error)))

(defconst org-linear-api--comment-create-mutation
  "mutation OrgLinearCommentCreate($input: CommentCreateInput!) {
  commentCreate(input: $input) { success comment { id } }
}"
  "GraphQL mutation that adds a comment.")

(defun org-linear-api-create-comment (issue-id body parent-id callback &optional on-error)
  "Add a comment with the Markdown BODY to the issue ISSUE-ID.
When PARENT-ID is non-nil, the comment replies to that comment.
Call CALLBACK with the new comment alist, or ON-ERROR with a message."
  (let ((on-error (or on-error #'org-linear-api--default-error)))
    (org-linear-api-query
     org-linear-api--comment-create-mutation
     (list :input (append (list :issueId issue-id :body body)
                          (when parent-id (list :parentId parent-id))))
     (lambda (data)
       (let ((result (alist-get 'commentCreate data)))
         (if (alist-get 'success result)
             (funcall callback (alist-get 'comment result))
           (funcall on-error "Linear did not add the comment"))))
     on-error)))

(defconst org-linear-api--comment-delete-mutation
  "mutation OrgLinearCommentDelete($id: String!) {
  commentDelete(id: $id) { success }
}"
  "GraphQL mutation that deletes a comment.")

(defun org-linear-api-delete-comment (id callback &optional on-error)
  "Delete the comment ID.
Call CALLBACK with no arguments, or ON-ERROR with a message string."
  (let ((on-error (or on-error #'org-linear-api--default-error)))
    (org-linear-api-query
     org-linear-api--comment-delete-mutation (list :id id)
     (lambda (data)
       (if (alist-get 'success (alist-get 'commentDelete data))
           (funcall callback)
         (funcall on-error "Linear did not delete the comment")))
     on-error)))

(provide 'org-linear-api)
;;; org-linear-api.el ends here
