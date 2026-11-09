;;; org-linear-render.el --- Turn Linear issues into Org text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure functions from issue alists (as returned by `org-linear-api') to
;; Org text.  Nothing here talks to the network or touches buffers, so
;; it is easy to test.
;;
;; Mapping:
;; - Linear workflow state type -> Org TODO keyword
;;   (`org-linear-state-keywords').
;; - Linear priority 1-4 -> Org priority cookie A-D; "No priority" has no
;;   cookie and sorts last (`#+PRIORITIES: A E E').
;; - Labels -> Org tags, due date -> DEADLINE, the rest -> properties.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'org-linear-markdown)
(require 'subr-x)

(defgroup org-linear-render nil
  "How org-linear renders Linear issues."
  :group 'org-linear
  :prefix "org-linear-")

(defcustom org-linear-state-keywords
  '(("triage" . "TRIAGE")
    ("backlog" . "BACKLOG")
    ("unstarted" . "TODO")
    ("started" . "STARTED")
    ("completed" . "DONE")
    ("canceled" . "CANCELED")
    ("duplicate" . "DUPLICATE"))
  "Org TODO keyword for each Linear workflow state type.
Linear's state types are triage, backlog, unstarted, started,
completed, canceled and duplicate.  The last three count as done
states in Org."
  :type '(alist :key-type string :value-type string))

(defcustom org-linear-state-type-order
  '("started" "unstarted" "backlog" "triage" "completed" "canceled" "duplicate")
  "Order of state groups in the issue view, by Linear state type."
  :type '(repeat string))

(defcustom org-linear-group-by 'state
  "How the issue view groups issues under top-level headings."
  :type '(choice (const :tag "Workflow state" state)
                 (const :tag "Team" team)
                 (const :tag "Project" project)
                 (const :tag "No grouping" nil)))

(defconst org-linear-render--priority-cookies
  '((1 . "A") (2 . "B") (3 . "C") (4 . "D"))
  "Org priority letter for Linear priorities Urgent, High, Medium, Low.")

(defconst org-linear-render--done-types '("completed" "canceled" "duplicate")
  "Linear state types that map to Org done keywords.")

;;;; Field helpers

(defun org-linear-render--get (issue &rest path)
  "Follow PATH of keys into the alist ISSUE."
  (let ((value issue))
    (dolist (key path value)
      (setq value (and (consp value) (alist-get key value))))))

(defun org-linear-render--keyword (issue)
  "Return the Org TODO keyword for ISSUE, or nil."
  (cdr (assoc (org-linear-render--get issue 'state 'type)
              org-linear-state-keywords)))

(defun org-linear-render--priority (issue)
  "Return the Org priority cookie for ISSUE, or nil for no priority."
  (when-let* ((letter (alist-get (org-linear-render--get issue 'priority)
                                 org-linear-render--priority-cookies)))
    (format "[#%s]" letter)))

(defun org-linear-render--tag (name)
  "Turn the label NAME into a valid Org tag."
  (let ((tag (replace-regexp-in-string "[^[:alnum:]_@#%]+" "_" (string-trim name))))
    (string-trim tag "_+" "_+")))

(defun org-linear-render--tags (issue)
  "Return the Org tag string for ISSUE's labels, or nil."
  (let ((tags (delete "" (mapcar (lambda (l) (org-linear-render--tag
                                              (or (alist-get 'name l) "")))
                                 (org-linear-render--get issue 'labels 'nodes)))))
    (when tags
      (concat ":" (string-join (delete-dups tags) ":") ":"))))

(defun org-linear-render--one-line (string)
  "Collapse whitespace in STRING so it fits in a heading."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or string ""))))

(defun org-linear-render--link-text (string)
  "Make STRING safe as the description of an Org link."
  (replace-regexp-in-string "\\[\\|\\]" "" string))

(defun org-linear-render--date (date)
  "Turn the ISO DATE \"YYYY-MM-DD\" into an Org timestamp, or nil."
  (when (and (stringp date)
             (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date))
    (let ((time (encode-time (list 0 0 12
                                   (string-to-number (match-string 3 date))
                                   (string-to-number (match-string 2 date))
                                   (string-to-number (match-string 1 date))
                                   nil -1 nil))))
      (format-time-string "<%Y-%m-%d %a>" time))))

;;;; Issues

(defun org-linear-render-heading (issue level)
  "Return the Org heading line for ISSUE at outline LEVEL."
  (let* ((identifier (org-linear-render--get issue 'identifier))
         (url (org-linear-render--get issue 'url))
         (link (if url
                   (format "[[%s][%s]]" url (org-linear-render--link-text identifier))
                 identifier)))
    (string-join
     (delq nil (list (make-string level ?*)
                     (org-linear-render--keyword issue)
                     (org-linear-render--priority issue)
                     link
                     (org-linear-render--one-line (org-linear-render--get issue 'title))
                     (org-linear-render--tags issue)))
     " ")))

(defun org-linear-render-properties (issue)
  "Return the property drawer lines for ISSUE as a list of strings."
  (let ((props `(("LINEAR_ID" . ,(org-linear-render--get issue 'id))
                 ("LINEAR_IDENTIFIER" . ,(org-linear-render--get issue 'identifier))
                 ("LINEAR_STATE" . ,(org-linear-render--get issue 'state 'name))
                 ("LINEAR_TEAM" . ,(org-linear-render--get issue 'team 'key))
                 ("LINEAR_ASSIGNEE" . ,(org-linear-render--get issue 'assignee 'name))
                 ("LINEAR_PROJECT" . ,(org-linear-render--get issue 'project 'name))
                 ("LINEAR_CYCLE" . ,(org-linear-render--get issue 'cycle 'number))
                 ("LINEAR_ESTIMATE" . ,(org-linear-render--get issue 'estimate))
                 ("LINEAR_PARENT" . ,(org-linear-render--get issue 'parent 'identifier))
                 ("LINEAR_CREATOR" . ,(org-linear-render--get issue 'creator 'name))
                 ("LINEAR_BRANCH" . ,(org-linear-render--get issue 'branchName))
                 ("LINEAR_CREATED" . ,(org-linear-render--get issue 'createdAt))
                 ("LINEAR_UPDATED" . ,(org-linear-render--get issue 'updatedAt))
                 ("LINEAR_URL" . ,(org-linear-render--get issue 'url)))))
    (append '(":PROPERTIES:")
            (cl-loop for (name . value) in props
                     when (and value (not (equal value "")))
                     collect (format ":%s: %s" name
                                     (org-linear-render--one-line
                                      (if (stringp value) value (format "%s" value)))))
            '(":END:"))))

(defun org-linear-render-issue (issue level)
  "Return ISSUE as an Org entry at outline LEVEL, without a final newline."
  (let ((deadline (org-linear-render--date (org-linear-render--get issue 'dueDate))))
    (string-join
     (append (list (org-linear-render-heading issue level))
             (when deadline (list (concat "DEADLINE: " deadline)))
             (org-linear-render-properties issue))
     "\n")))

;;;; Sorting and grouping

(defun org-linear-render--priority-rank (issue)
  "Sort rank of ISSUE's priority: Urgent first, no priority last."
  (let ((p (org-linear-render--get issue 'priority)))
    (if (and (numberp p) (> p 0)) p 5)))

(defun org-linear-render--issue< (a b)
  "Non-nil if issue A sorts before B: by priority, then newest update."
  (let ((pa (org-linear-render--priority-rank a))
        (pb (org-linear-render--priority-rank b)))
    (if (/= pa pb)
        (< pa pb)
      (string> (or (org-linear-render--get a 'updatedAt) "")
               (or (org-linear-render--get b 'updatedAt) "")))))

(defun org-linear-render--child< (a b)
  "Non-nil if sub-issue A sorts before B: by state type, then `issue<'."
  (let ((ra (org-linear-render--state-rank a))
        (rb (org-linear-render--state-rank b)))
    (if (equal ra rb)
        (org-linear-render--issue< a b)
      (org-linear-render--rank< ra rb))))

(defun org-linear-render--group-key (issue)
  "Return the group heading for ISSUE under `org-linear-group-by'."
  (pcase org-linear-group-by
    ('state (or (org-linear-render--get issue 'state 'name) "No state"))
    ('team (or (org-linear-render--get issue 'team 'name) "No team"))
    ('project (or (org-linear-render--get issue 'project 'name) "No project"))
    (_ nil)))

(defun org-linear-render--state-rank (issue)
  "Sort rank of ISSUE's workflow state, as a list of two numbers."
  (let* ((state (org-linear-render--get issue 'state))
         (pos (cl-position (alist-get 'type state) org-linear-state-type-order
                           :test #'equal)))
    (list (or pos (length org-linear-state-type-order))
          (or (alist-get 'position state) 0))))

(defun org-linear-render--group-rank (issues)
  "Sort rank for a group of ISSUES."
  (if (eq org-linear-group-by 'state)
      (org-linear-render--state-rank (car issues))
    (list 0 0)))

(defun org-linear-render--rank< (a b)
  "Compare the rank lists A and B."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(defun org-linear-render-groups (issues)
  "Group and sort ISSUES.
Return a list of (HEADING . ISSUES); HEADING is nil when
`org-linear-group-by' is nil."
  (let ((groups nil))
    (dolist (issue issues)
      (let* ((key (org-linear-render--group-key issue))
             (cell (assoc key groups)))
        (if cell
            (push issue (cdr cell))
          (push (list key issue) groups))))
    (setq groups (mapcar (lambda (g) (cons (car g) (sort (cdr g) #'org-linear-render--issue<)))
                         groups))
    (sort groups
          (lambda (a b)
            (let ((ra (org-linear-render--group-rank (cdr a)))
                  (rb (org-linear-render--group-rank (cdr b))))
              (if (equal ra rb)
                  (string< (or (car a) "") (or (car b) ""))
                (org-linear-render--rank< ra rb)))))))

;;;; Views

(defun org-linear-render--todo-line ()
  "Return the #+TODO line matching `org-linear-state-keywords'."
  (let (open done)
    (pcase-dolist (`(,type . ,keyword) org-linear-state-keywords)
      (if (member type org-linear-render--done-types)
          (push keyword done)
        (push keyword open)))
    (format "#+TODO: %s | %s"
            (string-join (delete-dups (nreverse open)) " ")
            (string-join (delete-dups (nreverse done)) " "))))

(defun org-linear-render-view (title issues &optional time)
  "Return an Org document titled TITLE listing ISSUES.
TIME is the fetch time shown in the header; it defaults to now."
  (let ((groups (org-linear-render-groups issues)))
    (concat
     (string-join
      (list (concat "#+TITLE: " title)
            (org-linear-render--todo-line)
            "#+PRIORITIES: A E E"
            "#+STARTUP: content"
            ""
            (format "Fetched %s, %d %s."
                    (format-time-string "[%Y-%m-%d %a %H:%M]" time)
                    (length issues)
                    (if (= (length issues) 1) "issue" "issues")))
      "\n")
     "\n"
     (mapconcat
      (lambda (group)
        (let ((heading (car group))
              (level (if (car group) 2 1)))
          (concat
           "\n"
           (when heading
             (format "* %s (%d)\n" heading (length (cdr group))))
           (mapconcat (lambda (issue) (org-linear-render-issue issue level))
                      (cdr group) "\n"))))
      groups "")
     "\n")))

;;;; Single issue

(defun org-linear-render--timestamp (iso)
  "Turn the ISO 8601 time ISO into an inactive Org timestamp, or nil."
  (when (stringp iso)
    (condition-case nil
        (format-time-string "[%Y-%m-%d %a %H:%M]" (encode-time (iso8601-parse iso)))
      (error nil))))

(defun org-linear-render--author (comment)
  "Return the display name of COMMENT's author."
  (or (org-linear-render--get comment 'user 'name)
      (org-linear-render--get comment 'externalUser 'name)
      (org-linear-render--get comment 'botActor 'name)
      "Unknown"))

(defun org-linear-render--comment (comment replies level)
  "Render COMMENT and its REPLIES at outline LEVEL.
REPLIES is a hash table from comment id to its replies, oldest first."
  (let ((body (org-linear-markdown-to-org (alist-get 'body comment) level)))
    (string-join
     (delq nil
           (append
            (list (format "%s %s %s" (make-string level ?*)
                          (org-linear-render--one-line (org-linear-render--author comment))
                          (or (org-linear-render--timestamp (alist-get 'createdAt comment)) ""))
                  ":PROPERTIES:"
                  (format ":LINEAR_COMMENT_ID: %s" (alist-get 'id comment))
                  ":END:"
                  (unless (string-empty-p body) body))
            (mapcar (lambda (reply) (org-linear-render--comment reply replies (1+ level)))
                    (gethash (alist-get 'id comment) replies))))
     "\n")))

(defun org-linear-render-comments (comments level)
  "Render COMMENTS as a thread tree with top-level comments at LEVEL.
Replies go under the comment they answer.  A reply whose parent is
missing from COMMENTS is shown at the top level."
  (let ((replies (make-hash-table :test #'equal))
        (ids (mapcar (lambda (c) (alist-get 'id c)) comments))
        (top nil))
    (dolist (comment (sort (copy-sequence comments)
                           (lambda (a b) (string< (or (alist-get 'createdAt a) "")
                                                  (or (alist-get 'createdAt b) "")))))
      (let ((parent (org-linear-render--get comment 'parent 'id)))
        (if (and parent (member parent ids))
            (puthash parent (append (gethash parent replies) (list comment)) replies)
          (push comment top))))
    (mapconcat (lambda (c) (org-linear-render--comment c replies level))
               (nreverse top) "\n")))

(defun org-linear-render-issue-document (issue &optional time)
  "Return an Org document for the single ISSUE.
The issue is a level-1 heading with sections for the description,
sub-issues, attachments and comments.  TIME is the fetch time shown
in the header; it defaults to now."
  (let* ((identifier (org-linear-render--get issue 'identifier))
         (parent (org-linear-render--get issue 'parent))
         (children (sort (copy-sequence (org-linear-render--get issue 'children 'nodes))
                         #'org-linear-render--child<))
         (attachments (org-linear-render--get issue 'attachments 'nodes))
         (comments (org-linear-render--get issue 'comments 'nodes))
         (description (org-linear-markdown-to-org
                       (org-linear-render--get issue 'description) 2)))
    (concat
     (string-join
      (delq nil
            (list
             (format "#+TITLE: %s %s" identifier
                     (org-linear-render--one-line (org-linear-render--get issue 'title)))
             (org-linear-render--todo-line)
             "#+PRIORITIES: A E E"
             "#+STARTUP: showall"
             ""
             (format "Fetched %s." (format-time-string "[%Y-%m-%d %a %H:%M]" time))
             ""
             (org-linear-render-issue issue 1)
             (when parent
               (format "Parent: [[%s][%s]] %s"
                       (alist-get 'url parent)
                       (alist-get 'identifier parent)
                       (org-linear-render--one-line (alist-get 'title parent))))
             "** Description"
             (if (string-empty-p description) "/No description./" description)
             (when children
               (concat (format "** Sub-issues (%d)\n" (length children))
                       (mapconcat (lambda (c) (org-linear-render-issue c 3)) children "\n")))
             (when attachments
               (concat (format "** Attachments (%d)\n" (length attachments))
                       (mapconcat (lambda (a)
                                    (format "- [[%s][%s]]" (alist-get 'url a)
                                            (org-linear-render--link-text
                                             (org-linear-render--one-line
                                              (or (alist-get 'title a) (alist-get 'url a))))))
                                  attachments "\n")))
             (concat (format "** Comments (%d)" (length comments))
                     (when comments
                       (concat "\n" (org-linear-render-comments comments 3))))))
      "\n")
     "\n")))

(provide 'org-linear-render)
;;; org-linear-render.el ends here
