;;; org-linear-capture.el --- Create Linear issues with org-capture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Create Linear issues from an Org capture.  The captured entry maps
;; to the issue like this:
;;
;;   * [#B] Title of the issue
;;   DEADLINE: <2026-11-20 Fri>
;;   Description, in Org, sent as Markdown.
;;
;; The heading is the title, an optional priority cookie the priority,
;; an optional DEADLINE the due date and the body the description.
;;
;; M-x org-linear-capture runs the capture directly.  To add it to your
;; own templates:
;;
;;   (add-to-list 'org-capture-templates (org-linear-capture-template "l"))

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-linear)

(defcustom org-linear-capture-team nil
  "Key of the team new issues go to, such as \"ENG\".
Nil means ask, unless there is only one team."
  :type '(choice (const :tag "Ask" nil) string)
  :group 'org-linear)

(defcustom org-linear-capture-assign-to-me t
  "Non-nil means new issues are assigned to you."
  :type 'boolean
  :group 'org-linear)

(defcustom org-linear-capture-timeout 30
  "Seconds to wait for Linear when finishing a capture."
  :type 'natnum
  :group 'org-linear)

(defconst org-linear-capture--buffer "*org-linear capture target*"
  "Buffer that receives captured entries before they go to Linear.")

(defconst org-linear-capture--priorities
  '(("A" . 1) ("B" . 2) ("C" . 3) ("D" . 4))
  "Linear priority for each Org priority letter.")

;;;; Waiting for Linear

(defun org-linear-capture--call (function &rest args)
  "Call the asynchronous FUNCTION with ARGS and wait for its result.
FUNCTION takes ARGS, then a success and an error callback.  Return the
success value, or signal a `user-error' with the error message."
  (let ((done nil) (value nil) (failure nil)
        (deadline (+ (float-time) org-linear-capture-timeout)))
    (apply function
           (append args
                   (list (lambda (&optional v) (setq value v done t))
                         (lambda (m) (setq failure m done t)))))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond (failure (user-error "Linear: %s" failure))
          ((not done) (user-error "Linear did not answer within %d s"
                                  org-linear-capture-timeout))
          (t value))))

;;;; Teams and viewer

(defvar org-linear-capture--teams nil
  "Cached list of teams, as alists with `id', `key' and `name'.")

(defvar org-linear-capture--viewer-id nil
  "Cached id of the user the API key belongs to.")

(defun org-linear-capture--fetch-teams (callback on-error)
  "Fetch the teams, then call CALLBACK with them, or ON-ERROR."
  (if org-linear-capture--teams
      (funcall callback org-linear-capture--teams)
    (org-linear-api-query
     "query { teams(first: 100) { nodes { id key name } } }" nil
     (lambda (data)
       (setq org-linear-capture--teams (alist-get 'nodes (alist-get 'teams data)))
       (funcall callback org-linear-capture--teams))
     on-error)))

(defun org-linear-capture--fetch-viewer (callback on-error)
  "Fetch the id of the API key's user, then call CALLBACK, or ON-ERROR."
  (if org-linear-capture--viewer-id
      (funcall callback org-linear-capture--viewer-id)
    (org-linear-api-query
     "query { viewer { id } }" nil
     (lambda (data)
       (setq org-linear-capture--viewer-id (alist-get 'id (alist-get 'viewer data)))
       (funcall callback org-linear-capture--viewer-id))
     on-error)))

(defun org-linear-capture--team ()
  "Return the team new issues go to, asking if needed."
  (let ((teams (org-linear-capture--call #'org-linear-capture--fetch-teams)))
    (cond
     (org-linear-capture-team
      (or (seq-find (lambda (team) (equal (alist-get 'key team) org-linear-capture-team))
                    teams)
          (user-error "No team with key %s" org-linear-capture-team)))
     ((null (cdr teams)) (car teams))
     (t (let* ((choices (mapcar (lambda (team)
                                  (cons (format "%s  %s" (alist-get 'key team)
                                                (alist-get 'name team))
                                        team))
                                teams)))
          (cdr (assoc (completing-read "Team: " choices nil t) choices)))))))

;;;; Reading the entry

(defun org-linear-capture-parse (text)
  "Parse the captured Org entry TEXT.
Return a plist with :title, :priority (Linear number or nil),
:due (\"YYYY-MM-DD\" or nil) and :description (Markdown)."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward org-heading-regexp nil t)
      (user-error "The capture has no heading to use as title"))
    (beginning-of-line)
    (let* ((cookie (and (looking-at org-complex-heading-regexp) (match-string 3)))
           (letter (and cookie (string-match "\\[#\\(.\\)\\]" cookie)
                        (match-string 1 cookie)))
           (title (string-trim (or (org-get-heading t t t t) "")))
           (deadline (org-entry-get nil "DEADLINE"))
           (body-start (save-excursion
                         (org-end-of-meta-data t)
                         (point)))
           (body (buffer-substring-no-properties body-start (point-max))))
      (when (string-empty-p title)
        (user-error "The issue needs a title"))
      (list :title title
            :priority (cdr (assoc letter org-linear-capture--priorities))
            :due (and deadline (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" deadline)
                      (match-string 0 deadline))
            :description (org-linear-markdown-from-org (string-trim body))))))

(defun org-linear-capture--input (entry team viewer-id)
  "Return the IssueCreateInput plist for ENTRY in TEAM.
VIEWER-ID is the assignee, or nil for none."
  (append (list :teamId (alist-get 'id team)
                :title (plist-get entry :title))
          (unless (string-empty-p (plist-get entry :description))
            (list :description (plist-get entry :description)))
          (when (plist-get entry :priority)
            (list :priority (plist-get entry :priority)))
          (when (plist-get entry :due)
            (list :dueDate (plist-get entry :due)))
          (when viewer-id
            (list :assigneeId viewer-id))))

(defun org-linear-capture--create (input callback on-error)
  "Create an issue from INPUT, then call CALLBACK with it, or ON-ERROR."
  (org-linear-api-query
   "mutation OrgLinearCreate($input: IssueCreateInput!) {
  issueCreate(input: $input) { success issue { id identifier url } }
}"
   (list :input input)
   (lambda (data)
     (let ((result (alist-get 'issueCreate data)))
       (if (alist-get 'success result)
           (funcall callback (alist-get 'issue result))
         (funcall on-error "Linear did not create the issue"))))
   on-error))

;;;; Capture hooks

(defun org-linear-capture--target ()
  "Capture target: an empty scratch buffer, never saved."
  (set-buffer (get-buffer-create org-linear-capture--buffer))
  (let ((inhibit-read-only t)) (erase-buffer))
  (unless (derived-mode-p 'org-mode) (org-mode))
  ;; Org bookmarks the last capture; this buffer has no file to point at.
  (setq-local org-bookmark-names-plist
              (plist-put (copy-sequence org-bookmark-names-plist) :last-capture nil))
  (goto-char (point-min)))

(defun org-linear-capture--finalize ()
  "Create the Linear issue from the capture buffer.
Runs before Org finishes the capture.  On failure it signals an
error, which keeps the capture buffer open with the text."
  (unless org-note-abort
    (let* ((entry (org-linear-capture-parse
                   (buffer-substring-no-properties (point-min) (point-max))))
           (team (org-linear-capture--team))
           (viewer (and org-linear-capture-assign-to-me
                        (org-linear-capture--call #'org-linear-capture--fetch-viewer)))
           (issue (org-linear-capture--call
                   #'org-linear-capture--create
                   (org-linear-capture--input entry team viewer))))
      (kill-new (alist-get 'identifier issue))
      (message "org-linear: created %s (identifier copied)" (alist-get 'identifier issue)))))

;;;###autoload
(defun org-linear-capture-template (&optional keys description)
  "Return an `org-capture-templates' entry that creates a Linear issue.
KEYS defaults to \"l\" and DESCRIPTION to \"Linear issue\"."
  (list (or keys "l") (or description "Linear issue")
        'entry '(function org-linear-capture--target)
        "* %?\n"
        :prepare-finalize #'org-linear-capture--finalize
        :kill-buffer t
        :no-save t))

;;;###autoload
(defun org-linear-capture ()
  "Create a Linear issue with `org-capture'.
Write the title as the heading and the description below it.
\\<org-capture-mode-map>\\[org-capture-finalize] creates the issue, \
\\[org-capture-kill] cancels."
  (interactive)
  (let ((org-capture-templates (list (org-linear-capture-template "l"))))
    (org-capture nil "l")))

(provide 'org-linear-capture)
;;; org-linear-capture.el ends here
