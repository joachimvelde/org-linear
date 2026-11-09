;;; org-linear.el --- Linear issues in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; URL: https://github.com/joachimvelde/org-linear
;; Keywords: outlines, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-linear shows Linear issues as Org mode documents.
;;
;; M-x org-linear-list-issues opens the default view (your open issues)
;; in an Org buffer.  With a prefix argument it asks which view from
;; `org-linear-views' to open.  Issues are grouped by workflow state;
;; Linear states map to Org TODO keywords, priorities to priority
;; cookies, labels to tags and due dates to deadlines.
;;
;; M-x org-linear-open-issue opens one issue with its description,
;; sub-issues, attachments and comments.  Descriptions and comments
;; are converted from Markdown to Org.
;;
;; In both kinds of buffer:
;;   RET      open the issue at point, or follow the link at point
;;   g        refresh (gr with evil)
;;   w        copy the identifier of the issue at point (gy with evil)
;;   q        quit
;;   C-c C-o  open the link at point, for example the issue in the browser
;;   C-c C-t  change the state of the issue at point in Linear
;;   C-c ,    change the priority of the issue at point in Linear
;;   C-c '    edit the description in an Org buffer
;;   C-c C-x t  edit the title
;;   C-c C-z  add a comment, or reply to the comment at point
;;   C-c C-x C-w  delete the comment at point
;;
;; The buffers are read-only: changes go to Linear, then the buffer is
;; fetched again.
;;
;; See README.org for setup.

;;; Code:

(require 'org)
(require 'org-element)
(require 'thingatpt)
(require 'org-linear-api)
(require 'org-linear-edit)
(require 'org-linear-markdown)
(require 'org-linear-render)

(defcustom org-linear-views
  '(("My open issues"
     :filter (:assignee (:isMe (:eq t))
			:state (:type (:nin ["completed" "canceled" "duplicate"])))))
  "Issue views offered by `org-linear-list-issues'.
Each entry is (NAME :filter FILTER).  FILTER is a plist in the shape
of Linear's IssueFilter GraphQL input; vectors become JSON arrays and
t becomes true.  The first entry is the default view.

Example, all open issues of team \"ENG\":

  (\"ENG open\" :filter (:team (:key (:eq \"ENG\"))
                       :state (:type (:nin [\"completed\" \"canceled\"]))))"
  :type '(alist :key-type string :value-type (plist :key-type symbol))
  :group 'org-linear)

;;;; Buffer state

(defvar-local org-linear--view nil
  "The `org-linear-views' entry shown in the current buffer.")

(defvar-local org-linear--issue-id nil
  "Identifier of the issue shown in the current issue buffer.")

;; `org-mode' clears buffer-local variables when it starts, and every
;; render restarts it so it reads the new #+TODO line.
(put 'org-linear--view 'permanent-local t)
(put 'org-linear--issue-id 'permanent-local t)

(defvar-keymap org-linear-view-mode-map
  :doc "Keymap for `org-linear-view-mode'."
  "RET" #'org-linear-open-at-point
  "g" #'org-linear-refresh
  "w" #'org-linear-copy-identifier
  "q" #'quit-window
  "C-c C-t" #'org-linear-set-state
  "C-c ," #'org-linear-set-priority
  "C-c '" #'org-linear-edit-description
  "C-c C-x t" #'org-linear-edit-title
  "C-c C-z" #'org-linear-add-comment
  "C-c C-x C-w" #'org-linear-delete-comment)

(define-minor-mode org-linear-view-mode
  "Minor mode for read-only Org buffers that show Linear data.

\\{org-linear-view-mode-map}"
  :lighter " Linear"
  :keymap org-linear-view-mode-map
  (when org-linear-view-mode
    (setq buffer-read-only t)))

(declare-function evil-define-minor-mode-key "evil-core")

;; Evil's normal state shadows minor mode keymaps, and plain "g" and "w"
;; are core motions there, so evil users get prefixed keys instead.
(with-eval-after-load 'evil
  (dolist (binding '(("RET" . org-linear-open-at-point)
                     ("gr" . org-linear-refresh)
                     ("gy" . org-linear-copy-identifier)
                     ("q" . quit-window)))
    (evil-define-minor-mode-key 'normal 'org-linear-view-mode
				(kbd (car binding)) (cdr binding))))

;;;; Helpers

(defun org-linear--view-name (view)
  "Return the name of VIEW."
  (car view))

(defun org-linear--read-view ()
  "Ask for a view from `org-linear-views'."
  (let ((name (completing-read "Linear view: " (mapcar #'car org-linear-views)
                               nil t nil nil (caar org-linear-views))))
    (assoc name org-linear-views)))

(defun org-linear--identifier-at-point (&optional inherit)
  "Return the Linear identifier of the issue at point, or nil.
With INHERIT, also look at the enclosing headings, so point in a
comment or the description gives the issue it belongs to."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (unless (org-before-first-heading-p)
        (org-back-to-heading t)
        (org-entry-get nil "LINEAR_IDENTIFIER" inherit)))))

(defun org-linear--goto-identifier (identifier)
  "Move point to the heading of the issue IDENTIFIER, if present."
  (when identifier
    (let ((pos (org-find-property "LINEAR_IDENTIFIER" identifier)))
      (when pos
        (goto-char pos)
        (org-fold-show-context)
        t))))

(defun org-linear--prepare-buffer (name placeholder)
  "Return the buffer NAME, showing PLACEHOLDER text if it is new."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          (insert placeholder "\n"))
        (org-mode)
        (org-linear-view-mode 1)))
    buffer))

(defun org-linear--replace-contents (buffer text)
  "Replace BUFFER's text with the Org TEXT, keeping point near its place."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (identifier (org-linear--identifier-at-point))
          (line (line-number-at-pos))
          (windows (get-buffer-window-list buffer nil t)))
      (erase-buffer)
      (insert text)
      ;; Org reads the #+TODO and #+PRIORITIES lines when the mode
      ;; starts, so the text must be in place first.
      (org-mode)
      (org-linear-view-mode 1)
      (org-align-tags t)
      (set-buffer-modified-p nil)
      (unless (org-linear--goto-identifier identifier)
        (goto-char (point-min))
        (forward-line (1- line)))
      (dolist (window windows)
        (set-window-point window (point))))))

(defun org-linear--report (message)
  "Show the error MESSAGE from a failed request."
  (message "org-linear: %s" message))

;;;; Issue lists

(defun org-linear--fetch-view (buffer view)
  "Fetch the issues for VIEW and render them into BUFFER."
  (message "org-linear: fetching %s..." (org-linear--view-name view))
  (org-linear-api-issues
   (plist-get (cdr view) :filter)
   (lambda (issues)
     (when (buffer-live-p buffer)
       (org-linear--replace-contents
        buffer (org-linear-render-view (org-linear--view-name view) issues))
       (message "org-linear: %d issue%s" (length issues)
                (if (= (length issues) 1) "" "s"))))
   #'org-linear--report))

;;;###autoload
(defun org-linear-list-issues (&optional view)
  "Show the Linear issues of VIEW in an Org buffer.
VIEW is an entry of `org-linear-views'.  Interactively, use the first
view, or ask with a prefix argument."
  (interactive (list (if current-prefix-arg
                         (org-linear--read-view)
                       (car org-linear-views))))
  (unless view
    (user-error "No views defined in `org-linear-views'"))
  (let ((buffer (org-linear--prepare-buffer
                 (format "*Linear: %s*" (org-linear--view-name view))
                 (format "Fetching %s from Linear..." (org-linear--view-name view)))))
    (with-current-buffer buffer
      (setq org-linear--view view))
    (pop-to-buffer-same-window buffer)
    (org-linear--fetch-view buffer view)))

;;;; Single issues

(defun org-linear--fetch-issue (buffer id)
  "Fetch the issue ID and render it into BUFFER."
  (message "org-linear: fetching %s..." id)
  (org-linear-api-issue
   id
   (lambda (issue)
     (when (buffer-live-p buffer)
       (org-linear--replace-contents buffer (org-linear-render-issue-document issue))
       (message "org-linear: %s" (alist-get 'identifier issue))))
   #'org-linear--report))

(defun org-linear--read-identifier ()
  "Ask for an issue identifier, defaulting to the one at point."
  (let* ((default (or (org-linear--identifier-at-point)
                      (when-let* ((word (thing-at-point 'symbol t)))
                        (and (string-match-p "\\`[A-Za-z][A-Za-z0-9]*-[0-9]+\\'" word)
                             (upcase word)))))
         (input (read-string (format-prompt "Linear issue" default) nil nil default)))
    (if (string-empty-p (string-trim input))
        (user-error "No issue given")
      (upcase (string-trim input)))))

;;;###autoload
(defun org-linear-open-issue (id)
  "Show the Linear issue ID in an Org buffer.
ID is an identifier such as \"ENG-12\".  Interactively, use the issue
at point, or ask."
  (interactive (list (org-linear--read-identifier)))
  (let ((buffer (org-linear--prepare-buffer
                 (format "*Linear: %s*" id)
                 (format "Fetching %s from Linear..." id))))
    (with-current-buffer buffer
      (setq org-linear--issue-id id))
    (pop-to-buffer-same-window buffer)
    (org-linear--fetch-issue buffer id)))

(defun org-linear-open-at-point ()
  "Follow the Org link at point, or open the issue of the heading at point."
  (interactive)
  (if (memq (org-element-type (org-element-context)) '(link))
      (org-open-at-point)
    (let ((id (org-linear--identifier-at-point)))
      (cond
       ((null id) (user-error "No Linear issue at point"))
       ((equal id org-linear--issue-id) (org-linear-refresh))
       (t (org-linear-open-issue id))))))

;;;; Editing

(defconst org-linear--priorities
  '(("Urgent" . 1) ("High" . 2) ("Medium" . 3) ("Low" . 4) ("No priority" . 0))
  "Linear priority names and values.")

(defun org-linear--issue-at-point ()
  "Return (UUID . IDENTIFIER) of the issue at point, or signal a `user-error'.
Point in a comment or the description gives the issue it belongs to."
  (let ((uuid (and (derived-mode-p 'org-mode)
                   (not (org-before-first-heading-p))
                   (save-excursion
                     (org-back-to-heading t)
                     (org-entry-get nil "LINEAR_ID" t))))
        (identifier (org-linear--identifier-at-point t)))
    (unless uuid
      (user-error "No Linear issue at point"))
    (cons uuid identifier)))

(defun org-linear--after-update (buffer what issue)
  "Report that WHAT changed on ISSUE, then refresh BUFFER."
  (message "org-linear: %s %s" (alist-get 'identifier issue) what)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (org-linear-refresh))))

(defun org-linear--state-candidates (states)
  "Return completion candidates (LABEL . STATE) for STATES, in workflow order."
  (mapcar (lambda (state)
            (cons (format "%-12s %s"
                          (or (cdr (assoc (alist-get 'type state) org-linear-state-keywords))
                              "")
                          (alist-get 'name state))
                  state))
          (sort (copy-sequence states)
                (lambda (a b)
                  (org-linear-render--rank<
                   (org-linear-render--state-rank `((state . ,a)))
                   (org-linear-render--state-rank `((state . ,b))))))))

(defun org-linear-set-state (&optional state-name)
  "Move the issue at point to another workflow state in Linear.
Offer the states of the issue's team.  STATE-NAME picks a state by
name without asking, which is useful from Lisp."
  (interactive)
  (pcase-let ((`(,uuid . ,identifier) (org-linear--issue-at-point))
              (buffer (current-buffer)))
    (org-linear-api-team-states
     uuid
     (lambda (states)
       (let* ((candidates (org-linear--state-candidates states))
              (state (if state-name
                         (seq-find (lambda (s) (equal (alist-get 'name s) state-name))
                                   states)
                       (cdr (assoc (completing-read
                                    (format "State for %s: " identifier)
                                    candidates nil t)
                                   candidates)))))
         (unless state
           (user-error "No state named %s" state-name))
         (org-linear-api-update-issue
          uuid (list :stateId (alist-get 'id state))
          (lambda (issue)
            (org-linear--after-update
             buffer (format "is now %s" (alist-get 'name state)) issue))
          #'org-linear--report)))
     #'org-linear--report
     (org-entry-get nil "LINEAR_TEAM" t))))

(defun org-linear-set-priority (&optional priority)
  "Change the priority of the issue at point in Linear.
PRIORITY is a number: 0 none, 1 Urgent, 2 High, 3 Medium, 4 Low.
Interactively, ask."
  (interactive
   (list (cdr (assoc (completing-read "Priority: " org-linear--priorities nil t)
                     org-linear--priorities))))
  (pcase-let ((`(,uuid . ,_) (org-linear--issue-at-point))
              (buffer (current-buffer)))
    (unless (memq priority '(0 1 2 3 4))
      (user-error "Priority must be 0-4, not %S" priority))
    (org-linear-api-update-issue
     uuid (list :priority priority)
     (lambda (issue)
       (org-linear--after-update
        buffer (format "has priority %s" (car (rassq priority org-linear--priorities)))
        issue))
     #'org-linear--report)))

;;;; Text: description, title, comments

(defun org-linear--title-at-point ()
  "Return the title of the issue heading at point, without the identifier."
  (save-excursion
    (org-back-to-heading t)
    (while (and (not (org-entry-get nil "LINEAR_ID"))
                (org-up-heading-safe)))
    (let ((heading (org-get-heading t t t t)))
      (string-trim
       (replace-regexp-in-string "\\`\\[\\[[^]]*\\]\\[[^]]*\\]\\][ \t]*" "" heading)))))

(defun org-linear-edit-description ()
  "Edit the description of the issue at point in an Org buffer.
\\<org-linear-edit-mode-map>\\[org-linear-edit-save] converts the text to \
Markdown and saves it in Linear.  If the
description changed in Linear since the buffer opened, ask before
overwriting it."
  (interactive)
  (pcase-let ((`(,uuid . ,identifier) (org-linear--issue-at-point))
              (source (current-buffer)))
    (org-linear-api-issue-text
     uuid
     (lambda (issue)
       (let* ((original (or (alist-get 'description issue) ""))
              (offset (1- (org-linear-markdown--top-heading (split-string original "\n")))))
         (org-linear-edit-open
          (format "*Linear description: %s*" identifier)
          (org-linear-markdown-to-org original 0)
          (format "Description of %s." identifier)
          (lambda (text done fail)
            (let ((markdown (org-linear-markdown-from-org text offset)))
              (org-linear-api-issue-text
               uuid
               (lambda (current)
                 (if (and (not (equal (or (alist-get 'description current) "") original))
                          (not (yes-or-no-p
                                (format "%s's description changed in Linear since you \
opened it.  Overwrite? " identifier))))
                     (funcall fail "Not saved")
                   (org-linear-api-update-issue
                    uuid (list :description markdown)
                    (lambda (updated)
                      (funcall done)
                      (org-linear--after-update source "description saved" updated))
                    fail)))
               fail))))))
     #'org-linear--report)))

(defun org-linear-edit-title (title)
  "Set the title of the issue at point to TITLE in Linear.
Interactively, edit the current title in the minibuffer."
  (interactive
   (list (read-string "Title: " (org-linear--title-at-point))))
  (pcase-let ((`(,uuid . ,_) (org-linear--issue-at-point))
              (buffer (current-buffer)))
    (when (string-empty-p (string-trim title))
      (user-error "The title cannot be empty"))
    (org-linear-api-update-issue
     uuid (list :title (string-trim title))
     (lambda (issue) (org-linear--after-update buffer "title saved" issue))
     #'org-linear--report)))

(defun org-linear--comment-at-point ()
  "Return (ID AUTHOR ROOT-ID) for the comment at point, or nil.
ROOT-ID is the comment that starts the thread: Linear threads have one
level of replies, so a reply to a reply goes to the thread's root."
  (when (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when-let* ((id (org-entry-get nil "LINEAR_COMMENT_ID")))
        (let ((author (replace-regexp-in-string
                       "[ \t]*\\[[^]]*\\][ \t]*\\'" "" (org-get-heading t t t t)))
              (root id))
          (while (and (org-up-heading-safe)
                      (when-let* ((parent (org-entry-get nil "LINEAR_COMMENT_ID")))
                        (setq root parent))))
          (list id author root))))))

(defun org-linear-add-comment (&optional top-level)
  "Write a comment on the issue at point in an Org buffer.
On a comment, the new comment replies to its thread.  With prefix
argument TOP-LEVEL, always start a new thread."
  (interactive "P")
  (pcase-let* ((`(,uuid . ,identifier) (org-linear--issue-at-point))
               (comment (unless top-level (org-linear--comment-at-point)))
               (source (current-buffer)))
    (org-linear-edit-open
     (format "*Linear comment: %s*" identifier)
     ""
     (if comment
         (format "Reply to %s on %s." (nth 1 comment) identifier)
       (format "New comment on %s." identifier))
     (lambda (text done fail)
       (if (string-empty-p text)
           (funcall fail "The comment is empty")
         (org-linear-api-create-comment
          uuid (org-linear-markdown-from-org text) (nth 2 comment)
          (lambda (_comment)
            (funcall done)
            (org-linear--after-update source "comment added"
                                      `((identifier . ,identifier))))
          fail))))))

(defun org-linear-delete-comment ()
  "Delete the comment at point in Linear, after confirming."
  (interactive)
  (pcase-let* ((`(,id ,author ,_) (or (org-linear--comment-at-point)
                                      (user-error "No comment at point")))
               (identifier (org-linear--identifier-at-point t))
               (buffer (current-buffer)))
    (when (yes-or-no-p (format "Delete the comment by %s? " author))
      (org-linear-api-delete-comment
       id
       (lambda ()
         (org-linear--after-update buffer "comment deleted"
                                   `((identifier . ,identifier))))
       #'org-linear--report))))

;;;; Common commands

(defun org-linear-refresh ()
  "Fetch the contents of the current org-linear buffer again."
  (interactive)
  (cond
   (org-linear--issue-id (org-linear--fetch-issue (current-buffer) org-linear--issue-id))
   (org-linear--view (org-linear--fetch-view (current-buffer) org-linear--view))
   (t (user-error "Not in an org-linear buffer"))))

(defun org-linear-copy-identifier ()
  "Copy the identifier of the issue at point to the kill ring."
  (interactive)
  (let ((identifier (org-linear--identifier-at-point t)))
    (unless identifier
      (user-error "No Linear issue at point"))
    (kill-new identifier)
    (message "Copied %s" identifier)))

(provide 'org-linear)
;;; org-linear.el ends here
