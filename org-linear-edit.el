;;; org-linear-edit.el --- Org buffers for writing Linear text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small edit buffer, in the spirit of `org-edit-special': the text
;; opens in an Org buffer, C-c C-c sends it and C-c C-k throws it away.
;; Sending is asynchronous; the buffer closes only when the save
;; succeeds, so a failed request never loses the text.

;;; Code:

(require 'org)

(defvar-local org-linear-edit--save-function nil
  "Function called with the buffer text, a done and a fail callback.")

(defvar-local org-linear-edit--window-config nil
  "Window configuration to restore when the edit buffer closes.")

(defvar-local org-linear-edit--saving nil
  "Non-nil while a save request is in flight.")

(put 'org-linear-edit--save-function 'permanent-local t)
(put 'org-linear-edit--window-config 'permanent-local t)

(defvar-keymap org-linear-edit-mode-map
  :doc "Keymap for `org-linear-edit-mode'."
  "C-c C-c" #'org-linear-edit-save
  "C-c C-k" #'org-linear-edit-abort)

(define-minor-mode org-linear-edit-mode
  "Minor mode for buffers that edit Linear text in Org.

\\{org-linear-edit-mode-map}"
  :lighter " Linear-Edit"
  :keymap org-linear-edit-mode-map)

(defun org-linear-edit-open (name text header save-function)
  "Open an Org edit buffer called NAME holding TEXT.
HEADER is shown in the header line.  On \\[org-linear-edit-save],
SAVE-FUNCTION is called with the buffer text and two callbacks.  It
calls the first with no arguments once the text is saved, which
closes the buffer, or the second with a message if saving failed,
which keeps the buffer open."
  (let ((config (current-window-configuration))
        (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert text)
      (org-mode)
      (org-linear-edit-mode 1)
      (setq org-linear-edit--save-function save-function
            org-linear-edit--window-config config
            header-line-format
            (substitute-command-keys
             (concat header
                     "  \\<org-linear-edit-mode-map>\\[org-linear-edit-save] save, \
\\[org-linear-edit-abort] cancel")))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun org-linear-edit--close (buffer)
  "Kill the edit BUFFER and restore the window layout."
  (when (buffer-live-p buffer)
    (let ((config (buffer-local-value 'org-linear-edit--window-config buffer)))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (when config (set-window-configuration config)))))

(defun org-linear-edit-save ()
  "Send the edit buffer's text to Linear."
  (interactive)
  (when org-linear-edit--saving
    (user-error "Already saving"))
  (let ((buffer (current-buffer))
        (text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (setq org-linear-edit--saving t)
    (condition-case err
        (funcall org-linear-edit--save-function text
                 (lambda () (org-linear-edit--close buffer))
                 (lambda (msg)
                   (org-linear-edit--failed buffer)
                   (message "org-linear: %s (text kept in %s)" msg
                            (buffer-name buffer))))
      ((debug error)
       (setq org-linear-edit--saving nil)
       (signal (car err) (cdr err))))))

(defun org-linear-edit--failed (buffer)
  "Mark the save in BUFFER as failed, so it can be tried again."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-linear-edit--saving nil))))

(defun org-linear-edit-abort ()
  "Close the edit buffer without sending anything."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard your changes? "))
    (org-linear-edit--close (current-buffer))))

(provide 'org-linear-edit)
;;; org-linear-edit.el ends here
