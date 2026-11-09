;;; org-linear-markdown.el --- Convert Linear Markdown to Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026 joachimvelde

;; Author: joachimvelde
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Linear stores issue descriptions and comments as Markdown.  This file
;; converts the Markdown Linear produces to Org text, without external
;; tools.
;;
;; Block level: ATX headings (shifted below a base outline level),
;; fenced code, block quotes, lists and task lists, tables and
;; horizontal rules.  Inline: bold, italic, strike-through, code, links,
;; images, autolinks and backslash escapes.
;;
;; Anything not recognised passes through as plain text.  The one rule
;; that always holds: no output line starts with "*" followed by a
;; space unless it is a heading we made, so Markdown can never create
;; stray Org headings.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Inline

(defun org-linear-markdown--org-emphasis-re (marker)
  "Return a regexp for Org emphasis with MARKER.
Follows Org's rules: the marker sits after the line start, a space or
an opening bracket, and before a space, punctuation or the line end.
Group 1 is the text before, 2 the content and 3 the text after."
  (let ((m (regexp-quote marker))
        (c (if (equal marker "-") "\\-" marker)))
    (concat "\\(^\\|[[:space:](\"'{-]\\)" m
            "\\([^[:space:]" c "]\\|[^[:space:]" c "][^\n]*?[^[:space:]" c "]\\)"
            m "\\([][:space:].,:!?;'\")}-]\\|$\\)")))

(defun org-linear-markdown--replace-all (regexp function string)
  "Replace REGEXP in STRING with FUNCTION's result until nothing changes.
One pass is not enough: a match takes the character after it, which
the next match may need as its leading character."
  (let ((previous nil))
    (while (not (equal previous string))
      (setq previous string
            string (replace-regexp-in-string regexp function string t t)))
    string))

(defconst org-linear-markdown--placeholder-format "\0%d\0"
  "Format of placeholders that protect converted spans.")

(defun org-linear-markdown--protect (table string)
  "Store STRING in TABLE and return a placeholder for it.
TABLE is a cons whose car is the list of stored strings."
  (push string (car table))
  (format org-linear-markdown--placeholder-format (1- (length (car table)))))

(defun org-linear-markdown--restore (table string)
  "Replace the placeholders in STRING with their values from TABLE."
  (let ((values (vconcat (reverse (car table)))))
    ;; Restored values may hold placeholders of their own (a link whose
    ;; text has code), so repeat until none are left.
    (while (string-match "\0\\([0-9]+\\)\0" string)
      (setq string (replace-regexp-in-string
                    "\0\\([0-9]+\\)\0"
                    (lambda (m)
                      (aref values (string-to-number (match-string 1 m))))
                    string t t)))
    string))

(defun org-linear-markdown--emphasis (string)
  "Convert bold, italic and strike-through markers in STRING."
  (let ((case-fold-search nil))
    ;; Bold first, through a temporary marker, so the italic rule below
    ;; does not read Org's *bold* as Markdown *italic*.
    (setq string (replace-regexp-in-string
                  "\\*\\*\\([^*\n]+?\\)\\*\\*" "\1\\1\1" string))
    (setq string (replace-regexp-in-string
                  "\\(^\\|[^[:alnum:]_]\\)__\\([^_\n]+?\\)__\\([^[:alnum:]_]\\|$\\)"
                  "\\1\1\\2\1\\3" string))
    (setq string (replace-regexp-in-string
                  "\\(^\\|[^*[:alnum:]]\\)\\*\\([^*[:space:]]\\(?:[^*\n]*?[^*[:space:]]\\)?\\)\\*\\([^*[:alnum:]]\\|$\\)"
                  "\\1/\\2/\\3" string))
    (setq string (replace-regexp-in-string
                  "\\(^\\|[^[:alnum:]_]\\)_\\([^_[:space:]]\\(?:[^_\n]*?[^_[:space:]]\\)?\\)_\\([^[:alnum:]_]\\|$\\)"
                  "\\1/\\2/\\3" string))
    (setq string (replace-regexp-in-string
                  "~~\\([^~\n]+?\\)~~" "+\\1+" string))
    (replace-regexp-in-string "\1" "*" string t t)))

(defun org-linear-markdown--guard-literals (string)
  "Guard characters in STRING that are literal in Markdown but markup in Org.
Each such marker gets a zero-width space on both sides.  \"~\" and
\"=\" are always guarded (except the \"~~\" of strike-through);
\"/\" and \"+\" only where they would pair up as Org emphasis."
  (let ((z "\u200b"))
    (setq string (replace-regexp-in-string
                  "\\(^\\|[^~]\\)\\([~=]\\)\\([^~]\\|$\\)"
                  (lambda (m) (concat (match-string 1 m) z (match-string 2 m) z
                                      (match-string 3 m)))
                  string t t))
    ;; A second pass catches markers next to each other ("a=b=c").
    (setq string (replace-regexp-in-string
                  (concat "\\(^\\|[^~" z "]\\)\\([~=]\\)\\([^~" z "]\\|$\\)")
                  (lambda (m) (concat (match-string 1 m) z (match-string 2 m) z
                                      (match-string 3 m)))
                  string t t))
    (dolist (marker '("/" "+"))
      (setq string (org-linear-markdown--replace-all
                    (org-linear-markdown--org-emphasis-re marker)
                    (lambda (m)
                      (concat (match-string 1 m) z marker z (match-string 2 m)
                              z marker z (match-string 3 m)))
                    string)))
    string))

(defun org-linear-markdown-inline (string)
  "Convert the inline Markdown in STRING to Org markup."
  (let ((table (list nil)))
    ;; Inline code: its contents stay literal.
    (setq string (replace-regexp-in-string
                  "`\\([^`\n]+\\)`"
                  (lambda (m)
                    (let ((code (match-string 1 m)))
                      (org-linear-markdown--protect
                       table (if (string-search "~" code)
                                 (format "=%s=" code)
                               (format "~%s~" code)))))
                  string t t))
    ;; Images, then links, then autolinks.  URLs are protected so the
    ;; emphasis rules cannot touch underscores in them.
    (setq string (replace-regexp-in-string
                  "!\\[\\([^]\n]*\\)\\](<?\\([^)<>[:space:]]+\\)>?\\(?: \"[^\"]*\"\\)?)"
                  (lambda (m)
                    (org-linear-markdown--protect
                     table (format "[[%s]]" (match-string 2 m))))
                  string t t))
    (setq string (replace-regexp-in-string
                  "\\[\\([^]\n]+\\)\\](<?\\([^)<>[:space:]]+\\)>?\\(?: \"[^\"]*\"\\)?)"
                  (lambda (m)
                    (let* ((url (match-string 2 m))
                           (text (save-match-data
                                   (org-linear-markdown--emphasis (match-string 1 m)))))
                      (org-linear-markdown--protect
                       table (format "[[%s][%s]]" url
                                     (replace-regexp-in-string "[][]" "" text)))))
                  string t t))
    (setq string (replace-regexp-in-string
                  "<\\(https?://[^>[:space:]]+\\)>"
                  (lambda (m)
                    (org-linear-markdown--protect
                     table (format "[[%s]]" (match-string 1 m))))
                  string t t))
    (setq string (replace-regexp-in-string
                  "https?://[^][()<>[:space:]\0]+"
                  (lambda (m) (org-linear-markdown--protect table m))
                  string t t))
    ;; Escaped punctuation stays literal.  Org emphasis markers get a
    ;; zero-width space on each side, so Org cannot read them as markup.
    (setq string (replace-regexp-in-string
                  "\\\\\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)"
                  (lambda (m)
                    (let ((char (match-string 1 m)))
                      (org-linear-markdown--protect
                       table (if (string-search char "*/_=~+")
                                 (concat "\u200b" char "\u200b")
                               char))))
                  string t t))
    (org-linear-markdown--restore
     table (org-linear-markdown--emphasis (org-linear-markdown--guard-literals string)))))

;;;; Blocks

(defconst org-linear-markdown--fence-re
  "\\`\\([ \t]*\\)\\(```+\\|~~~+\\)[ \t]*\\([^`[:space:]]*\\)"
  "Regexp for the opening line of a fenced code block.")

(defconst org-linear-markdown--table-rule-re
  "\\`[ \t]*|?[ \t]*:?-+:?[ \t]*\\(|[ \t]*:?-+:?[ \t]*\\)*|?[ \t]*\\'"
  "Regexp for the delimiter row under a Markdown table header.")

(defconst org-linear-markdown--heading-re
  "\\`\\(#\\{1,6\\}\\)[ \t]+\\(.*?\\)\\(?:[ \t]+#+\\)?[ \t]*\\'"
  "Regexp for an ATX heading line.")

(defun org-linear-markdown--top-heading (lines)
  "Return the smallest Markdown heading level in LINES outside code, or 1."
  (let ((fence nil) (top nil))
    (dolist (line lines)
      (cond
       (fence (when (string-match-p (concat "\\`[ \t]*" (regexp-quote fence)) line)
                (setq fence nil)))
       ((string-match org-linear-markdown--fence-re line)
        (setq fence (match-string 2 line)))
       ((string-match org-linear-markdown--heading-re line)
        (setq top (min (or top 6) (length (match-string 1 line)))))))
    (or top 1)))

(defun org-linear-markdown--line (line offset)
  "Convert one LINE outside code blocks.
OFFSET is added to a Markdown heading's level to give its Org level."
  (cond
   ;; ATX heading.
   ((string-match org-linear-markdown--heading-re line)
    (concat (make-string (+ offset (length (match-string 1 line))) ?*)
            " " (org-linear-markdown-inline (match-string 2 line))))
   ;; Horizontal rule.
   ((string-match-p "\\`[ \t]*\\([-*_]\\)[ \t]*\\(?:\\1[ \t]*\\)\\{2,\\}\\'" line)
    "-----")
   ;; Table delimiter row.
   ((and (string-search "|" line)
         (string-match-p org-linear-markdown--table-rule-re line))
    "|-")
   ;; List item, bullet or numbered, with an optional task box.
   ((string-match "\\`\\([ \t]*\\)\\([-*+]\\|[0-9]+[.)]\\)[ \t]+\\(\\[[ xX]\\][ \t]+\\)?\\(.*\\)\\'" line)
    (let ((indent (match-string 1 line))
          (bullet (match-string 2 line))
          (box (match-string 3 line))
          (text (match-string 4 line)))
      (concat indent
              (if (member bullet '("*" "+")) "-" bullet)
              " "
              (when box (if (string-match-p "[xX]" box) "[X] " "[ ] "))
              (org-linear-markdown-inline text))))
   (t
    (let ((text (org-linear-markdown-inline line)))
      ;; Plain text that Org would read as structure gets a zero-width
      ;; space in front: a heading ("* "), a keyword ("#+") or a
      ;; comment ("# ").
      (if (string-match-p "\\`\\(\\*+[ \t]\\|#\\+\\|#[ \t]\\|#\\'\\)" text)
          (concat "\u200b" text)
        text)))))

(defun org-linear-markdown-to-org (markdown &optional base-level)
  "Convert MARKDOWN to Org text.
Markdown headings become Org headings below BASE-LEVEL, which
defaults to 0: the shallowest Markdown heading goes one level below
it, deeper ones keep their distance.  Return a string without a trailing
newline, or the empty string when MARKDOWN is nil or blank."
  (let ((base-level (or base-level 0))
        (lines (split-string (string-trim-right
                              (replace-regexp-in-string "\r" "" (or markdown "")))
                             "\n"))
        (offset nil)
        (out nil)
        (fence nil)
        (quote-open nil))
    (setq offset (- (1+ base-level) (org-linear-markdown--top-heading lines)))
    (dolist (line lines)
      (cond
       ;; Inside a fenced block: copy lines until the closing fence.
       (fence
        (if (string-match-p (concat "\\`[ \t]*" (regexp-quote fence) "[ \t]*\\'") line)
            (progn (push "#+end_src" out) (setq fence nil))
          ;; A line starting with "*" or "#+" would break the block.
          (push (if (string-match-p "\\`[ \t]*\\(\\*\\|#\\+\\)" line)
                    (concat "," line)
                  line)
                out)))
       ((string-match org-linear-markdown--fence-re line)
        (when quote-open (push "#+end_quote" out) (setq quote-open nil))
        (setq fence (match-string 2 line))
        (let ((lang (match-string 3 line)))
          (push (if (string-empty-p lang) "#+begin_src" (concat "#+begin_src " lang))
                out)))
       ;; Block quote lines, grouped into one quote block.
       ((string-match "\\`[ \t]*>[ \t]?\\(.*\\)\\'" line)
        (unless quote-open (push "#+begin_quote" out) (setq quote-open t))
        (push (org-linear-markdown--line (match-string 1 line) offset) out))
       (t
        (when quote-open (push "#+end_quote" out) (setq quote-open nil))
        (push (org-linear-markdown--line line offset) out))))
    (when quote-open (push "#+end_quote" out))
    (when fence (push "#+end_src" out))
    (string-trim (string-join (nreverse out) "\n") "\n+" "\n+")))

;;;; Org to Markdown

;; The inverse of the converter above, for text edited in Org and sent
;; back to Linear.  It reverses each rule above, so a description that
;; is converted to Org and back without edits keeps its meaning.

(defconst org-linear-markdown--zwsp "\u200b"
  "Zero-width space, used by the converter above to guard markup.")

(defun org-linear-markdown--org-emphasis (string marker open close)
  "Replace Org emphasis MARKER in STRING with OPEN and CLOSE."
  (org-linear-markdown--replace-all
   (org-linear-markdown--org-emphasis-re marker)
   (lambda (match)
     (concat (match-string 1 match) open (match-string 2 match) close
             (match-string 3 match)))
   string))

(defconst org-linear-markdown--image-re
  "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|svg\\)\\(?:[?#].*\\)?\\'"
  "Regexp for URLs that point at images.")

(defun org-linear-markdown-inline-from-org (string)
  "Convert the inline Org markup in STRING to Markdown."
  (let ((table (list nil))
        (z org-linear-markdown--zwsp))
    ;; Guarded markers are literal characters.  Only "*" and "_" are
    ;; markup in Markdown and need a backslash.
    (setq string (replace-regexp-in-string
                  (concat z "\\([*/_=~+]\\)" z)
                  (lambda (m)
                    (let ((char (match-string 1 m)))
                      (org-linear-markdown--protect
                       table (if (member char '("*" "_")) (concat "\\" char) char))))
                  string t t))
    ;; Verbatim and code.
    (dolist (marker '("~" "="))
      (setq string (org-linear-markdown--replace-all
                    (org-linear-markdown--org-emphasis-re marker)
                    (lambda (m)
                      (concat (match-string 1 m)
                              (org-linear-markdown--protect
                               table (concat "`" (match-string 2 m) "`"))
                              (match-string 3 m)))
                    string)))
    ;; Links, with and without a description.
    (setq string (replace-regexp-in-string
                  "\\[\\[\\([^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]"
                  (lambda (m)
                    (let ((url (match-string 1 m))
                          (text (match-string 2 m)))
                      (org-linear-markdown--protect
                       table (format "[%s](%s)"
                                     (save-match-data
                                       (org-linear-markdown-inline-from-org text))
                                     url))))
                  string t t))
    (setq string (replace-regexp-in-string
                  "\\[\\[\\([^]\n]+\\)\\]\\]"
                  (lambda (m)
                    (let ((url (match-string 1 m)))
                      (org-linear-markdown--protect
                       table (cond ((save-match-data
                                      (string-match-p org-linear-markdown--image-re url))
                                    (format "![](%s)" url))
                                   ((string-match-p "\\`https?://" url) (format "<%s>" url))
                                   (t url)))))
                  string t t))
    (setq string (replace-regexp-in-string
                  "https?://[^][()<>[:space:]\0]+"
                  (lambda (m) (org-linear-markdown--protect table m))
                  string t t))
    ;; Bold goes through a marker so the italic rule cannot see it.
    (setq string (org-linear-markdown--org-emphasis string "*" "\1\1" "\1\1"))
    (setq string (org-linear-markdown--org-emphasis string "/" "*" "*"))
    (setq string (org-linear-markdown--org-emphasis string "+" "~~" "~~"))
    (setq string (replace-regexp-in-string "\1" "*" string t t))
    (org-linear-markdown--restore table (string-replace z "" string))))

(defun org-linear-markdown--table-rule (cells)
  "Return a Markdown table delimiter row for CELLS columns."
  (concat "|" (mapconcat #'identity (make-list (max cells 1) " --- ") "|") "|"))

(defun org-linear-markdown--line-from-org (line offset)
  "Convert one Org LINE outside blocks to Markdown.
OFFSET is added to an Org heading's level to give its Markdown level."
  (let ((z org-linear-markdown--zwsp))
    (cond
     ((string-match "\\`\\(\\*+\\)[ \t]+\\(.*?\\)[ \t]*\\'" line)
      (concat (make-string (min 6 (max 1 (+ offset (length (match-string 1 line))))) ?#)
              " " (org-linear-markdown-inline-from-org (match-string 2 line))))
     ((string-match-p "\\`[ \t]*-\\{5,\\}[ \t]*\\'" line) "---")
     ((string-match "\\`\\([ \t]*\\)\\([-+]\\|[0-9]+[.)]\\)[ \t]+\\(\\[[ X-]\\][ \t]+\\)?\\(.*\\)\\'" line)
      (let ((indent (match-string 1 line))
            (bullet (match-string 2 line))
            (box (match-string 3 line))
            (text (match-string 4 line)))
        (concat indent (if (equal bullet "+") "-" bullet) " "
                (when box (if (string-search "X" box) "[x] " "[ ] "))
                (org-linear-markdown-inline-from-org text))))
     ;; An Org comment line, or text the Markdown converter guarded
     ;; because it looked like Org structure: keep it as literal text.
     ((or (string-match (concat "\\`" z "\\([#*]\\)") line)
          (string-match "\\`\\(#\\)\\(?:[ \t+]\\|\\'\\)" line))
      (concat "\\" (match-string 1 line)
              (org-linear-markdown-inline-from-org (substring line (match-end 1)))))
     (t (org-linear-markdown-inline-from-org line)))))

(defun org-linear-markdown-from-org (org &optional offset)
  "Convert the Org text ORG to Markdown.
OFFSET is added to Org heading levels to give Markdown levels; it
defaults to 0, so \"*\" becomes \"#\".  Return a string without a
trailing newline."
  (let ((offset (or offset 0))
        (out nil)
        (block nil)
        (quote-open nil)
        (cells 0))
    (dolist (line (split-string (or org "") "\n"))
      (let ((case-fold-search t))
        (cond
         ;; Inside a source or example block: copy, undoing Org's comma
         ;; escapes.
         (block
          (if (string-match-p (concat "\\`[ \t]*#\\+end_" block "\\b") line)
              (progn (push "```" out) (setq block nil))
            (push (if (string-match "\\`\\([ \t]*\\),\\(\\*\\|#\\+\\)" line)
                      (concat (match-string 1 line) (substring line (1+ (match-end 1))))
                    line)
                  out)))
         ((string-match "\\`[ \t]*#\\+begin_\\(src\\|example\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?" line)
          (setq block (downcase (match-string 1 line)))
          (push (concat "```" (if (equal block "src") (or (match-string 2 line) "") "")) out))
         ((string-match-p "\\`[ \t]*#\\+begin_quote" line) (setq quote-open t))
         ((string-match-p "\\`[ \t]*#\\+end_quote" line) (setq quote-open nil))
         ;; Table rule: Markdown needs one delimiter per column.
         ((string-match-p "\\`[ \t]*|-" line)
          (push (org-linear-markdown--table-rule cells) out))
         (t
          (when (string-match-p "\\`[ \t]*|" line)
            (let ((row (string-trim line)))
              (setq cells (- (cl-count ?| row)
                             (if (string-suffix-p "|" row) 1 0)))))
          (let ((md (org-linear-markdown--line-from-org line offset)))
            (push (if quote-open (string-trim-right (concat "> " md)) md) out))))))
    (when block (push "```" out))
    (string-trim (string-join (nreverse out) "\n") "\n+" "\n+")))

(provide 'org-linear-markdown)
;;; org-linear-markdown.el ends here
