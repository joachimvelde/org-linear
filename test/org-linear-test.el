;;; org-linear-test.el --- Tests for org-linear -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.  No test touches the network: the HTTP layer
;; (`org-linear-api--post') is replaced with a stub.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-linear)

;; Stubbing primitives such as `yes-or-no-p' with `cl-letf' makes native
;; Emacs compile a trampoline, which needs a working C toolchain.
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defun org-linear-test--issue (&rest overrides)
  "Return a sample issue alist with OVERRIDES, a plist of key/value."
  (let ((issue (list (cons 'id "uuid-1")
                     (cons 'identifier "ENG-1")
                     (cons 'title "Fix the thing")
                     (cons 'url "https://linear.app/acme/issue/ENG-1")
                     (cons 'priority 2)
                     (cons 'updatedAt "2026-11-01T10:00:00.000Z")
                     (cons 'dueDate nil)
                     (cons 'state '((name . "In Progress") (type . "started") (position . 2)))
                     (cons 'team '((key . "ENG") (name . "Engineering")))
                     (cons 'assignee '((name . "Ada")))
                     (cons 'project nil)
                     (cons 'labels '((nodes . (((name . "Bug")) ((name . "Tech debt")))))))))
    (cl-loop for (key value) on overrides by #'cddr
             do (setf (alist-get key issue) value))
    issue))

;;;; Rendering

(ert-deftest org-linear-test-heading ()
  (should (equal (org-linear-render-heading (org-linear-test--issue) 2)
                 "** STARTED [#B] [[https://linear.app/acme/issue/ENG-1][ENG-1]] Fix the thing :Bug:Tech_debt:")))

(ert-deftest org-linear-test-heading-without-priority-or-labels ()
  (let ((issue (org-linear-test--issue 'priority 0 'labels '((nodes)))))
    (should (equal (org-linear-render-heading issue 1)
                   "* STARTED [[https://linear.app/acme/issue/ENG-1][ENG-1]] Fix the thing"))))

(ert-deftest org-linear-test-heading-cleans-title ()
  (let ((issue (org-linear-test--issue 'title "  two\nlines  ")))
    (should (string-suffix-p "two lines :Bug:Tech_debt:"
                             (org-linear-render-heading issue 1)))))

(ert-deftest org-linear-test-priority-cookies ()
  (should (equal (mapcar (lambda (p) (org-linear-render--priority
                                      (org-linear-test--issue 'priority p)))
                         '(0 1 2 3 4))
                 '(nil "[#A]" "[#B]" "[#C]" "[#D]"))))

(ert-deftest org-linear-test-keywords ()
  (dolist (case '(("triage" . "TRIAGE") ("backlog" . "BACKLOG")
                  ("unstarted" . "TODO") ("started" . "STARTED")
                  ("completed" . "DONE") ("canceled" . "CANCELED")
                  ("duplicate" . "DUPLICATE")))
    (should (equal (org-linear-render--keyword
                    (org-linear-test--issue 'state `((name . "x") (type . ,(car case)))))
                   (cdr case)))))

(ert-deftest org-linear-test-tags ()
  (should (equal (org-linear-render--tag "  Needs review! ") "Needs_review"))
  (should (equal (org-linear-render--tag "p0") "p0"))
  (should (equal (org-linear-render--tag "!!!") "")))

(ert-deftest org-linear-test-deadline-and-properties ()
  (let ((text (org-linear-render-issue (org-linear-test--issue 'dueDate "2026-11-12") 2)))
    (should (string-match-p "^DEADLINE: <2026-11-12 [[:alpha:]]+>$" text))
    (should (string-match-p "^:LINEAR_ID: uuid-1$" text))
    (should (string-match-p "^:LINEAR_STATE: In Progress$" text))
    ;; Empty fields are left out.
    (should-not (string-match-p "LINEAR_PROJECT" text))))

(ert-deftest org-linear-test-groups-order ()
  (let* ((org-linear-group-by 'state)
         (issues (list (org-linear-test--issue 'identifier "A" 'state '((name . "Backlog") (type . "backlog") (position . 0)))
                       (org-linear-test--issue 'identifier "B" 'priority 3)
                       (org-linear-test--issue 'identifier "C" 'priority 1)
                       (org-linear-test--issue 'identifier "D" 'priority 0)
                       (org-linear-test--issue 'identifier "E" 'state '((name . "Todo") (type . "unstarted") (position . 1)))))
         (groups (org-linear-render-groups issues)))
    (should (equal (mapcar #'car groups) '("In Progress" "Todo" "Backlog")))
    ;; Within a group: Urgent first, no priority last.
    (should (equal (mapcar (lambda (i) (alist-get 'identifier i)) (cdar groups))
                   '("C" "B" "D")))))

(ert-deftest org-linear-test-ungrouped ()
  (let* ((org-linear-group-by nil)
         (text (org-linear-render-view "V" (list (org-linear-test--issue)) 0)))
    (should (string-match-p "^\\* STARTED " text))))

(ert-deftest org-linear-test-view-parses-as-org ()
  "The rendered view must give Org the right keywords and priorities."
  (let ((text (org-linear-render-view
               "Mine" (list (org-linear-test--issue)
                            (org-linear-test--issue 'identifier "ENG-2" 'priority 4
                                                    'state '((name . "Done") (type . "completed") (position . 9))))
               0)))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (goto-char (point-min))
      (should (re-search-forward "ENG-1" nil t))
      (should (equal (org-get-todo-state) "STARTED"))
      (should (equal (org-entry-get nil "PRIORITY") "B"))
      (should (equal (org-get-tags nil t) '("Bug" "Tech_debt")))
      (should (equal (org-entry-get nil "LINEAR_IDENTIFIER") "ENG-1"))
      (should (re-search-forward "ENG-2" nil t))
      (should (org-entry-is-done-p))
      (should (equal (org-entry-get nil "PRIORITY") "D")))))

;;;; API

(defmacro org-linear-test--with-responses (responses &rest body)
  "Run BODY with `org-linear-api--post' answering from RESPONSES.
Each response is a `data' alist, or (:error MESSAGE).  The request
bodies sent are collected in the variable `requests'."
  (declare (indent 1))
  `(let ((queue ,responses)
         (requests nil)
         (org-linear-api-key "test-key"))
     (cl-letf (((symbol-function 'org-linear-api--post)
                (lambda (body callback on-error)
                  (push (json-parse-string body :object-type 'alist) requests)
                  (let ((response (pop queue)))
                    (if (eq (car-safe response) :error)
                        (funcall on-error (cadr response))
                      (funcall callback response))))))
       ,@body)))

(defun org-linear-test--page (ids next)
  "Return an issues `data' alist with IDS and NEXT cursor."
  `((issues . ((nodes . ,(mapcar (lambda (id) `((identifier . ,id))) ids))
               (pageInfo . ((hasNextPage . ,(and next t)) (endCursor . ,next)))))))

(ert-deftest org-linear-test-pagination ()
  (org-linear-test--with-responses
      (list (org-linear-test--page '("A" "B") "c1")
            (org-linear-test--page '("C") nil))
    (let (result)
      (org-linear-api-issues '(:assignee (:isMe (:eq t))) (lambda (issues) (setq result issues)))
      (should (equal (mapcar (lambda (i) (alist-get 'identifier i)) result) '("A" "B" "C")))
      (setq requests (nreverse requests))
      (should (eq (alist-get 'after (alist-get 'variables (nth 0 requests))) :null))
      (should (equal (alist-get 'after (alist-get 'variables (nth 1 requests))) "c1"))
      (should (eq (alist-get 'eq (alist-get 'isMe (alist-get 'assignee
                                                             (alist-get 'filter (alist-get 'variables (nth 0 requests))))))
                  t)))))

(ert-deftest org-linear-test-max-issues ()
  (org-linear-test--with-responses
      (list (org-linear-test--page '("A" "B") "c1")
            (org-linear-test--page '("C" "D") "c2"))
    (let ((org-linear-api-max-issues 3)
          result)
      (org-linear-api-issues nil (lambda (issues) (setq result issues)))
      (should (= (length result) 3))
      (should (= (length requests) 2)))))

(ert-deftest org-linear-test-error-callback ()
  (org-linear-test--with-responses (list '(:error "boom"))
    (let (got)
      (org-linear-api-issues nil #'ignore (lambda (msg) (setq got msg)))
      (should (equal got "boom")))))

(ert-deftest org-linear-test-parse-graphql-errors ()
  (with-temp-buffer
    (insert "{\"errors\":[{\"message\":\"Query too complex\"}]}")
    (setq-local url-http-end-of-headers (point-min))
    (setq-local url-http-response-status 400)
    (should (equal (cadr (should-error (org-linear-api--parse-response nil)
                                       :type 'org-linear-api-error))
                   "Query too complex"))))

(ert-deftest org-linear-test-parse-data ()
  (with-temp-buffer
    (insert (encode-coding-string "{\"data\":{\"viewer\":{\"name\":\"Åse\"}}}" 'utf-8))
    (setq-local url-http-end-of-headers (point-min))
    (setq-local url-http-response-status 200)
    (should (equal (org-linear-api--parse-response nil) '((viewer (name . "Åse")))))))

(ert-deftest org-linear-test-key-sources ()
  (let ((org-linear-api-key "abc"))
    (should (equal (org-linear-api--key) "abc")))
  (let ((org-linear-api-key (lambda () "from-fn")))
    (should (equal (org-linear-api--key) "from-fn")))
  (let ((org-linear-api-key nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (list (list :secret (lambda () "from-auth"))))))
      (should (equal (org-linear-api--key) "from-auth")))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should-error (org-linear-api--key) :type 'user-error))
    ;; A broken backend (no GPG) becomes a readable user-error.
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (signal 'epg-error '("no usable configuration")))))
      (should (string-match-p "Auth-source failed"
                              (cadr (should-error (org-linear-api--key)
                                                  :type 'user-error)))))))

;;;; View buffer

(ert-deftest org-linear-test-view-buffer ()
  (org-linear-test--with-responses
      (list `((issues . ((nodes . (,(org-linear-test--issue)))
			 (pageInfo . ((hasNextPage . nil)))))))
    (let ((org-linear-views '(("Test" :filter nil))))
      (unwind-protect
          (progn
            (org-linear-list-issues (car org-linear-views))
            (with-current-buffer "*Linear: Test*"
              (should (derived-mode-p 'org-mode))
              (should org-linear-view-mode)
              (should buffer-read-only)
              (should (equal org-linear--view (car org-linear-views)))
              (goto-char (point-min))
              (re-search-forward "ENG-1")
              (should (equal (org-linear--identifier-at-point) "ENG-1"))
              (org-linear-copy-identifier)
              (should (equal (car kill-ring) "ENG-1"))))
	(when (get-buffer "*Linear: Test*")
          (kill-buffer "*Linear: Test*"))))))

(ert-deftest org-linear-test-refresh-keeps-point ()
  (org-linear-test--with-responses
      (let ((page `((issues . ((nodes . (,(org-linear-test--issue)
					 ,(org-linear-test--issue 'identifier "ENG-2")))
                               (pageInfo . ((hasNextPage . nil))))))))
	(list page page))
    (let ((org-linear-views '(("Test" :filter nil))))
      (unwind-protect
          (progn
            (org-linear-list-issues (car org-linear-views))
            (with-current-buffer "*Linear: Test*"
              (goto-char (point-min))
              (re-search-forward "\\]\\[ENG-2\\]")
              (org-linear-refresh)
              (should (equal (org-linear--identifier-at-point) "ENG-2"))))
	(when (get-buffer "*Linear: Test*")
          (kill-buffer "*Linear: Test*"))))))

;;;; Markdown

(defun org-linear-test--md (markdown &optional base)
  "Convert MARKDOWN at BASE level."
  (org-linear-markdown-to-org markdown base))

(ert-deftest org-linear-test-md-inline ()
  (should (equal (org-linear-test--md "a **b** *c* _d_ ~~e~~ `f_g`")
                 "a *b* /c/ /d/ +e+ ~f_g~"))
  (should (equal (org-linear-test--md "snake_case_name stays") "snake_case_name stays"))
  (should (equal (org-linear-test--md "2 * 3 * 4") "2 * 3 * 4"))
  (should (equal (org-linear-test--md "`a~b`") "=a~b="))
  (should (equal (org-linear-test--md "[a **b**](https://x.io/a_b_c)")
                 "[[https://x.io/a_b_c][a *b*]]"))
  (should (equal (org-linear-test--md "![alt](https://x.io/i.png)") "[[https://x.io/i.png]]"))
  (should (equal (org-linear-test--md "see <https://x.io> and https://y.io/a_b_c")
                 "see [[https://x.io]] and https://y.io/a_b_c")))

(ert-deftest org-linear-test-md-headings ()
  ;; The shallowest heading goes one level below the base.
  (should (equal (org-linear-test--md "## A\ntext\n### B" 2) "*** A\ntext\n**** B"))
  (should (equal (org-linear-test--md "# A" 0) "* A"))
  (should (equal (org-linear-test--md "# C# #" 0) "* C#"))
  ;; A "#" inside a code block is not a heading and does not count.
  (should (equal (org-linear-test--md "```\n# x\n```\n### A" 1)
                 "#+begin_src\n# x\n#+end_src\n** A")))

(ert-deftest org-linear-test-md-lists ()
  (should (equal (org-linear-test--md "* a\n  + b\n- [ ] c\n- [x] d\n1. e")
                 "- a\n  - b\n- [ ] c\n- [X] d\n1. e")))

(ert-deftest org-linear-test-md-blocks ()
  (should (equal (org-linear-test--md "```python\n* x\n#+y\n```")
                 "#+begin_src python\n,* x\n,#+y\n#+end_src"))
  (should (equal (org-linear-test--md "> a *b*\n> c\nd")
                 "#+begin_quote\na /b/\nc\n#+end_quote\nd"))
  (should (equal (org-linear-test--md "| a | b |\n|---|:-:|\n| 1 | 2 |")
                 "| a | b |\n|-\n| 1 | 2 |"))
  (should (equal (org-linear-test--md "---") "-----"))
  ;; An unclosed fence is closed.
  (should (equal (org-linear-test--md "```\nx") "#+begin_src\nx\n#+end_src")))

(ert-deftest org-linear-test-md-no-stray-org-syntax ()
  (should (equal (org-linear-test--md "\\* not a heading")
                 "\u200b*\u200b not a heading"))
  (should (equal (org-linear-test--md "\\*a\\* \\[b\\]")
                 "\u200b*\u200ba\u200b*\u200b [b]"))
  (should (equal (org-linear-test--md "#+TITLE: x") "\u200b#+TITLE: x"))
  (should (equal (org-linear-test--md "") ""))
  (should (equal (org-linear-test--md nil) "")))

;;;; Single issue

(defun org-linear-test--full-issue ()
  "Return a sample issue with description, children and comments."
  (org-linear-test--issue
   'description "## Why\nBecause **reasons**."
   'parent '((identifier . "ENG-0") (title . "Epic") (url . "https://linear.app/acme/issue/ENG-0"))
   'children `((nodes . (,(org-linear-test--issue 'identifier "ENG-3" 'labels nil
                                                  'state '((name . "Done") (type . "completed")))
                         ,(org-linear-test--issue 'identifier "ENG-4" 'labels nil))))
   'attachments '((nodes . (((title . "PR [1]") (url . "https://github.com/x/1")))))
   'comments '((nodes . (((id . "c2") (body . "Reply") (createdAt . "2026-11-02T10:00:00Z")
                          (user . ((name . "Bob"))) (parent . ((id . "c1"))))
                         ((id . "c1") (body . "First") (createdAt . "2026-11-01T10:00:00Z")
                          (user . ((name . "Ada"))))
                         ((id . "c3") (body . "Bot says") (createdAt . "2026-11-03T10:00:00Z")
                          (botActor . ((name . "GitHub")))))))))

(ert-deftest org-linear-test-issue-document ()
  (let ((text (org-linear-render-issue-document (org-linear-test--full-issue) 0)))
    (should (string-match-p "^#\\+TITLE: ENG-1 Fix the thing$" text))
    (should (string-match-p "^\\* STARTED \\[#B\\] " text))
    (should (string-match-p "^Parent: \\[\\[https://linear.app/acme/issue/ENG-0\\]\\[ENG-0\\]\\] Epic$" text))
    (should (string-match-p "^\\*\\* Description\n\\*\\*\\* Why\nBecause \\*reasons\\*\\.$" text))
    (should (string-match-p "^\\*\\* Sub-issues (2)$" text))
    ;; Open sub-issues before done ones.
    (should (< (string-match "ENG-4\\]\\]" text) (string-match "ENG-3\\]\\]" text)))
    (should (string-match-p "^- \\[\\[https://github.com/x/1\\]\\[PR 1\\]\\]$" text))
    (should (string-match-p "^\\*\\* Comments (3)$" text))
    ;; Replies nest under their parent; top level is oldest first.
    (should (string-match-p "^\\*\\*\\* Ada \\[2026-11-01 [[:alpha:]]+ [0-9:]+\\]$" text))
    (should (string-match-p "^\\*\\*\\*\\* Bob \\[" text))
    (should (< (string-match "^\\*\\*\\* Ada" text) (string-match "^\\*\\*\\* GitHub" text)))))

(ert-deftest org-linear-test-issue-document-empty ()
  (let ((text (org-linear-render-issue-document
               (org-linear-test--issue 'description nil 'comments '((nodes))) 0)))
    (should (string-match-p "^/No description\\./$" text))
    (should (string-match-p "^\\*\\* Comments (0)$" text))
    (should-not (string-match-p "Sub-issues\\|Attachments\\|Parent:" text))))

(ert-deftest org-linear-test-open-issue-buffer ()
  (org-linear-test--with-responses
      (list `((issue . ,(org-linear-test--full-issue)))
            `((issue . ,(org-linear-test--full-issue))))
    (unwind-protect
	(progn
          (org-linear-open-issue "ENG-1")
          (with-current-buffer "*Linear: ENG-1*"
            (should org-linear-view-mode)
            (should (equal org-linear--issue-id "ENG-1"))
            (should (equal (alist-get 'id (alist-get 'variables (car requests))) "ENG-1"))
            ;; Point in a comment still knows its issue.
            (goto-char (point-min))
            (re-search-forward "^Reply")
            (should (equal (org-linear--identifier-at-point t) "ENG-1"))
            (should-not (org-linear--identifier-at-point))
            ;; g fetches the same issue again.
            (org-linear-refresh)
            (should (= (length requests) 2))))
      (when (get-buffer "*Linear: ENG-1*")
	(kill-buffer "*Linear: ENG-1*")))))

(ert-deftest org-linear-test-open-issue-not-found ()
  (org-linear-test--with-responses (list '((issue)))
    (let (got)
      (org-linear-api-issue "ENG-404" #'ignore (lambda (msg) (setq got msg)))
      (should (equal got "Issue ENG-404 not found")))))

(ert-deftest org-linear-test-ret-opens-sub-issue ()
  (org-linear-test--with-responses
      (list `((issue . ,(org-linear-test--full-issue))))
    (let (opened)
      (unwind-protect
          (progn
            (org-linear-open-issue "ENG-1")
            (with-current-buffer "*Linear: ENG-1*"
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* STARTED")
              (beginning-of-line)
              (cl-letf (((symbol-function 'org-linear-open-issue)
                         (lambda (id) (setq opened id))))
		(org-linear-open-at-point))
              (should (equal opened "ENG-4"))))
	(when (get-buffer "*Linear: ENG-1*")
          (kill-buffer "*Linear: ENG-1*"))))))

;;;; Editing

(defconst org-linear-test--states
  '(((id . "s-done") (name . "Done") (type . "completed") (position . 5))
    ((id . "s-todo") (name . "Todo") (type . "unstarted") (position . 1))
    ((id . "s-prog") (name . "In Progress") (type . "started") (position . 2)))
  "Sample workflow states.")

(defmacro org-linear-test--in-issue-buffer (extra-responses &rest body)
  "Open sample issue ENG-1, then run BODY with EXTRA-RESPONSES queued."
  (declare (indent 1))
  `(org-linear-test--with-responses
       (append (list `((issue . ,(org-linear-test--full-issue)))) ,extra-responses)
     (clrhash org-linear-api--states-cache)
     (unwind-protect
         (progn
           (org-linear-open-issue "ENG-1")
           (with-current-buffer "*Linear: ENG-1*"
             (goto-char (point-min))
             (re-search-forward "^\\* STARTED")
             ,@body))
       (clrhash org-linear-api--states-cache)
       (when (get-buffer "*Linear: ENG-1*")
         (kill-buffer "*Linear: ENG-1*")))))

(defun org-linear-test--variables (request)
  "Return the GraphQL variables of REQUEST."
  (alist-get 'variables request))

(ert-deftest org-linear-test-set-state ()
  (org-linear-test--in-issue-buffer
      (list `((issue . ((team . ((id . "t1") (key . "ENG")
				 (states . ((nodes . ,org-linear-test--states))))))))
            '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
            `((issue . ,(org-linear-test--full-issue))))
    (org-linear-set-state "Done")
    (setq requests (nreverse requests))
    ;; open, states, update, refresh
    (should (= (length requests) 4))
    (let ((vars (org-linear-test--variables (nth 2 requests))))
      (should (equal (alist-get 'id vars) "uuid-1"))
      (should (equal (alist-get 'stateId (alist-get 'input vars)) "s-done")))
    (should (gethash "ENG" org-linear-api--states-cache))))

(ert-deftest org-linear-test-set-state-uses-cache ()
  (org-linear-test--in-issue-buffer
      (list '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
            `((issue . ,(org-linear-test--full-issue))))
    (puthash "ENG" org-linear-test--states org-linear-api--states-cache)
    (org-linear-set-state "Todo")
    ;; open, update, refresh: no states request.
    (should (= (length requests) 3))
    (should (equal (alist-get 'stateId (alist-get 'input (org-linear-test--variables
                                                          (nth 1 requests))))
                   "s-todo"))))

(ert-deftest org-linear-test-set-state-from-comment ()
  "Point inside a comment edits the issue the comment belongs to."
  (org-linear-test--in-issue-buffer
      (list '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
            `((issue . ,(org-linear-test--full-issue))))
    (puthash "ENG" org-linear-test--states org-linear-api--states-cache)
    (re-search-forward "^First")
    (org-linear-set-state "Todo")
    (should (equal (alist-get 'id (org-linear-test--variables (nth 1 requests)))
                   "uuid-1"))))

(ert-deftest org-linear-test-state-candidates-order ()
  (should (equal (mapcar (lambda (c) (alist-get 'name (cdr c)))
                         (org-linear--state-candidates org-linear-test--states))
                 '("In Progress" "Todo" "Done"))))

(ert-deftest org-linear-test-set-priority ()
  (org-linear-test--in-issue-buffer
      (list '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
            `((issue . ,(org-linear-test--full-issue))))
    (org-linear-set-priority 1)
    (should (equal (alist-get 'input (org-linear-test--variables (nth 1 requests)))
                   '((priority . 1))))
    (should-error (org-linear-set-priority 7) :type 'user-error)))

(ert-deftest org-linear-test-update-failure ()
  (org-linear-test--with-responses (list '((issueUpdate . ((success . nil)))))
    (let (got)
      (org-linear-api-update-issue "u" '(:priority 1) #'ignore (lambda (m) (setq got m)))
      (should (equal got "Linear did not update u")))))

(ert-deftest org-linear-test-edit-outside-issue ()
  (with-temp-buffer
    (org-mode)
    (insert "* Plain heading\n")
    (goto-char (point-min))
    (should-error (org-linear-set-priority 1) :type 'user-error)))

;;;; Org to Markdown

(ert-deftest org-linear-test-org-to-md-inline ()
  (should (equal (org-linear-markdown-from-org "a *b* /c/ +d+ ~e~ =f=")
                 "a **b** *c* ~~d~~ `e` `f`"))
  (should (equal (org-linear-markdown-from-org "~a~ ~b~") "`a` `b`"))
  (should (equal (org-linear-markdown-from-org "[[https://x.io/a_b][t *x*]] [[https://y.io]] [[https://i.io/p.png]]")
                 "[t **x**](https://x.io/a_b) <https://y.io> ![](https://i.io/p.png)"))
  (should (equal (org-linear-markdown-from-org "path a/b/c and 2 * 3") "path a/b/c and 2 * 3"))
  (should (equal (org-linear-markdown-from-org "*Bold* first") "**Bold** first")))

(ert-deftest org-linear-test-org-to-md-blocks ()
  (should (equal (org-linear-markdown-from-org "** A\n*** B" 1) "### A\n#### B"))
  (should (equal (org-linear-markdown-from-org "+ a\n  - b\n- [X] c\n- [ ] d")
                 "- a\n  - b\n- [x] c\n- [ ] d"))
  (should (equal (org-linear-markdown-from-org "#+begin_src sh\n,* x\necho\n#+end_src")
                 "```sh\n* x\necho\n```"))
  (should (equal (org-linear-markdown-from-org "#+begin_quote\na\n\nb\n#+end_quote")
                 "> a\n>\n> b"))
  (should (equal (org-linear-markdown-from-org "|   | a |\n|---+---|\n| 1 | 2 |")
                 "|   | a |\n| --- | --- |\n| 1 | 2 |"))
  (should (equal (org-linear-markdown-from-org "-----") "---"))
  (should (equal (org-linear-markdown-from-org "# org comment") "\\# org comment")))

(ert-deftest org-linear-test-markdown-round-trip ()
  "Markdown in Linear's style survives Org and back unchanged."
  (dolist (md '("## Why\n\nA **bold** move with *italic*, `code_x` and ~~gone~~."
                "- a [link **x**](https://a.com/b_c_d)\n  - nested\n- [ ] open\n- [x] done\n1. first"
                "> quoted *text*\n> more"
                "```python\n* star\n#+x\nx = 1\n```"
                "| a | b |\n| --- | --- |\n| 1 | 2 |"
                "---"
                "See https://linear.app/x_y_z and <https://b.com> and \\*lit\\* ![](https://i.png)"
                "Recount ~10:00Z: rows are `paused` and `queued`, 1+1=2, a/b/c."
                "\\# not a heading"))
    (let* ((top (org-linear-markdown--top-heading (split-string md "\n")))
           (org (org-linear-markdown-to-org md 0)))
      (should (equal (org-linear-markdown-from-org org (1- top)) md)))))

;;;; Editing text

(defun org-linear-test--edit-buffer ()
  "Return the live org-linear edit buffer, or nil."
  (seq-find (lambda (b) (buffer-local-value 'org-linear-edit-mode b)) (buffer-list)))

(defmacro org-linear-test--cleanup-edit (&rest body)
  "Run BODY, then kill any edit buffers it left."
  `(unwind-protect (progn ,@body)
     (dolist (b (buffer-list))
       (when (buffer-local-value 'org-linear-edit-mode b)
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b)))))

(defun org-linear-test--text-response (description)
  "Return an issue-text response with DESCRIPTION."
  `((issue . ((id . "uuid-1") (identifier . "ENG-1") (title . "Fix the thing")
              (description . ,description)))))

(ert-deftest org-linear-test-edit-description ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer
       (list (org-linear-test--text-response "## Why\nBecause **reasons**.")
             (org-linear-test--text-response "## Why\nBecause **reasons**.")
             '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
             `((issue . ,(org-linear-test--full-issue))))
     (org-linear-edit-description)
     (with-current-buffer (org-linear-test--edit-buffer)
       (should (derived-mode-p 'org-mode))
       (should (equal (buffer-string) "* Why\nBecause *reasons*."))
       (goto-char (point-max))
       (insert " And /more/.")
       (org-linear-edit-save))
     (should-not (org-linear-test--edit-buffer))
     (setq requests (nreverse requests))
     (should (equal (alist-get 'input (org-linear-test--variables (nth 3 requests)))
                    '((description . "## Why\nBecause **reasons**. And *more*."))))
     ;; The issue buffer was fetched again.
     (should (= (length requests) 5)))))

(ert-deftest org-linear-test-edit-description-conflict ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer
       (list (org-linear-test--text-response "old")
             (org-linear-test--text-response "changed by someone"))
     (org-linear-edit-description)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
       (with-current-buffer (org-linear-test--edit-buffer)
         (insert "mine ")
         (org-linear-edit-save)
         ;; Not overwritten: the buffer stays and can be saved again.
         (should-not org-linear-edit--saving)))
     (should (org-linear-test--edit-buffer))
     (should (= (length requests) 3)))))

(ert-deftest org-linear-test-edit-failure-keeps-text ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer
       (list (org-linear-test--text-response "old")
             (org-linear-test--text-response "old")
             '(:error "network down"))
     (org-linear-edit-description)
     (with-current-buffer (org-linear-test--edit-buffer)
       (insert "new ")
       (org-linear-edit-save)
       (should (equal (buffer-string) "new old"))
       (should-not org-linear-edit--saving)))))

(ert-deftest org-linear-test-edit-title ()
  (org-linear-test--in-issue-buffer
      (list '((issueUpdate . ((success . t) (issue . ((identifier . "ENG-1"))))))
            `((issue . ,(org-linear-test--full-issue))))
    (re-search-forward "^First")
    (should (equal (org-linear--title-at-point) "Fix the thing"))
    (org-linear-edit-title "  Better title ")
    (should (equal (alist-get 'input (org-linear-test--variables (car (last requests 2))))
                   '((title . "Better title"))))
    (should-error (org-linear-edit-title " ") :type 'user-error)))

(ert-deftest org-linear-test-add-comment ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer
       (list '((commentCreate . ((success . t) (comment . ((id . "new"))))))
             `((issue . ,(org-linear-test--full-issue))))
     (re-search-forward "^\\*\\* Description")
     (org-linear-add-comment)
     (with-current-buffer (org-linear-test--edit-buffer)
       (insert "Looks *good*")
       (org-linear-edit-save))
     (should-not (org-linear-test--edit-buffer))
     (let ((input (alist-get 'input (org-linear-test--variables (nth 1 (reverse requests))))))
       (should (equal (alist-get 'issueId input) "uuid-1"))
       (should (equal (alist-get 'body input) "Looks **good**"))
       (should-not (assq 'parentId input))))))

(ert-deftest org-linear-test-reply-goes-to-thread-root ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer
       (list '((commentCreate . ((success . t) (comment . ((id . "new"))))))
             `((issue . ,(org-linear-test--full-issue))))
     ;; Bob's comment is itself a reply to Ada's (c1).
     (re-search-forward "^Reply")
     (should (equal (org-linear--comment-at-point) '("c2" "Bob" "c1")))
     (org-linear-add-comment)
     (with-current-buffer (org-linear-test--edit-buffer)
       (should (string-match-p "Reply to Bob" (format "%s" header-line-format)))
       (insert "Agreed")
       (org-linear-edit-save))
     (should (equal (alist-get 'parentId (alist-get 'input (org-linear-test--variables
                                                            (nth 1 (reverse requests)))))
                    "c1")))))

(ert-deftest org-linear-test-empty-comment-not-sent ()
  (org-linear-test--cleanup-edit
   (org-linear-test--in-issue-buffer nil
     (org-linear-add-comment)
     (with-current-buffer (org-linear-test--edit-buffer)
       (org-linear-edit-save))
     (should (org-linear-test--edit-buffer))
     (should (= (length requests) 1)))))

(ert-deftest org-linear-test-delete-comment ()
  (org-linear-test--in-issue-buffer
      (list '((commentDelete . ((success . t))))
            `((issue . ,(org-linear-test--full-issue))))
    (re-search-forward "^First")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (org-linear-delete-comment))
    (should (equal (alist-get 'id (org-linear-test--variables (nth 1 (reverse requests))))
                   "c1"))
    (should (= (length requests) 3))
    ;; Not on a comment.
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Description")
    (should-error (org-linear-delete-comment) :type 'user-error)))

;;;; Documentation

(ert-deftest org-linear-test-readme-documents-everything ()
  "Every command and every key in our keymaps appears in README.org."
  (let ((readme (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "../README.org"
                                     (file-name-directory
                                      (or load-file-name buffer-file-name
                                          (locate-library "org-linear-test")))))
                  (buffer-string)))
        (missing nil))
    (mapatoms (lambda (sym)
                (when (and (commandp sym)
                           (string-prefix-p "org-linear-" (symbol-name sym))
                           (not (string-search (format "=%s=" sym) readme)))
                  (push sym missing))))
    (dolist (map (list org-linear-view-mode-map org-linear-edit-mode-map))
      (dolist (binding (org-linear-test--bindings map))
        (unless (string-search (format "=%s=" (car binding)) readme)
          (push (car binding) missing))))
    (should-not missing)))

(defun org-linear-test--bindings (map &optional prefix)
  "Return (KEY-DESCRIPTION . COMMAND) for every binding in MAP under PREFIX."
  (let (out)
    (map-keymap
     (lambda (event binding)
       (let ((keys (vconcat prefix (vector event))))
         (if (keymapp binding)
             (setq out (append (org-linear-test--bindings binding keys) out))
           (push (cons (key-description keys) binding) out))))
     map)
    out))

(provide 'org-linear-test)
;;; org-linear-test.el ends here
