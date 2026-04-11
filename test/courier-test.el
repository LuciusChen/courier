;;; courier-test.el --- Tests for Courier -*- lexical-binding: t; -*-

;; Author: Lucius Chen <chenyh572@gmail.com>
;; URL: https://github.com/LuciusChen/courier
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; ERT tests for Courier.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'courier)

(defmacro courier-test--with-request (content &rest body)
  "Evaluate BODY in a temp buffer containing CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (setq-local buffer-file-name "/tmp/test.http")
     ,@body))

(defmacro courier-test--with-temp-files (bindings &rest body)
  "Create temp file BINDINGS and evaluate BODY."
  (declare (indent 1) (debug t))
  (let ((vars (mapcar #'car bindings)))
    `(let ,(mapcar (lambda (binding)
                     `(,(car binding) (make-temp-file "courier-test-" nil ,(cadr binding))))
                   bindings)
       (unwind-protect
           (progn ,@body)
         (dolist (path (list ,@vars))
           (when (and path (file-exists-p path))
             (delete-file path)))))))

(defun courier-test--parsed (content)
  "Parse request CONTENT and return a Courier request plist."
  (courier-test--with-request content
    (courier-parse-buffer)))

(defun courier-test--request (&rest pairs)
  "Create a resolved request plist from PAIRS."
  pairs)

(defun courier-test--response (&rest pairs)
  "Create a response plist from PAIRS."
  pairs)

(defun courier-test--make-response (status)
  "Return a minimal response plist with STATUS."
  (list :status-code status :reason "OK"
        :headers (list (cons "content-type" "text/plain"))
        :duration-ms 42 :size 5 :body-text "hello"
        :content-type "text/plain" :tests nil
        :stderr "" :exit-code 0 :command nil))

(defvar courier-test--request
  '(:method "GET" :url "https://example.com" :path "/tmp/test.http")
  "Reusable request fixture for Courier response-mode tests.")

(defmacro courier-test--with-temp-dir (binding &rest body)
  "Create a temp directory BINDING and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((,(car binding) (make-temp-file "courier-test-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,(car binding) t))))

(defun courier-test--argv-flag-value (argv flag)
  "Return the value immediately after FLAG in ARGV."
  (when-let* ((position (cl-position flag argv :test #'string=)))
    (nth (1+ position) argv)))

(defun courier-test--count-matches (regexp string)
  "Return the number of REGEXP matches in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

;; Parser tests.

(ert-deftest courier-parse-minimal-get ()
  (let ((request (courier-test--parsed "GET https://example.com\n")))
    (should (equal (plist-get request :method) "GET"))
    (should (equal (plist-get request :url) "https://example.com"))
    (should (equal (plist-get request :headers) nil))
    (should (equal (plist-get request :body) ""))))

(ert-deftest courier-parse-full-request-with-directives ()
  (let ((request
         (courier-test--parsed
          (concat "# @name Example\n"
                  "# @timeout 12\n"
                  "# @follow-redirects true\n"
                  "# @auth bearer {{token}}\n"
                  "# @var token abc\n"
                  "# @param page 1\n"
                  "# @test status == 200\n"
                  "POST https://example.com\n"
                  "Accept: application/json\n"
                  "\n"
                  "{}\n"))))
    (should (equal (plist-get request :name) "Example"))
    (should (equal (plist-get (plist-get request :settings) :timeout) 12))
    (should (equal (plist-get (plist-get request :settings) :follow-redirects) t))
    (should (equal (plist-get request :auth)
                   '(:type bearer :token "{{token}}")))
    (should (equal (plist-get request :params) '(("page" . "1"))))
    (should (equal (plist-get request :vars) '(("token" . "abc"))))
    (should (equal (plist-get request :tests) '("status == 200")))))

(ert-deftest courier-parse-post-with-body ()
  (let ((request
         (courier-test--parsed
          (concat "POST https://example.com\n"
                  "Content-Type: application/json\n"
                  "\n"
                  "{\n"
                  "  \"hello\": true\n"
                  "}\n"))))
    (should (equal (plist-get request :method) "POST"))
    (should (string-match-p "\"hello\"" (plist-get request :body)))))

(ert-deftest courier-parse-headers-lowercase ()
  (let ((request
         (courier-test--parsed
          (concat "GET https://example.com\n"
                  "Accept: application/json\n"
                  "X-Request-Id: abc\n"))))
    (should (equal (plist-get request :headers)
                   '(("accept" . "application/json")
                     ("x-request-id" . "abc"))))))

(ert-deftest courier-parse-rejects-unknown-directive ()
  (should-error
   (courier-test--parsed
    (concat "# @unknown nope\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-param-directive ()
  (should-error
   (courier-test--parsed
    (concat "# @param only-key\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-auth-directive ()
  (should-error
   (courier-test--parsed
    (concat "# @auth nope value\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-method ()
  (should-error
   (courier-test--parsed "TRACE https://example.com\n")
   :type 'user-error))

(ert-deftest courier-parse-rejects-missing-request-line ()
  (should-error
   (courier-test--parsed "# @name Missing\n")
   :type 'user-error))

(ert-deftest courier-parse-rejects-malformed-header ()
  (should-error
   (courier-test--parsed
    (concat "GET https://example.com\n"
            "Missing-Header\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-timeout-non-integer ()
  (should-error
   (courier-test--parsed
    (concat "# @timeout nope\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-timeout-zero ()
  (should-error
   (courier-test--parsed
    (concat "# @timeout 0\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-timeout-negative ()
  (should-error
   (courier-test--parsed
    (concat "# @timeout -1\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-follow-redirects ()
  (should-error
   (courier-test--parsed
    (concat "# @follow-redirects maybe\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-directive-missing-value ()
  (should-error
   (courier-test--parsed
    (concat "# @timeout\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-var-multi-word-value ()
  (let ((request
         (courier-test--parsed
          (concat "# @var greeting hello brave world\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :vars)
                   '(("greeting" . "hello brave world"))))))

(ert-deftest courier-parse-multiple-vars ()
  (let ((request
         (courier-test--parsed
          (concat "# @var one 1\n"
                  "# @var two 2\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :vars)
                   '(("one" . "1") ("two" . "2"))))))

(ert-deftest courier-parse-multiple-params ()
  (let ((request
         (courier-test--parsed
          (concat "# @param page 1\n"
                  "# @param sort created at\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :params)
                   '(("page" . "1")
                     ("sort" . "created at"))))))

(ert-deftest courier-parse-script-blocks ()
  (let ((request
         (courier-test--parsed
          (concat "# @begin pre-request\n"
                  "# (plist-put courier-script-request :url \"https://example.com/from-script\")\n"
                  "# courier-script-request\n"
                  "# @end\n"
                  "# @begin post-response\n"
                  "# (plist-put courier-script-response :reason \"Changed\")\n"
                  "# courier-script-response\n"
                  "# @end\n"
                  "GET https://example.com\n"))))
    (should (string-match-p "from-script"
                            (plist-get request :pre-request-script)))
    (should (string-match-p "Changed"
                            (plist-get request :post-response-script)))))

(ert-deftest courier-parse-multiple-tests ()
  (let ((request
         (courier-test--parsed
          (concat "# @test status == 200\n"
                  "# @test size < 1000\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :tests)
                   '("status == 200" "size < 1000")))))

(ert-deftest courier-parse-empty-body-after-blank-line ()
  (let ((request
         (courier-test--parsed
          (concat "GET https://example.com\n"
                  "Accept: application/json\n"
                  "\n"))))
    (should (equal (plist-get request :body) ""))))

(ert-deftest courier-parse-body-containing-header-like-lines ()
  (let ((request
         (courier-test--parsed
          (concat "POST https://example.com\n"
                  "Content-Type: text/plain\n"
                  "\n"
                  "Looks-Like: Header\n"
                  "Still body\n"))))
    (should (equal (plist-get request :body)
                   "Looks-Like: Header\nStill body\n"))))

;; Variable expansion tests.

(ert-deftest courier-expand-single-variable ()
  (should (equal (courier-expand-template "{{x}}" '(("x" . "42"))) "42")))

(ert-deftest courier-expand-multiple-variables-in-string ()
  (should (equal (courier-expand-template "a={{x}},b={{y}}" '(("x" . "1") ("y" . "2")))
                 "a=1,b=2")))

(ert-deftest courier-resolve-request-expands-url-header-and-body ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com/{{id}}"
                        :headers '(("authorization" . "Bearer {{token}}"))
                        :body "{\"id\":\"{{id}}\"}"
                        :params nil
                        :vars '(("id" . "42") ("token" . "abc"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :url) "https://example.com/42"))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Bearer abc"))))
    (should (equal (plist-get resolved :body) "{\"id\":\"42\"}"))))

(ert-deftest courier-resolve-request-adds-bearer-auth-header ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com"
                        :headers nil
                        :body ""
                        :params nil
                        :vars '(("token" . "abc"))
                        :auth '(:type bearer :token "{{token}}")
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Bearer abc"))))))

(ert-deftest courier-resolve-request-explicit-header-wins-over-auth ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com"
                        :headers '(("authorization" . "Bearer explicit"))
                        :body ""
                        :params nil
                        :vars '(("token" . "abc"))
                        :auth '(:type bearer :token "{{token}}")
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Bearer explicit"))))))

(ert-deftest courier-resolve-request-basic-auth-encodes-credentials ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com"
                        :headers nil
                        :body ""
                        :params nil
                        :vars '(("user" . "lucy")
                                ("password" . "secret"))
                        :auth '(:type basic
                                :username "{{user}}"
                                :password "{{password}}")
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Basic bHVjeTpzZWNyZXQ="))))))

(ert-deftest courier-resolve-request-appends-query-params ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/users"
                        :headers nil
                        :body ""
                        :params '(("page" . "1")
                                  ("sort" . "created at"))
                        :vars nil
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :params)
                   '(("page" . "1")
                     ("sort" . "created at"))))
    (should (equal (plist-get resolved :url)
                   "https://example.com/users?page=1&sort=created%20at"))))

(ert-deftest courier-resolve-request-appends-query-params-to-existing-query ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/users?active=true"
                        :headers nil
                        :body ""
                        :params '(("page" . "1"))
                        :vars nil
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :url)
                   "https://example.com/users?active=true&page=1"))))

(ert-deftest courier-resolve-request-expands-query-param-values ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/users"
                        :headers nil
                        :body ""
                        :params '(("page" . "{{page}}")
                                  ("q" . "{{term}}"))
                        :vars '(("page" . "2")
                                ("term" . "Lucy Chen"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :url)
                   "https://example.com/users?page=2&q=Lucy%20Chen"))))

(ert-deftest courier-run-pre-request-script-can-rewrite-request ()
  (let* ((request (list :path "/tmp/demo.http"
                        :method "GET"
                        :url "https://example.com"
                        :headers nil
                        :body ""
                        :params nil
                        :vars nil
                        :pre-request-script
                        (concat
                         "(setq courier-script-request\n"
                         "      (plist-put courier-script-request :url\n"
                         "                 \"https://example.com/from-script\"))\n"
                         "courier-script-request")))
         (updated (courier--run-pre-request-script request nil)))
    (should (equal (plist-get updated :url)
                   "https://example.com/from-script"))))

(ert-deftest courier-run-post-response-script-can-rewrite-response ()
  (let* ((request (list :path "/tmp/demo.http"
                        :post-response-script
                        (concat
                         "(setq courier-script-response\n"
                         "      (plist-put courier-script-response :reason \"Changed\"))\n"
                         "courier-script-response")))
         (response '(:status-code 200 :reason "OK")))
    (should (equal (plist-get (courier--run-post-response-script request response nil)
                              :reason)
                   "Changed"))))

(ert-deftest courier-expand-recursive-variable ()
  (should (equal (courier-expand-template "{{outer}}"
                                          '(("outer" . "value={{inner}}")
                                            ("inner" . "42")))
                 "value=42")))

(ert-deftest courier-expand-errors-on-unresolved-variable ()
  (should-error
   (courier-expand-template "token={{missing}}" '(("present" . "1")))
   :type 'user-error))

(ert-deftest courier-expand-errors-on-circular-variable ()
  (should-error
   (courier-expand-template "{{a}}" '(("a" . "{{b}}") ("b" . "{{a}}")))
   :type 'user-error))

(ert-deftest courier-expand-errors-on-depth-limit ()
  (should-error
   (courier-expand-template "{{a}}"
                            '(("a" . "{{b}}")
                              ("b" . "{{c}}")
                              ("c" . "{{d}}")
                              ("d" . "{{e}}")
                              ("e" . "{{f}}")
                              ("f" . "{{g}}")
                              ("g" . "{{h}}")
                              ("h" . "{{i}}")
                              ("i" . "{{j}}")
                              ("j" . "{{k}}")
                              ("k" . "{{l}}")
                              ("l" . "done")))
   :type 'user-error))

(ert-deftest courier-expand-rejects-nested-braces ()
  (should-error
   (courier-expand-template "{{{{x}}}}" '(("x" . "1")))
   :type 'user-error))

;; Test DSL tests.

(ert-deftest courier-test-dsl-status-equals-pass ()
  (should (plist-get (courier-run-test '(:status-code 200) "status == 200") :passed)))

(ert-deftest courier-test-dsl-status-equals-fail ()
  (should-not (plist-get (courier-run-test '(:status-code 404) "status == 200") :passed)))

(ert-deftest courier-test-dsl-status-not-equals-pass ()
  (should (plist-get (courier-run-test '(:status-code 200) "status != 404") :passed)))

(ert-deftest courier-test-dsl-header-contains-pass ()
  (should (plist-get
           (courier-run-test
            '(:headers (("content-type" . "application/json")))
            "header content-type contains json")
           :passed)))

(ert-deftest courier-test-dsl-header-contains-fail ()
  (should-not (plist-get
               (courier-run-test
                '(:headers (("content-type" . "application/json")))
                "header content-type contains xml")
               :passed)))

(ert-deftest courier-test-dsl-header-contains-case-insensitive ()
  (should (plist-get
           (courier-run-test
            '(:headers (("content-type" . "application/json")))
            "header Content-Type contains json")
           :passed)))

(ert-deftest courier-test-dsl-body-contains-pass ()
  (should (plist-get
           (courier-run-test '(:body-text "hello courier") "body contains hello")
           :passed)))

(ert-deftest courier-test-dsl-body-contains-fail ()
  (should-not (plist-get
               (courier-run-test '(:body-text "hello courier") "body contains missing")
               :passed)))

(ert-deftest courier-test-dsl-time-less-than-pass ()
  (should (plist-get
           (courier-run-test '(:duration-ms 100) "time < 500")
           :passed)))

(ert-deftest courier-test-dsl-time-less-than-fail ()
  (should-not (plist-get
               (courier-run-test '(:duration-ms 600) "time < 500")
               :passed)))

(ert-deftest courier-test-dsl-size-less-than-pass-and-fail ()
  (should (plist-get (courier-run-test '(:size 100) "size < 1024") :passed))
  (should-not (plist-get (courier-run-test '(:size 2048) "size < 1024") :passed)))

(ert-deftest courier-test-dsl-rejects-invalid-syntax ()
  (should-error
   (courier-run-test '(:status-code 200) "status > 200")
   :type 'user-error))

;; Curl command builder tests.

(ert-deftest courier-build-curl-command-basic-get ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "GET"
                   :url "https://example.com"
                   :headers nil
                   :body ""
                   :settings nil)
                 header body meta)))
      (should (equal (car argv) courier-curl-executable))
      (should (string= (courier-test--argv-flag-value argv "-X") "GET"))
      (should (string= (courier-test--argv-flag-value argv "-D") header))
      (should (string= (courier-test--argv-flag-value argv "-o") body))
      (should (equal (car (last argv)) "https://example.com")))))

(ert-deftest courier-build-curl-command-post-with-body ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "POST"
                   :url "https://example.com"
                   :headers nil
                   :body "{\"a\":1}"
                   :settings nil)
                 header body meta)))
      (should (string= (courier-test--argv-flag-value argv "--data-binary")
                       (format "@%s" body))))))

(ert-deftest courier-build-curl-command-includes-timeout ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "GET"
                   :url "https://example.com"
                   :headers nil
                   :body ""
                   :settings (:timeout 9))
                 header body meta)))
      (should (string= (courier-test--argv-flag-value argv "--max-time") "9")))))

(ert-deftest courier-build-curl-command-follow-redirects ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "GET"
                   :url "https://example.com"
                   :headers nil
                   :body ""
                   :settings (:follow-redirects t))
                 header body meta)))
      (should (member "-L" argv)))))

(ert-deftest courier-build-curl-command-multiple-headers ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "GET"
                   :url "https://example.com"
                   :headers (("accept" . "application/json")
                             ("x-id" . "42"))
                   :body ""
                   :settings nil)
                 header body meta)))
      (let ((header-values nil)
            (start 0)
            position)
        (while (setq position (cl-position "-H" argv :test #'string= :start start))
          (push (nth (1+ position) argv) header-values)
          (setq start (1+ position)))
        (setq header-values (nreverse header-values))
        (should (equal header-values
                       '("accept: application/json" "x-id: 42")))))))

;; Response parser tests.

(ert-deftest courier-parse-response-simple-200 ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n"))
    (with-temp-file body
      (insert "hello"))
    (with-temp-file meta
      (insert "200\n5\n0.150\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :status-code) 200))
      (should (equal (plist-get response :reason) "OK"))
      (should (equal (plist-get response :body-text) "hello")))))

(ert-deftest courier-parse-response-uses-last-header-block ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert (concat "HTTP/1.1 301 Moved Permanently\r\nLocation: /next\r\n\r\n"
                      "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n")))
    (with-temp-file body
      (insert "done"))
    (with-temp-file meta
      (insert "200\n4\n0.010\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :status-code) 200))
      (should (equal (cdr (assoc-string "content-type" (plist-get response :headers) nil))
                     "text/plain")))))

(ert-deftest courier-parse-response-extracts-content-type-and-charset ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert "HTTP/2 200 OK\r\nContent-Type: application/json; charset=utf-8\r\n\r\n"))
    (with-temp-file body
      (insert "{\"ok\":true}"))
    (with-temp-file meta
      (insert "200\n11\n0.010\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (equal (plist-get response :content-type) "application/json"))
      (should (equal (plist-get response :charset) "utf-8")))))

(ert-deftest courier-parse-response-preserves-non-2xx-status ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n"))
    (with-temp-file body
      (insert "missing"))
    (with-temp-file meta
      (insert "404\n7\n0.020\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :status-code) 404))
      (should (equal (plist-get response :reason) "Not Found")))))

(ert-deftest courier-parse-response-falls-back-to-content-length-when-meta-size-zero ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert "HTTP/1.1 403 Forbidden\r\nContent-Type: text/html\r\nContent-Length: 407\r\n\r\n"))
    (with-temp-file body
      (insert "short body"))
    (with-temp-file meta
      (insert "403\n0\n0.000\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :status-code) 403))
      (should (equal (plist-get response :reason) "Forbidden"))
      (should (= (plist-get response :size) 407)))))

(ert-deftest courier-parse-response-transport-error ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file meta
      (insert "000\n0\n0.001\n"))
    (let ((response (courier-parse-response header body meta "curl: could not connect" 7
                                            '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :exit-code) 7))
      (should (string-match-p "could not connect" (plist-get response :stderr))))))

(ert-deftest courier-parse-response-empty-header-file ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert ""))
    (with-temp-file body
      (insert "hello"))
    (with-temp-file meta
      (insert "200\n5\n0.050\n"))
    (let ((response (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))))
      (should (= (plist-get response :status-code) 200))
      (should (equal (plist-get response :headers) nil)))))

(ert-deftest courier-parse-response-errors-on-unsupported-charset ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (with-temp-file header
      (insert "HTTP/1.1 200 OK\nContent-Type: text/plain; charset=madeup-charset\n\n"))
    (with-temp-file body
      (insert "hello"))
    (with-temp-file meta
      (insert "200\n5\n0.050\n"))
    (should-error
     (courier-parse-response header body meta "" 0 '(:path "/tmp/test.http" :tests nil))
     :type 'user-error)))

(ert-deftest courier-apply-response-fallbacks-uses-body-size-and-elapsed-time ()
  (courier-test--with-temp-files ((body ".body"))
    (with-temp-file body
      (insert "hello"))
    (let* ((start-time (- (float-time) 1.2))
           (response (courier--apply-response-fallbacks
                      `(:status-code 200
                        :reason "OK"
                        :headers nil
                        :body-file ,body
                        :size 0
                        :duration-ms 0)
                      start-time)))
      (should (= (plist-get response :size) 5))
      (should (> (plist-get response :duration-ms) 1000)))))

;; Environment tests.

(ert-deftest courier-parse-env-file-simple ()
  (courier-test--with-temp-files ((env ".env"))
    (with-temp-file env
      (insert "base_url=https://api.example.com\ntoken=abc123\n"))
    (should (equal (courier-parse-env-file env)
                   '(("base_url" . "https://api.example.com")
                     ("token" . "abc123"))))))

(ert-deftest courier-parse-env-file-comments-and-blank-lines ()
  (courier-test--with-temp-files ((env ".env"))
    (with-temp-file env
      (insert "# comment\n\nbase_url=https://api.example.com\n\n# another\n"))
    (should (equal (courier-parse-env-file env)
                   '(("base_url" . "https://api.example.com"))))))

(ert-deftest courier-parse-env-file-trims-values ()
  (courier-test--with-temp-files ((env ".env"))
    (with-temp-file env
      (insert "# comment\n\nbase_url= https://api.example.com \n"))
    (should (equal (courier-parse-env-file env)
                   '(("base_url" . "https://api.example.com"))))))

(ert-deftest courier-parse-env-file-rejects-malformed-lines ()
  (courier-test--with-temp-files ((env ".env"))
    (with-temp-file env
      (insert "broken-line\n"))
    (should-error (courier-parse-env-file env) :type 'user-error)))

(ert-deftest courier-collection-config-uses-defaults ()
  (courier-test--with-temp-dir (root)
    (let ((config-file (expand-file-name "courier.json" root)))
      (with-temp-file config-file
        (insert "{\n  \"name\": \"Demo\"\n}\n"))
      (should (equal (courier--collection-config root)
                     `(:root ,(file-name-as-directory root)
                       :path ,config-file
                       :name "Demo"
                       :requests-dir "requests"
                       :env-dir "env"
                       :default-env nil))))))

(ert-deftest courier-collection-config-reads-env-settings ()
  (courier-test--with-temp-dir (root)
    (let ((config-file (expand-file-name "courier.json" root)))
      (with-temp-file config-file
        (insert "{\n"
                "  \"envDir\": \"config/envs\",\n"
                "  \"defaultEnv\": \"staging\"\n"
                "}\n"))
      (let ((config (courier--collection-config root)))
        (should (equal (plist-get config :env-dir) "config/envs"))
        (should (equal (plist-get config :default-env) "staging"))))))

(ert-deftest courier-merge-vars-request-overrides-env ()
  (should (equal (courier-merge-vars '(("token" . "env-token")
                                       ("base_url" . "https://env"))
                                     '(("token" . "request-token")))
                 '(("token" . "request-token")
                   ("base_url" . "https://env")))))

(ert-deftest courier-merge-vars-env-fills-gaps ()
  (should (equal (courier-merge-vars '(("base_url" . "https://env"))
                                     '(("token" . "request-token")))
                 '(("base_url" . "https://env")
                   ("token" . "request-token")))))

(ert-deftest courier-resolve-request-with-env-vars ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "{{base_url}}/users/{{user_id}}"
                        :headers '(("authorization" . "Bearer {{token}}"))
                        :body ""
                        :vars '(("token" . "request-token"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request
                    request
                    '(("base_url" . "https://env")
                      ("user_id" . "42")
                      ("token" . "env-token")))))
    (should (equal (plist-get resolved :url) "https://env/users/42"))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Bearer request-token"))))))

;; Response formatting tests.

(ert-deftest courier-format-body-pretty-prints-json ()
  (with-temp-buffer
    (setq-local courier--body-pretty t)
    (let ((body (courier--format-body
                 '(:content-type "application/json"
                   :body-text "{\"foo\":1,\"bar\":2}"))))
      (should (string-match-p "{\n" body))
      (should (string-match-p "\"foo\": 1" body)))))

(ert-deftest courier-format-body-surfaces-invalid-json ()
  (with-temp-buffer
    (setq-local courier--body-pretty t)
    (let ((body (courier--format-body
                 '(:content-type "application/json"
                   :body-text "{\"foo\":}"))))
      (should (string-match-p "\\[Invalid JSON body:" body))
      (should (string-match-p "{\"foo\":}" body)))))

(ert-deftest courier-format-body-skips-non-json ()
  (with-temp-buffer
    (setq-local courier--body-pretty t)
    (let ((body (courier--format-body
                 '(:content-type "text/plain"
                   :body-text "{\"foo\":1}"))))
      (should (equal body "{\"foo\":1}\n")))))

(ert-deftest courier-insert-response-body-surfaces-image-render-errors ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-images-p) (lambda () t))
              ((symbol-function 'create-image)
               (lambda (&rest _args)
                 (error "broken image decoder"))))
      (courier--insert-response-body
       '(:content-type "image/png"
         :body-file "/tmp/demo.png"))
      (let ((body (buffer-string)))
        (should (string-match-p "Image rendering failed: broken image decoder" body))
        (should (string-match-p "Image body saved to /tmp/demo.png" body))))))

(ert-deftest courier-auto-body-view-detects-image ()
  (should (eq (courier--auto-body-view
               '(:content-type "image/png" :body-file "/tmp/demo.png"))
              'image)))

(ert-deftest courier-format-body-base64-view ()
  (with-temp-buffer
    (setq-local courier--body-view 'base64)
    (let ((body (courier--format-body
                 '(:content-type "text/plain"
                   :body-text "hello"))))
      (should (string-match-p "aGVsbG8=" body)))))

(ert-deftest courier-format-body-hex-view ()
  (with-temp-buffer
    (setq-local courier--body-view 'hex)
    (let ((body (courier--format-body
                 '(:content-type "application/octet-stream"
                   :body-text "Hi"))))
      (should (string-match-p "48 69" body)))))

(ert-deftest courier-response-set-view-updates-buffer-state ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-set-view 'raw)
    (should (eq courier--body-view 'raw))
    (should (eq courier--response-tab 'response))
    (should (string-match-p "\\` Response  >>  •  200 OK  •  42ms  •  5B"
                            (substring-no-properties header-line-format)))))

(ert-deftest courier-response-header-line-uses-clutch-style-separator ()
  (let ((line (substring-no-properties
               (courier--response-header-line (courier-test--make-response 200)))))
    (should (string-match-p "200 OK  •  42ms  •  5B" line))
    (should-not (string-match-p " | " line))))

(ert-deftest courier-response-header-line-falls-back-to-standard-reason ()
  (let ((line (substring-no-properties
               (courier--response-header-line
                '(:status-code 403
                  :reason ""
                  :headers nil
                  :duration-ms 42
                  :size 5
                  :content-type "text/plain")))))
    (should (string-match-p "403 Forbidden" line))))

(ert-deftest courier-response-open-body-opens-viewer-buffer ()
  (with-temp-buffer
    (let (viewer-buffer)
      (courier--render-response
       '(:status-code 200
         :reason "OK"
         :headers (("content-type" . "application/json"))
         :duration-ms 10
         :size 12
         :body-text "{\"foo\":1}"
         :content-type "application/json"
         :tests nil
         :stderr ""
         :exit-code 0)
       courier-test--request)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _args)
                   (setq viewer-buffer buffer)
                   buffer)))
        (courier-response-open-body))
      (with-current-buffer viewer-buffer
        (should (string-match-p "\"foo\"" (buffer-string)))))))

(ert-deftest courier-response-body-section-fontifies-html-view ()
  (with-temp-buffer
    (courier--render-response
     '(:status-code 200
       :reason "OK"
       :headers (("content-type" . "text/html; charset=utf-8"))
       :duration-ms 10
       :size 44
       :body-text "<html><body><div class=\"x\">hi</div></body></html>"
       :content-type "text/html"
       :tests nil
       :stderr ""
       :exit-code 0)
     courier-test--request)
    (let ((pos (point-min))
          found-face)
      (while (and (< pos (point-max))
                  (not found-face))
        (setq found-face (get-text-property pos 'face))
        (setq pos (1+ pos)))
      (should found-face))))

(ert-deftest courier-response-show-tests-switches-tab ()
  (with-temp-buffer
    (courier--render-response
     '(:status-code 200
       :reason "OK"
       :headers (("content-type" . "text/plain"))
       :duration-ms 10
       :size 12
       :body-text "hello"
       :content-type "text/plain"
       :tests ((:expr "status == 200" :passed t :message "ok"))
       :stderr ""
       :exit-code 0)
     courier-test--request)
    (courier-response-show-tests)
    (should (eq courier--response-tab 'tests))
    (should (string-match-p "\\` Tests(1)  >>  •  200 OK  •  10ms  •  12B"
                            (substring-no-properties header-line-format)))
    (should (string-match-p "status == 200" (buffer-string)))))

(ert-deftest courier-response-next-tab-cycles ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-next-tab)
    (should (eq courier--response-tab 'headers))
    (courier-response-prev-tab)
    (should (eq courier--response-tab 'response))))

(ert-deftest courier-response-tab-switch-resets-point-to-top ()
  (with-temp-buffer
    (courier--render-response
     '(:status-code 200
       :reason "OK"
       :headers (("content-type" . "text/plain"))
       :duration-ms 10
       :size 12
       :body-text "line1\nline2\nline3\nline4\nline5\n"
       :content-type "text/plain"
       :tests nil
       :stderr ""
       :exit-code 0)
     courier-test--request)
    (goto-char (point-max))
    (courier-response-show-headers)
    (should (= (point) (point-min)))
    (goto-char (point-max))
    (courier-response-show-response)
    (should (= (point) (point-min)))))

(ert-deftest courier-test-history-push-stores-responses ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (should (= (length courier--history) 2))
    (should (= (plist-get (cdr (car courier--history)) :status-code) 404))))

(ert-deftest courier-response-timeline-activate-selects-history-entry ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (courier-response-activate)
    (should (= courier--history-index 1))
    (should (string-match-p "2/2"
                            (substring-no-properties header-line-format)))
    (should (= (plist-get courier--response :status-code) 404))
    (should (eq courier--response-tab 'timeline))
    (should (string-match-p "GET https://example.com" (buffer-string)))))

(ert-deftest courier-response-header-line-shows-summary-only ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (let ((line (substring-no-properties header-line-format)))
      (should (string-match-p
               "\\` Response  >>  •  200 OK  •  42ms  •  5B"
               line))
      (courier-response-show-headers)
      (should (string-match-p
               "\\` Headers(1)  >>  •  200 OK  •  42ms  •  5B"
               (substring-no-properties header-line-format))))))

(ert-deftest courier-response-buffer-does-not-render-top-navigation-row ()
  (with-temp-buffer
    (courier--render-response
     '(:status-code 200
       :reason "OK"
       :headers (("content-type" . "text/plain"))
       :duration-ms 10
       :size 12
       :body-text "hello"
       :content-type "text/plain"
       :tests ((:expr "status == 200" :passed t :message "ok"))
       :stderr ""
       :exit-code 0)
     courier-test--request)
    (goto-char (point-min))
    (should-not (search-forward "Headers(1)" (line-end-position) t))
    (should (string-match-p "\\` Response  >>  •  200 OK  •  10ms  •  12B"
                            (substring-no-properties header-line-format)))))

(ert-deftest courier-response-timeline-network-logs-switches-section ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 403)
     '(:method "POST"
       :url "https://example.com/users"
       :headers (("content-type" . "application/json"))
       :body "{\"name\":\"Lucy\"}"
       :path "/tmp/test.http"))
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'network-logs)
    (should (eq courier--response-tab 'timeline))
    (should (memq 'network-logs courier--timeline-expanded-sections))
    (should (string-match-p "Exit code:" (buffer-string)))))

(ert-deftest courier-response-timeline-defaults-to-collapsed-history ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (should (null courier--history-index))
    (should-not (string-match-p "Request\\|Network Logs" (buffer-string)))
    (should (string-match-p "404 OK" (buffer-string)))
    (should (string-match-p "200 OK" (buffer-string)))))

(ert-deftest courier-response-timeline-expanded-entry-is-removed-from-list ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (should (= 0 (courier-test--count-matches "404 OK" (buffer-string))))
    (should (= 1 (courier-test--count-matches "200 OK" (buffer-string))))))

(ert-deftest courier-response-timeline-selection-keeps-point-on-entry ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 1)
    (let ((button (button-at (point))))
      (should button)
      (should (= 1 (button-get button 'courier-history-index))))))

(ert-deftest courier-response-timeline-section-toggle-keeps-point-on-section ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'network-logs)
    (let ((button (button-at (point))))
      (should button)
      (should (eq 'network-logs (button-get button 'courier-timeline-tab))))))

(ert-deftest courier-response-timeline-section-toggle-preserves-header-line ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'network-logs)
    (should (string-match-p "\\` Timeline  >>  •  200 OK  •  42ms  •  5B"
                            (substring-no-properties header-line-format)))))

(ert-deftest courier-response-timeline-response-view-shows-header-count ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'response)
    (should (string-match-p "Headers(1)" (buffer-string)))))

(ert-deftest courier-response-timeline-entry-is-a-button ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (let ((button (button-at (point))))
      (should button)
      (should (= (button-get button 'courier-history-index) 0)))))

(ert-deftest courier-response-timeline-entry-button-uses-default-face ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (let* ((button (button-at (point)))
           (face (get-text-property (1- (point)) 'face)))
      (should button)
      (should-not (member 'button (if (listp face) face (list face)))))))

(ert-deftest courier-response-timeline-entry-summary-line-has-face ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (member 'courier-response-timeline-entry-face
                      (if (listp face) face (list face)))))))

(ert-deftest courier-response-timeline-selected-entry-summary-line-has-face ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (goto-char (point-min))
    (search-forward "GET https://example.com")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (member 'courier-response-timeline-selected-face
                      (if (listp face) face (list face)))))))

(ert-deftest courier-response-timeline-selected-entry-omits-summary-metadata ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (courier--select-history-index 0)
    (let ((buffer (buffer-string)))
      (should (string-match-p "GET https://example.com" buffer))
      (should-not (string-match-p "200 OK" buffer))
      (should-not (string-match-p "42ms  •  5B" buffer)))))

(ert-deftest courier-response-context-tab-activates-timeline-button ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (courier-response-context-tab)
    (should (= courier--history-index 0))))

(ert-deftest courier-response-context-tab-expands-lower-entry-inline ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (end-of-line)
    (courier-response-context-tab)
    (should (= courier--history-index 1))
    (should (= 1 (courier--timeline-index-at-point)))
    (let ((buffer (buffer-string)))
      (should (string-match-p "404 OK" buffer))
      (should-not (string-match-p "200 OK" buffer))
      (should (> (or (string-match "\\[[-+]\\] Request" buffer) -1)
                  (or (string-match "GET https://example.com" buffer) -1))))))

(ert-deftest courier-response-context-tab-collapses-current-entry-inline ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (goto-char (point-min))
    (search-forward "200 OK")
    (courier-response-context-tab)
    (should (= courier--history-index 1))
    (courier-response-context-tab)
    (should (null courier--history-index))
    (let ((button (button-at (point))))
      (should button)
      (should (= 1 (button-get button 'courier-history-index))))))

(ert-deftest courier-response-clear-timeline-removes-history ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-show-timeline)
    (courier-response-clear-timeline)
    (should-not courier--history)
    (should-not courier--history-index)
    (should (string-match-p "No history yet\\." (buffer-string)))))

(ert-deftest courier-response-clear-timeline-errors-without-history ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (setq courier--history nil)
    (should-error (courier-response-clear-timeline) :type 'user-error)))

(ert-deftest courier-test-history-respects-max ()
  (let ((courier-history-max 3))
    (with-temp-buffer
      (dolist (status '(200 201 202 203 204))
        (courier--render-response
         (courier-test--make-response status)
         courier-test--request))
      (should (= (length courier--history) 3))
      (should (= (plist-get (cdr (nth 0 courier--history)) :status-code) 204))
      (should (= (plist-get (cdr (nth 2 courier--history)) :status-code) 202)))))

(ert-deftest courier-request-search-root-prefers-collection-root ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (api-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" api-dir)))
      (make-directory (expand-file-name ".git" root))
      (make-directory api-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (should (equal (file-name-as-directory
                        (expand-file-name "requests" collection-root))
                       (courier--request-search-root)))))))

(ert-deftest courier-request-search-root-respects-configured-requests-dir ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (requests-root (expand-file-name "api-requests" collection-root))
           (api-dir (expand-file-name "users" requests-root))
           (request-file (expand-file-name "get-user.http" api-dir)))
      (make-directory api-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"requestsDir\": \"api-requests\"\n}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (should (equal (file-name-as-directory requests-root)
                       (courier--request-search-root)))))))

(ert-deftest courier-request-search-root-requires-collection ()
  (courier-test--with-temp-dir (root)
    (let ((request-file (expand-file-name "api/users/get-user.http" root)))
      (make-directory (file-name-directory request-file) t)
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (should-not (courier--request-search-root))))))

(ert-deftest courier-available-env-entries-use-collection-env-dir ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (env-dir (expand-file-name "env" collection-root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (request-local-env (expand-file-name "local.env" request-dir))
           (collection-default (expand-file-name ".env" env-dir))
           (collection-prod (expand-file-name "prod.env" env-dir)))
      (make-directory env-dir t)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"defaultEnv\": \"prod\"\n}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file request-local-env
        (insert "token=from-request-dir\n"))
      (with-temp-file collection-default
        (insert "token=default\n"))
      (with-temp-file collection-prod
        (insert "token=prod\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (should (equal (courier--available-env-entries)
                       `(("default" . ,collection-default)
                         ("prod" . ,collection-prod))))))))

(ert-deftest courier-available-env-entries-require-collection ()
  (courier-test--with-temp-dir (root)
    (let* ((request-dir (expand-file-name "api/users" root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (outer-env (expand-file-name ".env" root))
           (inner-env (expand-file-name "local.env" request-dir)))
      (make-directory request-dir t)
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file outer-env
        (insert "base_url=https://outer.example.com\n"))
      (with-temp-file inner-env
        (insert "token=inner\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (should-not (courier--available-env-entries))))))

(ert-deftest courier-selected-env-name-prefers-collection-default ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (env-dir (expand-file-name "env" collection-root))
           (default-env (expand-file-name "local.env" env-dir))
           (prod-env (expand-file-name "prod.env" env-dir)))
      (make-directory request-dir t)
      (make-directory env-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"defaultEnv\": \"prod\"\n}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file default-env
        (insert "token=local\n"))
      (with-temp-file prod-env
        (insert "token=prod\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (should (equal (courier--selected-env-name
                        (courier--available-env-entries))
                       "prod"))
        (should (equal courier--active-env "prod"))))))

(ert-deftest courier-collection-without-env-dir-does-not-fall-back-upward ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (outer-env (expand-file-name ".env" root))
           (request-env (expand-file-name "local.env" request-dir)))
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file outer-env
        (insert "base_url=https://outer.example.com\n"))
      (with-temp-file request-env
        (insert "token=request\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (should-not (courier--available-env-entries))))))

(ert-deftest courier-request-candidates-include-method-name-and-path ()
  (courier-test--with-temp-dir (root)
    (let* ((request-file (expand-file-name "users/get-user.http" root)))
      (make-directory (file-name-directory request-file) t)
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (let ((candidate (car (courier--request-candidates root))))
        (should (equal (plist-get (cdr candidate) :path) request-file))
        (should (eq (plist-get (cdr candidate) :kind) 'request))
        (should (string-match-p "^GET\\s-+Get User" (car candidate)))
        (should (string-match-p "users/get-user\\.http$" (car candidate)))))))

(ert-deftest courier-open-errors-on-malformed-request-file ()
  (courier-test--with-temp-dir (root)
    (let ((request-file (expand-file-name "users/broken.http" root)))
      (make-directory (file-name-directory request-file) t)
      (with-temp-file request-file
        (insert "# @name Broken\nGET \n"))
      (let ((default-directory root))
        (should-error (courier-open) :type 'user-error)))))

(ert-deftest courier-overview-errors-on-malformed-request-file ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "broken.http" request-dir)))
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "# @name Broken\nGET \n"))
      (let ((default-directory collection-root))
        (should-error (courier-overview) :type 'user-error)))))

(ert-deftest courier-open-opens-selected-request-file ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-root (expand-file-name "requests/users" collection-root))
           (first-file (expand-file-name "get-user.http" request-root))
           (second-file (expand-file-name "create-user.http" request-root))
           opened-file
           expected-file)
      (make-directory request-root t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (make-directory (file-name-directory first-file) t)
      (with-temp-file first-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (with-temp-file second-file
        (insert "# @name Create User\nPOST https://example.com/users\n"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (setq expected-file (plist-get (cdar collection) :path))
                   (caar collection)))
                ((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened-file path))))
        (let ((default-directory collection-root))
          (courier-open)))
      (should (equal opened-file expected-file)))))

(ert-deftest courier-open-requires-collection ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (should-error (courier-open) :type 'user-error)))

(ert-deftest courier-request-set-method-rewrites-request-line ()
  (courier-test--with-request
      (concat "# @name Demo\n"
              "GET https://example.com/users/42\n"
              "Accept: application/json\n")
    (courier-request-mode)
    (courier-request-set-method "POST")
    (goto-char (point-min))
    (should (search-forward "POST https://example.com/users/42" nil t))))

(ert-deftest courier-request-mode-adds-method-overlay ()
  (courier-test--with-request
      (concat "# @name Demo\n"
              "GET https://example.com/users/42\n"
              "Accept: application/json\n")
    (courier-request-mode)
    (should (overlayp courier--method-overlay))
    (should (equal (buffer-substring-no-properties
                    (overlay-start courier--method-overlay)
                    (overlay-end courier--method-overlay))
                   "GET"))))

(ert-deftest courier-request-header-line-shows-primary-navigation ()
  (courier-test--with-request
      (concat "# @name Demo\n"
              "GET https://example.com/users/42\n"
              "Accept: application/json\n"
              "\n"
              "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "URL" line))
      (should (string-match-p "Headers" line))
      (should (string-match-p "Body" line))
      (should (string-match-p ">>" line)))))

(ert-deftest courier-request-header-line-shows-current-secondary-section ()
  (courier-test--with-request
      (concat "# @auth bearer {{token}}\n"
              "GET https://example.com/users/42\n")
    (courier-request-mode)
    (goto-char (point-min))
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Auth" line)))))

(ert-deftest courier-dispatch-routes-request-mode ()
  (courier-test--with-request
      "GET https://example.com\n"
    (courier-request-mode)
    (let (called)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _args)
                   (setq called prefix))))
        (courier-dispatch))
      (should (eq called 'courier-request-menu)))))

(ert-deftest courier-dispatch-routes-response-mode ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (let (called)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _args)
                   (setq called prefix))))
        (courier-dispatch))
      (should (eq called 'courier-response-menu)))))

(ert-deftest courier-dispatch-routes-overview-mode ()
  (with-temp-buffer
    (courier-overview-mode)
    (let (called)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _args)
                   (setq called prefix))))
        (courier-dispatch))
      (should (eq called 'courier-overview-menu)))))

(ert-deftest courier-mode-maps-bind-c-c-question ()
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c ?"))
              #'courier-dispatch))
  (should (eq (lookup-key courier-overview-mode-map (kbd "C-c ?"))
              #'courier-dispatch))
  (should (eq (lookup-key courier--response-mode-map (kbd "C-c ?"))
              #'courier-dispatch)))

(ert-deftest courier-request-mode-binds-section-navigation ()
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c C-j"))
              #'courier-request-jump-section))
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c ["))
              #'courier-request-prev-section))
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c ]"))
              #'courier-request-next-section)))

(ert-deftest courier-response-mode-uses-bracket-view-navigation ()
  (should (eq (lookup-key courier--response-mode-map (kbd "]"))
              #'courier-response-next-tab))
  (should (eq (lookup-key courier--response-mode-map (kbd "["))
              #'courier-response-prev-tab)))

(ert-deftest courier-current-export-command-builds-curl ()
  (courier-test--with-request
      (concat "# @auth bearer {{token}}\n"
              "# @var token abc123\n"
              "# @param page 1\n"
              "POST https://example.com/users\n"
              "Accept: application/json\n"
              "\n"
              "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (setq-local courier--export-format 'curl
                courier--export-interpolate t)
    (let ((command (courier--current-export-command)))
      (should (string-match-p (regexp-quote "curl --request POST") command))
      (should (string-match-p
               (regexp-quote "--url https\\://example.com/users\\?page\\=1")
               command))
      (should (string-match-p
               (regexp-quote "authorization\\:\\ Bearer\\ abc123")
               command))
      (should (string-match-p (regexp-quote "--data-raw") command)))))

(ert-deftest courier-current-export-command-builds-httpie ()
  (courier-test--with-request
      (concat "# @param page 1\n"
              "GET https://example.com/users\n"
              "Accept: application/json\n")
    (courier-request-mode)
    (setq-local courier--export-format 'httpie
                courier--export-interpolate t)
    (let ((command (courier--current-export-command)))
      (should (string-match-p
               (regexp-quote "http GET https\\://example.com/users\\?page\\=1")
               command))
      (should (string-match-p
               (regexp-quote "accept\\:application/json")
               command)))))

(ert-deftest courier-current-export-command-preserves-source-placeholders ()
  (courier-test--with-request
      (concat "# @auth bearer {{token}}\n"
              "# @param page {{page}}\n"
              "GET https://example.com/users/{{user_id}}\n")
    (courier-request-mode)
    (setq-local courier--export-format 'curl
                courier--export-interpolate nil)
    (let ((command (courier--current-export-command)))
      (should (string-match-p
               (regexp-quote "users/\\{\\{user_id\\}\\}")
               command))
      (should (string-match-p
               (regexp-quote "page\\=\\{\\{page\\}\\}")
               command))
      (should (string-match-p
               (regexp-quote "Bearer\\ \\{\\{token\\}\\}")
               command)))))

(ert-deftest courier-request-insert-param-adds-directive-before-request-line ()
  (courier-test--with-request
      "GET https://example.com/users\n"
    (courier-request-mode)
    (courier-request-insert-param "page" "1")
    (should (equal (buffer-string)
                   (concat "# @param page 1\n"
                           "GET https://example.com/users\n")))))

(ert-deftest courier-request-insert-auth-adds-directive-before-request-line ()
  (courier-test--with-request
      "GET https://example.com/users\n"
    (courier-request-mode)
    (courier-request-insert-auth 'bearer "{{token}}")
    (goto-char (point-min))
    (should (looking-at (regexp-quote "# @auth bearer {{token}}")))))

(ert-deftest courier-request-insert-pre-request-script-adds-block ()
  (courier-test--with-request
      "GET https://example.com/users\n"
    (courier-request-mode)
    (courier-request-insert-pre-request-script)
    (should (equal (buffer-string)
                   (concat "# @begin pre-request\n"
                           "# \n"
                           "# @end\n"
                           "GET https://example.com/users\n")))
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))
                   "# "))))

(ert-deftest courier-request-goto-auth-moves-to-auth-line ()
  (courier-test--with-request
      (concat "# @auth bearer {{token}}\n"
              "GET https://example.com/users\n")
    (courier-request-mode)
    (goto-char (point-max))
    (courier-request-goto-auth)
    (should (looking-at (regexp-quote "# @auth bearer {{token}}")))))

(ert-deftest courier-request-goto-post-response-script-moves-to-script ()
  (courier-test--with-request
      (concat "# @begin post-response\n"
              "# courier-script-response\n"
              "# @end\n"
              "GET https://example.com/users\n")
    (courier-request-mode)
    (goto-char (point-max))
    (courier-request-goto-post-response-script)
    (should (looking-at (regexp-quote "# @begin post-response")))))

(ert-deftest courier-request-next-section-jumps-to-headers ()
  (courier-test--with-request
      (concat "GET https://example.com/users\n"
              "Accept: application/json\n"
              "\n"
              "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (goto-char (point-min))
    (courier-request-next-section)
    (should (looking-at "Accept: application/json"))))

(ert-deftest courier-request-jump-section-opens-params-editor ()
  (courier-test--with-request
      "GET https://example.com/users?page=1\n"
    (courier-request-mode)
    (let ((buffer (courier-request-jump-section 'params)))
      (should (buffer-live-p buffer))
      (with-current-buffer buffer
        (should (derived-mode-p 'courier-request-params-mode))))))

(ert-deftest courier-request-edit-params-loads-query-into-editor ()
  (courier-test--with-request
      "GET https://example.com/users?page=1&sort=created_at\n"
    (courier-request-mode)
    (let ((buffer (courier-request-edit-params)))
      (with-current-buffer buffer
        (should (derived-mode-p 'courier-request-params-mode))
        (should (equal (buffer-string)
                       "page = 1\nsort = created_at"))))))

(ert-deftest courier-request-params-apply-rewrites-request-url ()
  (courier-test--with-request
      "GET https://example.com/users?page=1&sort=created_at\n"
    (courier-request-mode)
    (let ((source-buffer (current-buffer))
          (buffer (courier-request-edit-params)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "page = 2\nfilter = active")
        (courier-request-params-apply))
      (with-current-buffer source-buffer
        (should (equal (courier--current-request-url)
                       "https://example.com/users?page=2&filter=active"))))))

(ert-deftest courier-request-set-method-works-on-incomplete-request-line ()
  (courier-test--with-request
      (concat "# @name Demo\n"
              "GET \n"
              "Accept: application/json\n")
    (courier-request-mode)
    (courier-request-set-method "POST")
    (goto-char (point-min))
    (should (search-forward "POST " nil t))))

(ert-deftest courier-request-set-method-refreshes-overlay ()
  (courier-test--with-request
      (concat "# @name Demo\n"
              "GET \n"
              "Accept: application/json\n")
    (courier-request-mode)
    (courier-request-set-method "POST")
    (should (overlayp courier--method-overlay))
    (should (equal (buffer-substring-no-properties
                    (overlay-start courier--method-overlay)
                    (overlay-end courier--method-overlay))
                   "POST"))))

(ert-deftest courier-request-set-method-rejects-missing-request-line ()
  (courier-test--with-request "# @name Demo\n"
    (courier-request-mode)
    (should-error (courier-request-set-method "POST")
                  :type 'user-error)))

(ert-deftest courier-open-switches-environment ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (env-dir (expand-file-name "env" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           selected-candidate)
      (make-directory request-dir t)
      (make-directory env-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"defaultEnv\": \"local\"\n}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file (expand-file-name "local.env" env-dir)
        (insert "token=local\n"))
      (with-temp-file (expand-file-name "prod.env" env-dir)
        (insert "token=prod\n"))
      (find-file request-file)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _args)
                         (setq selected-candidate
                               (car (seq-find
                                     (lambda (candidate)
                                       (eq (plist-get (cdr candidate) :kind) 'env))
                                     collection)))
                         selected-candidate)))
              (courier-open))
            (should (equal courier--active-env "local")))
        (kill-buffer (current-buffer))))))

(ert-deftest courier-open-candidates-group-requests-and-environments ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (env-dir (expand-file-name "env" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           candidates)
      (make-directory request-dir t)
      (make-directory env-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (with-temp-file (expand-file-name "local.env" env-dir)
        (insert "token=local\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (setq candidates (courier--open-candidates))
        (should (seq-some
                 (lambda (candidate)
                   (string= (get-text-property 0 'courier-group (car candidate))
                            "Requests"))
                 candidates))
        (should (seq-some
                 (lambda (candidate)
                   (string= (get-text-property 0 'courier-group (car candidate))
                            "Environments"))
                 candidates))))))

(ert-deftest courier-overview-renders-request-list ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           overview-buffer)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"name\": \"Demo API\"\n}\n"))
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _args)
                   (setq overview-buffer buffer)
                   buffer)))
        (let ((default-directory collection-root))
          (courier-overview)))
      (with-current-buffer overview-buffer
        (should (derived-mode-p 'courier-overview-mode))
        (should (string-match-p "Collection: Demo API"
                                (buffer-string)))
        (should (string-match-p "Get User" (buffer-string)))
        (should (string-match-p "users/get-user\\.http"
                                (buffer-string)))))))

(ert-deftest courier-overview-open-opens-request-at-point ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           overview-buffer
           opened-file)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _args)
                   (setq overview-buffer buffer)
                   buffer)))
        (let ((default-directory collection-root))
          (courier-overview)))
      (with-current-buffer overview-buffer
        (goto-char (point-min))
        (search-forward "Get User")
        (beginning-of-line)
        (cl-letf (((symbol-function 'find-file)
                   (lambda (path)
                     (setq opened-file path))))
          (courier-overview-open)))
      (should (equal opened-file request-file)))))

(ert-deftest courier-new-request-creates-untitled-draft ()
  (let ((courier--untitled-request-counter 0)
        created-buffer)
    (save-window-excursion
      (let ((default-directory temporary-file-directory))
        (courier-new-request)
        (setq created-buffer (current-buffer))
        (should (derived-mode-p 'courier-request-mode))
        (should-not buffer-file-name)
        (should (equal (buffer-string)
                       "# @name Untitled 1\nGET \nAccept: application/json\n"))))
    (when (buffer-live-p created-buffer)
      (kill-buffer created-buffer))))

(ert-deftest courier-new-request-uses-configured-default-method ()
  (let ((courier--untitled-request-counter 0)
        (courier-default-request-method "POST")
        created-buffer)
    (save-window-excursion
      (let ((default-directory temporary-file-directory))
        (courier-new-request)
        (setq created-buffer (current-buffer))
        (should (derived-mode-p 'courier-request-mode))
        (should-not buffer-file-name)
        (should (equal (buffer-string)
                       "# @name Untitled 1\nPOST \nAccept: application/json\n"))))
    (when (buffer-live-p created-buffer)
      (kill-buffer created-buffer))))

(ert-deftest courier-draft-and-response-buffers-use-distinct-names ()
  (should (equal (courier--draft-buffer-name "Untitled 1")
                 "*courier-request: Untitled 1*"))
  (should (equal (courier--response-buffer-name '(:name "Untitled 1"))
                 "*courier-response: Untitled 1*")))

(ert-deftest courier-request-save-buffer-uses-configured-requests-dir ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (requests-root (expand-file-name "api-requests" collection-root))
           draft-buffer)
      (make-directory collection-root t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"requestsDir\": \"api-requests\"\n}\n"))
      (setq draft-buffer (generate-new-buffer "*courier-save-test*"))
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "# @name Create User\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _args)
                         collection-root)))
              (courier-request-save-buffer))
            (should (equal (expand-file-name buffer-file-name)
                           (expand-file-name "create-user.http" requests-root)))
            (should (file-exists-p (expand-file-name "create-user.http" requests-root))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-request-save-buffer-creates-collection-when-needed ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (draft-buffer (generate-new-buffer "*courier-create-collection*")))
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "# @name Untitled 1\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _args)
                         collection-root))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest _args)
                         t)))
              (courier-request-save-buffer))
            (should (file-exists-p (expand-file-name "courier.json" collection-root)))
            (should (file-exists-p (expand-file-name "requests/untitled-1.http"
                                                     collection-root))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-request-save-buffer-prompts-from-default-collection-directory ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-default-collection-directory
            (expand-file-name "courier-home" root))
           (collection-root (expand-file-name "api-collection"
                                              courier-default-collection-directory))
           (draft-buffer (generate-new-buffer "*courier-save-default-root*"))
           captured-initial)
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "# @name Untitled 1\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (_prompt initial &rest _args)
                         (setq captured-initial initial)
                         collection-root))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest _args)
                         t)))
              (courier-request-save-buffer))
            (let ((expected
                   (file-name-as-directory
                    (expand-file-name courier-default-collection-directory))))
              (should (equal (file-name-as-directory
                              (expand-file-name captured-initial))
                             expected)))
            (should (file-exists-p
                     (expand-file-name "courier.json" collection-root))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-new-folder-creates-under-request-root ()
  (courier-test--with-temp-dir (root)
    (let ((collection-root (expand-file-name "api-collection" root)))
      (make-directory collection-root t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (let ((default-directory collection-root))
        (courier-new-folder "admin/tools"))
      (should (file-directory-p
               (expand-file-name "requests/admin/tools" collection-root))))))

(ert-deftest courier-rename-request-renames-file-and-directive ()
  (courier-test--with-temp-dir (root)
    (let* ((request-file (expand-file-name "get-user.http" root))
           (new-file (expand-file-name "create-user.http" root))
           buffer)
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-request-mode)
            (courier-rename-request "Create User")
            (should (equal (expand-file-name buffer-file-name) new-file))
            (should (file-exists-p new-file))
            (should-not (file-exists-p request-file))
            (should (string-match-p "# @name Create User" (buffer-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest courier-move-request-moves-file-and-buffer ()
  (courier-test--with-temp-dir (root)
    (let* ((request-dir (expand-file-name "users" root))
           (target-dir (expand-file-name "admin/tools" root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (new-file (expand-file-name "get-user.http" target-dir))
           buffer)
      (make-directory request-dir t)
      (with-temp-file request-file
        (insert "# @name Get User\nGET https://example.com/users/42\n"))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-request-mode)
            (courier-move-request target-dir)
            (should (equal (expand-file-name buffer-file-name) new-file))
            (should (file-exists-p new-file))
            (should-not (file-exists-p request-file)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'courier-test)

;;; courier-test.el ends here
