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
                  "# @var token abc\n"
                  "# @test status == 200\n"
                  "POST https://example.com\n"
                  "Accept: application/json\n"
                  "\n"
                  "{}\n"))))
    (should (equal (plist-get request :name) "Example"))
    (should (equal (plist-get (plist-get request :settings) :timeout) 12))
    (should (equal (plist-get (plist-get request :settings) :follow-redirects) t))
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
                        :vars '(("id" . "42") ("token" . "abc"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :url) "https://example.com/42"))
    (should (equal (plist-get resolved :headers)
                   '(("authorization" . "Bearer abc"))))
    (should (equal (plist-get resolved :body) "{\"id\":\"42\"}"))))

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

(ert-deftest courier-scan-env-files-finds-current-and-parent ()
  (courier-test--with-temp-dir (root)
    (let* ((child (expand-file-name "api/request" root))
           (outer-env (expand-file-name ".env" root))
           (inner-env (expand-file-name "local.env" child)))
      (make-directory child t)
      (with-temp-file outer-env (insert "base_url=https://outer.example.com\n"))
      (with-temp-file inner-env (insert "base_url=https://inner.example.com\n"))
      (let ((entries (courier-scan-env-files child)))
        (should (member (cons "default" outer-env) entries))
        (should (member (cons "local" inner-env) entries))))))

(ert-deftest courier-scan-env-files-extracts-names ()
  (courier-test--with-temp-dir (root)
    (let ((default-env (expand-file-name ".env" root))
          (prod-env (expand-file-name "prod.env" root)))
      (with-temp-file default-env (insert "foo=1\n"))
      (with-temp-file prod-env (insert "foo=2\n"))
      (should (equal (courier-scan-env-files root)
                     `(("default" . ,default-env)
                       ("prod" . ,prod-env)))))))

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

(ert-deftest courier-format-body-falls-back-on-invalid-json ()
  (with-temp-buffer
    (setq-local courier--body-pretty t)
    (let ((body (courier--format-body
                 '(:content-type "application/json"
                   :body-text "{\"foo\":}"))))
      (should (equal body "{\"foo\":}\n")))))

(ert-deftest courier-format-body-skips-non-json ()
  (with-temp-buffer
    (setq-local courier--body-pretty t)
    (let ((body (courier--format-body
                 '(:content-type "text/plain"
                   :body-text "{\"foo\":1}"))))
      (should (equal body "{\"foo\":1}\n")))))

(ert-deftest courier-test-response-next-section ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-next-section)
    (should (string= (get-text-property (point) 'courier-section) "Headers"))
    (courier-response-next-section)
    (should (string= (get-text-property (point) 'courier-section) "Body"))))

(ert-deftest courier-test-response-prev-section ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-next-section)
    (courier-response-next-section)
    (should (string= (get-text-property (point) 'courier-section) "Body"))
    (courier-response-prev-section)
    (should (string= (get-text-property (point) 'courier-section) "Headers"))
    (courier-response-prev-section)
    (should (string= (get-text-property (point) 'courier-section) "Summary"))))

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

(ert-deftest courier-test-history-prev-shows-older ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier--render-response
     (courier-test--make-response 404)
     courier-test--request)
    (courier-response-history-prev)
    (should (= courier--history-index 1))
    (should (string-match-p "2/2" (format "%s" header-line-format)))
    (should (= (plist-get courier--response :status-code) 200))))

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

(provide 'courier-test)

;;; courier-test.el ends here
