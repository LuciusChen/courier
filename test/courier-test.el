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

(defun courier-test--http-content (&rest pairs)
  "Return a Courier v1 `.http' fixture from plist PAIRS."
  (let ((request (courier--empty-request "/tmp/test.http"))
        key
        value)
    (setq request (plist-put request :method "GET"))
    (setq request (plist-put request :url "https://example.com"))
    (while pairs
      (setq key (pop pairs)
            value (pop pairs))
      (setq request (plist-put request key value)))
    (courier--serialize-request request)))

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

(defun courier-test--local-map-binding (map key)
  "Return MAP's local binding for KEY without inherited parent bindings."
  (let ((copy (copy-keymap map)))
    (set-keymap-parent copy nil)
    (lookup-key copy (kbd key))))

(defun courier-test--face-includes-p (face expected)
  "Return non-nil when FACE includes EXPECTED."
  (if (listp face)
      (memq expected face)
    (eq face expected)))

;; Parser tests.

(ert-deftest courier-parse-minimal-get ()
  (let ((request (courier-test--parsed "GET https://example.com\n")))
    (should (equal (plist-get request :method) "GET"))
    (should (equal (plist-get request :url) "https://example.com"))
    (should (equal (plist-get request :headers) nil))
    (should (equal (plist-get request :body) ""))))

(ert-deftest courier-parse-full-request-with-front-matter ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :name "Example"
           :method "POST"
           :url "https://example.com"
           :headers '(("accept" . "application/json"))
           :body "{}\n"
           :params '(("page" . "1"))
           :vars '(("token" . "abc"))
           :auth '(:type bearer :token "{{token}}")
           :tests '("status == 200")
           :settings '(:timeout 12 :follow-redirects t)))))
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

(ert-deftest courier-parse-rejects-unknown-front-matter-key ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "unknown = \"nope\"\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-front-matter-table ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "[params]\n"
            "page = \"1\"\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-auth-table ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "[auth]\n"
            "type = \"nope\"\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-api-key-auth-from-front-matter ()
  (let ((request
         (courier-test--parsed
          (concat "+++\n"
                  "[auth]\n"
                  "type = \"api_key\"\n"
                  "in = \"header\"\n"
                  "name = \"x-api-key\"\n"
                  "value = \"{{api_key}}\"\n"
                  "+++\n\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :auth)
                   '(:type api_key
                     :in "header"
                     :name "x-api-key"
                     :value "{{api_key}}")))))

(ert-deftest courier-parse-oauth2-auth-from-front-matter ()
  (let ((request
         (courier-test--parsed
          (concat "+++\n"
                  "[auth]\n"
                  "type = \"oauth2\"\n"
                  "grant_type = \"client_credentials\"\n"
                  "token_url = \"https://example.com/oauth/token\"\n"
                  "client_id = \"client-id\"\n"
                  "client_secret = \"client-secret\"\n"
                  "scopes = [\"read\", \"write\"]\n"
                  "+++\n\n"
                  "GET https://example.com\n"))))
    (should (equal (plist-get request :auth)
                   '(:type oauth2
                     :grant-type "client_credentials"
                     :token-url "https://example.com/oauth/token"
                     :client-id "client-id"
                     :client-secret "client-secret"
                     :scopes ("read" "write"))))))

(ert-deftest courier-parse-body-type-from-front-matter ()
  (let ((request
         (courier-test--parsed
          (concat "+++\n"
                  "[body]\n"
                  "type = \"xml\"\n"
                  "+++\n\n"
                  "POST https://example.com\n"
                  "\n"
                  "<ok/>\n"))))
    (should (eq (plist-get request :body-type) 'xml))))

(ert-deftest courier-parse-binary-body-from-front-matter ()
  (let ((request
         (courier-test--parsed
          (concat "+++\n"
                  "[body]\n"
                  "type = \"binary\"\n"
                  "path = \"./payload.bin\"\n"
                  "content_type = \"application/octet-stream\"\n"
                  "+++\n\n"
                  "POST https://example.com/upload\n"))))
    (should (eq (plist-get request :body-type) 'binary))
    (should (equal (plist-get request :body-file-path) "./payload.bin"))
    (should (equal (plist-get request :body-file-content-type)
                   "application/octet-stream"))))

(ert-deftest courier-parse-multipart-body-from-front-matter ()
  (let ((request
         (courier-test--parsed
          (concat "+++\n"
                  "[body]\n"
                  "type = \"multipart\"\n"
                  "\n"
                  "[[body.parts]]\n"
                  "name = \"avatar\"\n"
                  "kind = \"file\"\n"
                  "path = \"./avatar.png\"\n"
                  "content_type = \"image/png\"\n"
                  "\n"
                  "[[body.parts]]\n"
                  "name = \"display_name\"\n"
                  "kind = \"text\"\n"
                  "value = \"Lucy\"\n"
                  "+++\n\n"
                  "POST https://example.com/upload\n"))))
    (should (eq (plist-get request :body-type) 'multipart))
    (should (equal (plist-get request :body-parts)
                   '((:name "avatar"
                      :kind file
                      :path "./avatar.png"
                      :content-type "image/png")
                     (:name "display_name"
                      :kind text
                      :value "Lucy"))))))

(ert-deftest courier-serialize-binary-body-round-trips ()
  (let* ((request (list :path "/tmp/test.http"
                        :method "POST"
                        :url "https://example.com/upload"
                        :headers nil
                        :body ""
                        :body-type 'binary
                        :body-file-path "./payload.bin"
                        :body-file-content-type "application/octet-stream"
                        :auth nil
                        :vars nil
                        :tests nil
                        :settings nil))
         (parsed (courier-test--parsed (courier--serialize-request request))))
    (should (eq (plist-get parsed :body-type) 'binary))
    (should (equal (plist-get parsed :body-file-path) "./payload.bin"))
    (should (equal (plist-get parsed :body-file-content-type)
                   "application/octet-stream"))))

(ert-deftest courier-serialize-oauth2-auth-round-trips ()
  (let* ((request (list :path "/tmp/test.http"
                        :method "GET"
                        :url "https://example.com/data"
                        :headers nil
                        :body ""
                        :body-type 'json
                        :auth '(:type oauth2
                                :grant-type "client_credentials"
                                :token-url "https://example.com/oauth/token"
                                :client-id "client-id"
                                :client-secret "client-secret"
                                :scopes ("read" "write"))
                        :vars nil
                        :tests nil
                        :settings nil))
         (parsed (courier-test--parsed (courier--serialize-request request))))
    (should (equal (plist-get parsed :auth)
                   '(:type oauth2
                     :grant-type "client_credentials"
                     :token-url "https://example.com/oauth/token"
                     :client-id "client-id"
                     :client-secret "client-secret"
                     :scopes ("read" "write"))))))

(ert-deftest courier-oauth2-token-request-client-credentials ()
  (let* ((request (list :path "/tmp/test.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/data"
                        :headers nil
                        :body ""
                        :body-type 'json
                        :auth '(:type oauth2
                                :grant-type "client_credentials"
                                :token-url "https://example.com/oauth/token"
                                :client-id "{{client_id}}"
                                :client-secret "{{client_secret}}"
                                :scopes ("read" "{{scope}}"))
                        :resolved-vars '(("client_id" . "courier-client")
                                         ("client_secret" . "courier-secret")
                                         ("scope" . "write"))
                        :settings '(:timeout 12)))
         (token-request (courier--oauth2-token-request request)))
    (should (equal (plist-get token-request :method) "POST"))
    (should (equal (plist-get token-request :url)
                   "https://example.com/oauth/token"))
    (should (eq (plist-get token-request :body-type) 'form-urlencoded))
    (should (equal (plist-get token-request :body)
                   (string-join
                    '("grant_type=client_credentials"
                      "client_id=courier-client"
                      "client_secret=courier-secret"
                      "scope=read%20write")
                    "&")))
    (should (equal (plist-get token-request :headers)
                   '(("accept" . "application/json")
                     ("content-type" . "application/x-www-form-urlencoded"))))
    (should (equal (plist-get token-request :settings) '(:timeout 12)))))

(ert-deftest courier-oauth2-access-token-parses-json-response ()
  (should (equal
           (courier--oauth2-access-token
            '(:status-code 200
              :content-type "application/json"
              :body-text "{\"access_token\":\"abc123\"}"))
           "abc123")))

(ert-deftest courier-parse-rejects-invalid-body-type ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "[body]\n"
            "type = \"yaml\"\n"
            "+++\n\n"
            "POST https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-method ()
  (should-error
   (courier-test--parsed "TRACE https://example.com\n")
   :type 'user-error))

(ert-deftest courier-parse-rejects-missing-request-line ()
  (should-error
   (courier-test--parsed "+++\nname = \"Missing\"\n+++\n")
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
    (concat "+++\n"
            "timeout = \"nope\"\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-timeout-zero ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "timeout = 0\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-timeout-negative ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "timeout = -1\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-invalid-follow-redirects ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "follow_redirects = \"maybe\"\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-rejects-malformed-toml-assignment ()
  (should-error
   (courier-test--parsed
    (concat "+++\n"
            "timeout\n"
            "+++\n\n"
            "GET https://example.com\n"))
   :type 'user-error))

(ert-deftest courier-parse-var-multi-word-value ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :vars '(("greeting" . "hello brave world"))))))
    (should (equal (plist-get request :vars)
                   '(("greeting" . "hello brave world"))))))

(ert-deftest courier-parse-multiple-vars ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :vars '(("one" . "1") ("two" . "2"))))))
    (should (equal (plist-get request :vars)
                   '(("one" . "1") ("two" . "2"))))))

(ert-deftest courier-parse-phased-vars-from-front-matter ()
  (let ((request
         (courier-test--parsed
          "+++\n[vars]\ntoken = \"root\"\n\n[vars.pre_request]\nrequest_id = \"42\"\n\n[[vars.post_response]]\nname = \"token\"\nfrom = \"json\"\nexpr = \"$.token\"\n+++\n\nGET https://example.com\n")))
    (should (equal (plist-get request :vars)
                   '(("token" . "root"))))
    (should (equal (plist-get request :pre-request-vars)
                   '(("request_id" . "42"))))
    (should (equal (plist-get request :post-response-vars)
                   '((:name "token" :from json :expr "$.token"))))))

(ert-deftest courier-parse-multiple-params ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :params '(("page" . "1")
                     ("sort" . "created at"))))))
    (should (equal (plist-get request :params)
                   '(("page" . "1")
                     ("sort" . "created at"))))))

(ert-deftest courier-parse-script-blocks ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :pre-request-script
           "(plist-put courier-script-request :url \"https://example.com/from-script\")\ncourier-script-request"
           :post-response-script
           "(plist-put courier-script-response :reason \"Changed\")\ncourier-script-response"))))
    (should (string-match-p "from-script"
                            (plist-get request :pre-request-script)))
    (should (string-match-p "Changed"
                            (plist-get request :post-response-script)))))

(ert-deftest courier-parse-multiple-tests ()
  (let ((request
         (courier-test--parsed
          (courier-test--http-content
           :tests '("status == 200" "size < 1000")))))
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
                   '(("authorization" . "Bearer abc")
                     ("content-type" . "application/json"))))
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

(ert-deftest courier-resolve-request-adds-api-key-header ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/data"
                        :headers nil
                        :body ""
                        :params nil
                        :vars '(("token" . "abc123"))
                        :auth '(:type api_key
                                :in "header"
                                :name "x-api-key"
                                :value "{{token}}")
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("x-api-key" . "abc123"))))))

(ert-deftest courier-resolve-request-appends-api-key-query-param ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "GET"
                        :url "https://example.com/data"
                        :headers nil
                        :body ""
                        :params '(("page" . "1"))
                        :vars '(("token" . "abc123"))
                        :auth '(:type api_key
                                :in "query"
                                :name "api_key"
                                :value "{{token}}")
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :url)
                   "https://example.com/data?page=1&api_key=abc123"))))

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

(ert-deftest courier-resolve-request-adds-json-content-type-by-body-type ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com"
                        :headers nil
                        :body "{\"id\":1}"
                        :body-type 'json
                        :params nil
                        :vars nil
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("content-type" . "application/json"))))))

(ert-deftest courier-resolve-request-keeps-explicit-content-type ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com"
                        :headers '(("content-type" . "application/merge-patch+json"))
                        :body "{\"id\":1}"
                        :body-type 'json
                        :params nil
                        :vars nil
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :headers)
                   '(("content-type" . "application/merge-patch+json"))))))

(ert-deftest courier-resolve-request-encodes-form-urlencoded-body ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com"
                        :headers nil
                        :body "page={{page}}&q={{term}}"
                        :body-type 'form-urlencoded
                        :params nil
                        :vars '(("page" . "2")
                                ("term" . "Lucy Chen"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :body)
                   "page=2&q=Lucy%20Chen"))
    (should (equal (plist-get resolved :headers)
                   '(("content-type" . "application/x-www-form-urlencoded"))))))

(ert-deftest courier-resolve-request-clears-body-for-none-type ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com"
                        :headers nil
                        :body "{\"id\":1}"
                        :body-type 'none
                        :params nil
                        :vars nil
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :body) ""))
    (should (equal (plist-get resolved :headers) nil))))

(ert-deftest courier-resolve-request-expands-binary-body-metadata ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com/upload"
                        :headers nil
                        :body ""
                        :body-type 'binary
                        :body-file-path "./{{file_name}}"
                        :body-file-content-type "application/pdf"
                        :params nil
                        :vars '(("file_name" . "payload.pdf"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :body-file-path) "./payload.pdf"))
    (should (equal (plist-get resolved :headers)
                   '(("content-type" . "application/pdf"))))))

(ert-deftest courier-resolve-request-expands-multipart-body-parts ()
  (let* ((request (list :path "/tmp/demo.http"
                        :name "Demo"
                        :method "POST"
                        :url "https://example.com/upload"
                        :headers nil
                        :body ""
                        :body-type 'multipart
                        :body-parts '((:name "avatar"
                                        :kind file
                                        :path "./{{file_name}}"
                                        :content-type "image/png")
                                       (:name "display_name"
                                        :kind text
                                        :value "{{name}}"))
                        :params nil
                        :vars '(("file_name" . "avatar.png")
                                ("name" . "Lucy Chen"))
                        :tests nil
                        :settings nil))
         (resolved (courier-resolve-request request)))
    (should (equal (plist-get resolved :body-parts)
                   '((:name "avatar"
                      :kind file
                      :path "./avatar.png"
                      :content-type "image/png")
                     (:name "display_name"
                      :kind text
                      :value "Lucy Chen"))))
    (should (equal (plist-get resolved :headers) nil))))

(ert-deftest courier-validate-request-rejects-body-for-none-type ()
  (should-error
   (courier-validate-request
    '(:path "/tmp/demo.http"
      :method "POST"
      :url "https://example.com"
      :headers nil
      :body "still here"
      :body-type none
      :params nil
      :vars nil
      :tests nil
      :settings nil))
   :type 'user-error))

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

(ert-deftest courier-build-curl-command-binary-uses-source-file ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "POST"
                   :url "https://example.com/upload"
                   :headers (("content-type" . "application/pdf"))
                   :body ""
                   :body-type binary
                   :body-file-path "./payload.pdf"
                   :settings nil)
                 header body meta)))
      (should (string= (courier-test--argv-flag-value argv "--data-binary")
                       "@./payload.pdf"))
      (should-not (member "-F" argv)))))

(ert-deftest courier-build-curl-command-multipart-uses-form-parts ()
  (courier-test--with-temp-files ((header ".headers") (body ".body") (meta ".meta"))
    (let ((argv (courier-build-curl-command
                 '(:method "POST"
                   :url "https://example.com/upload"
                   :headers nil
                   :body ""
                   :body-type multipart
                   :body-parts ((:name "avatar"
                                 :kind file
                                 :path "./avatar.png"
                                 :content-type "image/png")
                                (:name "display_name"
                                 :kind text
                                 :value "Lucy"))
                   :settings nil)
                 header body meta)))
      (let ((form-values nil)
            (start 0)
            position)
        (while (setq position (cl-position "-F" argv :test #'string= :start start))
          (push (nth (1+ position) argv) form-values)
          (setq start (1+ position)))
        (setq form-values (nreverse form-values))
        (should (equal form-values
                       '("avatar=@./avatar.png;type=image/png"
                         "display_name=Lucy"))))
      (should-not (courier-test--argv-flag-value argv "--data-binary")))))

(ert-deftest courier-send-request-oauth2-fetches-token-before-main-request ()
  (let (calls final-response processes)
    (cl-letf (((symbol-function 'courier--send-resolved-request)
               (lambda (request callback)
                 (let ((process (make-pipe-process
                                 :name (format "courier-test-oauth-%d" (length calls))
                                 :buffer nil
                                 :noquery t)))
                   (push process processes)
                   (push request calls)
                   (if (string= (plist-get request :url)
                                "https://example.com/oauth/token")
                       (funcall callback
                                '(:status-code 200
                                  :reason "OK"
                                  :headers (("content-type" . "application/json"))
                                  :content-type "application/json"
                                  :body-text "{\"access_token\":\"abc123\"}"
                                  :body-file nil
                                  :stderr ""
                                  :exit-code 0
                                  :duration-ms 10
                                  :size 27))
                     (funcall callback
                              '(:status-code 200
                                :reason "OK"
                                :headers nil
                                :content-type "application/json"
                                :body-text "{}"
                                :body-file nil
                                :stderr ""
                                :exit-code 0
                                :duration-ms 20
                                :size 2)))
                   process))))
      (unwind-protect
          (let ((request (list :path "/tmp/test.http"
                               :name "Demo"
                               :method "GET"
                               :url "https://example.com/data"
                               :headers nil
                               :body ""
                               :body-type 'json
                               :auth '(:type oauth2
                                       :grant-type "client_credentials"
                                       :token-url "https://example.com/oauth/token"
                                       :client-id "client-id"
                                       :client-secret "client-secret"
                                       :scopes ("read"))
                               :resolved-vars nil
                               :settings nil)))
            (courier-send-request request
                                  (lambda (response)
                                    (setq final-response response)))
            (setq calls (nreverse calls))
            (should (= (length calls) 2))
            (should (equal (plist-get (car calls) :url)
                           "https://example.com/oauth/token"))
            (should (equal (plist-get (cadr calls) :headers)
                           '(("authorization" . "Bearer abc123"))))
            (should (= (plist-get final-response :status-code) 200)))
        (dolist (process processes)
          (when (process-live-p process)
            (delete-process process)))))))

(ert-deftest courier-send-request-oauth2-surfaces-token-errors ()
  (let (final-response processes)
    (cl-letf (((symbol-function 'courier--send-resolved-request)
               (lambda (_request callback)
                 (let ((process (make-pipe-process
                                 :name "courier-test-oauth-fail"
                                 :buffer nil
                                 :noquery t)))
                   (push process processes)
                   (funcall callback
                            '(:status-code 200
                              :reason "OK"
                              :headers (("content-type" . "application/json"))
                              :content-type "application/json"
                              :body-text "{\"token_type\":\"bearer\"}"
                              :body-file nil
                              :stderr ""
                              :exit-code 0
                              :duration-ms 10
                              :size 25))
                   process))))
      (unwind-protect
          (let ((request (list :path "/tmp/test.http"
                               :name "Demo"
                               :method "GET"
                               :url "https://example.com/data"
                               :headers nil
                               :body ""
                               :body-type 'json
                               :auth '(:type oauth2
                                       :grant-type "client_credentials"
                                       :token-url "https://example.com/oauth/token"
                                       :client-id "client-id"
                                       :client-secret "client-secret")
                               :resolved-vars nil
                               :settings nil)))
            (courier-send-request request
                                  (lambda (response)
                                    (setq final-response response)))
            (should (equal (plist-get final-response :reason) "OAuth2 Token Error"))
            (should (string-match-p "access_token" (or (plist-get final-response :stderr) ""))))
        (dolist (process processes)
          (when (process-live-p process)
            (delete-process process)))))))

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
                       :default-env nil
                       :defaults nil))))))

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

(ert-deftest courier-resolved-current-request-uses-runtime-and-pre-request-vars ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir)))
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"name\": \"API Collection\"\n}\n"))
      (with-temp-file request-file
        (insert "+++\n"
                "[vars.pre_request]\n"
                "request_id = \"42\"\n"
                "+++\n\n"
                "GET https://example.com/users/{{token}}/{{request_id}}\n"))
      (courier--write-runtime-vars collection-root nil '(("token" . "runtime-token")))
      (with-current-buffer (find-file-noselect request-file)
        (unwind-protect
            (let ((resolved (courier--resolved-current-request)))
              (should (equal (plist-get resolved :url)
                             "https://example.com/users/runtime-token/42")))
          (kill-buffer (current-buffer)))))))

(ert-deftest courier-post-response-vars-persist-runtime-values ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests" collection-root))
           (request-file (expand-file-name "login.http" request-dir))
           request
           response
           result)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"name\": \"API Collection\"\n}\n"))
      (with-temp-file request-file
        (insert "+++\n"
                "[[vars.post_response]]\n"
                "name = \"token\"\n"
                "from = \"json\"\n"
                "expr = \"$.token\"\n"
                "+++\n\n"
                "POST https://example.com/login\n"))
      (setq request (courier-parse-file request-file))
      (setq request (plist-put request :path request-file))
      (setq response (courier-test--response
                      :status-code 200
                      :content-type "application/json"
                      :body-text "{\"token\":\"abc123\"}"))
      (setq result (courier--apply-post-response-vars request response nil))
      (should (equal (plist-get result :post-response-vars)
                     '(("token" . "abc123"))))
      (should (equal (courier--read-runtime-vars collection-root nil)
                     '(("token" . "abc123")))))))

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

(ert-deftest courier-auto-body-view-detects-document ()
  (should (eq (courier--auto-body-view
               '(:content-type "application/pdf" :body-file "/tmp/demo.pdf"))
              'document)))

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
    (should (string-match-p "\\` Response  >>  200 OK  •  42ms  •  5B"
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

(ert-deftest courier-response-open-body-surfaces-image-render-errors ()
  (with-temp-buffer
    (let (viewer-buffer)
      (courier--render-response
       '(:status-code 200
         :reason "OK"
         :headers (("content-type" . "image/png"))
         :duration-ms 10
         :size 12
         :body-file "/tmp/demo.png"
         :content-type "image/png"
         :tests nil
         :stderr ""
         :exit-code 0)
       courier-test--request)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _args)
                   (setq viewer-buffer buffer)
                   buffer))
                ((symbol-function 'display-images-p) (lambda () t))
                ((symbol-function 'create-image)
                 (lambda (&rest _args)
                   (error "broken image decoder"))))
        (courier-response-open-body))
      (with-current-buffer viewer-buffer
        (should (string-match-p "\\[Failed to render image: broken image decoder\\]"
                                (buffer-string)))
        (should (string-match-p "/tmp/demo.png" (buffer-string)))))))

(ert-deftest courier-response-open-body-opens-document-file ()
  (let ((response '(:status-code 200
                    :reason "OK"
                    :headers (("content-type" . "application/pdf"))
                    :duration-ms 10
                    :size 12
                    :body-file "/tmp/demo.pdf"
                    :content-type "application/pdf"
                    :tests nil
                    :stderr ""
                    :exit-code 0))
        (opened nil)
        (displayed nil)
        (doc-view-called nil)
        (viewer-buffer (generate-new-buffer " *courier-pdf-view*")))
    (cl-letf (((symbol-function 'find-file-noselect)
               (lambda (path &optional _nowarn _rawfile _wildcards)
                 (setq opened path)
                 viewer-buffer))
              ((symbol-function 'display-buffer)
               (lambda (buffer &rest _args)
                 (setq displayed buffer)
                 buffer))
              ((symbol-function 'doc-view-mode)
               (lambda (&optional _arg)
                 (setq doc-view-called t)
                 (setq major-mode 'doc-view-mode))))
      (with-temp-buffer
        (setq-local courier--response response)
        (setq-local courier--request courier-test--request)
        (courier-response-open-body))
      (should (equal opened "/tmp/demo.pdf"))
      (should (eq displayed viewer-buffer))
      (should doc-view-called)
      (should (eq (buffer-local-value 'major-mode viewer-buffer)
                  'doc-view-mode)))
    (kill-buffer viewer-buffer)))

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

(ert-deftest courier-response-set-tab-switches-to-tests ()
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
    (courier-response-set-tab 'tests)
    (should (eq courier--response-tab 'tests))
    (should (string-match-p "\\` Response  Tests(1)  >>  200 OK  •  10ms  •  12B"
                            (substring-no-properties header-line-format)))
    (should (string-match-p "status == 200" (buffer-string)))))

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
    (courier-response-set-tab 'headers)
    (should (= (point) (point-min)))
    (goto-char (point-max))
    (courier-response-set-tab 'response)
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
    (courier-response-set-tab 'timeline)
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
               "\\` Response  >>  200 OK  •  42ms  •  5B"
               line))
      (courier-response-set-tab 'headers)
      (should (string-match-p
               "\\` Response  Headers(1)  >>  200 OK  •  42ms  •  5B"
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
    (should (string-match-p "\\` Response  >>  200 OK  •  10ms  •  12B"
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
    (courier--select-history-index 1)
    (let ((button (button-at (point))))
      (should button)
      (should (= 1 (button-get button 'courier-history-index))))))

(ert-deftest courier-response-timeline-section-toggle-keeps-point-on-section ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'network-logs)
    (should (string-match-p "\\` Response  Timeline  >>  200 OK  •  42ms  •  5B"
                            (substring-no-properties header-line-format)))))

(ert-deftest courier-response-timeline-response-view-shows-header-count ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-set-tab 'timeline)
    (courier--select-history-index 0)
    (courier--toggle-timeline-section 'response)
    (should (string-match-p "Headers(1)" (buffer-string)))))

(ert-deftest courier-response-timeline-entry-is-a-button ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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
    (courier-response-set-tab 'timeline)
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

(ert-deftest courier-request-candidates-use-file-system-labels ()
  (courier-test--with-temp-dir (root)
    (let* ((request-file (expand-file-name "users/get-user.http" root)))
      (make-directory (file-name-directory request-file) t)
      (with-temp-file request-file
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (let ((candidate (car (courier--request-candidates root))))
        (should (equal (plist-get (cdr candidate) :path) request-file))
        (should (eq (plist-get (cdr candidate) :kind) 'request))
        (should (equal (car candidate) "users/get-user"))))))

(ert-deftest courier-open-does-not-parse-request-files ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-root (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "broken.http" request-root)))
      (make-directory (file-name-directory request-file) t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "+++\nname = \"Broken\"\n+++\n\nGET \n"))
      (with-temp-buffer
        (setq default-directory root)
        (cl-letf (((symbol-function 'courier-parse-file)
                   (lambda (&rest _args)
                     (ert-fail "courier-open should not parse request files")))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (car (all-completions "" collection))))
                  ((symbol-function 'find-file)
                   #'ignore))
          (courier-open))))))

(ert-deftest courier-open-opens-selected-request-file ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
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
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (with-temp-file second-file
        (insert (courier-test--http-content
                 :name "Create User"
                 :method "POST"
                 :url "https://example.com/users")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (let* ((selection (car (all-completions "" collection)))
                          (candidate
                           (assoc selection (courier--open-candidates))))
                     (setq expected-file (plist-get (cdr candidate) :path))
                     selection)))
                ((symbol-function 'find-file)
                 (lambda (path)
                   (setq opened-file path))))
        (let ((default-directory collection-root))
          (courier-open)))
      (should (equal opened-file expected-file)))))

(ert-deftest courier-open-finds-collections-outside-context ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-root (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-root))
           opened-file)
      (make-directory request-root t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (with-temp-buffer
        (setq default-directory root)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (car (all-completions "" collection))))
                  ((symbol-function 'find-file)
                   (lambda (path)
                     (setq opened-file path))))
          (courier-open)))
      (should (equal opened-file request-file)))))

(ert-deftest courier-request-set-method-rewrites-request-line ()
  (courier-test--with-request
      (courier-test--http-content
       :name "Demo"
       :url "https://example.com/users/42"
       :headers '(("accept" . "application/json")))
    (courier-request-mode)
    (courier-request-set-method "POST")
    (goto-char (point-min))
    (should (search-forward "POST https://example.com/users/42" nil t))))

(ert-deftest courier-request-mode-adds-method-overlay ()
  (courier-test--with-request
      (courier-test--http-content
       :name "Demo"
       :url "https://example.com/users/42"
       :headers '(("accept" . "application/json")))
    (courier-request-mode)
    (should (overlayp courier--method-overlay))
    (should (equal (buffer-substring-no-properties
                    (overlay-start courier--method-overlay)
                    (overlay-end courier--method-overlay))
                   "GET"))))

(ert-deftest courier-request-header-line-shows-primary-navigation ()
  (courier-test--with-request
      (courier-test--http-content
       :name "Demo"
       :url "https://example.com/users/42"
       :headers '(("accept" . "application/json"))
       :body "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Params" line))
      (should (string-match-p "Headers" line))
      (should (string-match-p "Body" line))
      (should (string-match-p ">>" line)))))

(ert-deftest courier-request-header-line-shows-current-secondary-section ()
  (courier-test--with-request
      (courier-test--http-content
       :url "https://example.com/users/42"
       :auth '(:type bearer :token "{{token}}"))
    (courier-request-mode)
    (courier-request-jump-section 'auth)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Auth(Bearer)" line))
      (should (string-match-p "Auth(Bearer)  >>" line)))))

(ert-deftest courier-request-header-line-shows-none-auth-label ()
  (courier-test--with-request
      (courier-test--http-content
       :url "https://example.com/users/42")
    (courier-request-mode)
    (courier-request-jump-section 'auth)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Auth(None)" line)))))

(ert-deftest courier-request-header-line-shows-body-type-label ()
  (courier-test--with-request
      (courier-test--http-content
       :url "https://example.com/users/42"
       :body-type 'xml
       :body "<ok/>\n")
    (courier-request-mode)
    (courier-request-jump-section 'body)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Body(XML)" line)))))

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

(ert-deftest courier-mode-maps-bind-c-c-question ()
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c ?"))
              #'courier-dispatch))
  (should (eq (lookup-key courier--response-mode-map (kbd "C-c ?"))
              #'courier-dispatch)))

(ert-deftest courier-request-mode-binds-section-navigation ()
  (should (eq (lookup-key courier-request-mode-map (kbd "C-c C-j"))
              #'courier-request-jump-section))
  (should-not (lookup-key courier-request-mode-map (kbd "C-c [")))
  (should-not (lookup-key courier-request-mode-map (kbd "C-c ]"))))

(ert-deftest courier-response-mode-binds-jump-navigation ()
  (should (eq (lookup-key courier--response-mode-map (kbd "C-c C-j"))
              #'courier-response-jump-tab))
  (should-not (courier-test--local-map-binding courier--response-mode-map "["))
  (should-not (courier-test--local-map-binding courier--response-mode-map "]"))
  (should-not (courier-test--local-map-binding courier--response-mode-map "r"))
  (should-not (courier-test--local-map-binding courier--response-mode-map "h"))
  (should-not (courier-test--local-map-binding courier--response-mode-map "l"))
  (should-not (courier-test--local-map-binding courier--response-mode-map "t")))

(ert-deftest courier-response-jump-tab-switches-view ()
  (with-temp-buffer
    (courier--render-response
     (courier-test--make-response 200)
     courier-test--request)
    (courier-response-jump-tab 'tests)
    (should (eq courier--response-tab 'tests))))

(ert-deftest courier-request-jump-section-shows-script-section ()
  (courier-test--with-request
      (courier-test--http-content
       :url "https://example.com/users"
       :pre-request-script "courier-script-request"
       :post-response-script "courier-script-response")
    (courier-request-mode)
    (courier-request-jump-section 'script)
    (should (eq courier--request-tab 'script))
    (should (string-match-p "pre_request = \\\"\\\"\\\"" (buffer-string)))
    (should (string-match-p "courier-script-request" (buffer-string)))
    (should (string-match-p "post_response = \\\"\\\"\\\"" (buffer-string)))
    (should (string-match-p "courier-script-response" (buffer-string)))))

(ert-deftest courier-request-jump-section-shows-request-section ()
  (courier-test--with-request
      "GET https://example.com/users?page=1\n"
    (courier-request-mode)
    (courier-request-jump-section 'params)
    (should (eq courier--request-tab 'params))
    (should (string-match-p "^page = 1$" (buffer-string)))))

(ert-deftest courier-request-empty-sections-show-usable-placeholders ()
  (courier-test--with-request
      "GET https://example.com/users\n"
    (courier-request-mode)
    (courier-request-jump-section 'params)
    (should (string-match-p "^# Query params$" (buffer-string)))
    (should (string-match-p "^# include = profile,roles$" (buffer-string)))
    (courier-request-jump-section 'vars)
    (should (string-match-p "^# token = \"courier-pigeon-token\"$" (buffer-string)))
    (should (string-match-p "^# name = \"session_token\"$" (buffer-string)))
    (should (string-match-p "^# expr = \"\\$\\.data\\.token\"$" (buffer-string)))))

(ert-deftest courier-request-render-establishes-content-start-marker ()
  (courier-test--with-request
      (courier-test--http-content
       :url "https://example.com/users/42"
       :headers '(("accept" . "application/json"))
       :body-type 'json
       :body "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (should (markerp courier--request-content-start))
    (should (= (marker-position courier--request-content-start)
               (save-excursion
                 (goto-char (point-min))
                 (forward-line 3)
                 (point))))
    (should (string-prefix-p "{\"name\":\"Lucy\"}"
                             (buffer-substring-no-properties
                              (marker-position courier--request-content-start)
                              (point-max))))
    (courier-request-jump-section 'headers)
    (should (markerp courier--request-content-start))
    (should (string-prefix-p "accept: application/json"
                             (downcase
                              (buffer-substring-no-properties
                               (marker-position courier--request-content-start)
                               (point-max)))))))

(ert-deftest courier-request-vars-comments-use-comment-face ()
  (courier-test--with-request
      "GET https://example.com/users\n"
    (courier-request-mode)
    (courier-request-jump-section 'vars)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "# [vars]" nil t)
    (let ((face (get-text-property (line-beginning-position) 'face)))
      (should (courier-test--face-includes-p face 'font-lock-comment-face)))))

(ert-deftest courier-request-edit-params-loads-query-into-editor ()
  (courier-test--with-request
      "GET https://example.com/users?page=1&sort=created_at\n"
    (courier-request-mode)
    (let ((buffer (courier-request-edit-params)))
      (with-current-buffer buffer
        (should (derived-mode-p 'courier-request-params-mode))
        (should (equal (buffer-string)
                       "page = 1\nsort = created_at"))))))

(ert-deftest courier-request-params-apply-normalizes-params-into-model ()
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
        (should (equal (plist-get courier--request-model :url)
                       "https://example.com/users"))
        (should (equal (plist-get courier--request-model :params)
                       '(("page" . "2") ("filter" . "active"))))
        (should (string-match-p
                 (regexp-quote "GET https://example.com/users?page=2&filter=active\n\n")
                 (courier--serialize-request courier--request-model)))))))

(ert-deftest courier-request-set-method-works-on-incomplete-request-line ()
  (courier-test--with-request
      (concat "+++\nname = \"Demo\"\n+++\n\n"
              "GET \n"
              "Accept: application/json\n")
    (courier-request-mode)
    (courier-request-set-method "POST")
    (goto-char (point-min))
    (should (search-forward "POST " nil t))))

(ert-deftest courier-request-set-method-refreshes-overlay ()
  (courier-test--with-request
      (concat "+++\nname = \"Demo\"\n+++\n\n"
              "GET \n"
              "Accept: application/json\n")
    (courier-request-mode)
    (courier-request-set-method "POST")
    (should (overlayp courier--method-overlay))
    (should (equal (buffer-substring-no-properties
                    (overlay-start courier--method-overlay)
                    (overlay-end courier--method-overlay))
                   "POST"))))

(ert-deftest courier-request-set-method-backfills-missing-request-line ()
  (courier-test--with-request "+++\nname = \"Demo\"\n+++\n\n"
    (courier-request-mode)
    (courier-request-set-method "POST")
    (should (equal (plist-get courier--request-model :method) "POST"))
    (should (string-match-p "^POST $" (buffer-string)))
    (should (string-match-p "Body(JSON)"
                            (substring-no-properties
                             (courier--request-header-line-format))))))

(ert-deftest courier-request-set-body-type-updates-request-model ()
  (courier-test--with-request
      (courier-test--http-content
       :method "POST"
       :url "https://example.com/users"
       :body-type 'json
       :body "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (courier-request-set-body-type 'none)
    (should (eq (plist-get courier--request-model :body-type) 'none))
    (should (equal (plist-get courier--request-model :body) ""))))

(ert-deftest courier-request-set-body-type-refreshes-header-line ()
  (courier-test--with-request
      (courier-test--http-content
       :method "POST"
       :url "https://example.com/users"
       :body-type 'json
       :body "{\"name\":\"Lucy\"}\n")
    (courier-request-mode)
    (courier-request-set-body-type 'form-urlencoded)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Body(Form)" line)))))

(ert-deftest courier-request-body-section-round-trips-binary-metadata ()
  (courier-test--with-request
      (courier-test--http-content
       :method "POST"
       :url "https://example.com/upload"
       :body-type 'binary
       :body-file-path "./payload.bin"
       :body-file-content-type "application/octet-stream")
    (courier-request-mode)
    (courier-request-jump-section 'body)
    (should (string-match-p "^path = \\./payload\\.bin$" (buffer-string)))
    (should (string-match-p "^content_type = application/octet-stream$"
                            (buffer-string)))
    (courier--sync-request-model)
    (should (eq (plist-get courier--request-model :body-type) 'binary))
    (should (equal (plist-get courier--request-model :body-file-path)
                   "./payload.bin"))
    (should (equal (plist-get courier--request-model :body-file-content-type)
                   "application/octet-stream"))))

(ert-deftest courier-request-body-section-round-trips-multipart-parts ()
  (courier-test--with-request
      (courier-test--http-content
       :method "POST"
       :url "https://example.com/upload"
       :body-type 'multipart
       :body-parts '((:name "avatar"
                             :kind file
                             :path "./avatar.png"
                             :content-type "image/png")
                     (:name "display_name"
                             :kind text
                             :value "{{name}}")))
    (courier-request-mode)
    (courier-request-jump-section 'body)
    (should (string-match-p "^name = avatar$" (buffer-string)))
    (should (string-match-p "^kind = file$" (buffer-string)))
    (should (string-match-p "^path = \\./avatar\\.png$" (buffer-string)))
    (courier--sync-request-model)
    (should (eq (plist-get courier--request-model :body-type) 'multipart))
    (should (equal (plist-get courier--request-model :body-parts)
                   '((:name "avatar"
                            :kind file
                            :path "./avatar.png"
                            :content-type "image/png")
                     (:name "display_name"
                            :kind text
                            :value "{{name}}"))))))

(ert-deftest courier-request-script-section-round-trips-both-phases ()
  (courier-test--with-request
      (courier-test--http-content
       :method "GET"
       :url "https://example.com/users"
       :pre-request-script "(message \"before\")"
       :post-response-script "(message \"after\")")
    (courier-request-mode)
    (courier-request-jump-section 'script)
    (courier--sync-request-model)
    (should (equal (plist-get courier--request-model :pre-request-script)
                   "(message \"before\")"))
    (should (equal (plist-get courier--request-model :post-response-script)
                   "(message \"after\")"))))

(ert-deftest courier-request-set-auth-type-updates-request-model ()
  (courier-test--with-request
      (courier-test--http-content
       :method "GET"
       :url "https://example.com/users")
    (courier-request-mode)
    (courier-request-set-auth-type 'basic)
    (should (equal (plist-get courier--request-model :auth)
                   '(:type basic
                     :username "{{user}}"
                     :password "{{password}}")))))

(ert-deftest courier-request-set-auth-type-refreshes-header-line ()
  (courier-test--with-request
      (courier-test--http-content
       :method "GET"
       :url "https://example.com/users")
    (courier-request-mode)
    (courier-request-set-auth-type 'header)
    (let ((line (substring-no-properties
                 (courier--request-header-line-format))))
      (should (string-match-p "Auth(Header)" line)))))

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
                               (seq-find
                                (lambda (candidate)
                                  (string= (get-text-property 0 'courier-group candidate)
                                           "Environments"))
                                (all-completions "" collection)))
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
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (with-temp-file (expand-file-name "local.env" env-dir)
        (insert "token=local\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local courier--request-path request-file)
        (courier-request-mode)
        (let ((courier-home-directory root))
          (setq candidates (courier--open-candidates)))
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

(ert-deftest courier-open-candidates-only-include-current-collection-envs ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (first-root (expand-file-name "collections/first-api" root))
           (second-root (expand-file-name "collections/second-api" root))
           (first-request (expand-file-name "requests/get-user.http" first-root))
           (second-request (expand-file-name "requests/get-other.http" second-root))
           (first-env (expand-file-name "env/local.env" first-root))
           (second-env (expand-file-name "env/prod.env" second-root))
           candidates)
      (make-directory (file-name-directory first-request) t)
      (make-directory (file-name-directory second-request) t)
      (make-directory (file-name-directory first-env) t)
      (make-directory (file-name-directory second-env) t)
      (with-temp-file (expand-file-name "courier.json" first-root)
        (insert "{}\n"))
      (with-temp-file (expand-file-name "courier.json" second-root)
        (insert "{}\n"))
      (with-temp-file first-request
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file second-request
        (insert "GET https://example.com/other\n"))
      (with-temp-file first-env
        (insert "token=local\n"))
      (with-temp-file second-env
        (insert "token=prod\n"))
      (find-file first-request)
      (unwind-protect
          (progn
            (setq candidates (courier--open-candidates))
            (should (seq-some
                     (lambda (candidate)
                       (and (eq (plist-get (cdr candidate) :kind) 'env)
                            (equal (plist-get (cdr candidate) :path) first-env)))
                     candidates))
            (should-not (seq-some
                         (lambda (candidate)
                           (and (eq (plist-get (cdr candidate) :kind) 'env)
                                (equal (plist-get (cdr candidate) :path) second-env)))
                         candidates))
            (should (seq-some
                     (lambda (candidate)
                       (and (eq (plist-get (cdr candidate) :kind) 'request)
                            (equal (plist-get (cdr candidate) :path) second-request)))
                     candidates)))
        (kill-buffer (current-buffer))))))

(ert-deftest courier-open-candidates-outside-request-buffer-hide-environments ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (env-dir (expand-file-name "env" collection-root))
           candidates)
      (make-directory request-dir t)
      (make-directory env-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file (expand-file-name "get-user.http" request-dir)
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (with-temp-file (expand-file-name "local.env" env-dir)
        (insert "token=local\n"))
      (with-temp-buffer
        (setq candidates (courier--open-candidates))
        (should (seq-some
                 (lambda (candidate)
                   (string= (get-text-property 0 'courier-group (car candidate))
                            "Requests"))
                 candidates))
        (should-not (seq-some
                     (lambda (candidate)
                       (string= (get-text-property 0 'courier-group (car candidate))
                                "Environments"))
                     candidates))))))

(ert-deftest courier-open-candidates-annotate-collection-name ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           request-candidate
           candidates)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"name\": \"Demo API\"\n}\n"))
      (with-temp-file request-file
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (with-temp-buffer
        (setq candidates (courier--open-candidates)))
      (setq request-candidate
            (car (seq-filter
                  (lambda (candidate)
                    (eq (plist-get (cdr candidate) :kind) 'request))
                  candidates)))
      (should (equal (get-text-property 0 'courier-annotation
                                        (car request-candidate))
                     "Demo API")))))

(ert-deftest courier-completion-table-exposes-affixation-metadata ()
  (let* ((candidates
          (list
           (cons (propertize "users/get-user"
                             'courier-group "Requests"
                             'courier-annotation "api-collection")
                 (list :kind 'request :path "/tmp/get-user.http"))))
         (table (courier--completion-table
                 candidates
                 '(category . courier-open)
                 '(group-function . courier--completion-group)
                 '(affixation-function . courier--completion-affixation)))
         (metadata (completion-metadata "" table nil))
         (affixation (completion-metadata-get metadata 'affixation-function))
         (group (completion-metadata-get metadata 'group-function))
         (candidate (caar candidates))
         (rows (funcall affixation (list candidate))))
    (should affixation)
    (should group)
    (should (equal (funcall group candidate nil) "Requests"))
    (should (equal (cadar rows) ""))
    (should (string-match-p "api-collection" (cl-caddar rows)))))

(ert-deftest courier-open-env-candidates-use-annotation-for-context ()
  (courier-test--with-temp-dir (root)
    (let* ((collection-root (expand-file-name "api-collection" root))
           (request-dir (expand-file-name "requests" collection-root))
           (env-dir (expand-file-name "env" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           env-candidate
           candidates)
      (make-directory request-dir t)
      (make-directory env-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-file (expand-file-name "local.env" env-dir)
        (insert "token=local\n"))
      (find-file request-file)
      (unwind-protect
          (progn
            (setq candidates (courier--open-candidates))
            (setq env-candidate
                  (seq-find
                   (lambda (candidate)
                     (eq (plist-get (cdr candidate) :kind) 'env))
                   candidates))
            (should env-candidate)
            (should (equal (car env-candidate) "* local"))
            (should (equal (get-text-property 0 'courier-annotation
                                              (car env-candidate))
                           "api-collection • env/local.env")))
        (kill-buffer (current-buffer))))))

(ert-deftest courier-discover-collection-roots-reads-home-collections-only ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (home-root (file-name-as-directory root))
           (collections-root (expand-file-name "collections" root))
           (first-root (expand-file-name "collections/first-api" root))
           (second-root (expand-file-name "collections/second-api" root)))
      (make-directory collections-root t)
      (make-directory first-root t)
      (make-directory second-root t)
      (with-temp-file (expand-file-name "courier.json" home-root)
        (insert "{\n  \"name\": \"Wrong Home\"\n}\n"))
      (with-temp-file (expand-file-name "courier.json" first-root)
        (insert "{\n  \"name\": \"First API\"\n}\n"))
      (with-temp-file (expand-file-name "courier.json" second-root)
        (insert "{\n  \"name\": \"Second API\"\n}\n"))
      (should (equal (courier--discover-collection-roots)
                     (list (file-name-as-directory first-root)
                           (file-name-as-directory second-root)))))))

(ert-deftest courier-collection-root-ignores-home-marker ()
  (courier-test--with-temp-dir (root)
    (let ((courier-home-directory root))
      (with-temp-file (expand-file-name "courier.json" root)
        (insert "{\n  \"name\": \"Wrong Home\"\n}\n"))
      (with-temp-buffer
        (setq default-directory root)
        (should-not (courier--collection-root))))))

(ert-deftest courier-import-openapi-json-creates-collection-and-copies-spec ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (spec-file (expand-file-name "demo-openapi.json" root))
           result)
      (with-temp-file spec-file
        (insert "{\n"
                "  \"openapi\": \"3.0.0\",\n"
                "  \"info\": {\"title\": \"Demo API\"},\n"
                "  \"servers\": [{\"url\": \"https://api.example.com\"}],\n"
                "  \"paths\": {}\n"
                "}\n"))
      (setq result (courier--import-openapi-file spec-file "demo-api"))
      (should (file-directory-p (plist-get result :collection-root)))
      (should (file-exists-p (plist-get result :spec-path)))
      (should (equal (plist-get (courier--collection-config
                                 (plist-get result :collection-root))
                                :name)
                     "Demo API"))
      (should (equal (cdr (assoc-string "base_url"
                                        (plist-get (plist-get (courier--collection-config
                                                               (plist-get result :collection-root))
                                                              :defaults)
                                                   :vars)
                                        nil))
                     "https://api.example.com")))))

(ert-deftest courier-import-openapi-json-generates-basic-request-file ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (spec-file (expand-file-name "demo-openapi.json" root))
           result
           request-file
           request)
      (with-temp-file spec-file
        (insert "{\n"
                "  \"openapi\": \"3.0.0\",\n"
                "  \"info\": {\"title\": \"Demo API\"},\n"
                "  \"servers\": [{\"url\": \"https://api.example.com\"}],\n"
                "  \"paths\": {\n"
                "    \"/users/{userId}\": {\n"
                "      \"get\": {\n"
                "        \"summary\": \"Get User\"\n"
                "      }\n"
                "    }\n"
                "  }\n"
                "}\n"))
      (setq result (courier--import-openapi-file spec-file "demo-api"))
      (setq request-file
            (expand-file-name "requests/users/get-user.http"
                              (plist-get result :collection-root)))
      (should (file-exists-p request-file))
      (setq request (courier-parse-file request-file))
      (should (equal (plist-get request :name) "Get User"))
      (should (equal (plist-get request :method) "GET"))
      (should (equal (plist-get request :url) "{{base_url}}/users/{{userId}}")))))

(ert-deftest courier-import-openapi-json-maps-api-key-and-json-body ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (spec-file (expand-file-name "demo-openapi.json" root))
           result
           request-file
           request)
      (with-temp-file spec-file
        (insert "{\n"
                "  \"openapi\": \"3.0.0\",\n"
                "  \"info\": {\"title\": \"Demo API\"},\n"
                "  \"servers\": [{\"url\": \"https://api.example.com\"}],\n"
                "  \"components\": {\n"
                "    \"securitySchemes\": {\n"
                "      \"ApiKeyAuth\": {\n"
                "        \"type\": \"apiKey\",\n"
                "        \"in\": \"header\",\n"
                "        \"name\": \"X-API-Key\"\n"
                "      }\n"
                "    }\n"
                "  },\n"
                "  \"paths\": {\n"
                "    \"/users\": {\n"
                "      \"post\": {\n"
                "        \"summary\": \"Create User\",\n"
                "        \"security\": [{\"ApiKeyAuth\": []}],\n"
                "        \"requestBody\": {\n"
                "          \"content\": {\n"
                "            \"application/json\": {}\n"
                "          }\n"
                "        }\n"
                "      }\n"
                "    }\n"
                "  }\n"
                "}\n"))
      (setq result (courier--import-openapi-file spec-file "demo-api"))
      (setq request-file
            (expand-file-name "requests/users/create-user.http"
                              (plist-get result :collection-root)))
      (should (file-exists-p request-file))
      (setq request (courier-parse-file request-file))
      (should (eq (plist-get request :body-type) 'json))
      (should (equal (plist-get request :auth)
                     '(:type api_key
                       :in "header"
                       :name "X-API-Key"
                       :value "{{x_api_key}}"))))))

(ert-deftest courier-import-openapi-json-writes-report-for-unsupported-auth ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (spec-file (expand-file-name "demo-openapi.json" root))
           result
           report-path)
      (with-temp-file spec-file
        (insert "{\n"
                "  \"openapi\": \"3.0.0\",\n"
                "  \"info\": {\"title\": \"Demo API\"},\n"
                "  \"servers\": [{\"url\": \"https://api.example.com\"}],\n"
                "  \"components\": {\n"
                "    \"securitySchemes\": {\n"
                "      \"DigestAuth\": {\n"
                "        \"type\": \"http\",\n"
                "        \"scheme\": \"digest\"\n"
                "      }\n"
                "    }\n"
                "  },\n"
                "  \"paths\": {\n"
                "    \"/users\": {\n"
                "      \"get\": {\n"
                "        \"summary\": \"List Users\",\n"
                "        \"security\": [{\"DigestAuth\": []}]\n"
                "      }\n"
                "    }\n"
                "  }\n"
                "}\n"))
      (setq result (courier--import-openapi-file spec-file "demo-api"))
      (setq report-path (plist-get result :report-path))
      (should (file-exists-p report-path))
      (with-temp-buffer
        (insert-file-contents report-path)
        (should (string-match-p "Unsupported auth scheme" (buffer-string)))
        (should (string-match-p "GET /users" (buffer-string)))))))

(ert-deftest courier-import-openapi-yaml-creates-collection-and-preserves-extension ()
  (skip-unless (executable-find "ruby"))
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (spec-file (expand-file-name "demo-openapi.yaml" root))
           result
           request-file
           request)
      (with-temp-file spec-file
        (insert "openapi: 3.0.0\n"
                "info:\n"
                "  title: Demo YAML API\n"
                "servers:\n"
                "  - url: https://api.example.com\n"
                "paths:\n"
                "  /users/{userId}:\n"
                "    get:\n"
                "      summary: Get User\n"))
      (setq result (courier--import-openapi-file spec-file "demo-yaml-api"))
      (should (string-suffix-p "openapi.yaml" (plist-get result :spec-path)))
      (setq request-file
            (expand-file-name "requests/users/get-user.http"
                              (plist-get result :collection-root)))
      (should (file-exists-p request-file))
      (setq request (courier-parse-file request-file))
      (should (equal (plist-get request :name) "Get User"))
      (should (equal (plist-get request :url) "{{base_url}}/users/{{userId}}")))))

(ert-deftest courier-collection-root-prefers-top-level-home-collection ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (nested-dir (expand-file-name "requests/admin" collection-root))
           (request-file (expand-file-name "get-user.http" nested-dir)))
      (make-directory nested-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"name\": \"API Collection\"\n}\n"))
      (with-temp-file (expand-file-name "courier.json" nested-dir)
        (insert "{\n  \"defaults\": {\n    \"timeout\": 12\n  }\n}\n"))
      (with-temp-file request-file
        (insert "GET https://example.com/users/42\n"))
      (with-temp-buffer
        (setq-local buffer-file-name request-file)
        (setq-local default-directory nested-dir)
        (should (equal (file-name-as-directory collection-root)
                       (courier--collection-root)))))))

(ert-deftest courier-prepared-current-request-inherits-collection-defaults ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           buffer)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n"
                "  \"name\": \"API Collection\",\n"
                "  \"defaults\": {\n"
                "    \"vars\": {\"base_url\": \"https://api.example.com\", \"token\": \"root-token\"},\n"
                "    \"headers\": {\"accept\": \"application/json\"},\n"
                "    \"auth\": {\"type\": \"bearer\", \"token\": \"{{token}}\"},\n"
                "    \"timeout\": 15,\n"
                "    \"follow_redirects\": true\n"
                "  }\n"
                "}\n"))
      (with-temp-file request-file
        (insert "GET {{base_url}}/users/42\n"))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-request-mode)
            (let ((request (courier--prepared-current-request)))
              (should (equal (plist-get request :vars)
                             '(("base_url" . "https://api.example.com")
                               ("token" . "root-token"))))
              (should (equal (plist-get request :headers)
                             '(("accept" . "application/json"))))
              (should (equal (plist-get request :auth)
                             '(:type bearer :token "{{token}}")))
              (should (equal (plist-get request :settings)
                             '(:timeout 15 :follow-redirects t)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest courier-prepared-current-request-folder-defaults-override-collection ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests/admin/tools" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           (folder-config (expand-file-name "courier.json"
                                            (expand-file-name "requests/admin" collection-root)))
           buffer)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n"
                "  \"defaults\": {\n"
                "    \"vars\": {\"token\": \"root-token\", \"base_url\": \"https://api.example.com\"},\n"
                "    \"headers\": {\"accept\": \"application/json\", \"x-root\": \"1\"},\n"
                "    \"auth\": {\"type\": \"bearer\", \"token\": \"{{token}}\"},\n"
                "    \"timeout\": 15\n"
                "  }\n"
                "}\n"))
      (with-temp-file folder-config
        (insert "{\n"
                "  \"defaults\": {\n"
                "    \"vars\": {\"token\": \"folder-token\"},\n"
                "    \"headers\": {\"accept\": \"application/xml\", \"x-folder\": \"1\"},\n"
                "    \"auth\": {\"type\": \"header\", \"header\": \"X-Admin-Key\", \"value\": \"{{token}}\"},\n"
                "    \"timeout\": 20,\n"
                "    \"follow_redirects\": true\n"
                "  }\n"
                "}\n"))
      (with-temp-file request-file
        (insert "+++\n"
                "timeout = 25\n"
                "[vars]\n"
                "token = \"request-token\"\n"
                "+++\n\n"
                "GET {{base_url}}/users/42\n"
                "Accept: text/plain\n"))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-request-mode)
            (let ((request (courier--prepared-current-request)))
              (should (equal (cdr (assoc-string "base_url"
                                                (plist-get request :vars)
                                                nil))
                             "https://api.example.com"))
              (should (equal (cdr (assoc-string "token"
                                                (plist-get request :vars)
                                                nil))
                             "request-token"))
              (should (equal (plist-get request :headers)
                             '(("accept" . "text/plain")
                               ("x-root" . "1")
                               ("x-folder" . "1"))))
              (should (equal (plist-get request :auth)
                             '(:type header :header "X-Admin-Key" :value "{{token}}")))
              (should (equal (plist-get request :settings)
                             '(:timeout 25 :follow-redirects t)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest courier-prepared-current-request-explicit-none-auth-beats-defaults ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (request-dir (expand-file-name "requests/users" collection-root))
           (request-file (expand-file-name "get-user.http" request-dir))
           buffer)
      (make-directory request-dir t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n"
                "  \"defaults\": {\n"
                "    \"auth\": {\"type\": \"bearer\", \"token\": \"{{token}}\"}\n"
                "  }\n"
                "}\n"))
      (with-temp-file request-file
        (insert "+++\n"
                "[auth]\n"
                "type = \"none\"\n"
                "+++\n\n"
                "GET https://example.com/users/42\n"))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-request-mode)
            (let* ((request (courier--prepared-current-request))
                   (resolved (courier-resolve-request request nil)))
              (should (equal (plist-get request :auth) '(:type none)))
              (should-not (assoc-string "authorization"
                                        (plist-get resolved :headers)
                                        nil))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest courier-new-request-creates-untitled-draft ()
  (let ((courier--untitled-request-counter 0)
        created-buffer)
    (save-window-excursion
      (let ((default-directory temporary-file-directory))
        (courier-new-request)
        (setq created-buffer (current-buffer))
        (should (derived-mode-p 'courier-request-mode))
        (should-not buffer-file-name)
        (should (string-prefix-p "GET \n" (buffer-string)))
        (should (string-match-p "^----------------------------------------$"
                                (nth 1 (split-string (buffer-string) "\n"))))
        (should (string-match-p "Body(JSON)"
                                (substring-no-properties
                                 (courier--request-header-line-format))))
        (should (= (line-number-at-pos) 1))
        (should (= (point) (line-end-position)))
        (should (equal (plist-get courier--request-model :name) "Untitled 1"))
        (should (equal (plist-get courier--request-model :method) "GET"))))
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
        (should (string-prefix-p "POST \n" (buffer-string)))
        (should (string-match-p "^----------------------------------------$"
                                (nth 1 (split-string (buffer-string) "\n"))))
        (should (string-match-p "Body(JSON)"
                                (substring-no-properties
                                 (courier--request-header-line-format))))
        (should (= (line-number-at-pos) 1))
        (should (= (point) (line-end-position)))
        (should (equal (plist-get courier--request-model :method) "POST"))))
    (when (buffer-live-p created-buffer)
      (kill-buffer created-buffer))))

(ert-deftest courier-draft-and-response-buffers-use-distinct-names ()
  (should (equal (courier--draft-buffer-name "Untitled 1")
                 "*courier-request: Untitled 1*"))
  (should (equal (courier--response-buffer-name '(:name "Untitled 1"))
                 "*courier-response: Untitled 1*")))

(ert-deftest courier-request-save-buffer-uses-configured-requests-dir ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (requests-root (expand-file-name "api-requests" collection-root))
           draft-buffer)
      (make-directory collection-root t)
      (with-temp-file (expand-file-name "courier.json" collection-root)
        (insert "{\n  \"requestsDir\": \"api-requests\"\n}\n"))
      (setq draft-buffer (generate-new-buffer "*courier-save-test*"))
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "+++\nname = \"Create User\"\n+++\n\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt collection &rest _args)
                         (if (string-prefix-p "Collection:" prompt)
                             (car (all-completions "" collection))
                           (error "Unexpected completing-read prompt: %s" prompt))))
                      ((symbol-function 'read-string)
                       (lambda (prompt &optional initial _history _default-value _inherit-input-method)
                         (cond
                          ((string-prefix-p "Request file name:" prompt) "create-user")
                          (t initial)))))
              (courier-request-save-buffer))
            (should (equal (expand-file-name buffer-file-name)
                           (expand-file-name "create-user.http" requests-root)))
            (should (file-exists-p (expand-file-name "create-user.http" requests-root))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-request-save-buffer-creates-collection-when-needed ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (draft-buffer (generate-new-buffer "*courier-create-collection*")))
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "+++\nname = \"Untitled 1\"\n+++\n\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _collection &rest _args)
                         "api-collection"))
                      ((symbol-function 'read-string)
                       (lambda (prompt &optional initial _history _default-value _inherit-input-method)
                         (cond
                          ((string-prefix-p "Request file name:" prompt) "untitled-1")
                          (t initial)))))
              (courier-request-save-buffer))
            (should (file-exists-p (expand-file-name "courier.json" collection-root)))
            (should (file-exists-p (expand-file-name "requests/untitled-1.http"
                                                     collection-root))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-request-save-buffer-creates-collections-under-home ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory (expand-file-name "courier-home" root))
           (collection-root (expand-file-name "collections/api-collection"
                                              courier-home-directory))
           (draft-buffer (generate-new-buffer "*courier-save-home-root*")))
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "+++\nname = \"Untitled 1\"\n+++\n\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _collection &rest _args)
                         "api-collection"))
                      ((symbol-function 'read-string)
                       (lambda (prompt &optional initial _history _default-value _inherit-input-method)
                         (cond
                          ((string-prefix-p "Request file name:" prompt) "untitled-1")
                          (t initial)))))
              (courier-request-save-buffer))
            (should (file-exists-p
                     (expand-file-name "courier.json" collection-root)))
            (should (string-prefix-p
                     (file-name-as-directory
                      (expand-file-name (expand-file-name "collections"
                                                          courier-home-directory)))
                     (file-name-directory (expand-file-name buffer-file-name)))))
        (when (buffer-live-p draft-buffer)
          (kill-buffer draft-buffer))))))

(ert-deftest courier-request-save-buffer-prompts-with-basename-without-http-suffix ()
  (courier-test--with-temp-dir (root)
    (let* ((courier-home-directory root)
           (collection-root (expand-file-name "collections/api-collection" root))
           (draft-buffer (generate-new-buffer "*courier-save-basename*"))
           captured-initial)
      (unwind-protect
          (with-current-buffer draft-buffer
            (insert "+++\nname = \"Create User\"\n+++\n\nGET \nAccept: application/json\n")
            (setq default-directory root)
            (courier-request-mode)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _collection &rest _args)
                         "api-collection"))
                      ((symbol-function 'read-string)
                       (lambda (prompt &optional initial _history _default-value _inherit-input-method)
                         (when (string-prefix-p "Request file name:" prompt)
                           (setq captured-initial initial)
                           "create-user")
                         initial)))
              (courier-request-save-buffer))
            (should (equal captured-initial "create-user"))
            (should (string-suffix-p ".http" buffer-file-name)))
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

(ert-deftest courier-rename-request-renames-file-and-source-backed-name ()
  (courier-test--with-temp-dir (root)
    (let* ((request-file (expand-file-name "get-user.http" root))
           (new-file (expand-file-name "create-user.http" root))
           buffer)
      (with-temp-file request-file
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
      (setq buffer (find-file-noselect request-file))
      (unwind-protect
          (with-current-buffer buffer
            (courier-rename-request "Create User")
            (should (equal (expand-file-name buffer-file-name) new-file))
            (should (file-exists-p new-file))
            (should-not (file-exists-p request-file))
            (should (equal (plist-get courier--request-model :name) "Create User"))
            (should (string-match-p "^GET https://example.com/users/42$" (buffer-string)))
            (should (string-match-p
                     (regexp-quote "+++\nname = \"Create User\"\n\n[body]\ntype = \"json\"\n+++\n\nGET https://example.com/users/42\n\n")
                     (with-temp-buffer
                       (insert-file-contents new-file)
                       (buffer-string)))))
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
        (insert (courier-test--http-content
                 :name "Get User"
                 :url "https://example.com/users/42")))
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
