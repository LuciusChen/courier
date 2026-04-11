;;; courier.el --- Curl-backed HTTP client -*- lexical-binding: t; -*-

;; Author: Lucius Chen <chenyh572@gmail.com>
;; URL: https://github.com/LuciusChen/courier
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.0"))

;;; Commentary:

;; Courier is a lightweight HTTP client for Emacs.  It sends requests
;; defined in `.http' files using curl and displays responses in
;; structured read-only buffers.
;;
;; Load this file with (require 'courier) to set up the package.
;; Opening a `.http' file automatically enters `courier-request-mode'.
;;
;; See README.org for the full documentation.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'url-util)

(defconst courier--allowed-methods
  '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS")
  "Allowed HTTP methods in request files.")

;;;; Customization

(defgroup courier nil
  "Minimal curl-backed HTTP requests from `.http' files."
  :group 'tools
  :prefix "courier-")

(defcustom courier-curl-executable "curl"
  "Executable name or path used to invoke curl."
  :type 'string
  :group 'courier)

(defcustom courier-body-text-max-size 524288
  "Maximum response body size in bytes to decode as text."
  :type 'integer
  :group 'courier)

(defcustom courier-default-timeout 30
  "Default timeout in seconds when a request file does not define one.
Set to nil to disable the default timeout."
  :type '(choice (const :tag "No timeout" nil)
                 integer)
  :group 'courier)

(defcustom courier-default-request-method "GET"
  "Default HTTP method used when creating a new Courier request draft."
  :type '(choice (const "GET")
                 (const "POST")
                 (const "PUT")
                 (const "PATCH")
                 (const "DELETE")
                 (const "HEAD")
                 (const "OPTIONS"))
  :group 'courier)

(defcustom courier-default-collection-directory
  (file-name-as-directory (expand-file-name "courier" "~"))
  "Default directory used when creating or saving Courier collections.

This directory is used as the initial prompt location when saving a new
request draft or creating a collection outside an existing Courier
collection."
  :type 'directory
  :group 'courier)

(defcustom courier-history-max 20
  "Maximum number of response history entries per request."
  :type 'integer
  :group 'courier)

(defcustom courier-body-view-max-bytes 8192
  "Maximum number of body bytes to render for binary-oriented body views."
  :type 'integer
  :group 'courier)

(defconst courier--collection-marker-file "courier.json"
  "Filename that marks the root of a Courier collection.")

(defvar courier--untitled-request-counter 0
  "Running counter used for untitled Courier request drafts.")

(defvar courier--request-metadata (make-hash-table :test 'equal)
  "Hash table of request metadata keyed by absolute request path.")

(defconst courier--body-views
  '(auto json html xml javascript raw hex base64 image)
  "Supported Courier response body views.")

(defconst courier--export-formats
  '(curl httpie wget)
  "Supported Courier export formats.")

(defconst courier--response-tabs
  '(response headers timeline tests)
  "Supported Courier response tabs.")

(defconst courier--timeline-tabs
  '(request response network-logs)
  "Supported Courier timeline detail sections.")

(defconst courier--header-separator
  (propertize "  •  " 'face 'shadow)
  "Separator used in Courier header lines.")

(defconst courier--status-reason-phrases
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (102 . "Processing")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (413 . "Payload Too Large")
    (415 . "Unsupported Media Type")
    (418 . "I'm a teapot")
    (422 . "Unprocessable Content")
    (429 . "Too Many Requests")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout"))
  "Fallback HTTP reason phrases keyed by status code.")

(defface courier-response-tab-active-face
  '((t :inherit header-line :weight bold :underline t))
  "Face used for the active Courier response tab.")

(defface courier-response-tab-inactive-face
  '((t :inherit shadow))
  "Face used for inactive Courier response tabs.")

(defface courier-response-tab-count-face
  '((t :inherit shadow :height 0.95))
  "Face used for response tab counts in Courier.")

(defface courier-response-timeline-tab-active-face
  '((t :inherit font-lock-keyword-face :weight bold :underline t))
  "Face used for the active Courier timeline detail tab.")

(defface courier-response-timeline-tab-inactive-face
  '((t :inherit shadow))
  "Face used for inactive Courier timeline detail tabs.")

(defface courier-response-timeline-heading-face
  '((t :inherit warning :weight bold))
  "Face used for Courier timeline section headings.")

(defface courier-response-status-success-face
  '((t :inherit success :weight bold))
  "Face used for successful Courier response statuses.")

(defface courier-response-status-warning-face
  '((t :inherit warning :weight bold))
  "Face used for warning Courier response statuses.")

(defface courier-response-status-error-face
  '((t :inherit error :weight bold))
  "Face used for error Courier response statuses.")

(defface courier-response-method-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for Courier response request methods.")

(defface courier-response-url-face
  '((t :inherit link))
  "Face used for Courier response URLs.")

(defface courier-response-timeline-selected-face
  '((t :inherit highlight))
  "Face used for the selected Courier timeline history entry.")

;;;; Parsing

(defconst courier--directive-regexp
  "^\\(#\\)\\s-*\\(@[[:alnum:]-]+\\)\\(?:\\s-+\\(.*\\)\\)?$"
  "Regexp used to parse directive lines.")

(defconst courier--script-block-end-regexp
  "^#\\s-*\\(@end\\)\\s-*$"
  "Regexp used to terminate Courier script blocks.")

(defconst courier--request-line-regexp
  "^\\([A-Z]+\\)\\s-+\\(\\S-+\\)\\s-*$"
  "Regexp used to parse request lines.")

(defconst courier--header-line-regexp
  "^\\([^: \t][^:]*\\):\\s-*\\(.*\\)$"
  "Regexp used to parse header lines.")

(defconst courier--env-line-regexp
  "\\`\\([^[:space:]#=][^[:space:]=]*\\)=\\(.*\\)\\'"
  "Regexp used to parse environment variable lines.")

(defun courier--read-json-file (path)
  "Parse PATH as JSON and return an alist.
Signal `user-error' when the file cannot be parsed as a JSON object."
  (with-temp-buffer
    (insert-file-contents path)
    (condition-case err
        (let ((data (json-parse-buffer :object-type 'alist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object nil)))
          (unless (listp data)
            (user-error "Courier config %s must contain a JSON object" path))
          data)
      (error
       (user-error "Failed to parse Courier config %s: %s"
                   path
                   (error-message-string err))))))

(defun courier--json-string-field (data key &optional default)
  "Return string KEY from DATA, or DEFAULT when KEY is absent.
Signal `user-error' when KEY is present but not a string."
  (let ((value (alist-get key data)))
    (cond
     ((null value) default)
     ((stringp value) value)
     (t
      (user-error "Courier config field %s must be a string" key)))))

(defun courier--parse-error (line format-string &rest args)
  "Signal a parse error at LINE using FORMAT-STRING and ARGS."
  (user-error "Parse error on line %d: %s"
              line
              (apply #'format format-string args)))

(defun courier--env-name-for-file (file-name)
  "Return the environment name for FILE-NAME."
  (if (string= file-name ".env")
      "default"
    (string-remove-suffix ".env" file-name)))

(defun courier--env-file-name-p (file-name)
  "Return non-nil when FILE-NAME is a supported Courier env file."
  (or (string= file-name ".env")
      (and (string-suffix-p ".env" file-name)
           (not (string-prefix-p "." file-name)))))

(defun courier--parse-directive (line-number line request)
  "Parse directive LINE on LINE-NUMBER into REQUEST plist."
  (unless (string-match courier--directive-regexp line)
    (courier--parse-error line-number "Malformed directive"))
  (let ((key (match-string 2 line))
        (value (or (match-string 3 line) "")))
    (pcase key
      ("@name"
       (plist-put request :name value))
      ("@timeout"
       (unless (and (string-match-p "\\`[0-9]+\\'" value)
                    (> (string-to-number value) 0))
         (courier--parse-error line-number "Invalid @timeout value: %s" value))
       (let ((settings (plist-get request :settings)))
         (plist-put request :settings
                    (plist-put settings :timeout (string-to-number value)))))
      ("@follow-redirects"
       (unless (member value '("true" "false"))
         (courier--parse-error line-number "Invalid @follow-redirects value: %s" value))
       (let ((settings (plist-get request :settings)))
         (plist-put request :settings
                    (plist-put settings :follow-redirects (string= value "true")))))
      ("@var"
       (unless (string-match "\\`\\([^[:space:]]+\\)\\s-+\\(.*\\)\\'" value)
         (courier--parse-error line-number "Invalid @var directive"))
       (plist-put request :vars
                  (append (plist-get request :vars)
                          (list (cons (match-string 1 value)
                                      (match-string 2 value))))))
      ("@param"
       (unless (string-match "\\`\\([^[:space:]]+\\)\\s-+\\(.*\\)\\'" value)
         (courier--parse-error line-number "Invalid @param directive"))
       (plist-put request :params
                  (append (plist-get request :params)
                          (list (cons (match-string 1 value)
                                      (match-string 2 value))))))
      ("@auth"
       (plist-put request :auth
                  (courier--parse-auth-directive line-number value)))
      ("@test"
       (when (string-empty-p value)
         (courier--parse-error line-number "Invalid @test directive"))
       (plist-put request :tests
                  (append (plist-get request :tests) (list value))))
      (_
       (courier--parse-error line-number "Unknown directive: %s" key))))
  request)

(defun courier--parse-auth-directive (line-number value)
  "Parse @auth VALUE at LINE-NUMBER into a plist."
  (cond
   ((string-match "\\`bearer\\s-+\\(.+\\)\\'" value)
    (list :type 'bearer
          :token (match-string 1 value)))
   ((string-match "\\`basic\\s-+\\([^[:space:]]+\\)\\s-+\\(.+\\)\\'" value)
    (list :type 'basic
          :username (match-string 1 value)
          :password (match-string 2 value)))
   ((string-match "\\`header\\s-+\\([^[:space:]]+\\)\\s-+\\(.+\\)\\'" value)
    (list :type 'header
          :name (match-string 1 value)
          :value (match-string 2 value)))
   (t
    (courier--parse-error
     line-number
     "Invalid @auth directive; use bearer/basic/header forms"))))

(defun courier--script-field-for-kind (line-number kind)
  "Return plist field symbol for script KIND at LINE-NUMBER."
  (pcase kind
    ("pre-request" :pre-request-script)
    ("post-response" :post-response-script)
    (_
     (courier--parse-error
      line-number
      "Invalid script block kind: %s"
      kind))))

(defun courier--parse-script-block (line-number kind)
  "Parse a script block of KIND starting after LINE-NUMBER.

Point must be on the `# @begin ...' line. On success, point is left on the
line after the matching `# @end'. Return a cons cell `(FIELD . SCRIPT)'."
  (let ((field (courier--script-field-for-kind line-number kind))
        lines)
    (forward-line 1)
    (catch 'courier-script-block
      (while (not (eobp))
        (let ((current-line (line-number-at-pos))
              (line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match-p courier--script-block-end-regexp line)
            (forward-line 1)
            (throw 'courier-script-block
                   (cons field (string-join (nreverse lines) "\n"))))
           ((string-match "\\`#\\( ?.*\\)?\\'" line)
            (let ((content (or (match-string 1 line) "")))
              (push (if (and (not (string-empty-p content))
                             (eq (aref content 0) ?\s))
                        (substring content 1)
                      content)
                    lines))
            (forward-line 1))
           (t
            (courier--parse-error
             current-line
             "Script block lines must start with #")))))
      (courier--parse-error line-number "Unclosed script block: %s" kind))))

(defun courier--resolve-var (name vars stack depth)
  "Resolve variable NAME from VARS with STACK and DEPTH tracking."
  (when (> depth 10)
    (user-error "Variable expansion exceeded 10 levels while resolving %s" name))
  (when (member name stack)
    (user-error "Circular variable reference detected for %s" name))
  (let ((pair (assoc-string name vars nil)))
    (unless pair
      (user-error "Unresolved variable: %s" name))
    (courier--expand-string
     (cdr pair)
     (lambda (dependency)
       (courier--resolve-var dependency vars (cons name stack) (1+ depth))))))

(defun courier--expand-string (string resolver)
  "Expand placeholders in STRING using RESOLVER."
  (let ((index 0)
        (length (length string))
        parts)
    (while (< index length)
      (if (and (< (1+ index) length)
               (eq (aref string index) ?{)
               (eq (aref string (1+ index)) ?{))
          (progn
            (when (and (< (+ index 2) length)
                       (eq (aref string (+ index 2)) ?{))
              (user-error "Malformed variable placeholder in %s" string))
            (let ((end (string-match "}}" string (+ index 2))))
              (unless end
                (user-error "Unclosed variable placeholder in %s" string))
              (let ((name (substring string (+ index 2) end)))
                (when (or (string-empty-p name)
                          (string-match-p "[{}[:space:]]" name))
                  (user-error "Malformed variable placeholder: {{%s}}" name))
                (push (funcall resolver name) parts)
                (setq index (+ end 2)))))
        (push (string (aref string index)) parts)
        (setq index (1+ index))))
    (apply #'concat (nreverse parts))))

(defun courier--resolve-vars (vars)
  "Resolve all variables in VARS and return a new alist."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (courier--resolve-var (car pair) vars nil 0)))
          vars))

(defun courier--response-header (response name)
  "Return header NAME from RESPONSE."
  (cdr (assoc-string (downcase name) (plist-get response :headers) nil)))

;;;###autoload
(defun courier-parse-buffer ()
  "Parse the current buffer into a request plist."
  (let ((request (list :path (and buffer-file-name (expand-file-name buffer-file-name))
                       :headers nil
                       :body ""
                       :params nil
                       :vars nil
                       :auth nil
                       :pre-request-script nil
                       :post-response-script nil
                       :tests nil
                       :settings nil))
        (line-number 0)
        request-line-seen
        body-start)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (looking-at-p "[ \t]*$"))
        (forward-line 1))
      (while (and (not (eobp))
                  (not request-line-seen))
        (setq line-number (line-number-at-pos))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-empty-p line)
            (forward-line 1))
           ((string-prefix-p "#" line)
            (unless (string-match courier--directive-regexp line)
              (courier--parse-error line-number "Malformed directive"))
            (let ((key (match-string 2 line))
                  (value (or (match-string 3 line) "")))
              (if (string= key "@begin")
                  (pcase-let ((`(,field . ,script)
                               (courier--parse-script-block line-number value)))
                    (plist-put request field script))
                (setq request (courier--parse-directive line-number line request))
                (forward-line 1))))
           ((string-match courier--request-line-regexp line)
            (let ((method (match-string 1 line))
                  (url (match-string 2 line)))
              (unless (member method courier--allowed-methods)
                (courier--parse-error line-number "Invalid HTTP method: %s" method))
              (setq request-line-seen t)
              (plist-put request :method method)
              (plist-put request :url url)
              (forward-line 1)))
           (t
            (courier--parse-error line-number "Missing or malformed request line")))))
      (unless request-line-seen
        (courier--parse-error 1 "Missing request line"))
      (while (and (not (eobp))
                  (not body-start))
        (setq line-number (line-number-at-pos))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-empty-p line)
            (forward-line 1)
            (setq body-start (point)))
           ((string-match courier--header-line-regexp line)
            (plist-put request :headers
                       (append (plist-get request :headers)
                               (list (cons (downcase (match-string 1 line))
                                           (match-string 2 line)))))
            (forward-line 1))
           (t
            (courier--parse-error line-number "Malformed header: %s" line)))))
      (when body-start
        (plist-put request :body
                   (buffer-substring-no-properties body-start (point-max)))))
    request))

;;;###autoload
(defun courier-parse-file (path)
  "Parse PATH and return a request plist."
  (with-temp-buffer
    (insert-file-contents path)
    (setq-local buffer-file-name path)
    (courier-parse-buffer)))

;;;###autoload
(defun courier-parse-env-file (path)
  "Parse environment file at PATH and return an alist."
  (with-temp-buffer
    (insert-file-contents path)
    (let (vars)
      (while (not (eobp))
        (let* ((line-number (line-number-at-pos))
               (line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
          (cond
           ((or (string-empty-p line)
                (string-prefix-p "#" line))
            nil)
           ((string-match courier--env-line-regexp line)
            (setq vars
                  (append vars
                          (list (cons (match-string 1 line)
                                      (string-trim (match-string 2 line)))))))
           (t
            (user-error "Parse error in %s on line %d: malformed env line"
                        path line-number))))
        (forward-line 1))
      vars)))

;;;###autoload
(defun courier-merge-vars (env-vars request-vars)
  "Merge ENV-VARS with REQUEST-VARS, with REQUEST-VARS taking precedence."
  (let ((merged (copy-tree env-vars)))
    (dolist (pair request-vars)
      (if-let* ((existing (assoc-string (car pair) merged nil)))
          (setcdr existing (cdr pair))
        (setq merged (append merged (list (cons (car pair) (cdr pair)))))))
    merged))

;;;###autoload
(defun courier-validate-request (request)
  "Validate REQUEST and return REQUEST."
  (let ((method (plist-get request :method))
        (url (plist-get request :url))
        (settings (plist-get request :settings)))
    (unless (and (stringp method) (member method courier--allowed-methods))
      (user-error "Invalid HTTP method: %s" method))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error "Request URL must be present"))
    (when-let* ((timeout (plist-get settings :timeout)))
      (unless (and (integerp timeout) (> timeout 0))
        (user-error "Timeout must be a positive integer")))
    (when (plist-member settings :follow-redirects)
      (unless (memq (plist-get settings :follow-redirects) '(t nil))
        (user-error "Follow redirects must be a boolean"))))
  request)

;;;###autoload
(defun courier-expand-template (string vars)
  "Expand placeholders in STRING using VARS."
  (let ((resolved-vars (courier--resolve-vars vars)))
    (courier--expand-string
     string
     (lambda (name)
       (or (cdr (assoc-string name resolved-vars nil))
           (user-error "Unresolved variable: %s" name))))))

;;;###autoload
(defun courier-resolve-request (request &optional env-vars)
  "Resolve variables in REQUEST with optional ENV-VARS and return a new plist."
  (let* ((vars (courier-merge-vars env-vars (plist-get request :vars)))
         (resolved-vars (courier--resolve-vars vars))
         (resolved (copy-sequence request)))
    (plist-put resolved :params
               (mapcar (lambda (param)
                         (cons (car param)
                               (courier-expand-template (cdr param) resolved-vars)))
                       (plist-get request :params)))
    (plist-put resolved :url
               (let* ((base-url
                       (courier-expand-template (plist-get request :url) resolved-vars))
                      (params (plist-get resolved :params)))
                 (if params
                     (let* ((fragment-pos (string-match "#" base-url))
                            (fragment (and fragment-pos
                                           (substring base-url fragment-pos)))
                            (url-no-fragment (if fragment-pos
                                                 (substring base-url 0 fragment-pos)
                                               base-url))
                            (separator (if (string-match-p "\\?" url-no-fragment)
                                           "&"
                                         "?"))
                            (query
                             (mapconcat
                              (lambda (param)
                                (format "%s=%s"
                                        (url-hexify-string (car param))
                                        (url-hexify-string (cdr param))))
                              params "&")))
                       (concat url-no-fragment separator query fragment))
                   base-url)))
    (plist-put resolved :headers
               (mapcar (lambda (header)
                         (cons (car header)
                               (courier-expand-template (cdr header) resolved-vars)))
                       (plist-get request :headers)))
    (when-let* ((auth-header
                 (courier--resolve-auth-header (plist-get request :auth)
                                               resolved-vars)))
      (unless (assoc-string (car auth-header) (plist-get resolved :headers) nil)
        (plist-put resolved :headers
                   (append (plist-get resolved :headers)
                           (list auth-header)))))
    (plist-put resolved :body
               (courier-expand-template (or (plist-get request :body) "") resolved-vars))
    (plist-put resolved :resolved-vars resolved-vars)
    resolved))

(defun courier--resolve-auth-header (auth resolved-vars)
  "Resolve AUTH into a header cons cell using RESOLVED-VARS."
  (when auth
    (pcase (plist-get auth :type)
      ('bearer
       (cons "authorization"
             (format "Bearer %s"
                     (courier-expand-template
                      (plist-get auth :token)
                      resolved-vars))))
      ('basic
       (let* ((username (courier-expand-template
                         (plist-get auth :username)
                         resolved-vars))
              (password (courier-expand-template
                         (plist-get auth :password)
                         resolved-vars))
              (token (base64-encode-string (format "%s:%s" username password) t)))
         (cons "authorization" (format "Basic %s" token))))
      ('header
       (cons (downcase (plist-get auth :name))
             (courier-expand-template
              (plist-get auth :value)
              resolved-vars))))))

(defvar courier-script-request nil
  "Request plist bound while running Courier request scripts.")

(defvar courier-script-response nil
  "Response plist bound while running Courier response scripts.")

(defvar courier-script-env-vars nil
  "Environment variable alist bound while running Courier request scripts.")

(defvar courier-script-phase nil
  "Current Courier script phase symbol.")

(defvar-local courier--export-format 'curl
  "Current export format for the active Courier request buffer.")

(defvar-local courier--export-interpolate t
  "Whether current Courier exports should interpolate variables.")

(defun courier--plist-like-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (listp value)
       (or (null value)
           (keywordp (car value)))))

(defun courier--eval-script-form (script phase)
  "Evaluate SCRIPT for PHASE and return its final form result."
  (condition-case err
      (eval (read (format "(progn\n%s\n)" script)) t)
    (error
     (user-error "Courier %s script failed: %s"
                 phase
                 (error-message-string err)))))

(defun courier--run-pre-request-script (request env-vars)
  "Run the pre-request script from REQUEST using ENV-VARS."
  (if-let* ((script (plist-get request :pre-request-script))
            ((not (string-empty-p (string-trim script)))))
      (let ((courier-script-request (copy-tree request))
            (courier-script-env-vars (copy-tree env-vars))
            (courier-script-response nil)
            (courier-script-phase 'pre-request))
        (let ((result (courier--eval-script-form script "pre-request")))
          (cond
           ((courier--plist-like-p result) result)
           ((courier--plist-like-p courier-script-request) courier-script-request)
           (t request))))
    request))

(defun courier--run-post-response-script (request response env-vars)
  "Run the post-response script from REQUEST against RESPONSE using ENV-VARS."
  (if-let* ((script (plist-get request :post-response-script))
            ((not (string-empty-p (string-trim script)))))
      (let ((courier-script-request (copy-tree request))
            (courier-script-response (copy-tree response))
            (courier-script-env-vars (copy-tree env-vars))
            (courier-script-phase 'post-response))
        (let ((result (courier--eval-script-form script "post-response")))
          (cond
           ((courier--plist-like-p result) result)
           ((courier--plist-like-p courier-script-response) courier-script-response)
           (t response))))
    response))

;;;###autoload
(defun courier-run-test (response expr)
  "Run EXPR against RESPONSE and return a result plist."
  (pcase expr
    ((rx string-start "status " (group (or "==" "!=")) " " (group (+ digit)) string-end)
     (let* ((operator (match-string 1 expr))
            (expected (string-to-number (match-string 2 expr)))
            (actual (or (plist-get response :status-code) 0))
            (passed (if (string= operator "==")
                        (= actual expected)
                      (/= actual expected))))
       (list :expr expr
             :passed passed
             :message (if passed
                          "ok"
                        (format "expected status %s %d, got %d"
                                operator expected actual)))))
    ((rx string-start "header " (group (+ (not space))) " contains " (group (+ anything)) string-end)
     (let* ((header-name (downcase (match-string 1 expr)))
            (needle (match-string 2 expr))
            (value (or (courier--response-header response header-name) ""))
            (passed (string-match-p (regexp-quote needle) value)))
       (list :expr expr
             :passed passed
             :message (if passed
                          "ok"
                        (format "header %s does not contain %s" header-name needle)))))
    ((rx string-start "body contains " (group (+ anything)) string-end)
     (let* ((needle (match-string 1 expr))
            (body (or (plist-get response :body-text) ""))
            (passed (string-match-p (regexp-quote needle) body)))
       (list :expr expr
             :passed passed
             :message (if passed
                          "ok"
                        (format "response body does not contain %s" needle)))))
    ((rx string-start "time < " (group (+ digit)) string-end)
     (let* ((limit (string-to-number (match-string 1 expr)))
            (actual (or (plist-get response :duration-ms) 0))
            (passed (< actual limit)))
       (list :expr expr
             :passed passed
             :message (if passed
                          "ok"
                        (format "expected time < %d, got %d" limit actual)))))
    ((rx string-start "size < " (group (+ digit)) string-end)
     (let* ((limit (string-to-number (match-string 1 expr)))
            (actual (or (plist-get response :size) 0))
            (passed (< actual limit)))
       (list :expr expr
             :passed passed
             :message (if passed
                          "ok"
                        (format "expected size < %d, got %d" limit actual)))))
    (_
     (user-error "Invalid test expression: %s" expr))))

;;;###autoload
(defun courier-run-tests (response tests)
  "Run TESTS against RESPONSE and return result plists."
  (mapcar (lambda (expr) (courier-run-test response expr)) tests))

;;;; HTTP transport

(defconst courier--write-out-template
  "%{http_code}\n%{size_download}\n%{time_total}\n"
  "curl --write-out template used by Courier.")

(defun courier--human-readable-size (size)
  "Return a human-readable string for SIZE bytes."
  (cond
   ((>= size 1048576) (format "%.1fMB" (/ size 1048576.0)))
   ((>= size 1024) (format "%.1fKB" (/ size 1024.0)))
   (t (format "%dB" size))))

(defun courier--human-readable-duration (duration-ms)
  "Return a human-readable string for DURATION-MS."
  (let ((duration-ms (or duration-ms 0)))
    (if (>= duration-ms 1000)
        (format "%.2fs" (/ duration-ms 1000.0))
      (format "%dms" duration-ms))))

(defun courier--text-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE should be decoded as text."
  (and content-type
       (or (string-prefix-p "text/" content-type)
           (member content-type '("application/json"
                                  "application/xml"
                                  "application/javascript"
                                  "application/xhtml+xml"
                                  "application/x-www-form-urlencoded"))
           (string-suffix-p "+json" content-type)
           (string-suffix-p "+xml" content-type))))

(defun courier--decode-body-file (body-file charset)
  "Decode BODY-FILE using CHARSET.

Signal `user-error' when CHARSET is unsupported. Let decoding failures
surface to the request boundary."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally body-file)
    (let* ((bytes (buffer-string))
           (coding-system
            (if charset
                (let ((symbol (intern-soft charset)))
                  (if (and symbol (coding-system-p symbol))
                      symbol
                    (user-error "Unsupported response charset: %s" charset)))
              'utf-8)))
      (decode-coding-string bytes coding-system))))

(defun courier--parse-content-type (value)
  "Parse CONTENT-TYPE header VALUE."
  (when value
    (let* ((parts (split-string value ";"))
           (content-type (downcase (string-trim (car parts))))
           (charset nil))
      (dolist (part (cdr parts))
        (when (string-match "\\`\\s-*charset=\\([^;]+\\)\\s-*\\'" part)
          (setq charset (downcase (string-trim (match-string 1 part))))))
      (list content-type charset))))

(defun courier--response-content-length (headers)
  "Return numeric content-length from HEADERS, or nil."
  (when-let* ((value (cdr (assoc-string "content-length" headers nil)))
              ((string-match-p "\\`[0-9]+\\'" value)))
    (string-to-number value)))

(defun courier--parse-meta-file (meta-file)
  "Parse META-FILE and return a plist."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents meta-file)
                  (split-string (buffer-string) "\n" t)))
         (status (if lines (string-to-number (nth 0 lines)) 0))
         (size (if (> (length lines) 1)
                   (truncate (string-to-number (nth 1 lines)))
                 0))
         (seconds (if (> (length lines) 2)
                      (string-to-number (nth 2 lines))
                    0.0)))
    (list :status status
          :size size
          :duration-ms (round (* seconds 1000)))))

(defun courier--parse-header-block (header-file)
  "Parse the last header block from HEADER-FILE."
  (if (and (file-exists-p header-file)
           (> (file-attribute-size (file-attributes header-file)) 0))
      (let* ((raw (with-temp-buffer
                    (insert-file-contents header-file)
                    (buffer-string)))
             (normalized (replace-regexp-in-string "\r" "" raw))
             (blocks (split-string normalized "\n\n+" t))
             (block (car (last blocks)))
             (lines (and block (split-string block "\n" t)))
             (status-line (car lines))
             headers)
        (dolist (line (cdr lines))
          (when (string-match "\\`\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
            (setq headers
                  (append headers
                          (list (cons (downcase (match-string 1 line))
                                      (match-string 2 line)))))))
        (list :status-line status-line :headers headers))
    (list :status-line nil :headers nil)))

(defun courier--temp-files-for-request ()
  "Return Courier temp file paths for one request."
  (list :header (make-temp-file "courier-" nil ".headers")
        :body (make-temp-file "courier-" nil ".body")
        :meta (make-temp-file "courier-" nil ".meta")
        :stdout (generate-new-buffer " *courier-stdout*")
        :stderr (generate-new-buffer " *courier-stderr*")))

(defun courier--delete-temp-path (path)
  "Delete temp PATH if it exists."
  (when (and path (stringp path) (file-exists-p path))
    (delete-file path)))

;;;###autoload
(defun courier-build-curl-command (resolved-request header-file body-file meta-file)
  "Build curl argv for RESOLVED-REQUEST.
Use HEADER-FILE, BODY-FILE, and META-FILE for curl output."
  (with-temp-file meta-file
    (insert courier--write-out-template))
  (let* ((settings (plist-get resolved-request :settings))
         (timeout (or (plist-get settings :timeout) courier-default-timeout))
         (follow-redirects (plist-get settings :follow-redirects))
         (command (list courier-curl-executable
                        "-sS"
                        "-D" header-file
                        "-o" body-file
                        "-w" (format "@%s" meta-file)
                        "-X" (plist-get resolved-request :method))))
    (when timeout
      (setq command (append command (list "--max-time" (number-to-string timeout)))))
    (when follow-redirects
      (setq command (append command (list "-L"))))
    (dolist (header (plist-get resolved-request :headers))
      (setq command
            (append command
                    (list "-H" (format "%s: %s" (car header) (cdr header))))))
    (when-let* ((body (plist-get resolved-request :body)))
      (unless (string-empty-p body)
        (with-temp-file body-file
          (insert body))
        (setq command
              (append command (list "--data-binary" (format "@%s" body-file))))))
    (append command (list (plist-get resolved-request :url)))))

;;;###autoload
(defun courier-parse-response (header-file body-file meta-file stderr exit-code request)
  "Parse files HEADER-FILE BODY-FILE META-FILE and STDERR for REQUEST."
  (let* ((header-data (courier--parse-header-block header-file))
         (meta (courier--parse-meta-file meta-file))
         (status-line (plist-get header-data :status-line))
         (headers (plist-get header-data :headers))
         (status-code (or (plist-get meta :status) 0))
         (reason "")
         (content-type-header (cdr (assoc-string "content-type" headers nil)))
         (parsed-type (courier--parse-content-type content-type-header))
         (content-type (car parsed-type))
         (charset (cadr parsed-type))
         (meta-size (plist-get meta :size))
         (header-size (courier--response-content-length headers))
         (body-size (if (file-exists-p body-file)
                        (file-attribute-size (file-attributes body-file))
                      0))
         (size (cond
                ((and (numberp meta-size) (> meta-size 0)) meta-size)
                ((and (numberp header-size) (> header-size 0)) header-size)
                ((and (numberp body-size) (> body-size 0)) body-size)
                (t 0)))
         body-text)
    (when (and status-line
               (string-match "\\`HTTP/[0-9.]+\\s-+\\([0-9]+\\)\\(?:\\s-+\\(.*\\)\\)?\\'" status-line))
      (setq status-code (string-to-number (match-string 1 status-line)))
      (setq reason (or (match-string 2 status-line) "")))
    (when (and (file-exists-p body-file)
               (courier--text-content-type-p content-type)
               (<= size courier-body-text-max-size))
      (setq body-text (courier--decode-body-file body-file charset)))
    (let ((response (list :request-path (plist-get request :path)
                          :status-code status-code
                          :reason reason
                          :headers headers
                          :content-type content-type
                          :charset charset
                          :duration-ms (plist-get meta :duration-ms)
                          :size size
                          :body-file body-file
                          :body-text body-text
                          :stderr stderr
                          :exit-code exit-code
                          :command nil)))
      response)))

(defun courier--apply-response-fallbacks (response &optional start-time)
  "Apply fallback metrics to RESPONSE using START-TIME when needed."
  (unless (> (or (plist-get response :size) 0) 0)
    (plist-put response :size
               (or (courier--response-content-length (plist-get response :headers))
                   (when-let* ((body-file (plist-get response :body-file))
                               ((file-exists-p body-file)))
                     (file-attribute-size (file-attributes body-file)))
                   0)))
  (unless (> (or (plist-get response :duration-ms) 0) 0)
    (when start-time
      (let ((elapsed-ms (max 0 (round (* 1000 (- (float-time) start-time))))))
        (when (> elapsed-ms 0)
          (plist-put response :duration-ms elapsed-ms)))))
  response)

(defun courier--request-error-response (request body-file exit-code command err)
  "Return a response plist for REQUEST when ERR occurs."
  (list :request-path (plist-get request :path)
        :status-code 0
        :reason "Parse Error"
        :headers nil
        :content-type nil
        :charset nil
        :duration-ms 0
        :size 0
        :body-file body-file
        :body-text nil
        :stderr (format "%s" err)
        :exit-code exit-code
        :tests nil
        :command command))

(defun courier--finish-request (process _event)
  "Finish Courier PROCESS after _EVENT."
  (unless (process-live-p process)
    (let* ((stdout-buffer (process-get process 'courier-stdout-buffer))
           (stderr-buffer (process-get process 'courier-stderr-buffer))
           (header-file (process-get process 'courier-header-file))
           (body-file (process-get process 'courier-body-file))
           (meta-file (process-get process 'courier-meta-file))
           (callback (process-get process 'courier-callback))
           (request (process-get process 'courier-request))
           (command (process-get process 'courier-command))
           (exit-code (process-exit-status process))
           response)
      (unwind-protect
          (unless (process-get process 'courier-cancelled)
            (with-temp-file meta-file
              (when (buffer-live-p stdout-buffer)
                (insert (with-current-buffer stdout-buffer (buffer-string)))))
            (setq response
                  (condition-case err
                      (courier-parse-response
                       header-file
                       body-file
                       meta-file
                       (if (buffer-live-p stderr-buffer)
                           (with-current-buffer stderr-buffer (buffer-string))
                         "")
                       exit-code
                       request)
                    (error
                     (courier--request-error-response
                      request body-file exit-code command err))))
            (courier--apply-response-fallbacks
             response
             (process-get process 'courier-start-time))
            (setq response
                  (courier--run-post-response-script
                   request
                   response
                   (plist-get request :env-vars)))
            (plist-put response :tests
                       (courier-run-tests response (plist-get request :tests)))
            (plist-put response :command command)
            (when callback
              (funcall callback response)))
        (when (buffer-live-p stdout-buffer)
          (kill-buffer stdout-buffer))
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))))))

;;;###autoload
(defun courier-send-request (resolved-request callback)
  "Send RESOLVED-REQUEST asynchronously and invoke CALLBACK with the response."
  (let* ((temps (courier--temp-files-for-request))
         (header-file (plist-get temps :header))
         (body-file (plist-get temps :body))
         (meta-file (plist-get temps :meta))
         (stdout-buffer (plist-get temps :stdout))
         (stderr-buffer (plist-get temps :stderr))
         (command (courier-build-curl-command
                   resolved-request header-file body-file meta-file))
         (process (make-process :name "courier-curl"
                                :buffer stdout-buffer
                                :command command
                                :stderr stderr-buffer
                                :noquery t
                                :sentinel #'courier--finish-request)))
    (process-put process 'courier-request resolved-request)
    (process-put process 'courier-callback callback)
    (process-put process 'courier-command command)
    (process-put process 'courier-header-file header-file)
    (process-put process 'courier-body-file body-file)
    (process-put process 'courier-meta-file meta-file)
    (process-put process 'courier-temp-files (list header-file body-file meta-file))
    (process-put process 'courier-stdout-buffer stdout-buffer)
    (process-put process 'courier-stderr-buffer stderr-buffer)
    (process-put process 'courier-start-time (float-time))
    process))

;;;###autoload
(defun courier-cancel-request (process)
  "Cancel Courier PROCESS and clean up its temp files."
  (when (process-live-p process)
    (process-put process 'courier-cancelled t)
    (delete-process process))
  (dolist (path (process-get process 'courier-temp-files))
    (courier--delete-temp-path path))
  (when-let* ((buffer (process-get process 'courier-response-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (courier--response-mark-cancelled)))))

;;;; Overview mode

(defvar-local courier--overview-root nil
  "Request root rendered by the current Courier overview buffer.")

(defvar-local courier--overview-collection-root nil
  "Collection root rendered by the current Courier overview buffer.")

(defvar-local courier--overview-filter nil
  "Current filter string for the Courier overview buffer.")

(defun courier--refresh-overview-buffers ()
  "Refresh all live Courier overview buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'courier-overview-mode)
          (when (and courier--overview-root
                     courier--overview-collection-root)
            (courier-overview-refresh)))))))

(defun courier--request-metadata (path)
  "Return metadata plist stored for PATH, or nil."
  (gethash path courier--request-metadata))

(defun courier--record-request-metadata (request response)
  "Store REQUEST and RESPONSE metadata for overview displays."
  (when-let* ((path (plist-get request :path)))
    (puthash path
             (list :status-code (plist-get response :status-code)
                   :reason (plist-get response :reason)
                   :duration-ms (plist-get response :duration-ms)
                   :timestamp (current-time)
                   :env-name (plist-get request :env-name))
             courier--request-metadata)
    (courier--refresh-overview-buffers)))

(defun courier--collection-name (&optional start)
  "Return a display name for the current collection around START."
  (if-let* ((config (courier--collection-config start))
            (name (plist-get config :name)))
      name
    (if-let* ((root (or (courier--collection-root start)
                        (courier--request-search-root))))
        (file-name-nondirectory
         (directory-file-name root))
      "Courier")))

(defun courier--request-display-env (path)
  "Return the most relevant environment name for request PATH."
  (or (when-let* ((buffer (courier--request-buffer-for-path path)))
        (with-current-buffer buffer
          courier--active-env))
      (plist-get (courier--request-metadata path) :env-name)
      (plist-get (courier--collection-config (file-name-directory path)) :default-env)
      ""))

(defun courier--request-status-string (path)
  "Return a short status string for request PATH."
  (if-let* ((status (plist-get (courier--request-metadata path) :status-code)))
      (number-to-string status)
    ""))

(defun courier--request-updated-at-string (path)
  "Return a short timestamp string for request PATH."
  (if-let* ((timestamp (plist-get (courier--request-metadata path) :timestamp)))
      (format-time-string "%m-%d %H:%M" timestamp)
    ""))

(defun courier--overview-entry (path root)
  "Return an overview entry for request PATH under ROOT."
  (let* ((request (courier-parse-file path))
         (method (or (plist-get request :method) ""))
         (name (or (plist-get request :name)
                   (file-name-base path)))
         (relative-path (file-relative-name path root)))
    (list :path path
          :method method
          :name name
          :relative-path relative-path
          :status (courier--request-status-string path)
          :env (courier--request-display-env path)
          :updated-at (courier--request-updated-at-string path))))

(defun courier--overview-matches-filter-p (entry)
  "Return non-nil when ENTRY matches `courier--overview-filter'."
  (let ((filter (string-trim (or courier--overview-filter ""))))
    (or (string-empty-p filter)
        (let ((haystack
               (downcase
                (mapconcat
                 #'identity
                 (delq nil
                       (list (plist-get entry :method)
                             (plist-get entry :name)
                             (plist-get entry :relative-path)
                             (plist-get entry :status)
                             (plist-get entry :env)))
                 " "))))
          (string-match-p (regexp-quote (downcase filter)) haystack)))))

(defun courier--overview-entries ()
  "Return rendered overview entries for `courier--overview-root'."
  (seq-filter
   #'courier--overview-matches-filter-p
   (mapcar (lambda (path)
             (courier--overview-entry path courier--overview-root))
           (courier--request-files courier--overview-root))))

(defun courier--overview-request-path-at-point ()
  "Return the Courier request path at point, or nil."
  (get-text-property (point) 'courier-request-path))

(defun courier--overview-open-request-buffer (path)
  "Return a Courier request buffer for PATH."
  (let ((buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'courier-request-mode)
        (courier-request-mode)))
    buffer))

(defun courier--overview-header-line ()
  "Return the header line string for the current overview buffer."
  (format " %s | %s | filter: %s "
          (courier--collection-name courier--overview-collection-root)
          (abbreviate-file-name courier--overview-root)
          (if (string-empty-p (or courier--overview-filter ""))
              "(none)"
            courier--overview-filter)))

(defun courier--overview-line (entry)
  "Insert one overview line for ENTRY."
  (let* ((start (point))
         (status (or (plist-get entry :status) ""))
         (env (or (plist-get entry :env) ""))
         (updated-at (or (plist-get entry :updated-at) ""))
         (line
          (format "%-7s %-24s %-38s %-6s %-10s %s"
                  (plist-get entry :method)
                  (truncate-string-to-width (plist-get entry :name) 24 nil nil t)
                  (truncate-string-to-width (plist-get entry :relative-path) 38 nil nil t)
                  status
                  env
                  updated-at)))
    (insert line "\n")
    (add-text-properties
     start (point)
     `(courier-request-path ,(plist-get entry :path)
                            mouse-face highlight
                            help-echo ,(plist-get entry :path)))))

(define-derived-mode courier-overview-mode special-mode "Courier-Overview"
  "Major mode used for Courier collection overview buffers."
  (setq-local truncate-lines t))

;;;###autoload
(defun courier-overview-refresh ()
  "Refresh the current Courier overview buffer."
  (interactive)
  (unless courier--overview-root
    (user-error "No Courier overview is active in this buffer"))
  (let ((inhibit-read-only t)
        (entries (courier--overview-entries)))
    (erase-buffer)
    (setq header-line-format (courier--overview-header-line))
    (insert (format "Collection: %s\n"
                    (courier--collection-name courier--overview-collection-root)))
    (insert (format "Root:       %s\n\n"
                    (abbreviate-file-name courier--overview-root)))
    (insert (format "%-7s %-24s %-38s %-6s %-10s %s\n"
                    "Method" "Name" "Path" "Last" "Env" "Updated"))
    (insert (make-string 104 ?-))
    (insert "\n")
    (if entries
        (dolist (entry entries)
          (courier--overview-line entry))
      (insert "(no requests)\n"))
    (goto-char (point-min))
    (forward-line 4)))

;;;###autoload
(defun courier-overview-set-filter (filter)
  "Set the current Courier overview FILTER."
  (interactive
   (list (read-string "Courier filter: "
                      nil nil courier--overview-filter)))
  (setq courier--overview-filter (unless (string-empty-p filter) filter))
  (courier-overview-refresh))

;;;###autoload
(defun courier-overview-open ()
  "Open the Courier request at point."
  (interactive)
  (if-let* ((path (courier--overview-request-path-at-point)))
      (find-file path)
    (user-error "No Courier request on this line")))

;;;###autoload
(defun courier-overview-preview ()
  "Preview the Courier request at point."
  (interactive)
  (if-let* ((path (courier--overview-request-path-at-point)))
      (with-current-buffer (courier--overview-open-request-buffer path)
        (call-interactively #'courier-request-preview))
    (user-error "No Courier request on this line")))

;;;###autoload
(defun courier-overview-send ()
  "Send the Courier request at point."
  (interactive)
  (if-let* ((path (courier--overview-request-path-at-point)))
      (with-current-buffer (courier--overview-open-request-buffer path)
        (call-interactively #'courier-request-send))
    (user-error "No Courier request on this line")))

;;;###autoload
(defun courier-overview-switch-env ()
  "Switch the environment of the Courier request at point."
  (interactive)
  (if-let* ((path (courier--overview-request-path-at-point)))
      (with-current-buffer (courier--overview-open-request-buffer path)
        (call-interactively #'courier-request-switch-env))
    (user-error "No Courier request on this line")))

;;;###autoload
(defun courier-overview ()
  "Open the Courier overview for the current collection."
  (interactive)
  (let* ((collection-root (courier--collection-root))
         (request-root (courier--request-search-root)))
    (unless collection-root
      (user-error "No Courier collection found"))
    (let ((buffer (get-buffer-create
                   (format "*courier-overview: %s*"
                           (courier--collection-name collection-root)))))
      (with-current-buffer buffer
        (courier-overview-mode)
        (setq-local courier--overview-root request-root)
        (setq-local courier--overview-collection-root collection-root)
        (unless (local-variable-p 'courier--overview-filter buffer)
          (setq-local courier--overview-filter nil))
        (courier-overview-refresh))
      (display-buffer buffer))))

(define-key courier-overview-mode-map (kbd "RET") #'courier-overview-open)
(define-key courier-overview-mode-map (kbd "f") #'courier-new-folder)
(define-key courier-overview-mode-map (kbd "C-c ?") #'courier-dispatch)
(define-key courier-overview-mode-map (kbd "e") #'courier-overview-switch-env)
(define-key courier-overview-mode-map (kbd "g") #'courier-overview-refresh)
(define-key courier-overview-mode-map (kbd "m") #'courier-move-request)
(define-key courier-overview-mode-map (kbd "n") #'courier-new-request)
(define-key courier-overview-mode-map (kbd "p") #'courier-overview-preview)
(define-key courier-overview-mode-map (kbd "r") #'courier-rename-request)
(define-key courier-overview-mode-map (kbd "s") #'courier-overview-send)
(define-key courier-overview-mode-map (kbd "/") #'courier-overview-set-filter)

;;;; Response mode

(defvar-local courier--response nil
  "Response plist displayed in the current Courier response buffer.")

(defvar-local courier--request nil
  "Resolved request plist associated with the current Courier response buffer.")

(defvar-local courier--process nil
  "In-flight curl process for the current Courier response buffer.")

(defvar-local courier--temp-files nil
  "Temp file paths associated with the current Courier response buffer.")

(defvar-local courier--body-pretty t
  "Whether the current Courier response buffer pretty-prints JSON bodies.")

(defvar-local courier--body-view nil
  "Explicit body view for the current Courier response buffer.")

(defvar-local courier--response-tab 'response
  "Current tab displayed in the current Courier response buffer.")

(defvar-local courier--timeline-expanded-sections '(request)
  "Expanded detail sections in the Courier timeline view.")

(defvar-local courier--history nil
  "Response history for the current Courier response buffer.
Each entry is a cons cell of the form `(TIMESTAMP . RESPONSE)'.")

(defvar-local courier--history-index nil
  "Expanded history index in the current timeline, or nil when collapsed.")

(defun courier--json-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies JSON."
  (and content-type
       (string-match-p "json" content-type)))

(defun courier--html-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies HTML."
  (and content-type
       (string-match-p "html" content-type)))

(defun courier--xml-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies XML."
  (and content-type
       (string-match-p "xml" content-type)))

(defun courier--javascript-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies JavaScript."
  (and content-type
       (string-match-p "javascript\\|ecmascript" content-type)))

(defun courier--image-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies an image."
  (and content-type
       (string-match-p "\\`image/" content-type)))

(defun courier--pretty-json-body (body-text)
  "Pretty-print BODY-TEXT as JSON and return the rendered string."
  (with-temp-buffer
    (insert body-text)
    (json-pretty-print-buffer)
    (buffer-string)))

(defun courier--set-body-view-local (view)
  "Set the current response body VIEW locally."
  (setq courier--body-view view
        courier--body-pretty (not (eq view 'raw))))

(defun courier--auto-body-view (response)
  "Return the automatic body view for RESPONSE."
  (let ((content-type (plist-get response :content-type)))
    (cond
     ((courier--image-content-type-p content-type) 'image)
     ((courier--json-content-type-p content-type) 'json)
     ((courier--html-content-type-p content-type) 'html)
     ((courier--xml-content-type-p content-type) 'xml)
     ((courier--javascript-content-type-p content-type) 'javascript)
     ((plist-get response :body-text) 'raw)
     ((plist-get response :body-file) 'hex)
     (t 'raw))))

(defun courier--effective-body-view (response)
  "Return the effective body view for RESPONSE."
  (let ((view (or courier--body-view
                  (if courier--body-pretty 'auto 'raw))))
    (if (eq view 'auto)
        (courier--auto-body-view response)
      view)))

(defun courier--body-view-label (response)
  "Return a display label for the current body view of RESPONSE."
  (let ((view (or courier--body-view
                  (if courier--body-pretty 'auto 'raw))))
    (if (eq view 'auto)
        (format "auto:%s" (courier--auto-body-view response))
      (symbol-name view))))

(defun courier--response-body-bytes (response)
  "Return `(BYTES . TRUNCATEDP)' for RESPONSE body bytes."
  (let ((max-bytes (max 0 courier-body-view-max-bytes)))
    (cond
     ((plist-get response :body-file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally
         (plist-get response :body-file) nil 0 max-bytes)
        (let* ((bytes (buffer-substring-no-properties (point-min) (point-max)))
               (size (or (plist-get response :size) (length bytes))))
          (cons bytes (> size (length bytes))))))
     ((plist-get response :body-text)
      (let* ((bytes (encode-coding-string (plist-get response :body-text)
                                          'utf-8 t))
             (truncatedp (> (length bytes) max-bytes)))
        (cons (if truncatedp
                  (substring bytes 0 max-bytes)
                bytes)
              truncatedp)))
     (t
      (cons nil nil)))))

(defun courier--hex-dump-string (bytes)
  "Return a hex dump string for BYTES."
  (let ((index 0)
        (length (length bytes))
        lines)
    (while (< index length)
      (let* ((end (min length (+ index 16)))
             (chunk (substring bytes index end))
             (hex-bytes
              (mapconcat (lambda (byte)
                           (format "%02x" byte))
                         (string-to-list chunk)
                         " "))
             (ascii
              (apply #'string
                     (mapcar (lambda (byte)
                               (if (<= 32 byte 126) byte ?.))
                             (string-to-list chunk)))))
        (push (format "%08x  %-47s  %s" index hex-bytes ascii) lines)
        (setq index end)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun courier--base64-body-string (response)
  "Return a base64 rendering string for RESPONSE."
  (pcase-let ((`(,bytes . ,truncatedp) (courier--response-body-bytes response)))
    (if bytes
        (concat
         (base64-encode-string bytes t)
         "\n"
         (if truncatedp
             (format "\n[truncated to %d bytes]\n" courier-body-view-max-bytes)
           ""))
      "(empty)\n")))

(defun courier--hex-body-string (response)
  "Return a hex rendering string for RESPONSE."
  (pcase-let ((`(,bytes . ,truncatedp) (courier--response-body-bytes response)))
    (if bytes
        (concat
         (courier--hex-dump-string bytes)
         "\n"
         (if truncatedp
             (format "\n[truncated to %d bytes]\n" courier-body-view-max-bytes)
           ""))
      "(empty)\n")))

(defun courier--response-reason (response)
  "Return the best available reason phrase for RESPONSE."
  (let* ((status (or (plist-get response :status-code) 0))
         (reason (string-trim (or (plist-get response :reason) ""))))
    (if (string-empty-p reason)
        (or (alist-get status courier--status-reason-phrases) "")
      reason)))

(defun courier--response-status-label (response)
  "Return a status label for RESPONSE."
  (let* ((status (or (plist-get response :status-code) 0))
         (reason (courier--response-reason response)))
    (if (string-empty-p reason)
        (number-to-string status)
      (format "%d %s" status reason))))

(defun courier--response-tab-display-name (tab)
  "Return a display name for response TAB."
  (pcase tab
    ('response "Response")
    ('headers "Headers")
    ('timeline "Timeline")
    ('tests "Tests")
    (_ "Response")))

(defun courier--timeline-tab-display-name (tab)
  "Return a display name for timeline TAB."
  (pcase tab
    ('request "Request")
    ('response "Response")
    ('network-logs "Network Logs")
    (_ "Request")))

(defun courier--response-status-face (response)
  "Return a face appropriate for RESPONSE status."
  (let ((status (or (plist-get response :status-code) 0)))
    (cond
     ((and (>= status 200) (< status 300))
      'courier-response-status-success-face)
     ((and (>= status 300) (< status 400))
      'courier-response-status-warning-face)
     ((and (>= status 400) (< status 600))
      'courier-response-status-error-face)
     (t
      'bold))))

(defun courier--current-history-index ()
  "Return the selected history index for the current response buffer."
  courier--history-index)

(defun courier--current-history-entry ()
  "Return the currently selected history entry, or nil."
  (and (numberp (courier--current-history-index))
       (nth (courier--current-history-index) courier--history)))

(defun courier--current-history-response ()
  "Return the response displayed by the current history selection."
  (cdr-safe (courier--current-history-entry)))

(defun courier--current-history-request ()
  "Return the request snapshot displayed by the current history selection."
  (when-let* ((response (courier--current-history-response)))
    (or (plist-get response :request-snapshot)
        courier--request)))

(defun courier--response-tab-count (tab response)
  "Return a count for response TAB using RESPONSE when appropriate."
  (pcase tab
    ('headers
     (length (plist-get response :headers)))
    ('tests
     (length (plist-get response :tests)))
    (_
     nil)))

(defun courier--response-tab-button (tab response)
  "Return a clickable header-line button string for TAB using RESPONSE."
  (let* ((label (courier--response-tab-display-name tab))
         (count (courier--response-tab-count tab response))
         (text (if (and count (> count 0))
                   (concat label " "
                           (propertize (number-to-string count)
                                       'face 'courier-response-tab-count-face))
                 label))
         (map (make-sparse-keymap))
         (face (if (eq tab courier--response-tab)
                   'courier-response-tab-active-face
                 'courier-response-tab-inactive-face)))
    (define-key map [header-line mouse-1]
      (lambda ()
        (interactive)
        (courier-response-set-tab tab)))
    (define-key map [header-line down-mouse-1] #'ignore)
    (propertize text
                'face face
                'mouse-face 'mode-line-highlight
                'help-echo (format "Switch to %s"
                                   (courier--response-tab-display-name tab))
                'follow-link t
                'local-map map)))

(defun courier--current-response-tab-button (response)
  "Return the clickable header-line button for the current response tab."
  (let ((tab (or courier--response-tab 'response))
        (map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'courier-response-set-tab)
    (define-key map [header-line down-mouse-1] #'ignore)
    (propertize (substring-no-properties
                 (courier--response-tab-button tab response))
                'face 'courier-response-tab-active-face
                'mouse-face 'mode-line-highlight
                'help-echo "Switch response view"
                'follow-link t
                'local-map map)))

(defun courier--timeline-entry-button-action (button)
  "Toggle the timeline entry referenced by BUTTON."
  (courier--select-history-index (button-get button 'courier-history-index)))

(defun courier--timeline-section-expanded-p (tab)
  "Return non-nil when timeline detail section TAB is expanded."
  (memq tab courier--timeline-expanded-sections))

(defun courier--timeline-section-button-action (button)
  "Switch to the timeline detail section referenced by BUTTON."
  (courier--toggle-timeline-section (button-get button 'courier-timeline-tab)))

(defun courier--insert-timeline-section-heading (tab)
  "Insert a clickable heading for timeline detail section TAB."
  (let ((label (format "%s %s"
                       (if (courier--timeline-section-expanded-p tab) "[-]" "[+]")
                       (courier--timeline-tab-display-name tab))))
    (insert-text-button
     label
     'action #'courier--timeline-section-button-action
     'follow-link t
     'courier-timeline-tab tab
     'face (if (courier--timeline-section-expanded-p tab)
               'courier-response-timeline-tab-active-face
             'courier-response-timeline-tab-inactive-face)
     'help-echo (format "RET or mouse-1 to show %s"
                        (courier--timeline-tab-display-name tab)))
    (insert "\n")))

(defun courier--response-header-line (response)
  "Return a header line string for RESPONSE."
  (concat
   (propertize (courier--response-status-label response)
               'face (courier--response-status-face response))
   courier--header-separator
   (courier--human-readable-duration (plist-get response :duration-ms))
   courier--header-separator
   (courier--human-readable-size (or (plist-get response :size) 0))))

(defun courier--response-header-line-with-history (response timestamp index total)
  "Return a history header line for RESPONSE at TIMESTAMP, INDEX, and TOTAL."
  (concat
   (courier--response-header-line response)
   courier--header-separator
   (propertize (format "[%s %d/%d]" timestamp (1+ index) total)
               'face 'shadow)))

(defun courier--header-summary-response ()
  "Return the response that should drive the current header summary."
  (if (and (eq courier--response-tab 'timeline)
           (numberp courier--history-index))
      (or (courier--current-history-response)
          courier--response)
    courier--response))

(defun courier--clear-timeline-button ()
  "Return a clickable header-line button that clears the timeline."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'courier-response-clear-timeline)
    (define-key map [header-line down-mouse-1] #'ignore)
    (propertize "Clear Timeline"
                'face 'link
                'mouse-face 'mode-line-highlight
                'help-echo "Clear the timeline history for this response buffer"
                'follow-link t
                'local-map map)))

(defun courier--response-header-line-format (response)
  "Return the complete header line format for RESPONSE."
  (let* ((summary-response (or (courier--header-summary-response)
                               response))
         (summary
          (if (and (eq courier--response-tab 'timeline)
                   (numberp courier--history-index))
              (let* ((entry (courier--current-history-entry))
                     (timestamp (car entry)))
                (courier--response-header-line-with-history
                 summary-response timestamp courier--history-index
                 (length courier--history)))
            (courier--response-header-line summary-response))))
    (concat
     " "
     (courier--current-response-tab-button response)
     courier--header-separator
     summary
     (if (and (eq courier--response-tab 'timeline)
              (numberp courier--history-index))
         (concat courier--header-separator
                 (courier--clear-timeline-button))
       ""))))

(defun courier--body-viewer-buffer-name ()
  "Return the body viewer buffer name for the current response buffer."
  (format "*courier-body: %s*"
          (or (plist-get courier--request :name)
              (format "%s %s"
                      (plist-get courier--request :method)
                      (plist-get courier--request :url)))))

(defun courier--body-view-major-mode (view)
  "Return the preferred major mode for body VIEW."
  (pcase view
    ('json
     (cond
      ((fboundp 'json-ts-mode) #'json-ts-mode)
      ((fboundp 'js-json-mode) #'js-json-mode)
      ((fboundp 'js-mode) #'js-mode)
      (t #'fundamental-mode)))
    ('html
     (if (fboundp 'html-mode) #'html-mode #'fundamental-mode))
    ('xml
     (if (fboundp 'nxml-mode) #'nxml-mode #'fundamental-mode))
    ('javascript
     (if (fboundp 'js-mode) #'js-mode #'fundamental-mode))
    (_
     #'fundamental-mode)))

(defun courier--fontify-string-for-view (string view)
  "Return STRING fontified for response body VIEW."
  (let ((mode (courier--body-view-major-mode view)))
    (if (or (null string)
            (string-empty-p string)
            (eq mode #'fundamental-mode))
        string
      (with-temp-buffer
        (insert string)
        (funcall mode)
        (font-lock-ensure)
        (buffer-substring (point-min) (point-max))))))

(defun courier--format-summary (response)
  "Return a summary section string for RESPONSE."
  (format "Status: %s %s\nDuration: %d ms\nSize: %s\nContent-Type: %s\n"
          (or (plist-get response :status-code) 0)
          (or (plist-get response :reason) "")
          (or (plist-get response :duration-ms) 0)
          (courier--human-readable-size (or (plist-get response :size) 0))
          (or (cdr (assoc-string "content-type" (plist-get response :headers) nil))
              "unknown")))

(defun courier--response-text-width ()
  "Return the preferred text width for the current Courier response buffer."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 60
         (or (and window (window-body-width window))
             100))))

(defun courier--wrap-response-cell (text width)
  "Wrap TEXT into a list of lines no wider than WIDTH when practical."
  (let ((words (split-string (or text "") "[[:space:]\n]+" t))
        (lines nil)
        (current ""))
    (dolist (word words)
      (if (string-empty-p current)
          (setq current word)
        (if (<= (+ (string-width current) 1 (string-width word)) width)
            (setq current (concat current " " word))
          (push current lines)
          (setq current word))))
    (when (or (not (string-empty-p current))
              (null lines))
      (push current lines))
    (nreverse lines)))

(defun courier--format-headers (response)
  "Return formatted headers for RESPONSE."
  (if-let* ((headers (plist-get response :headers)))
      (let* ((name-width
              (min 24
                   (max 8
                        (apply #'max
                               (string-width "Name")
                               (mapcar (lambda (header)
                                         (string-width (car header)))
                                       headers)))))
             (value-width (max 24 (- (courier--response-text-width)
                                     name-width
                                     3)))
             (indent (make-string (+ name-width 2) ?\s))
             (header-name (propertize
                           (format (format "%%-%ds" name-width) "Name")
                           'face 'bold))
             (header-value (propertize "Value" 'face 'bold))
             (divider (concat (make-string name-width ?-)
                              "  "
                              (make-string (min 40 value-width) ?-)))
             (rows
              (mapcar
               (lambda (header)
                 (let* ((name (car header))
                        (value-lines (courier--wrap-response-cell
                                      (cdr header) value-width))
                        (first-line
                         (concat
                          (propertize
                           (format (format "%%-%ds" name-width) name)
                           'face 'font-lock-variable-name-face)
                          "  "
                          (car value-lines)))
                        (rest-lines
                         (mapcar (lambda (line)
                                   (concat indent line))
                                 (cdr value-lines))))
                   (mapconcat #'identity (cons first-line rest-lines) "\n")))
               headers)))
        (concat
         header-name "  " header-value "\n"
         divider "\n"
         (mapconcat #'identity rows "\n")
         "\n"))
    "(none)\n"))

(defun courier--format-body (response)
  "Return formatted body text for RESPONSE."
  (let* ((view (courier--effective-body-view response))
         (body
          (pcase view
            ('json
             (if-let* ((body-text (plist-get response :body-text)))
                 (concat
                  (condition-case _err
                      (courier--pretty-json-body body-text)
                    (error body-text))
                  "\n")
               (format "Body saved to %s\n" (plist-get response :body-file))))
            ((or 'html 'xml 'javascript 'raw)
             (cond
              ((plist-get response :body-text)
               (concat (plist-get response :body-text) "\n"))
              ((plist-get response :body-file)
               (format "Body saved to %s\n" (plist-get response :body-file)))
              (t
               "(empty)\n")))
            ('base64
             (courier--base64-body-string response))
            ('hex
             (courier--hex-body-string response))
            ('image
             (if-let* ((body-file (plist-get response :body-file)))
                 (format "Image body saved to %s\n" body-file)
               "(empty)\n"))
            (_
             "(empty)\n"))))
    (if (memq view '(json html xml javascript))
        (courier--fontify-string-for-view body view)
      body)))

(defun courier--format-tests (response)
  "Return formatted test results for RESPONSE."
  (if-let* ((tests (plist-get response :tests)))
      (concat
       (mapconcat
        (lambda (test)
          (format "%s %s%s"
                  (if (plist-get test :passed) "✓" "✗")
                  (plist-get test :expr)
                  (if (plist-get test :passed)
                      ""
                    (format "  — %s" (plist-get test :message)))))
        tests
        "\n")
       "\n")
    "(none)\n"))

(defun courier--format-network-logs (response request)
  "Return a network log section string for RESPONSE and REQUEST."
  (let ((command (plist-get response :command)))
    (concat
     (format "Exit code: %d\n" (or (plist-get response :exit-code) 0))
     (when command
       (format "$ %s\n"
               (mapconcat #'identity command " ")))
     (unless (string-empty-p (or (plist-get response :stderr) ""))
       (concat (plist-get response :stderr)
               (unless (string-suffix-p "\n" (plist-get response :stderr))
                 "\n")))
     (unless (plist-get response :command)
       (format "Request: %s %s\n"
               (plist-get request :method)
               (plist-get request :url))))))

(defun courier--insert-timeline-heading (title)
  "Insert timeline section heading TITLE."
  (insert (propertize title 'face 'courier-response-timeline-heading-face))
  (insert "\n"))

(defun courier--set-response-tab-local (tab)
  "Set the current response TAB locally."
  (setq courier--response-tab tab))

(defun courier--set-timeline-expanded-sections-local (sections)
  "Set expanded timeline detail SECTIONS locally."
  (setq courier--timeline-expanded-sections sections))

(defun courier--response-timeline-entry-line (index timestamp response)
  "Return one formatted timeline entry for INDEX, TIMESTAMP, and RESPONSE."
  (let* ((selectedp (and (numberp courier--history-index)
                         (= index courier--history-index)))
         (status (courier--response-status-label response))
         (duration (courier--human-readable-duration
                    (plist-get response :duration-ms)))
         (size (courier--human-readable-size
                (or (plist-get response :size) 0)))
         (method (or (plist-get response :request-method)
                     (plist-get courier--request :method)
                     ""))
         (url (or (plist-get response :request-url)
                  (plist-get courier--request :url)
                  ""))
         (status-face (courier--response-status-face response))
         (line-face (when selectedp 'courier-response-timeline-selected-face)))
    (concat
     (propertize status 'face (if selectedp
                                  `(,status-face courier-response-timeline-selected-face)
                                status-face))
     (propertize (format "  %s" timestamp)
                 'face (or line-face 'shadow))
     "\n"
     (propertize method
                 'face (if selectedp
                           '(courier-response-method-face courier-response-timeline-selected-face)
                         'courier-response-method-face))
     " "
     (propertize url
                 'face (if selectedp
                           '(courier-response-url-face courier-response-timeline-selected-face)
                         'courier-response-url-face))
     "\n"
     (propertize (concat duration courier--header-separator size)
                 'face (or line-face 'shadow))
     "\n")))

(defun courier--insert-response-body (response)
  "Insert the active response body for RESPONSE."
  (let ((view (courier--effective-body-view response))
        (body-file (plist-get response :body-file)))
    (pcase view
      ('image
       (cond
        ((and body-file
              (display-images-p)
              (ignore-errors (create-image body-file)))
         (insert-image (create-image body-file))
         (insert "\n\n")
         (insert body-file "\n"))
        (body-file
         (insert (format "Image body saved to %s\n" body-file)))
        (t
         (insert "(empty)\n"))))
      (_
       (insert (courier--format-body response))))))

(defun courier--insert-timeline-request-view (request)
  "Insert timeline request details for REQUEST."
  (insert (propertize (or (plist-get request :url) "(no URL)")
                      'face 'courier-response-url-face))
  (insert "\n\n")
  (courier--insert-timeline-heading "Headers")
  (if-let* ((headers (plist-get request :headers)))
      (insert (courier--format-headers (list :headers headers)))
    (insert "No Headers found\n"))
  (insert "\n")
  (courier--insert-timeline-heading "Body")
  (let ((body (plist-get request :body)))
    (if (and body (not (string-empty-p body)))
        (progn
          (insert body)
          (unless (string-suffix-p "\n" body)
            (insert "\n")))
      (insert "No Body found\n"))))

(defun courier--insert-timeline-response-view (response)
  "Insert timeline response details for RESPONSE."
  (courier--insert-timeline-heading "Headers")
  (if (plist-get response :headers)
      (insert (courier--format-headers response))
    (insert "No Headers found\n"))
  (insert "\n")
  (courier--insert-timeline-heading "Body")
  (let ((body (courier--format-body response)))
    (if (string-empty-p (string-trim body))
        (insert "No Body found\n")
      (insert body))))

(defun courier--insert-timeline-network-logs-view (response request)
  "Insert timeline network logs for RESPONSE and REQUEST."
  (let ((logs (string-trim-right (courier--format-network-logs response request))))
    (if (string-empty-p logs)
        (insert "No network logs found\n")
      (insert logs "\n"))))

(defun courier--insert-expanded-timeline-details (response request timestamp)
  "Insert expanded timeline details for RESPONSE, REQUEST, and TIMESTAMP."
  (ignore timestamp)
  (dolist (tab courier--timeline-tabs)
    (courier--insert-timeline-section-heading tab)
    (when (courier--timeline-section-expanded-p tab)
      (pcase tab
        ('request
         (courier--insert-timeline-request-view request))
        ('response
         (courier--insert-timeline-response-view response))
        ('network-logs
         (courier--insert-timeline-network-logs-view response request)))
      (insert "\n"))))

(defun courier--insert-response-timeline ()
  "Insert the response timeline for the current request history."
  (if (null courier--history)
      (insert "No history yet.\n")
    (cl-loop for history-entry in courier--history
             for index from 0
             for response = (cdr history-entry)
             for request = (or (plist-get response :request-snapshot)
                               courier--request)
             for timestamp = (car history-entry)
             do
             (insert-text-button
              (courier--response-timeline-entry-line
               index timestamp response)
              'action #'courier--timeline-entry-button-action
              'follow-link t
              'courier-history-index index
              'help-echo "RET, TAB, or mouse-1 to expand or collapse this response history entry")
             (insert "\n")
             (when (and (numberp courier--history-index)
                        (= index courier--history-index))
               (courier--insert-expanded-timeline-details response request timestamp)
               (insert "\n")))))

(defun courier--insert-response-tab (response _request)
  "Insert the currently selected response tab for RESPONSE."
  (pcase courier--response-tab
    ('response
     (courier--insert-response-body response))
    ('headers
     (insert (courier--format-headers response)))
    ('timeline
     (courier--insert-response-timeline))
    ('tests
     (insert (courier--format-tests response)))
    (_
     (courier--insert-response-body response))))

(defun courier--refresh-response-display ()
  "Refresh the current Courier response buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq header-line-format
          (courier--response-header-line-format courier--response))
    (courier--insert-response-tab courier--response courier--request)
    (goto-char (point-min))))

(defun courier--history-push (response)
  "Push RESPONSE onto the history for the current response buffer."
  (push (cons (format-time-string "%Y-%m-%d %H:%M:%S") response)
        courier--history)
  (let ((max (max 0 courier-history-max)))
    (when (> (length courier--history) max)
      (setq courier--history (seq-take courier--history max))))
  (setq courier--history-index nil))

(defun courier--history-render-current ()
  "Render the response at `courier--history-index'."
  (unless courier--history
    (user-error "No response history"))
  (courier--refresh-response-display))

(defun courier--response-cleanup ()
  "Clean up process and temp files for the current response buffer."
  (when (and (processp courier--process)
             (process-live-p courier--process))
    (courier-cancel-request courier--process))
  (dolist (path courier--temp-files)
    (when (and path (stringp path) (file-exists-p path))
      (delete-file path))))

(define-derived-mode courier--response-mode special-mode "Courier-Response"
  "Major mode used for Courier response buffers."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'courier--response-cleanup nil t))

(defun courier--response-buffer-name (request)
  "Return a response buffer name for REQUEST."
  (format "*courier-response: %s*"
          (or (plist-get request :name)
              (format "%s %s"
                      (plist-get request :method)
                      (plist-get request :url)))))

(defun courier--response-show-sending (request)
  "Render a sending state for REQUEST in the current buffer."
  (let ((inhibit-read-only t)
        (history courier--history)
        (temp-files courier--temp-files)
        (body-view courier--body-view)
        (body-pretty courier--body-pretty)
        (tab courier--response-tab)
        (timeline-sections courier--timeline-expanded-sections))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--request request
          courier--response nil
          courier--process nil
          courier--body-pretty body-pretty
          courier--body-view body-view
          courier--response-tab tab
          courier--timeline-expanded-sections timeline-sections
          courier--temp-files temp-files
          courier--history history
          courier--history-index nil)
    (setq header-line-format "Sending...")
    (insert "Sending...\n\n")
    (insert (format "Waiting for response from %s %s\n"
                    (plist-get request :method)
                    (plist-get request :url)))))

(defun courier--response-mark-cancelled ()
  "Mark the current response buffer as cancelled."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq courier--process nil)
    (setq header-line-format "Cancelled")
    (insert "Request cancelled.\n")))

(defun courier--render-response (response request)
  "Render RESPONSE for REQUEST in the current buffer."
  (let* ((inhibit-read-only t)
         (prev-history courier--history)
         (prev-temp-files courier--temp-files)
         (body-view courier--body-view)
         (body-pretty courier--body-pretty)
         (tab courier--response-tab)
         (timeline-sections courier--timeline-expanded-sections))
    (courier--response-mode)
    (erase-buffer)
    (plist-put response :request-method (plist-get request :method))
    (plist-put response :request-url (plist-get request :url))
    (plist-put response :request-snapshot (copy-tree request t))
    (setq courier--response response
          courier--request request
          courier--process nil
          courier--body-pretty body-pretty
          courier--body-view body-view
          courier--response-tab (or tab 'response)
          courier--timeline-expanded-sections (or timeline-sections '(request))
          courier--temp-files prev-temp-files
          courier--history prev-history
          courier--history-index nil)
    (courier--history-push response)
    (courier--record-request-metadata request response)
    (courier--refresh-response-display)))

;;;###autoload
(defun courier-response-retry ()
  "Retry the request associated with the current response buffer."
  (interactive)
  (unless courier--request
    (user-error "No Courier request associated with this buffer"))
  (let ((path (plist-get courier--request :path)))
    (unless path
      (user-error "Current Courier response has no source file"))
    (with-current-buffer (find-file-noselect path)
      (call-interactively #'courier-request-send))))

;;;###autoload
(defun courier-response-cancel ()
  "Cancel the in-flight request associated with the current response buffer."
  (interactive)
  (unless (and (processp courier--process)
               (process-live-p courier--process))
    (user-error "No Courier request is in flight"))
  (courier-cancel-request courier--process))

;;;###autoload
(defun courier-response-toggle-pretty ()
  "Toggle pretty-printing for the body in the current Courier response buffer."
  (interactive)
  (unless courier--response
    (user-error "No Courier response is available"))
  (if (eq (courier--effective-body-view courier--response) 'raw)
      (courier--set-body-view-local 'auto)
    (courier--set-body-view-local 'raw))
  (courier--set-response-tab-local 'response)
  (courier--refresh-response-display))

;;;###autoload
(defun courier-response-set-view (view)
  "Set the current Courier response body VIEW."
  (interactive
   (list
    (intern
     (completing-read "Courier body view: "
                      (mapcar #'symbol-name courier--body-views)
                      nil t nil nil
                      (symbol-name
                       (or courier--body-view
                           (if courier--body-pretty 'auto 'raw)))))))
  (unless courier--response
    (user-error "No Courier response is available"))
  (courier--set-body-view-local view)
  (courier--set-response-tab-local 'response)
  (courier--refresh-response-display))

(defun courier--set-response-tab (tab)
  "Switch the current response buffer to TAB."
  (unless courier--response
    (user-error "No Courier response is available"))
  (when (and (eq tab 'timeline)
             (null courier--timeline-expanded-sections))
    (courier--set-timeline-expanded-sections-local '(request)))
  (courier--set-response-tab-local tab)
  (courier--refresh-response-display))

(defun courier--toggle-timeline-section (tab)
  "Toggle timeline detail section TAB in the current response buffer."
  (unless courier--response
    (user-error "No Courier response is available"))
  (courier--set-response-tab-local 'timeline)
  (courier--set-timeline-expanded-sections-local
   (if (courier--timeline-section-expanded-p tab)
       (delq tab (copy-sequence courier--timeline-expanded-sections))
     (append courier--timeline-expanded-sections (list tab))))
  (courier--refresh-response-display)
  (courier--goto-timeline-section-button tab))

;;;###autoload
(defun courier-response-set-tab (tab)
  "Set the current Courier response TAB."
  (interactive
   (list
    (intern
     (completing-read "Courier tab: "
                      (mapcar #'symbol-name courier--response-tabs)
                      nil t nil nil
                      (symbol-name (or courier--response-tab 'response))))))
  (courier--set-response-tab tab))

;;;###autoload
(defun courier-response-show-response ()
  "Show the response tab in the current Courier response buffer."
  (interactive)
  (courier--set-response-tab 'response))

;;;###autoload
(defun courier-response-show-headers ()
  "Show the headers tab in the current Courier response buffer."
  (interactive)
  (courier--set-response-tab 'headers))

;;;###autoload
(defun courier-response-show-timeline ()
  "Show the timeline tab in the current Courier response buffer."
  (interactive)
  (courier--set-response-tab 'timeline))

;;;###autoload
(defun courier-response-show-tests ()
  "Show the tests tab in the current Courier response buffer."
  (interactive)
  (courier--set-response-tab 'tests))

;;;###autoload
(defun courier-response-next-tab ()
  "Move to the next Courier response tab."
  (interactive)
  (unless courier--response
    (user-error "No Courier response is available"))
  (let* ((tabs courier--response-tabs)
         (current (or courier--response-tab 'response))
         (position (or (cl-position current tabs) 0))
         (next (nth (mod (1+ position) (length tabs)) tabs)))
    (courier--set-response-tab next)))

;;;###autoload
(defun courier-response-prev-tab ()
  "Move to the previous Courier response tab."
  (interactive)
  (unless courier--response
    (user-error "No Courier response is available"))
  (let* ((tabs courier--response-tabs)
         (current (or courier--response-tab 'response))
         (position (or (cl-position current tabs) 0))
         (prev (nth (mod (1- position) (length tabs)) tabs)))
    (courier--set-response-tab prev)))

(defun courier--timeline-index-at-point ()
  "Return the timeline history index stored at point, or nil."
  (or (button-get (button-at (point)) 'courier-history-index)
      (get-text-property (point) 'courier-history-index)
      (and (> (point) (point-min))
           (button-get (button-at (1- (point))) 'courier-history-index))
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'courier-history-index))
      (save-excursion
        (let ((start (line-beginning-position))
              (end (min (point-max) (1+ (line-end-position))))
              found)
          (while (and (< start end)
                      (not found))
            (setq found
                  (or (button-get (button-at start) 'courier-history-index)
                      (get-text-property start 'courier-history-index)))
            (setq start (1+ start)))
          found))))

(defun courier--timeline-tab-at-point ()
  "Return the timeline detail tab stored at point, or nil."
  (or (button-get (button-at (point)) 'courier-timeline-tab)
      (get-text-property (point) 'courier-timeline-tab)
      (save-excursion
        (let ((start (line-beginning-position))
              (end (min (point-max) (1+ (line-end-position))))
              found)
          (while (and (< start end)
                      (not found))
            (setq found
                  (or (button-get (button-at start) 'courier-timeline-tab)
                      (get-text-property start 'courier-timeline-tab)))
            (setq start (1+ start)))
          found))))

(defun courier--timeline-action-at-point ()
  "Return the timeline action at point, or nil.

The return value is one of:
- `(section . TAB)` for a timeline detail section heading
- `(history . INDEX)` for a timeline history entry"
  (cond
   ((and (eq courier--response-tab 'timeline)
         (courier--timeline-tab-at-point))
    (cons 'section (courier--timeline-tab-at-point)))
   ((and (eq courier--response-tab 'timeline)
         (numberp (courier--timeline-index-at-point)))
    (cons 'history (courier--timeline-index-at-point)))))

(defun courier--goto-button-matching (predicate)
  "Move point to the first button for which PREDICATE returns non-nil."
  (goto-char (point-min))
  (let ((button (next-button (point-min) t)))
    (while (and button
                (not (funcall predicate button)))
      (setq button (next-button (button-end button) t)))
    (when button
      (goto-char (button-start button))
      button)))

(defun courier--goto-timeline-history-button (index)
  "Move point to the timeline history button for INDEX."
  (courier--goto-button-matching
   (lambda (button)
     (equal (button-get button 'courier-history-index) index))))

(defun courier--goto-timeline-section-button (tab)
  "Move point to the timeline section button for TAB."
  (courier--goto-button-matching
   (lambda (button)
     (eq (button-get button 'courier-timeline-tab) tab))))

(defun courier--select-history-index (index)
  "Select history INDEX in the current response buffer."
  (unless courier--history
    (user-error "No response history"))
  (if (and (numberp courier--history-index)
           (= courier--history-index index))
      (setq courier--history-index nil)
    (setq courier--history-index index)
    (setq courier--timeline-expanded-sections '(request)))
  (courier--history-render-current)
  (courier--goto-timeline-history-button index))

;;;###autoload
(defun courier-response-clear-timeline ()
  "Clear the Courier timeline history for the current response buffer."
  (interactive)
  (unless courier--history
    (user-error "No response history"))
  (setq courier--history nil)
  (setq courier--history-index nil)
  (courier--refresh-response-display))

;;;###autoload
(defun courier-response-activate ()
  "Activate the response UI element at point."
  (interactive)
  (pcase (courier--timeline-action-at-point)
    (`(section . ,tab)
     (courier--toggle-timeline-section tab))
    (`(history . ,index)
     (courier--select-history-index index))
    (_
     (if-let* ((button (button-at (point))))
         (push-button button)
       (courier-response-open-body)))))

;;;###autoload
(defun courier-response-context-tab ()
  "Perform the most useful tab action in the current Courier response buffer."
  (interactive)
  (cond
   ((courier--timeline-action-at-point)
    (courier-response-activate))
   ((button-at (point))
    (push-button (button-at (point))))
   ((derived-mode-p 'courier--response-mode)
    (condition-case nil
        (forward-button 1 t t)
      (error
       (courier-response-activate))))
   (t
    (courier-response-activate))))

;;;###autoload
(defun courier-response-open-body ()
  "Open the current Courier response body in a dedicated viewer buffer."
  (interactive)
  (unless courier--response
    (user-error "No Courier response is available"))
  (let* ((response courier--response)
         (request courier--request)
         (view (courier--effective-body-view response))
         (buffer (get-buffer-create (courier--body-viewer-buffer-name)))
         (body-file (plist-get response :body-file)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase view
          ('image
           (special-mode)
           (when (and body-file
                      (display-images-p)
                      (ignore-errors (create-image body-file)))
             (insert-image (create-image body-file)))
           (when body-file
             (insert (format "\n\n%s\n" body-file))))
          (_
           (setq-local courier--response response)
           (setq-local courier--request request)
           (setq-local courier--body-view view)
           (insert (courier--format-body response))
           (funcall (courier--body-view-major-mode view))
           (setq buffer-read-only t)))))
    (display-buffer buffer)))

(define-key courier--response-mode-map (kbd "RET") #'courier-response-activate)
(define-key courier--response-mode-map (kbd "<return>") #'courier-response-activate)
(define-key courier--response-mode-map (kbd "TAB") #'courier-response-context-tab)
(define-key courier--response-mode-map (kbd "<tab>") #'courier-response-context-tab)
(define-key courier--response-mode-map (kbd "C-c ?") #'courier-dispatch)
(define-key courier--response-mode-map (kbd "h") #'courier-response-show-headers)
(define-key courier--response-mode-map (kbd "l") #'courier-response-show-timeline)
(define-key courier--response-mode-map (kbd "o") #'courier-response-open-body)
(define-key courier--response-mode-map (kbd "n") #'courier-response-next-tab)
(define-key courier--response-mode-map (kbd "p") #'courier-response-prev-tab)
(define-key courier--response-mode-map (kbd "r") #'courier-response-show-response)
(define-key courier--response-mode-map (kbd "g") #'courier-response-retry)
(define-key courier--response-mode-map (kbd "t") #'courier-response-show-tests)
(define-key courier--response-mode-map (kbd "v") #'courier-response-set-view)
(define-key courier--response-mode-map (kbd "V") #'courier-response-toggle-pretty)
(define-key courier--response-mode-map (kbd "C-c C-k") #'courier-response-cancel)

;;;; Request mode

(defvar-local courier--request-path nil
  "Path associated with the current Courier request buffer.")

(defvar-local courier--active-env nil
  "Name of the active Courier environment for the current request buffer.")

(defvar-local courier--collection-root-hint nil
  "Suggested collection root for first saving the current request buffer.")

(defvar-local courier--method-overlay nil
  "Overlay highlighting the current Courier request method.")

(defface courier-request-method-get-face
  '((t :inherit success :weight bold))
  "Face used for GET request methods in Courier.")

(defface courier-request-method-post-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for POST request methods in Courier.")

(defface courier-request-method-write-face
  '((t :inherit warning :weight bold))
  "Face used for mutating Courier request methods.")

(defface courier-request-method-default-face
  '((t :inherit font-lock-keyword-face :weight bold :underline t))
  "Fallback face used for Courier request method overlays.")

(defconst courier-request-font-lock-keywords
  '(("^\\(#\\)\\s-*\\(@[[:alnum:]-]+\\)\\(?:\\s-+\\(.*\\)\\)?$"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-keyword-face)
     (3 font-lock-string-face nil t))
    ("^\\([A-Z]+\\)\\s-+\\(\\S-+.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^\\([^:# \t][^:]*\\):\\s-*\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face))
    ("{{[^{}[:space:]]+}}"
     . font-lock-variable-name-face))
  "Font-lock keywords for `courier-request-mode'.")

(defun courier--mode-line-lighter ()
  "Return the mode-line lighter for `courier-request-mode'."
  (if courier--active-env
      (format "Courier[%s]" courier--active-env)
    "Courier"))

(defun courier--preview-buffer ()
  "Return the Courier preview buffer."
  (get-buffer-create "*courier-preview*"))

(defun courier--buffer-start-directory ()
  "Return the current Courier search start directory."
  (expand-file-name
   (if buffer-file-name
       (file-name-directory buffer-file-name)
     default-directory)))

(defun courier--collection-root (&optional start)
  "Return the Courier collection root for START, or nil.
START defaults to the current buffer directory."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or start
                                          (courier--buffer-start-directory))))))
    (locate-dominating-file directory courier--collection-marker-file)))

(defun courier--active-collection-root ()
  "Return the active Courier collection root for the current context, or nil."
  (or (and (derived-mode-p 'courier-overview-mode)
           courier--overview-collection-root)
      courier--collection-root-hint
      (courier--collection-root)))

(defun courier--collection-config-path (&optional start)
  "Return the Courier config path for START, or nil."
  (when-let* ((root (courier--collection-root start)))
    (expand-file-name courier--collection-marker-file root)))

(defun courier--collection-config (&optional start)
  "Return Courier collection config for START, or nil.
The result is a plist with `:root', `:path', `:name', `:requests-dir',
`:env-dir', and `:default-env'."
  (when-let* ((path (courier--collection-config-path start))
              (root (file-name-directory path)))
    (let* ((data (courier--read-json-file path))
           (name (courier--json-string-field data 'name))
           (requests-dir (courier--json-string-field data 'requestsDir "requests"))
           (env-dir (courier--json-string-field data 'envDir "env"))
           (default-env (courier--json-string-field data 'defaultEnv)))
      (list :root (file-name-as-directory root)
            :path path
            :name name
            :requests-dir requests-dir
            :env-dir env-dir
            :default-env default-env))))

(defun courier--collection-requests-root (&optional start)
  "Return the collection request root for START, or nil."
  (when-let* ((config (courier--collection-config start)))
    (let* ((root (plist-get config :root))
           (requests-root (expand-file-name (plist-get config :requests-dir) root)))
      (if (file-directory-p requests-root)
          (file-name-as-directory requests-root)
        root))))

(defun courier--preferred-requests-root (&optional start)
  "Return the preferred request root for START.
Unlike `courier--collection-requests-root', this keeps the configured requests
directory even when it does not exist yet."
  (if-let* ((config (courier--collection-config start)))
      (expand-file-name (plist-get config :requests-dir)
                        (plist-get config :root))
    nil))

(defun courier--request-search-root ()
  "Return the root directory used to discover Courier request files."
  (when-let* ((root (courier--active-collection-root)))
    (courier--collection-requests-root root)))

(defun courier--request-files (root)
  "Return Courier request files found under ROOT."
  (if (file-directory-p root)
      (sort (directory-files-recursively root "\\.http\\'") #'string<)
    nil))

(defun courier--request-buffer-for-path (path)
  "Return the live request buffer visiting PATH, or nil."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and buffer-file-name
            (equal (expand-file-name buffer-file-name) path)
            (derived-mode-p 'courier-request-mode))))
   (buffer-list)))

(defun courier--context-request-path ()
  "Return the Courier request path for the current context, or nil."
  (cond
   ((derived-mode-p 'courier-overview-mode)
    (courier--overview-request-path-at-point))
   ((and (derived-mode-p 'courier-request-mode)
         courier--request-path)
    courier--request-path)
   (t
    nil)))

(defun courier--management-root ()
  "Return the root directory used for Courier management commands."
  (cond
   ((derived-mode-p 'courier-overview-mode)
    courier--overview-root)
   ((courier--collection-root)
    (courier--preferred-requests-root))
   (t
    nil)))

(defun courier--management-directory ()
  "Return the directory used for Courier creation commands."
  (or (when-let* ((path (courier--context-request-path)))
        (file-name-directory path))
      (courier--management-root)))

(defun courier--slugify-name (name)
  "Return a filesystem-friendly slug for NAME."
  (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                            (downcase (string-trim name))))

(defun courier--request-file-name (name)
  "Return a Courier request filename for NAME."
  (concat (courier--slugify-name name) ".http"))

(defun courier--buffer-request-name ()
  "Return the current buffer request name, or a default untitled name."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^# @name\\s-+\\(.+\\)\\s-*$" nil t)
        (string-trim (match-string-no-properties 1))
      "Untitled")))

(defun courier--next-untitled-request-name ()
  "Return the next untitled request display name."
  (setq courier--untitled-request-counter
        (1+ courier--untitled-request-counter))
  (format "Untitled %d" courier--untitled-request-counter))

(defun courier--draft-buffer-name (name)
  "Return a draft buffer name for request NAME."
  (format "*courier-request: %s*" name))

(defun courier--request-skeleton (name)
  "Return initial draft text for request NAME."
  (concat "# @name " name "\n"
          courier-default-request-method " \n"
          "Accept: application/json\n"))

(defun courier--unique-request-path (directory name)
  "Return a unique request path for NAME under DIRECTORY."
  (let* ((slug (courier--slugify-name name))
         (base (if (string-empty-p slug) "untitled" slug))
         (index 1)
         candidate)
    (while
        (progn
          (setq candidate
                (expand-file-name
                 (format "%s%s.http"
                         base
                         (if (= index 1)
                             ""
                           (format "-%d" index)))
                 directory))
          (setq index (1+ index))
          (file-exists-p candidate)))
    candidate))

(defun courier--collection-template (root)
  "Return JSON text for a new Courier collection rooted at ROOT."
  (let ((json-encoding-pretty-print t))
    (concat
     (json-serialize
      `((name . ,(file-name-nondirectory
                  (directory-file-name (file-name-as-directory root))))
        (requestsDir . "requests")
        (envDir . "env")))
     "\n")))

(defun courier--create-collection (root)
  "Create a new Courier collection rooted at ROOT and return it."
  (let* ((collection-root (file-name-as-directory (expand-file-name root)))
         (existing-root (courier--collection-root collection-root))
         (config-path (expand-file-name courier--collection-marker-file collection-root)))
    (when (and existing-root
               (not (file-equal-p existing-root collection-root)))
      (user-error "Directory %s is already inside Courier collection %s"
                  collection-root existing-root))
    (when (file-exists-p config-path)
      (user-error "Courier collection already exists at %s" collection-root))
    (make-directory collection-root t)
    (make-directory (expand-file-name "requests" collection-root) t)
    (make-directory (expand-file-name "env" collection-root) t)
    (with-temp-file config-path
      (insert (courier--collection-template collection-root)))
    collection-root))

(defun courier--save-collection-prompt-directory ()
  "Return the initial directory used for Courier collection prompts."
  (or courier--collection-root-hint
      (courier--collection-root)
      courier-default-collection-directory
      default-directory))

(defun courier--read-save-collection-root ()
  "Prompt for the collection used to save the current request buffer."
  (let* ((initial (courier--save-collection-prompt-directory))
         (selected (file-name-as-directory
                    (expand-file-name
                     (read-directory-name "Save request to collection: "
                                          initial nil nil)))))
    (or (courier--collection-root selected)
        (when (y-or-n-p
               (format "No Courier collection at %s. Create one? "
                       (abbreviate-file-name selected)))
          (courier--create-collection selected))
        (user-error "No Courier collection selected"))))

(defun courier--request-display-name (path)
  "Return the display name for request PATH."
  (let ((request (if-let* ((buffer (courier--request-buffer-for-path path)))
                     (with-current-buffer buffer
                       (courier-parse-buffer))
                   (courier-parse-file path))))
    (or (plist-get request :name)
        (file-name-base path))))

(defun courier--ensure-directory-under-root (directory root)
  "Validate DIRECTORY against ROOT and create it when missing."
  (let ((expanded-directory (file-name-as-directory (expand-file-name directory)))
        (expanded-root (and root (file-name-as-directory (expand-file-name root)))))
    (when (and expanded-root
               (not (string-prefix-p expanded-root expanded-directory)))
      (user-error "Directory %s is outside Courier root %s"
                  expanded-directory expanded-root))
    (make-directory expanded-directory t)
    expanded-directory))

(defun courier--write-request-name (buffer name)
  "Ensure BUFFER contains a `# @name' directive set to NAME."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^# @name\\(?:\\s-+.*\\)?$" nil t)
          (replace-match (format "# @name %s" name) t t)
        (goto-char (point-min))
        (insert (format "# @name %s\n" name))))))

(defun courier--migrate-request-artifacts (old-path new-path &optional new-name)
  "Move Courier state from OLD-PATH to NEW-PATH.
When NEW-NAME is non-nil, also update any live response buffer name metadata."
  (when-let* ((metadata (courier--request-metadata old-path)))
    (remhash old-path courier--request-metadata)
    (puthash new-path metadata courier--request-metadata))
  (when-let* ((response-buffer (courier--response-buffer-for-path old-path)))
    (when (buffer-live-p response-buffer)
      (with-current-buffer response-buffer
        (plist-put courier--request :path new-path)
        (when new-name
          (plist-put courier--request :name new-name))
        (rename-buffer (courier--response-buffer-name courier--request) t)))))

(defun courier--request-candidate (path root)
  "Return a completion candidate for request PATH under ROOT."
  (let* ((request (courier-parse-file path))
         (method (or (plist-get request :method) ""))
         (name (or (plist-get request :name)
                   (file-name-base path)))
         (relative-path (file-relative-name path root))
         (label (format "%-7s %-24s %s" method name relative-path)))
    (cons (propertize label 'courier-group "Requests")
          (list :kind 'request
                :path path))))

(defun courier--request-candidates (root)
  "Return completion candidates for Courier request files under ROOT."
  (mapcar (lambda (path)
            (courier--request-candidate path root))
          (courier--request-files root)))

(defun courier--env-sort-rank (name)
  "Return a stable sort rank for environment NAME."
  (cond
   ((and courier--active-env
         (string= name courier--active-env))
    -1)
   ((string= name "local") 0)
   ((string= name "staging") 1)
   ((string= name "prod") 2)
   ((string= name "default") 3)
   (t 10)))

(defun courier--env-candidate (entry root)
  "Return a completion candidate for env ENTRY under ROOT."
  (let* ((name (car entry))
         (path (cdr entry))
         (marker (if (and courier--active-env
                          (string= name courier--active-env))
                     "*"
                   " "))
         (relative-path (file-relative-name path root))
         (label (format "ENV     %s %-22s %s" marker name relative-path)))
    (cons (propertize label 'courier-group "Environments")
          (list :kind 'env
                :name name
                :path path))))

(defun courier--env-candidates (entries root)
  "Return completion candidates for environment ENTRIES under ROOT."
  (let ((sorted-entries
         (sort (copy-sequence entries)
               (lambda (left right)
                 (let ((left-rank (courier--env-sort-rank (car left)))
                       (right-rank (courier--env-sort-rank (car right))))
                   (if (= left-rank right-rank)
                       (string< (car left) (car right))
                     (< left-rank right-rank)))))))
    (mapcar (lambda (entry)
              (courier--env-candidate entry root))
            sorted-entries)))

(defun courier--open-candidates ()
  "Return grouped completion candidates for requests and environments."
  (let* ((request-root (courier--request-search-root))
         (env-root (or (courier--collection-root)
                       request-root))
         (request-candidates (courier--request-candidates request-root))
         (env-candidates (courier--env-candidates
                          (or (courier--available-env-entries) nil)
                          env-root)))
    (append request-candidates env-candidates)))

(defun courier--completion-group (candidate transform)
  "Return the completion group title for CANDIDATE.
TRANSFORM is ignored; it is part of the completion metadata protocol."
  (if transform
      candidate
    (or (get-text-property 0 'courier-group candidate)
        "Other")))

(defun courier--set-active-env (name)
  "Set the current request buffer environment to NAME."
  (setq courier--active-env name)
  (force-mode-line-update)
  (courier--refresh-overview-buffers)
  (message "Courier environment set to %s." courier--active-env))

(defun courier--handle-open-selection (selection candidates)
  "Handle SELECTION from CANDIDATES."
  (pcase (plist-get (cdr (assoc selection candidates)) :kind)
    ('request
     (find-file (plist-get (cdr (assoc selection candidates)) :path)))
    ('env
     (unless (derived-mode-p 'courier-request-mode)
       (user-error "Environment selection requires a Courier request buffer"))
     (courier--set-active-env
      (plist-get (cdr (assoc selection candidates)) :name)))
    (_
     (user-error "Unknown Courier selection: %s" selection))))

(defun courier--collection-env-dir (&optional start)
  "Return the collection env directory for START, or nil."
  (when-let* ((config (courier--collection-config start)))
    (expand-file-name (plist-get config :env-dir)
                      (plist-get config :root))))

(defun courier--collection-env-entries (&optional start)
  "Return collection-scoped environment entries for START.
Entries are read from the configured env directory only."
  (when-let* ((env-dir (courier--collection-env-dir start))
              ((file-directory-p env-dir)))
    (let (entries)
      (dolist (file (directory-files env-dir nil nil t))
        (when (courier--env-file-name-p file)
          (setq entries
                (append entries
                        (list (cons (courier--env-name-for-file file)
                                    (expand-file-name file env-dir)))))))
      (sort entries
            (lambda (left right)
              (string< (format "%s:%s" (car left) (cdr left))
                       (format "%s:%s" (car right) (cdr right))))))))

(defun courier--available-env-entries ()
  "Return available environment entries for the current request buffer."
  (when-let* ((path (or courier--request-path
                        (and buffer-file-name (expand-file-name buffer-file-name))))
              (directory (file-name-directory path)))
    (when (courier--collection-root directory)
      (courier--collection-env-entries directory))))

(defun courier--available-env-names (entries)
  "Return unique environment names from ENTRIES."
  (delete-dups (mapcar #'car entries)))

(defun courier--read-env-name (names)
  "Prompt for an environment name from NAMES."
  (completing-read "Environment: " names nil t nil nil courier--active-env))

(defun courier--preferred-env-name (entries)
  "Return the preferred environment name from ENTRIES, or nil."
  (let* ((names (courier--available-env-names entries))
         (default-env (plist-get (courier--collection-config) :default-env)))
    (cond
     ((and default-env
           (member default-env names))
      default-env)
     ((= (length names) 1)
      (car names))
     (t
      nil))))

(defun courier--selected-env-name (entries)
  "Return the selected environment name from ENTRIES."
  (let ((names (courier--available-env-names entries)))
    (cond
     ((null names) nil)
     ((and courier--active-env
           (member courier--active-env names))
      courier--active-env)
     ((courier--preferred-env-name entries)
      (setq courier--active-env (courier--preferred-env-name entries))
      (force-mode-line-update)
      courier--active-env)
     (t
      (setq courier--active-env (courier--read-env-name names))
      (force-mode-line-update)
      courier--active-env))))

(defun courier--env-vars-for-name (entries env-name)
  "Return merged environment variables from ENTRIES for ENV-NAME."
  (let ((merged nil))
    (dolist (entry entries)
      (when (string= (car entry) env-name)
        (setq merged
              (courier-merge-vars merged
                                  (courier-parse-env-file (cdr entry))))))
    merged))

(defun courier--current-env-vars ()
  "Return merged environment variables for the current request buffer."
  (let* ((entries (courier--available-env-entries))
         (env-name (courier--selected-env-name entries)))
    (if env-name
        (courier--env-vars-for-name entries env-name)
      nil)))

(defun courier--current-env-selection ()
  "Return the current environment selection as `(ENV-NAME . VARS)'."
  (let* ((entries (courier--available-env-entries))
         (env-name (courier--selected-env-name entries)))
    (cons env-name
          (if env-name
              (courier--env-vars-for-name entries env-name)
            nil))))

(defun courier--prepared-current-request ()
  "Return the current request after env selection and pre-request scripts."
  (let* ((parsed-request (courier-parse-buffer))
         (env-selection (courier--current-env-selection))
         (env-name (car env-selection))
         (env-vars (cdr env-selection))
         (request (courier--run-pre-request-script parsed-request env-vars)))
    (plist-put request :env-name env-name)
    (plist-put request :env-vars env-vars)
    request))

(defun courier--resolved-current-request ()
  "Return the validated and resolved request for the current buffer."
  (let* ((request (courier--prepared-current-request))
         (env-name (plist-get request :env-name))
         (env-vars (plist-get request :env-vars))
         (_validated (courier-validate-request request))
         (resolved (courier-resolve-request request env-vars)))
    (plist-put resolved :env-name env-name)
    (plist-put resolved :env-vars env-vars)))

(defun courier--export-source-url (request)
  "Return REQUEST URL with raw query params appended for export."
  (let ((base-url (plist-get request :url))
        (params (plist-get request :params)))
    (if params
        (let* ((fragment-pos (string-match "#" base-url))
               (fragment (and fragment-pos (substring base-url fragment-pos)))
               (url-no-fragment (if fragment-pos
                                    (substring base-url 0 fragment-pos)
                                  base-url))
               (separator (if (string-match-p "\\?" url-no-fragment) "&" "?"))
               (query (mapconcat (lambda (param)
                                   (format "%s=%s" (car param) (cdr param)))
                                 params "&")))
          (concat url-no-fragment separator query fragment))
      base-url)))

(defun courier--export-source-auth-header (auth)
  "Return a raw auth header cons cell for AUTH, or nil."
  (when auth
    (pcase (plist-get auth :type)
      ('bearer
       (cons "authorization"
             (format "Bearer %s" (plist-get auth :token))))
      ('basic
       (cons "authorization"
             (format "Basic %s:%s"
                     (plist-get auth :username)
                     (plist-get auth :password))))
      ('header
       (cons (downcase (plist-get auth :name))
             (plist-get auth :value))))))

(defun courier--request-for-export (request)
  "Return the current REQUEST prepared for export."
  (courier-validate-request request)
  (if courier--export-interpolate
      (courier-resolve-request request (plist-get request :env-vars))
    (let ((export (copy-tree request t)))
      (plist-put export :url (courier--export-source-url request))
      (when-let* ((auth-header
                   (courier--export-source-auth-header (plist-get request :auth))))
        (unless (assoc-string (car auth-header) (plist-get export :headers) nil)
          (plist-put export :headers
                     (append (plist-get export :headers)
                             (list auth-header)))))
      export)))

(defun courier--shell-quote (string)
  "Return STRING quoted for shell output."
  (shell-quote-argument (or string "")))

(defun courier--export-curl-command (request)
  "Return a curl command string for REQUEST."
  (let ((lines (list (format "curl --request %s" (plist-get request :method))
                     (format "  --url %s"
                             (courier--shell-quote (plist-get request :url))))))
    (dolist (header (plist-get request :headers))
      (setq lines
            (append lines
                    (list (format "  --header %s"
                                  (courier--shell-quote
                                   (format "%s: %s" (car header) (cdr header))))))))
    (unless (string-empty-p (or (plist-get request :body) ""))
      (setq lines
            (append lines
                    (list (format "  --data-raw %s"
                                  (courier--shell-quote
                                   (plist-get request :body)))))))
    (mapconcat #'identity lines " \\\n")))

(defun courier--export-httpie-command (request)
  "Return an httpie command string for REQUEST."
  (let ((lines (list (format "http %s %s"
                             (plist-get request :method)
                             (courier--shell-quote (plist-get request :url))))))
    (dolist (header (plist-get request :headers))
      (setq lines
            (append lines
                    (list (format "  %s"
                                  (courier--shell-quote
                                   (format "%s:%s" (car header) (cdr header))))))))
    (unless (string-empty-p (or (plist-get request :body) ""))
      (setq lines
            (append lines
                    (list (format "  <<< %s"
                                  (courier--shell-quote
                                   (plist-get request :body)))))))
    (mapconcat #'identity lines " \\\n")))

(defun courier--export-wget-command (request)
  "Return a wget command string for REQUEST."
  (let ((lines (list "wget -O -"
                     (format "  --method=%s" (plist-get request :method)))))
    (dolist (header (plist-get request :headers))
      (setq lines
            (append lines
                    (list (format "  --header=%s"
                                  (courier--shell-quote
                                   (format "%s: %s" (car header) (cdr header))))))))
    (unless (string-empty-p (or (plist-get request :body) ""))
      (setq lines
            (append lines
                    (list (format "  --body-data=%s"
                                  (courier--shell-quote
                                   (plist-get request :body)))))))
    (setq lines
          (append lines
                  (list (courier--shell-quote (plist-get request :url)))))
    (mapconcat #'identity lines " \\\n")))

(defun courier--export-command-string (request)
  "Return an export command string for REQUEST using current export settings."
  (pcase courier--export-format
    ('curl (courier--export-curl-command request))
    ('httpie (courier--export-httpie-command request))
    ('wget (courier--export-wget-command request))
    (_ (user-error "Unsupported export format: %s" courier--export-format))))

(defun courier--current-export-command ()
  "Return the current request export command string."
  (courier--export-command-string
   (courier--request-for-export (courier--prepared-current-request))))

(defun courier--export-state-label ()
  "Return a short label for current export settings."
  (format "%s • %s"
          (upcase (symbol-name courier--export-format))
          (if courier--export-interpolate
              "interpolate"
            "source")))

(defun courier--response-buffer-for-path (path)
  "Return the Courier response buffer currently associated with PATH."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (boundp 'courier--request)
            courier--request
            (equal (plist-get courier--request :path) path))))
   (buffer-list)))

(defun courier--in-flight-process-for-path (path)
  "Return the in-flight Courier process associated with PATH."
  (when-let* ((buffer (courier--response-buffer-for-path path)))
    (with-current-buffer buffer
      (and (boundp 'courier--process)
           (processp courier--process)
           (process-live-p courier--process)
           courier--process))))

(defun courier--display-response-buffer (request)
  "Create or reuse the response buffer for REQUEST."
  (let ((buffer (get-buffer-create (courier--response-buffer-name request))))
    (display-buffer buffer '(display-buffer-pop-up-window))
    buffer))

(defun courier--preview-request (request argv)
  "Render REQUEST preview with ARGV into the preview buffer."
  (let ((buffer (courier--preview-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Name:    %s\n" (or (plist-get request :name) "(none)")))
        (insert (format "Method:  %s\n" (plist-get request :method)))
        (insert (format "URL:     %s\n\n" (plist-get request :url)))
        (insert "Headers:\n")
        (if-let* ((headers (plist-get request :headers)))
            (dolist (header headers)
              (insert (format "  %s: %s\n" (car header) (cdr header))))
          (insert "  (none)\n"))
        (insert "\nBody:\n")
        (if (string-empty-p (or (plist-get request :body) ""))
            (insert "  (empty)\n")
          (insert (plist-get request :body) "\n"))
        (insert "\nCurl argv:\n")
        (dolist (arg argv)
          (insert (format "  %s\n" arg)))
        (special-mode)))
    (display-buffer buffer)))

(defun courier--goto-request-line ()
  "Move point to the first Courier request-line candidate and return non-nil."
  (goto-char (point-min))
  (while (and (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (or (string-empty-p line)
                    (string-prefix-p "#" line))))
    (forward-line 1))
  (not (eobp)))

(defun courier--current-method ()
  "Return the current request method from the buffer, or nil."
  (save-excursion
    (when (courier--goto-request-line)
      (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
        (match-string-no-properties 1)))))

(defun courier--method-overlay-face (method)
  "Return the overlay face used for request METHOD."
  (cond
   ((equal method "GET") 'courier-request-method-get-face)
   ((equal method "POST") 'courier-request-method-post-face)
   ((member method '("PUT" "PATCH" "DELETE")) 'courier-request-method-write-face)
   (t 'courier-request-method-default-face)))

(defun courier--current-method-bounds ()
  "Return `(START . END)' for the current request method token, or nil."
  (save-excursion
    (when (courier--goto-request-line)
      (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
        (cons (match-beginning 1) (match-end 1))))))

(defun courier--after-change-refresh-method-overlay (&rest _args)
  "Refresh the Courier request method overlay after buffer changes."
  (when (derived-mode-p 'courier-request-mode)
    (courier--refresh-method-overlay)))

(defun courier--delete-method-overlay ()
  "Delete the current Courier request method overlay."
  (when (overlayp courier--method-overlay)
    (delete-overlay courier--method-overlay)
    (setq courier--method-overlay nil)))

(defun courier--refresh-method-overlay ()
  "Refresh the Courier request method overlay in the current buffer."
  (if-let* ((bounds (courier--current-method-bounds))
            (method (courier--current-method)))
      (let ((overlay (or courier--method-overlay
                         (make-overlay (car bounds) (cdr bounds) nil t t))))
        (setq courier--method-overlay overlay)
        (move-overlay overlay (car bounds) (cdr bounds))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority 1000)
        (overlay-put overlay 'face (courier--method-overlay-face method))
        (overlay-put overlay 'help-echo (format "Courier method: %s" method)))
    (courier--delete-method-overlay)))

(defun courier--directive-insertion-point ()
  "Return the buffer position where Courier directives should be inserted."
  (save-excursion
    (if (courier--goto-request-line)
        (line-beginning-position)
      (point-max))))

(defun courier--insert-directive-text (text)
  "Insert Courier directive TEXT before the request line."
  (goto-char (courier--directive-insertion-point))
  (insert text))

(defun courier--goto-directive (regexp label)
  "Move point to the first directive matching REGEXP, described by LABEL."
  (let ((origin (point)))
    (goto-char (point-min))
    (unless (re-search-forward regexp nil t)
      (goto-char origin)
      (user-error "No Courier %s in this request" label))
    (push-mark origin t)
    (beginning-of-line)
    (message "Jumped to Courier %s." label)))

;;;###autoload
(define-derived-mode courier-request-mode text-mode "Courier"
  "Major mode for editing Courier request files."
  (setq-local mode-name '(:eval (courier--mode-line-lighter)))
  (setq-local font-lock-defaults '(courier-request-font-lock-keywords))
  (setq-local outline-regexp "^# @\\|^[A-Z]+ ")
  (setq-local courier--request-path (and buffer-file-name (expand-file-name buffer-file-name)))
  (setq-local courier--collection-root-hint
              (or courier--collection-root-hint
                  (courier--collection-root)))
  (add-hook 'after-change-functions
            #'courier--after-change-refresh-method-overlay nil t)
  (add-hook 'change-major-mode-hook #'courier--delete-method-overlay nil t)
  (courier--refresh-method-overlay)
  (unless courier--active-env
    (when-let* ((preferred-env (courier--preferred-env-name
                                (courier--available-env-entries))))
      (setq-local courier--active-env preferred-env))))

;;;###autoload
(defun courier-request-validate ()
  "Parse and validate the current Courier request buffer."
  (interactive)
  (let ((request (courier-parse-buffer)))
    (courier-validate-request request)
    (message "Courier request is valid.")))

;;;###autoload
(defun courier-request-preview ()
  "Preview the resolved Courier request in a read-only buffer."
  (interactive)
  (let* ((resolved (courier--resolved-current-request))
         (header-file (make-temp-file "courier-preview-" nil ".headers"))
         (body-file (make-temp-file "courier-preview-" nil ".body"))
         (meta-file (make-temp-file "courier-preview-" nil ".meta")))
    (unwind-protect
        (courier--preview-request
         resolved
         (courier-build-curl-command resolved header-file body-file meta-file))
      (dolist (path (list header-file body-file meta-file))
        (when (file-exists-p path)
          (delete-file path))))))

;;;###autoload
(defun courier-request-switch-env ()
  "Select the active Courier environment for the current request buffer."
  (interactive)
  (let* ((entries (courier--available-env-entries))
         (names (courier--available-env-names entries)))
    (unless names
      (user-error "No Courier environment files found"))
    (courier--set-active-env (courier--read-env-name names))))

;;;###autoload
(defun courier-request-set-method (method)
  "Set the current request line HTTP METHOD."
  (interactive
   (list
    (completing-read "Method: "
                     courier--allowed-methods
                     nil t nil nil
                     (courier--current-method))))
  (save-excursion
    (unless (courier--goto-request-line)
      (user-error "No Courier request in this buffer"))
    (beginning-of-line)
    (cond
     ((looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
      (replace-match method t t nil 1))
     (t
      (insert method " "))))
  (courier--refresh-overview-buffers)
  (message "Courier method set to %s." method))

;;;###autoload
(defun courier-request-export-set-format (format)
  "Set the current request export FORMAT."
  (interactive
   (list
    (intern
     (completing-read "Export format: "
                      (mapcar #'symbol-name courier--export-formats)
                      nil t nil nil
                      (symbol-name courier--export-format)))))
  (unless (memq format courier--export-formats)
    (user-error "Unsupported export format: %s" format))
  (setq courier--export-format format)
  (message "Courier export format: %s" (symbol-name format)))

;;;###autoload
(defun courier-request-export-toggle-interpolation ()
  "Toggle variable interpolation for Courier export."
  (interactive)
  (setq courier--export-interpolate (not courier--export-interpolate))
  (message "Courier export interpolation %s"
           (if courier--export-interpolate
               "enabled"
             "disabled")))

;;;###autoload
(defun courier-request-export-copy ()
  "Copy the current request export command to the kill ring."
  (interactive)
  (let ((command (courier--current-export-command)))
    (kill-new command)
    (message "Copied Courier export: %s" (courier--export-state-label))))

;;;###autoload
(defun courier-request-insert-param (name value)
  "Insert a Courier query parameter directive with NAME and VALUE."
  (interactive
   (list (read-string "Param name: ")
         (read-string "Param value: ")))
  (when (string-empty-p name)
    (user-error "Courier param name cannot be empty"))
  (courier--insert-directive-text
   (format "# @param %s %s\n" name value)))

;;;###autoload
(defun courier-request-insert-auth (kind &optional first second)
  "Insert a Courier auth directive of KIND using FIRST and SECOND values.

For `bearer', FIRST is the token.
For `basic', FIRST is the username and SECOND is the password.
For `header', FIRST is the header name and SECOND is the header value."
  (interactive
   (let ((kind
          (intern
           (completing-read "Auth kind: "
                            '("bearer" "basic" "header")
                            nil t))))
     (pcase kind
       ('bearer
        (list kind (read-string "Bearer token: " "{{token}}")))
       ('basic
        (list kind
              (read-string "Basic username: " "{{user}}")
              (read-string "Basic password: " "{{password}}")))
       ('header
        (list kind
              (read-string "Header name: " "X-API-Key")
              (read-string "Header value: " "{{token}}"))))))
  (let ((directive
         (pcase kind
           ('bearer
            (format "# @auth bearer %s\n" (or first "{{token}}")))
           ('basic
            (format "# @auth basic %s %s\n"
                    (or first "{{user}}")
                    (or second "{{password}}")))
           ('header
            (format "# @auth header %s %s\n"
                    (or first "X-API-Key")
                    (or second "{{token}}")))
           (_
            (user-error "Unsupported auth kind: %s" kind)))))
    (courier--insert-directive-text directive)))

(defun courier--insert-script-block (kind)
  "Insert a Courier script block of KIND before the request line."
  (let ((start (courier--directive-insertion-point)))
    (goto-char start)
    (insert (format "# @begin %s\n# \n# @end\n" kind))
    (goto-char start)
    (forward-line 1)
    (end-of-line)))

;;;###autoload
(defun courier-request-insert-pre-request-script ()
  "Insert a Courier pre-request script block."
  (interactive)
  (courier--insert-script-block "pre-request"))

;;;###autoload
(defun courier-request-insert-post-response-script ()
  "Insert a Courier post-response script block."
  (interactive)
  (courier--insert-script-block "post-response"))

;;;###autoload
(defun courier-request-goto-auth ()
  "Jump to the first Courier auth directive in the current request."
  (interactive)
  (courier--goto-directive "^#\\s-*@auth\\b" "auth directive"))

;;;###autoload
(defun courier-request-goto-param ()
  "Jump to the first Courier query parameter directive in the current request."
  (interactive)
  (courier--goto-directive "^#\\s-*@param\\b" "param directive"))

;;;###autoload
(defun courier-request-goto-pre-request-script ()
  "Jump to the first Courier pre-request script block."
  (interactive)
  (courier--goto-directive "^#\\s-*@begin\\s-+pre-request\\b"
                           "pre-request script"))

;;;###autoload
(defun courier-request-goto-post-response-script ()
  "Jump to the first Courier post-response script block."
  (interactive)
  (courier--goto-directive "^#\\s-*@begin\\s-+post-response\\b"
                           "post-response script"))

;;;###autoload
(defun courier-request-save-buffer ()
  "Save the current Courier request buffer.
Unsaved request buffers choose a collection on first save."
  (interactive)
  (if buffer-file-name
      (save-buffer)
    (let* ((collection-root (courier--read-save-collection-root))
           (request-root (courier--preferred-requests-root collection-root))
           (request-name (courier--buffer-request-name))
           (path (courier--unique-request-path request-root request-name)))
      (make-directory request-root t)
      (set-visited-file-name path)
      (setq-local default-directory (file-name-directory path))
      (setq-local courier--request-path path)
      (setq-local courier--collection-root-hint collection-root)
      (save-buffer)
      (courier--refresh-overview-buffers)
      (message "Courier request saved to %s" path))))

;;;###autoload
(defun courier-request-send ()
  "Send the current Courier request buffer with curl."
  (interactive)
  (let* ((resolved (courier--resolved-current-request))
         (path (plist-get resolved :path)))
    (when-let* ((process (courier--in-flight-process-for-path path)))
      (unless (yes-or-no-p "Request in progress. Cancel and resend? ")
        (user-error "Courier request aborted"))
      (courier-cancel-request process))
    (let ((response-buffer (courier--display-response-buffer resolved)))
      (with-current-buffer response-buffer
        (courier--response-show-sending resolved))
      (let ((process
             (courier-send-request
              resolved
              (lambda (response)
                (when (buffer-live-p response-buffer)
                  (with-current-buffer response-buffer
                    (setq courier--process nil
                          courier--response response)
                    (courier--render-response response resolved)))))))
        (process-put process 'courier-response-buffer response-buffer)
        (with-current-buffer response-buffer
          (setq courier--request resolved
                courier--process process
                courier--temp-files (process-get process 'courier-temp-files)))))))

;;;###autoload
(defun courier-open ()
  "Open another Courier request or switch environments."
  (interactive)
  (let ((request-root (or (courier--request-search-root)
                          (user-error "No Courier collection in current context")))
        (candidates (courier--open-candidates)))
    (unless candidates
      (user-error "No Courier requests or environments found"))
    (let ((completion-extra-properties
           '(:group-function courier--completion-group))
          (selection
           (completing-read
            (format "Courier open (%s): "
                    (abbreviate-file-name request-root))
            candidates nil t)))
      (courier--handle-open-selection selection candidates))))

;;;###autoload
(defun courier-new-request ()
  "Create a new unsaved Courier request draft."
  (interactive)
  (let* ((name (courier--next-untitled-request-name))
         (origin-root (or (and (derived-mode-p 'courier-overview-mode)
                               courier--overview-collection-root)
                          (courier--collection-root)))
         (buffer (generate-new-buffer (courier--draft-buffer-name name))))
    (with-current-buffer buffer
      (setq-local default-directory (or origin-root default-directory))
      (insert (courier--request-skeleton name))
      (courier-request-mode)
      (setq-local courier--collection-root-hint origin-root)
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line))
    (switch-to-buffer buffer)
    (message "Courier draft created. Save it into a collection with C-x C-s.")))

;;;###autoload
(defun courier-create-collection (directory)
  "Create a Courier collection rooted at DIRECTORY."
  (interactive
   (list (read-directory-name "Create Courier collection in: "
                              (courier--save-collection-prompt-directory)
                              nil nil)))
  (let ((root (courier--create-collection directory)))
    (message "Courier collection created at %s" root)
    root))

;;;###autoload
(defun courier-new-folder (name)
  "Create a Courier folder named NAME in the current management directory."
  (interactive (list (read-string "Folder name: ")))
  (let* ((base-directory (or (courier--management-directory)
                             (user-error "No Courier collection or request context")))
         (root (courier--management-root))
         (directory (courier--ensure-directory-under-root
                     (expand-file-name name base-directory)
                     root)))
    (courier--refresh-overview-buffers)
    (message "Courier folder created at %s" directory)))

;;;###autoload
(defun courier-rename-request (name)
  "Rename the current Courier request to NAME.
This renames the request file and updates its `# @name' directive."
  (interactive
   (let ((path (or (courier--context-request-path)
                   (user-error "No Courier request in current context"))))
     (list (read-string "Rename request to: "
                        (courier--request-display-name path)))))
  (let* ((old-path (or (courier--context-request-path)
                       (user-error "No Courier request in current context")))
         (directory (file-name-directory old-path))
         (new-path (expand-file-name (courier--request-file-name name) directory))
         (existing-buffer (courier--request-buffer-for-path old-path))
         (buffer (or existing-buffer
                     (find-file-noselect old-path)))
         (created-buffer-p (null existing-buffer)))
    (unless (and (equal old-path new-path)
                 (string= (courier--request-display-name old-path) name))
      (when (and (not (equal old-path new-path))
                 (file-exists-p new-path))
        (user-error "File already exists: %s" new-path))
      (with-current-buffer buffer
        (courier-request-mode)
        (courier--write-request-name buffer name)
        (unless (equal old-path new-path)
          (rename-file old-path new-path)
          (set-visited-file-name new-path t))
        (setq-local courier--request-path new-path)
        (save-buffer))
      (unless (equal old-path new-path)
        (courier--migrate-request-artifacts old-path new-path name))
      (when (and created-buffer-p (buffer-live-p buffer))
        (kill-buffer buffer))
      (courier--refresh-overview-buffers)
      (message "Courier request renamed to %s" name))))

;;;###autoload
(defun courier-move-request (directory)
  "Move the current Courier request into DIRECTORY."
  (interactive
   (let* ((path (or (courier--context-request-path)
                    (user-error "No Courier request in current context")))
          (root (courier--management-root))
          (current-directory (file-name-directory path)))
     (list
      (read-directory-name "Move request to directory: "
                           current-directory root nil current-directory))))
  (let* ((old-path (or (courier--context-request-path)
                       (user-error "No Courier request in current context")))
         (root (courier--management-root))
         (target-directory (courier--ensure-directory-under-root directory root))
         (new-path (expand-file-name (file-name-nondirectory old-path) target-directory))
         (existing-buffer (courier--request-buffer-for-path old-path))
         (buffer (or existing-buffer
                     (find-file-noselect old-path)))
         (created-buffer-p (null existing-buffer)))
    (when (and (not (equal old-path new-path))
               (file-exists-p new-path))
      (user-error "File already exists: %s" new-path))
    (unless (equal old-path new-path)
      (rename-file old-path new-path)
      (with-current-buffer buffer
        (courier-request-mode)
        (set-visited-file-name new-path t)
        (setq-local courier--request-path new-path)
        (save-buffer))
      (courier--migrate-request-artifacts old-path new-path)
      (when (and created-buffer-p (buffer-live-p buffer))
        (kill-buffer buffer))
      (courier--refresh-overview-buffers)
      (message "Courier request moved to %s" new-path))))

;;;###autoload
(transient-define-prefix courier-request-export-menu ()
  "Show export actions for the current Courier request buffer."
  [["State"
    ("f" "Set format" courier-request-export-set-format)
    ("i" "Toggle interpolate" courier-request-export-toggle-interpolation)]
   ["Action"
    ("y" "Copy command" courier-request-export-copy)]])

;;;###autoload
(transient-define-prefix courier-request-insert-menu ()
  "Show insert actions for the current Courier request buffer."
  [["Directive"
    ("p" "Param" courier-request-insert-param)
    ("a" "Auth" courier-request-insert-auth)]
   ["Script"
    ("r" "Pre-request" courier-request-insert-pre-request-script)
    ("R" "Post-response" courier-request-insert-post-response-script)]])

;;;###autoload
(transient-define-prefix courier-request-inspect-menu ()
  "Show inspect actions for the current Courier request buffer."
  [["Jump"
    ("p" "Params" courier-request-goto-param)
    ("a" "Auth" courier-request-goto-auth)
    ("r" "Pre-request" courier-request-goto-pre-request-script)
    ("R" "Post-response" courier-request-goto-post-response-script)]])

;;;###autoload
(transient-define-prefix courier-request-menu ()
  "Show request actions for the current Courier buffer."
  [["Run"
    ("c" "Send" courier-request-send)
    ("p" "Preview" courier-request-preview)
    ("l" "Validate" courier-request-validate)]
   ["Edit"
    ("m" "Method" courier-request-set-method)
    ("e" "Environment" courier-request-switch-env)
    ("s" "Save" courier-request-save-buffer)]
   ["Navigate"
    ("o" "Open" courier-open)
    ("b" "Overview" courier-overview)]
   ["Structure"
    ("I" "Insert..." courier-request-insert-menu)
    ("S" "Inspect..." courier-request-inspect-menu)]
   ["Manage"
    ("n" "New request" courier-new-request)
    ("r" "Rename" courier-rename-request)
    ("F" "New folder" courier-new-folder)
    ("C" "New collection" courier-create-collection)]
   ["Share"
    ("x" "Export..." courier-request-export-menu)]])

;;;###autoload
(transient-define-prefix courier-response-menu ()
  "Show response actions for the current Courier buffer."
  [["View"
    ("r" "Response" courier-response-show-response)
    ("h" "Headers" courier-response-show-headers)
    ("t" "Timeline" courier-response-show-timeline)
    ("T" "Tests" courier-response-show-tests)]
   ["Body"
    ("v" "Body view" courier-response-set-view)
    ("V" "Raw/auto" courier-response-toggle-pretty)
    ("o" "Open body" courier-response-open-body)]
   ["Run"
    ("g" "Retry" courier-response-retry)
    ("k" "Cancel" courier-response-cancel)]
   ["Navigate"
    ("n" "Next view" courier-response-next-tab)
    ("p" "Prev view" courier-response-prev-tab)]])

;;;###autoload
(transient-define-prefix courier-overview-menu ()
  "Show overview actions for the current Courier buffer."
  [["Request"
    ("o" "Open" courier-overview-open)
    ("s" "Send" courier-overview-send)
    ("p" "Preview" courier-overview-preview)
    ("e" "Environment" courier-overview-switch-env)]
   ["List"
    ("/" "Filter" courier-overview-set-filter)
    ("g" "Refresh" courier-overview-refresh)
    ("j" "Jump/Open" courier-open)]
   ["Manage"
    ("n" "New request" courier-new-request)
    ("F" "New folder" courier-new-folder)
    ("r" "Rename" courier-rename-request)
    ("m" "Move" courier-move-request)]])

;;;###autoload
(defun courier-dispatch ()
  "Show the Courier action menu for the current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'courier-request-mode)
    (transient-setup 'courier-request-menu))
   ((derived-mode-p 'courier--response-mode)
    (transient-setup 'courier-response-menu))
   ((derived-mode-p 'courier-overview-mode)
    (transient-setup 'courier-overview-menu))
   (t
    (user-error "No Courier action menu is available in this buffer"))))

(define-key courier-request-mode-map (kbd "C-c C-c") #'courier-request-send)
(define-key courier-request-mode-map (kbd "C-c C-b") #'courier-overview)
(define-key courier-request-mode-map (kbd "C-c ?") #'courier-dispatch)
(define-key courier-request-mode-map (kbd "C-c C-e") #'courier-request-switch-env)
(define-key courier-request-mode-map (kbd "C-c C-l") #'courier-request-validate)
(define-key courier-request-mode-map (kbd "C-c C-m") #'courier-request-set-method)
(define-key courier-request-mode-map (kbd "C-c C-n") #'courier-new-request)
(define-key courier-request-mode-map (kbd "C-c C-o") #'courier-open)
(define-key courier-request-mode-map (kbd "C-c C-p") #'courier-request-preview)
(define-key courier-request-mode-map (kbd "C-c C-r") #'courier-rename-request)
(define-key courier-request-mode-map [remap save-buffer] #'courier-request-save-buffer)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . courier-request-mode))

(provide 'courier)

;;; courier.el ends here
