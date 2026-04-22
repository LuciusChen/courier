;;; courier.el --- Curl-backed HTTP client -*- lexical-binding: t; -*-

;; Author: Lucius Chen <chenyh572@gmail.com>
;; URL: https://github.com/LuciusChen/courier
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.0.0"))

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
(require 'xref)

(declare-function treesit-ready-p "treesit" (language &optional quiet))

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

(defcustom courier-default-body-type 'json
  "Default body type used for new Courier requests.

This affects request-side editing and send-time behavior when a request does
not declare an explicit `[body].type` in its front matter."
  :type '(choice (const none)
                 (const json)
                 (const xml)
                 (const text)
                 (const form-urlencoded)
                 (const multipart)
                 (const binary))
  :group 'courier)

(defcustom courier-home-directory
  (file-name-as-directory (expand-file-name "courier" "~"))
  "Root directory used for Courier-managed persistent content.

Courier stores collections under a dedicated `collections/' subdirectory of
this home.  Other persisted Courier-managed content may also live under this
root."
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

(defconst courier--collections-directory-name "collections"
  "Directory name under `courier-home-directory' that stores collections.")

(defconst courier--specs-directory-name "specs"
  "Directory name under `courier-home-directory' that stores API specs.")

(defconst courier--state-directory-name "state"
  "Directory name under `courier-home-directory' that stores Courier state.")

(defconst courier--runtime-vars-directory-name "runtime-vars"
  "Directory name under Courier state that stores runtime variables.")

(defvar courier--untitled-request-counter 0
  "Running counter used for untitled Courier request drafts.")

(defconst courier--body-views
  '(auto json html xml javascript raw hex base64 image document)
  "Supported Courier response body views.")

(defconst courier--allowed-body-types
  '(none json xml text form-urlencoded multipart binary)
  "Supported Courier request body types.")

(defconst courier--allowed-auth-types
  '(none bearer basic header api_key oauth2)
  "Supported Courier request auth types.")

(defconst courier--mime-type-alist
  '(("json" . "application/json")
    ("xml" . "application/xml")
    ("html" . "text/html")
    ("css" . "text/css")
    ("js" . "application/javascript")
    ("txt" . "text/plain")
    ("csv" . "text/csv")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("svg" . "image/svg+xml")
    ("webp" . "image/webp")
    ("pdf" . "application/pdf")
    ("zip" . "application/zip")
    ("gz" . "application/gzip")
    ("tar" . "application/x-tar")
    ("bin" . "application/octet-stream")
    ("mp4" . "video/mp4")
    ("webm" . "video/webm")
    ("mp3" . "audio/mpeg")
    ("wav" . "audio/wav"))
  "File extension to MIME type mapping for Courier file attachment.")

(defconst courier--common-content-types
  (delete-dups
   (append (mapcar #'cdr courier--mime-type-alist)
           '("application/x-www-form-urlencoded"
             "multipart/form-data")))
  "Common MIME types offered by Courier completion.")

(defconst courier--post-response-var-sources
  '("json" "header" "status")
  "Allowed `from' values for `[[vars.post_response]]' rules.")

(defconst courier--tests-dsl-completions
  '("status == 200"
    "status != 404"
    "header content-type contains json"
    "body contains hello"
    "time < 500"
    "size < 102400")
  "Common Courier test DSL snippets offered by completion.")

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
  '((t :inherit bold :underline t))
  "Face used for the active Courier response navigation tab.")

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

(defface courier-response-timeline-entry-face
  '((((background dark)) :background "#1f2227" :extend t)
    (((background light)) :background "#eceff4" :extend t)
    (t :inherit shadow))
  "Face used for the first summary line of Courier timeline entries.")

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
  '((((background dark)) :background "#2a313a" :extend t)
    (((background light)) :background "#dbe5f2" :extend t)
    (t :inherit highlight))
  "Face used for the selected Courier timeline history entry summary line.")

;;;; Parsing

(defconst courier--request-line-regexp
  "^\\([A-Z]+\\)\\s-+\\(\\S-+\\)\\s-*$"
  "Regexp used to parse request lines.")

(defconst courier--header-line-regexp
  "^\\([^: \t][^:]*\\):\\s-*\\(.*\\)$"
  "Regexp used to parse header lines.")

(defconst courier--toml-table-regexp
  "^\\[\\([A-Za-z_][A-Za-z0-9_]*\\(?:\\.[A-Za-z_][A-Za-z0-9_]*\\)?\\)\\]\\s-*$"
  "Regexp used to parse supported Courier TOML tables.")

(defconst courier--toml-array-table-regexp
  "^\\[\\[\\([A-Za-z_][A-Za-z0-9_]*\\(?:\\.[A-Za-z_][A-Za-z0-9_]*\\)?\\)\\]\\]\\s-*$"
  "Regexp used to parse supported Courier TOML array tables.")

(defconst courier--toml-assignment-regexp
  "^\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(.*\\)$"
  "Regexp used to parse supported Courier TOML assignments.")

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

(defun courier--json-key-name (key)
  "Return KEY normalized as a string."
  (cond
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t (user-error "Courier config keys must be strings"))))

(defun courier--json-object-field (data key)
  "Return object KEY from DATA, or nil when KEY is absent."
  (let ((value (alist-get key data)))
    (cond
     ((null value) nil)
     ((listp value) value)
     (t
      (user-error "Courier config field %s must be an object" key)))))

(defun courier--json-positive-int-field (data key)
  "Return positive integer KEY from DATA, or nil when KEY is absent."
  (let ((value (alist-get key data 'courier--missing)))
    (cond
     ((eq value 'courier--missing) nil)
     ((and (integerp value) (> value 0)) value)
     (t
      (user-error "Courier config field %s must be a positive integer" key)))))

(defun courier--json-string-map (data key &optional normalize-keys)
  "Return KEY from DATA as an alist of strings.

When NORMALIZE-KEYS is non-nil, apply it to each key before storing the alist."
  (when-let* ((object (courier--json-object-field data key)))
    (mapcar
     (lambda (pair)
       (let ((name (courier--json-key-name (car pair)))
             (value (cdr pair)))
         (unless (stringp value)
           (user-error "Courier config field %s values must be strings" key))
         (cons (if normalize-keys
                   (funcall normalize-keys name)
                 name)
               value)))
     object)))

(defun courier--json-auth-config (data)
  "Return DATA parsed as a Courier auth plist."
  (when data
    (let* ((type-name (courier--json-string-field data 'type))
           (type (and type-name (intern type-name))))
      (unless (memq type courier--allowed-auth-types)
        (user-error "Courier config auth type %s is not supported" type-name))
      (pcase type
        ('none
         '(:type none))
        ('bearer
         (list :type 'bearer
               :token (or (courier--json-string-field data 'token) "")))
        ('basic
         (list :type 'basic
               :username (or (courier--json-string-field data 'username) "")
               :password (or (courier--json-string-field data 'password) "")))
        ('header
         (list :type 'header
               :header (or (courier--json-string-field data 'header) "")
               :value (or (courier--json-string-field data 'value) "")))
        ('api_key
         (list :type 'api_key
               :in (or (courier--json-string-field data 'in) "header")
               :name (or (courier--json-string-field data 'name) "")
               :value (or (courier--json-string-field data 'value) "")))
        ('oauth2
         (list :type 'oauth2
               :grant-type (or (courier--json-string-field data 'grant_type)
                               "client_credentials")
               :token-url (or (courier--json-string-field data 'token_url) "")
               :client-id (or (courier--json-string-field data 'client_id) "")
               :client-secret (or (courier--json-string-field data 'client_secret) "")
               :scopes (let ((value (alist-get 'scopes data 'courier--missing)))
                         (cond
                          ((eq value 'courier--missing) nil)
                          ((and (listp value) (cl-every #'stringp value)) value)
                          (t
                           (user-error "Courier config auth scopes must be strings"))))))
        (_
         (user-error "Unsupported Courier config auth type %s" type-name))))))

(defun courier--json-defaults-config (data)
  "Return parsed Courier defaults from DATA."
  (when-let* ((defaults (courier--json-object-field data 'defaults)))
    (let ((settings nil)
          (timeout (courier--json-positive-int-field defaults 'timeout))
          (follow-redirects
           (alist-get 'follow_redirects defaults 'courier--missing)))
      (when timeout
        (setq settings (plist-put settings :timeout timeout)))
      (unless (eq follow-redirects 'courier--missing)
        (unless (memq follow-redirects '(t nil))
          (user-error "Courier config field defaults.follow_redirects must be a boolean"))
        (setq settings (plist-put settings :follow-redirects follow-redirects)))
      (list :vars (courier--json-string-map defaults 'vars)
            :headers (courier--json-string-map defaults 'headers #'downcase)
            :auth (courier--json-auth-config
                   (courier--json-object-field defaults 'auth))
            :settings settings))))

(defun courier--write-json-file (path data)
  "Write DATA to PATH as pretty-printed JSON."
  (let ((json-encoding-pretty-print t))
    (with-temp-file path
      (insert (json-serialize data))
      (insert "\n"))))

(defun courier--slugify (string)
  "Return STRING normalized for Courier file and directory names."
  (let* ((downcased (downcase (or string "")))
         (collapsed (replace-regexp-in-string "[^[:alnum:]]+" "-" downcased))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" collapsed)))
    (if (string-empty-p trimmed)
        "request"
      trimmed)))

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

(defun courier--empty-request (&optional path)
  "Return a fresh Courier request plist for PATH."
  (list :path path
        :headers nil
        :body ""
        :body-type courier-default-body-type
        :body-parts nil
        :body-file-path nil
        :body-file-content-type nil
        :params nil
        :vars nil
        :pre-request-vars nil
        :post-response-vars nil
        :auth nil
        :pre-request-script nil
        :post-response-script nil
        :tests nil
        :settings nil))

(defun courier--read-toml-basic-string (raw line-number)
  "Parse RAW as a TOML basic string for LINE-NUMBER."
  (condition-case err
      (pcase-let ((`(,value . ,end) (read-from-string raw)))
        (unless (stringp value)
          (courier--parse-error line-number "Expected TOML string"))
        (unless (string-empty-p (string-trim (substring raw end)))
          (courier--parse-error line-number "Trailing characters after TOML string"))
        value)
    (error
     (courier--parse-error
      line-number
      "Invalid TOML string: %s"
      (error-message-string err)))))

(defun courier--parse-toml-string-array-content (content line-number)
  "Parse TOML string array CONTENT starting at LINE-NUMBER."
  (let ((index 0)
        (length (length content))
        values)
    (while (< index length)
      (while (and (< index length)
                  (memq (aref content index) '(?\s ?\t ?\n ?, ?\r)))
        (setq index (1+ index)))
      (when (< index length)
        (unless (eq (aref content index) ?\")
          (courier--parse-error line-number "tests must contain only strings"))
        (pcase-let* ((`(,value . ,end)
                      (condition-case err
                          (read-from-string (substring content index))
                        (error
                         (courier--parse-error
                          line-number
                          "Invalid TOML string in tests: %s"
                          (error-message-string err))))))
          (unless (stringp value)
            (courier--parse-error line-number "tests must contain only strings"))
          (push value values)
          (setq index (+ index end)))))
    (nreverse values)))

(defun courier--parse-toml-array (raw lines index line-number)
  "Parse TOML string array RAW from LINES at INDEX and LINE-NUMBER.
Return `(VALUES . NEXT-INDEX)'."
  (let ((trimmed (string-trim raw)))
    (cond
     ((string-match "\\`\\[\\(.*\\)\\]\\s-*\\'" trimmed)
      (cons (courier--parse-toml-string-array-content
             (match-string 1 trimmed)
             line-number)
            (1+ index)))
     ((string-match-p "\\`\\[\\s-*\\'" trimmed)
      (let ((content-lines nil)
            (cursor (1+ index))
            (done nil))
        (while (and (< cursor (length lines))
                    (not done))
          (let ((line (nth cursor lines)))
            (if (string-match "^\\s-*]\\s-*$" line)
                (setq done t)
              (push line content-lines)
              (setq cursor (1+ cursor)))))
        (unless done
          (courier--parse-error line-number "Unclosed TOML array"))
        (cons (courier--parse-toml-string-array-content
               (string-join (nreverse content-lines) "\n")
               line-number)
              (1+ cursor))))
     (t
      (courier--parse-error line-number "Invalid TOML array")))))

(defun courier--parse-toml-multiline-string (raw lines index line-number)
  "Parse TOML multiline string RAW from LINES at INDEX and LINE-NUMBER.
Return `(STRING . NEXT-INDEX)'."
  (let* ((trimmed (string-trim-left raw))
         (after-open (substring trimmed 3))
         (close-pos (string-match "\"\"\"" after-open)))
    (if close-pos
        (let ((value (substring after-open 0 close-pos))
              (rest (substring after-open (+ close-pos 3))))
          (unless (string-empty-p (string-trim rest))
            (courier--parse-error line-number "Trailing characters after multiline string"))
          (cons value (1+ index)))
      (let ((pieces nil)
            (cursor (1+ index))
            done)
        (unless (string-empty-p after-open)
          (push after-open pieces))
        (while (and (< cursor (length lines))
                    (not done))
          (let* ((line (nth cursor lines))
                 (end-pos (string-match "\"\"\"" line)))
            (if end-pos
                (let ((prefix (substring line 0 end-pos))
                      (rest (substring line (+ end-pos 3))))
                  (unless (string-empty-p prefix)
                    (push prefix pieces))
                  (unless (string-empty-p (string-trim rest))
                    (courier--parse-error
                     (+ line-number (- cursor index))
                     "Trailing characters after multiline string"))
                  (setq done t))
              (push line pieces)
              (setq cursor (1+ cursor)))))
        (unless done
          (courier--parse-error line-number "Unclosed TOML multiline string"))
        (cons (string-join (nreverse pieces) "\n")
              (1+ cursor))))))

(defun courier--parse-toml-value (raw lines index line-number)
  "Parse TOML RAW value from LINES at INDEX and LINE-NUMBER.
Return `(VALUE . NEXT-INDEX)'."
  (let ((trimmed (string-trim raw)))
    (cond
     ((string-prefix-p "\"\"\"" trimmed)
      (courier--parse-toml-multiline-string trimmed lines index line-number))
     ((string-prefix-p "\"" trimmed)
      (cons (courier--read-toml-basic-string trimmed line-number) (1+ index)))
     ((string-prefix-p "[" trimmed)
      (courier--parse-toml-array trimmed lines index line-number))
     ((string-match-p "\\`[0-9]+\\'" trimmed)
      (cons (string-to-number trimmed) (1+ index)))
     ((member trimmed '("true" "false"))
      (cons (string= trimmed "true") (1+ index)))
     (t
      (courier--parse-error line-number "Unsupported TOML value: %s" trimmed)))))

(defun courier--parse-front-matter (text start-line request)
  "Parse TOML front matter TEXT starting at START-LINE into REQUEST."
  (let ((lines (split-string text "\n"))
        (index 0)
        (current-table nil)
        (seen-tables nil)
        (settings (plist-get request :settings))
        (body-type (plist-get request :body-type))
        (body-file-path (plist-get request :body-file-path))
        (body-file-content-type (plist-get request :body-file-content-type))
        (body-parts (plist-get request :body-parts))
        (auth nil)
        (vars nil)
        (pre-request-vars (plist-get request :pre-request-vars))
        (post-response-vars (plist-get request :post-response-vars))
        (tests nil)
        (pre-request nil)
        (post-response nil)
        (current-body-part nil)
        (current-post-response-var nil))
    (while (< index (length lines))
      (let* ((line (nth index lines))
             (line-number (+ start-line index))
             (trimmed (string-trim line)))
        (cond
         ((or (string-empty-p trimmed)
              (string-prefix-p "#" trimmed))
          (setq index (1+ index)))
         ((string-match courier--toml-array-table-regexp trimmed)
          (when current-body-part
            (setq body-parts (append body-parts (list current-body-part)))
            (setq current-body-part nil))
          (when current-post-response-var
            (setq post-response-vars
                  (append post-response-vars (list current-post-response-var)))
            (setq current-post-response-var nil))
          (setq current-table (match-string 1 trimmed))
          (unless (member current-table '("body.parts" "vars.post_response"))
            (courier--parse-error line-number
                                  "Unsupported TOML array table: [[%s]]"
                                  current-table))
          (setq index (1+ index)))
         ((string-match courier--toml-table-regexp trimmed)
          (when current-body-part
            (setq body-parts (append body-parts (list current-body-part)))
            (setq current-body-part nil))
          (when current-post-response-var
            (setq post-response-vars
                  (append post-response-vars (list current-post-response-var)))
            (setq current-post-response-var nil))
          (setq current-table (match-string 1 trimmed))
          (when (member current-table seen-tables)
            (courier--parse-error line-number "Duplicate TOML table: [%s]" current-table))
          (unless (member current-table '("auth" "vars" "vars.pre_request" "scripts" "body"))
            (courier--parse-error line-number "Unsupported TOML table: [%s]" current-table))
          (push current-table seen-tables)
          (setq index (1+ index)))
         ((string-match courier--toml-assignment-regexp line)
          (let* ((key (match-string 1 line))
                 (raw-value (match-string 2 line))
                 (parsed (courier--parse-toml-value raw-value lines index line-number))
                 (value (car parsed)))
            (setq index (cdr parsed))
            (pcase current-table
              ((pred null)
               (pcase key
                 ("name"
                  (unless (stringp value)
                    (courier--parse-error line-number "name must be a string"))
                  (setq request (plist-put request :name value)))
                 ("timeout"
                  (unless (and (integerp value) (> value 0))
                    (courier--parse-error line-number "timeout must be a positive integer"))
                  (setq settings (plist-put settings :timeout value)))
                 ("follow_redirects"
                  (unless (memq value '(t nil))
                    (courier--parse-error line-number "follow_redirects must be a boolean"))
                  (setq settings (plist-put settings :follow-redirects value)))
                 ("tests"
                  (unless (cl-every #'stringp value)
                    (courier--parse-error line-number "tests must be an array of strings"))
                  (setq tests value))
                 (_
                  (courier--parse-error line-number "Unsupported TOML key: %s" key))))
              ("auth"
               (pcase key
                 ("type" (setq auth (plist-put auth :type (intern value))))
                 ("token" (setq auth (plist-put auth :token value)))
                 ("username" (setq auth (plist-put auth :username value)))
                 ("password" (setq auth (plist-put auth :password value)))
                 ("header" (setq auth (plist-put auth :header value)))
                 ("value" (setq auth (plist-put auth :value value)))
                 ("in" (setq auth (plist-put auth :in value)))
                 ("name" (setq auth (plist-put auth :name value)))
                 ("grant_type" (setq auth (plist-put auth :grant-type value)))
                 ("token_url" (setq auth (plist-put auth :token-url value)))
                 ("client_id" (setq auth (plist-put auth :client-id value)))
                 ("client_secret" (setq auth (plist-put auth :client-secret value)))
                 ("scopes" (setq auth (plist-put auth :scopes value)))
                 (_
                  (courier--parse-error line-number "Unsupported [auth] key: %s" key))))
              ("vars"
               (unless (stringp value)
                 (courier--parse-error line-number "[vars] values must be strings"))
               (setq vars (append vars (list (cons key value)))))
              ("vars.pre_request"
               (unless (stringp value)
                 (courier--parse-error line-number "[vars.pre_request] values must be strings"))
               (setq pre-request-vars
                     (append pre-request-vars (list (cons key value)))))
              ("body"
               (pcase key
                 ("type" (setq body-type (intern value)))
                 ("path" (setq body-file-path value))
                 ("content_type" (setq body-file-content-type value))
                 (_
                  (courier--parse-error line-number "Unsupported [body] key: %s" key))))
              ("body.parts"
               (pcase key
                 ("name" (setq current-body-part (plist-put current-body-part :name value)))
                 ("kind"
                  (setq current-body-part
                        (plist-put current-body-part :kind (intern value))))
                 ("value" (setq current-body-part (plist-put current-body-part :value value)))
                 ("path" (setq current-body-part (plist-put current-body-part :path value)))
                 ("content_type"
                  (setq current-body-part
                        (plist-put current-body-part :content-type value)))
                 (_
                  (courier--parse-error line-number
                                        "Unsupported [[body.parts]] key: %s"
                                        key))))
              ("vars.post_response"
               (pcase key
                 ("name"
                  (setq current-post-response-var
                        (plist-put current-post-response-var :name value)))
                 ("from"
                  (setq current-post-response-var
                        (plist-put current-post-response-var :from (intern value))))
                 ("expr"
                  (setq current-post-response-var
                        (plist-put current-post-response-var :expr value)))
                 (_
                  (courier--parse-error line-number
                                        "Unsupported [[vars.post_response]] key: %s"
                                        key))))
              ("scripts"
               (pcase key
                 ("pre_request"
                  (unless (stringp value)
                    (courier--parse-error line-number "pre_request must be a string"))
                  (setq pre-request value))
                 ("post_response"
                 (unless (stringp value)
                    (courier--parse-error line-number "post_response must be a string"))
                  (setq post-response value))
                 (_
                  (courier--parse-error line-number "Unsupported [scripts] key: %s" key)))))))
         (t
          (courier--parse-error line-number "Malformed TOML line: %s" line)))))
    (when current-body-part
      (setq body-parts (append body-parts (list current-body-part))))
    (when current-post-response-var
      (setq post-response-vars
            (append post-response-vars (list current-post-response-var))))
    (when body-type
      (unless (memq body-type courier--allowed-body-types)
        (courier--parse-error start-line "Invalid [body].type: %s" body-type)))
    (pcase body-type
      ('multipart
       (unless body-parts
         (courier--parse-error start-line
                               "[body] multipart requires at least one [[body.parts]] entry"))
       (dolist (part body-parts)
         (unless (stringp (plist-get part :name))
           (courier--parse-error start-line "[[body.parts]] requires name"))
         (unless (memq (plist-get part :kind) '(text file))
           (courier--parse-error start-line "[[body.parts]] kind must be text or file"))
         (pcase (plist-get part :kind)
           ('text
            (unless (stringp (plist-get part :value))
              (courier--parse-error start-line "[[body.parts]] text part requires value")))
           ('file
            (unless (stringp (plist-get part :path))
              (courier--parse-error start-line "[[body.parts]] file part requires path"))))))
      ('binary
       (unless (stringp body-file-path)
         (courier--parse-error start-line "[body] binary requires path")))
      ((or 'none 'json 'xml 'text 'form-urlencoded)
       (when body-parts
         (courier--parse-error start-line "[[body.parts]] is only valid for multipart bodies"))
       (when body-file-path
         (courier--parse-error start-line "[body].path is only valid for binary bodies"))
       (when body-file-content-type
         (courier--parse-error start-line
                               "[body].content_type is only valid for binary bodies"))))
    (dolist (rule post-response-vars)
      (unless (stringp (plist-get rule :name))
        (courier--parse-error start-line "[[vars.post_response]] requires name"))
      (unless (memq (plist-get rule :from) '(json header status))
        (courier--parse-error start-line
                              "[[vars.post_response]] from must be json, header, or status"))
      (pcase (plist-get rule :from)
        ((or 'json 'header)
         (unless (stringp (plist-get rule :expr))
           (courier--parse-error start-line
                                 "[[vars.post_response]] %s rule requires expr"
                                 (plist-get rule :from))))
        ('status nil)))
    (when auth
      (let ((type (plist-get auth :type)))
        (unless (memq type courier--allowed-auth-types)
          (courier--parse-error start-line "Invalid [auth].type: %s" type))
        (pcase type
          ('none
           (setq auth '(:type none)))
          ('bearer
           (unless (stringp (plist-get auth :token))
             (courier--parse-error start-line "[auth] bearer auth requires token")))
          ('basic
           (unless (stringp (plist-get auth :username))
             (courier--parse-error start-line "[auth] basic auth requires username"))
           (unless (stringp (plist-get auth :password))
             (courier--parse-error start-line "[auth] basic auth requires password")))
          ('header
           (unless (stringp (plist-get auth :header))
             (courier--parse-error start-line "[auth] header auth requires header"))
           (unless (stringp (plist-get auth :value))
             (courier--parse-error start-line "[auth] header auth requires value")))
          ('api_key
           (unless (member (plist-get auth :in) '("header" "query"))
             (courier--parse-error start-line
                                   "[auth] api_key requires in = \"header\" or \"query\""))
           (unless (stringp (plist-get auth :name))
             (courier--parse-error start-line "[auth] api_key requires name"))
           (unless (stringp (plist-get auth :value))
             (courier--parse-error start-line "[auth] api_key requires value")))
          ('oauth2
           (unless (string= (plist-get auth :grant-type) "client_credentials")
             (courier--parse-error
              start-line
              "[auth] oauth2 currently requires grant_type = \"client_credentials\""))
           (unless (stringp (plist-get auth :token-url))
             (courier--parse-error start-line "[auth] oauth2 requires token_url"))
           (unless (stringp (plist-get auth :client-id))
             (courier--parse-error start-line "[auth] oauth2 requires client_id"))
           (unless (stringp (plist-get auth :client-secret))
             (courier--parse-error start-line "[auth] oauth2 requires client_secret"))
           (when-let* ((scopes (plist-get auth :scopes)))
             (unless (cl-every #'stringp scopes)
               (courier--parse-error start-line
                                     "[auth] oauth2 scopes must be strings")))))))
    (plist-put request :settings settings)
    (plist-put request :body-type body-type)
    (plist-put request :body-parts body-parts)
    (plist-put request :body-file-path body-file-path)
    (plist-put request :body-file-content-type body-file-content-type)
    (plist-put request :auth auth)
    (plist-put request :vars vars)
    (plist-put request :pre-request-vars pre-request-vars)
    (plist-put request :post-response-vars post-response-vars)
    (plist-put request :tests tests)
    (plist-put request :pre-request-script pre-request)
    (plist-put request :post-response-script post-response)))

(defun courier--buffer-components ()
  "Return the current buffer split into Courier front matter and HTTP block.

The result is a plist with `:front-matter', `:front-matter-line', `:http', and
`:http-line'."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at-p "\\+\\+\\+$")
        (let ((front-start-line 2))
          (forward-line 1)
          (let ((front-start (point))
                close-pos)
            (while (and (not (eobp))
                        (not close-pos))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (if (string= line "+++")
                    (setq close-pos (line-beginning-position))
                  (forward-line 1))))
            (unless close-pos
              (courier--parse-error 1 "Unclosed TOML front matter"))
            (goto-char close-pos)
            (let ((front-matter
                   (buffer-substring-no-properties front-start close-pos)))
              (forward-line 1)
              (list :front-matter front-matter
                    :front-matter-line front-start-line
                    :http (buffer-substring-no-properties (point) (point-max))
                    :http-line (line-number-at-pos)))))
      (list :front-matter nil
            :front-matter-line 1
            :http (buffer-substring-no-properties (point-min) (point-max))
            :http-line 1))))

(defun courier--parse-http-block (http http-line request &optional relaxed)
  "Parse raw HTTP block HTTP starting at HTTP-LINE into REQUEST.
When RELAXED is non-nil, allow incomplete draft request lines."
  (with-temp-buffer
    (insert http)
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at-p "[ \t]*$"))
      (forward-line 1))
    (let ((line-number http-line)
          request-line-seen
          body-start)
      (if (eobp)
          (unless relaxed
            (courier--parse-error http-line "Missing request line"))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match courier--request-line-regexp line)
            (let* ((method (match-string 1 line))
                   (url (match-string 2 line))
                   (parts (courier--split-request-url url))
                   (base (nth 0 parts))
                   (fragment (nth 2 parts)))
              (unless (member method courier--allowed-methods)
                (courier--parse-error line-number "Invalid HTTP method: %s" method))
              (setq request-line-seen t)
              (plist-put request :method method)
              (plist-put request :url (concat base (or fragment "")))
              (plist-put request :params (courier--parse-url-query-params url))
              (forward-line 1)))
           (relaxed
            (if (string-match "^\\([A-Za-z]+\\)\\(?:\\s-+\\(.*\\)\\)?$" line)
                (let* ((method (upcase (match-string 1 line)))
                       (url (string-trim-right (or (match-string 2 line) ""))))
                  (plist-put request :method method)
                  (pcase-let ((`(,base ,_query ,fragment) (courier--split-request-url url)))
                    (plist-put request :url (concat base (or fragment ""))))
                  (plist-put request :params (courier--parse-url-query-params url))
                  (setq request-line-seen t)
                  (forward-line 1))))
           (t
            (courier--parse-error line-number "Missing or malformed request line")))))
      (while (and (not (eobp))
                  (not body-start))
        (setq line-number (+ http-line (1- (line-number-at-pos))))
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
           (relaxed
            (setq body-start (point)))
           (t
            (courier--parse-error line-number "Malformed header: %s" line)))))
      (unless (or request-line-seen relaxed)
        (courier--parse-error http-line "Missing request line"))
      (when body-start
        (plist-put request :body
                   (buffer-substring-no-properties body-start (point-max)))))
    request))

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
      (cond
       ((and (< (1+ index) length)
             (eq (aref string index) ?{)
             (eq (aref string (1+ index)) ?{))
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
       (t
        (push (string (aref string index)) parts)
        (setq index (1+ index)))))
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

(defun courier--parse-buffer-for-editor ()
  "Parse the current buffer into a relaxed request model for editing."
  (let* ((path (and buffer-file-name (expand-file-name buffer-file-name)))
         (request (courier--empty-request path))
         (parts (courier--buffer-components)))
    (plist-put request :method courier-default-request-method)
    (plist-put request :url "")
    (when-let* ((front-matter (plist-get parts :front-matter)))
      (setq request
            (courier--parse-front-matter
             front-matter
             (plist-get parts :front-matter-line)
             request)))
    (courier--parse-http-block
     (plist-get parts :http)
     (plist-get parts :http-line)
     request
     t)))

(defun courier--parse-file-for-editor (path)
  "Parse PATH into a relaxed request model for editing."
  (with-temp-buffer
    (insert-file-contents path)
    (setq-local buffer-file-name path)
    (courier--parse-buffer-for-editor)))

;;;###autoload
(defun courier-parse-buffer ()
  "Parse the current buffer into a request plist."
  (let* ((path (and buffer-file-name (expand-file-name buffer-file-name)))
         (request (courier--empty-request path))
         (parts (courier--buffer-components)))
    (when-let* ((front-matter (plist-get parts :front-matter)))
      (setq request
            (courier--parse-front-matter
             front-matter
             (plist-get parts :front-matter-line)
             request)))
    (courier--parse-http-block
     (plist-get parts :http)
     (plist-get parts :http-line)
     request)))

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
  (courier--read-env-like-file
   path
   (lambda (raw _path _line-number)
     (string-trim raw))
   "env"))

;;;###autoload
(defun courier-merge-vars (env-vars request-vars)
  "Merge ENV-VARS with REQUEST-VARS, with REQUEST-VARS taking precedence."
  (let ((merged (copy-tree env-vars)))
    (dolist (pair request-vars)
      (if-let* ((existing (assoc-string (car pair) merged nil)))
          (setcdr existing (cdr pair))
        (setq merged (append merged (list (cons (car pair) (cdr pair)))))))
    merged))

(defun courier--merge-headers (defaults headers)
  "Merge DEFAULTS with HEADERS, with HEADERS taking precedence."
  (let ((merged (copy-tree defaults t)))
    (dolist (pair headers)
      (let ((name (downcase (car pair))))
        (if-let* ((existing (assoc-string name merged nil)))
            (setcdr existing (cdr pair))
          (setq merged (append merged (list (cons name (cdr pair))))))))
    merged))

(defun courier--merge-settings (defaults settings)
  "Merge DEFAULTS with SETTINGS, with SETTINGS taking precedence."
  (let ((merged (copy-sequence defaults)))
    (dolist (key '(:timeout :follow-redirects))
      (when (plist-member settings key)
        (setq merged (plist-put merged key (plist-get settings key)))))
    merged))

(defun courier--merge-request-defaults (defaults overrides)
  "Merge DEFAULTS with OVERRIDES, with OVERRIDES taking precedence."
  (list :vars (courier-merge-vars (plist-get defaults :vars)
                                  (plist-get overrides :vars))
        :headers (courier--merge-headers (plist-get defaults :headers)
                                         (plist-get overrides :headers))
        :auth (or (plist-get overrides :auth)
                  (plist-get defaults :auth))
        :settings (courier--merge-settings (plist-get defaults :settings)
                                           (plist-get overrides :settings))))

(defun courier--directory-config-defaults (directory)
  "Return parsed Courier defaults declared directly in DIRECTORY."
  (let ((path (expand-file-name courier--collection-marker-file directory)))
    (when (file-exists-p path)
      (courier--json-defaults-config (courier--read-json-file path)))))

(defun courier--request-default-directories (collection-root request-directory)
  "Return default-bearing directories from COLLECTION-ROOT to REQUEST-DIRECTORY."
  (let* ((requests-root (courier--preferred-requests-root collection-root))
         (normalized-requests-root
          (and requests-root
               (file-name-as-directory (expand-file-name requests-root))))
         (normalized-request-directory
          (and request-directory
               (file-name-as-directory (expand-file-name request-directory))))
         directories)
    (when (and normalized-requests-root
               normalized-request-directory
               (file-in-directory-p normalized-request-directory
                                    normalized-requests-root))
      (let ((current normalized-request-directory))
        (while (and current
                    (file-in-directory-p current normalized-requests-root))
          (push current directories)
          (if (file-equal-p current normalized-requests-root)
              (setq current nil)
            (setq current
                  (file-name-directory
                   (directory-file-name current)))))))
    (nreverse directories)))

(defun courier--request-defaults (&optional request)
  "Return merged collection and folder defaults for REQUEST."
  (let* ((request-path (plist-get request :path))
         (collection-root
          (or (and request-path (courier--collection-root request-path))
              (courier--active-collection-root)))
         (request-directory
          (or (and request-path (file-name-directory request-path))
              default-directory))
         (defaults (or (and collection-root
                            (courier--collection-config collection-root)
                            (plist-get (courier--collection-config collection-root) :defaults))
                       nil)))
    (dolist (directory (courier--request-default-directories collection-root request-directory))
      (when (and (file-directory-p directory)
                 (not (file-equal-p directory collection-root)))
        (setq defaults
              (courier--merge-request-defaults
               defaults
               (or (courier--directory-config-defaults directory)
                   nil)))))
    defaults))

(defun courier--apply-request-defaults (request)
  "Return REQUEST with collection and folder defaults applied."
  (let* ((defaults (courier--request-defaults request))
         (default-vars (plist-get defaults :vars))
         (request-auth (plist-get request :auth)))
    (setq request (plist-put request :default-vars default-vars))
    (setq request
          (plist-put request :headers
                     (courier--merge-headers (plist-get defaults :headers)
                                             (plist-get request :headers))))
    (setq request
          (plist-put request :settings
                     (courier--merge-settings (plist-get defaults :settings)
                                              (plist-get request :settings))))
    (if request-auth
        request
      (setq request (plist-put request :auth (plist-get defaults :auth))))))

;;;###autoload
(defun courier-validate-request (request)
  "Validate REQUEST and return REQUEST."
  (let ((method (plist-get request :method))
        (url (plist-get request :url))
        (settings (plist-get request :settings))
        (body-type (or (plist-get request :body-type)
                       courier-default-body-type))
        (body (or (plist-get request :body) ""))
        (body-file-path (plist-get request :body-file-path))
        (body-parts (plist-get request :body-parts))
        (auth (plist-get request :auth)))
    (unless (and (stringp method) (member method courier--allowed-methods))
      (user-error "Invalid HTTP method: %s" method))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error "Request URL must be present"))
    (unless (memq body-type courier--allowed-body-types)
      (user-error "Invalid body type: %s" body-type))
    (pcase body-type
      ('none
       (when (not (string-empty-p (string-trim body)))
         (user-error "Body type `none` cannot have a body")))
      ('binary
       (unless (and (stringp body-file-path)
                    (not (string-empty-p (string-trim body-file-path))))
         (user-error "Body type `binary` requires a file path")))
      ('multipart
       (unless body-parts
         (user-error "Body type `multipart` requires at least one body part"))
       (dolist (part body-parts)
         (unless (and (stringp (plist-get part :name))
                      (not (string-empty-p (string-trim (plist-get part :name)))))
           (user-error "Multipart body parts require name"))
         (unless (memq (plist-get part :kind) '(text file))
           (user-error "Multipart body parts require kind = text or file"))
         (pcase (plist-get part :kind)
           ('text
            (unless (stringp (plist-get part :value))
              (user-error "Multipart text parts require value")))
           ('file
            (unless (and (stringp (plist-get part :path))
                         (not (string-empty-p (string-trim (plist-get part :path)))))
              (user-error "Multipart file parts require path")))))))
    (when auth
      (pcase (plist-get auth :type)
        ('api_key
         (unless (member (plist-get auth :in) '("header" "query"))
           (user-error "Auth type `api_key` requires in = header or query"))
         (unless (and (stringp (plist-get auth :name))
                      (not (string-empty-p (string-trim (plist-get auth :name)))))
           (user-error "Auth type `api_key` requires name"))
         (unless (and (stringp (plist-get auth :value))
                      (not (string-empty-p (string-trim (plist-get auth :value)))))
           (user-error "Auth type `api_key` requires value")))
        ('oauth2
         (unless (string= (plist-get auth :grant-type) "client_credentials")
           (user-error "Auth type `oauth2` currently supports only client_credentials"))
         (dolist (key '(:token-url :client-id :client-secret))
           (unless (and (stringp (plist-get auth key))
                        (not (string-empty-p (string-trim (plist-get auth key)))))
             (user-error "Auth type `oauth2` requires %s" key))))))
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
  (courier--expand-string
   string
   (lambda (name)
     (courier--resolve-var name vars nil 0))))

(defun courier--request-body-type (request)
  "Return the effective body type for REQUEST."
  (or (plist-get request :body-type)
      courier-default-body-type))

(defun courier--body-type-label (body-type)
  "Return a display label for BODY-TYPE."
  (pcase body-type
    ('none "None")
    ('json "JSON")
    ('xml "XML")
    ('text "Text")
    ('form-urlencoded "Form")
    ('multipart "Multipart")
    ('binary "Binary")
    (_ (capitalize (symbol-name body-type)))))

(defun courier--request-auth-type (request)
  "Return the effective auth type for REQUEST."
  (if-let* ((auth (plist-get request :auth)))
      (or (plist-get auth :type) 'none)
    'none))

(defun courier--auth-type-label (auth-type)
  "Return a display label for AUTH-TYPE."
  (pcase auth-type
    ('none "None")
    ('basic "Basic")
    ('bearer "Bearer")
    ('header "Header")
    ('api_key "API Key")
    ('oauth2 "OAuth2")
    (_ (capitalize (symbol-name auth-type)))))

(defun courier--default-content-type-for-body-type (body-type)
  "Return the default Content-Type header value for BODY-TYPE, or nil."
  (pcase body-type
    ('json "application/json")
    ('xml "application/xml")
    ('text "text/plain; charset=utf-8")
    ('form-urlencoded "application/x-www-form-urlencoded")
    (_ nil)))

(defun courier--form-body-pairs-from-string (body)
  "Parse BODY as a form-urlencoded-like raw string into an alist."
  (let ((body (string-trim (or body ""))))
    (if (string-empty-p body)
        nil
      (mapcar
       (lambda (part)
         (setq part (string-trim part))
         (pcase-let* ((`(,name ,value)
                       (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" part)
                           (list (match-string 1 part) (match-string 2 part))
                         (list part ""))))
           (cons (url-unhex-string name)
                 (url-unhex-string value))))
       (split-string body "&" t)))))

(defun courier--form-body-string-from-pairs (pairs)
  "Return a raw unencoded form body string from PAIRS."
  (mapconcat (lambda (pair)
               (format "%s=%s" (car pair) (cdr pair)))
             pairs "&"))

(defun courier--resolve-form-body (body resolved-vars)
  "Return BODY resolved and URL-encoded as form data using RESOLVED-VARS."
  (let ((pairs (courier--form-body-pairs-from-string body)))
    (mapconcat
     (lambda (pair)
       (format "%s=%s"
               (url-hexify-string
                (courier-expand-template (car pair) resolved-vars))
               (url-hexify-string
                (courier-expand-template (cdr pair) resolved-vars))))
     pairs "&")))

(defun courier--maybe-add-default-body-header (request)
  "Add a default Content-Type header to REQUEST when body semantics require it."
  (let* ((body-type (courier--request-body-type request))
         (body (or (plist-get request :body) ""))
         (headers (plist-get request :headers))
         (body-file-content-type (plist-get request :body-file-content-type))
         (should-add
          (pcase body-type
            ('binary (and body-file-content-type
                          (not (string-empty-p body-file-content-type))))
            (_ (not (string-empty-p body)))))
         (content-type
          (pcase body-type
            ('binary body-file-content-type)
            (_ (courier--default-content-type-for-body-type body-type)))))
    (when (and should-add
               content-type
               (not (assoc-string "content-type" headers nil)))
      (plist-put request :headers
                 (append headers (list (cons "content-type" content-type)))))
    request))

(defun courier--resolve-multipart-body-parts (parts resolved-vars)
  "Return PARTS with template values expanded using RESOLVED-VARS."
  (mapcar
   (lambda (part)
     (let ((resolved (copy-sequence part)))
       (dolist (key '(:name :value :path :content-type))
         (when-let* ((value (plist-get part key)))
           (plist-put resolved key
                      (courier-expand-template value resolved-vars))))
       resolved))
   parts))

(defun courier--resolve-auth-query-param (auth resolved-vars)
  "Resolve AUTH into a query param cons cell using RESOLVED-VARS."
  (when auth
    (pcase (plist-get auth :type)
      ('api_key
       (when (string= (plist-get auth :in) "query")
         (cons (courier-expand-template (plist-get auth :name) resolved-vars)
               (courier-expand-template (plist-get auth :value) resolved-vars)))))))

(defun courier--oauth2-token-request (resolved-request)
  "Build a resolved OAuth2 token request from RESOLVED-REQUEST."
  (let* ((auth (plist-get resolved-request :auth))
         (resolved-vars (plist-get resolved-request :resolved-vars))
         (scopes (mapcar (lambda (scope)
                           (courier-expand-template scope resolved-vars))
                         (plist-get auth :scopes)))
         (body-pairs
          (append
           '(("grant_type" . "client_credentials"))
           (list (cons "client_id"
                       (courier-expand-template (plist-get auth :client-id)
                                                resolved-vars))
                 (cons "client_secret"
                       (courier-expand-template (plist-get auth :client-secret)
                                                resolved-vars)))
           (when scopes
             (list (cons "scope" (string-join scopes " "))))))
         (token-request
          (list :path (plist-get resolved-request :path)
                :name (format "%s [oauth2 token]"
                              (or (plist-get resolved-request :name)
                                  "Courier request"))
                :method "POST"
                :url (courier-expand-template (plist-get auth :token-url)
                                              resolved-vars)
                :headers '(("accept" . "application/json"))
                :body (courier--form-body-string-from-pairs body-pairs)
                :body-type 'form-urlencoded
                :params nil
                :vars nil
                :auth nil
                :tests nil
                :settings (copy-tree (plist-get resolved-request :settings) t))))
    (courier-resolve-request token-request nil)))

(defun courier--oauth2-access-token (response)
  "Extract an OAuth2 access token from RESPONSE.

Signal `user-error' when RESPONSE does not contain a valid token payload."
  (let ((status (or (plist-get response :status-code) 0)))
    (unless (and (>= status 200) (< status 300))
      (user-error "OAuth2 token request failed with status %d" status)))
  (let ((body (or (plist-get response :body-text) "")))
    (when (string-empty-p body)
      (user-error "OAuth2 token response body is empty"))
    (let* ((payload (json-parse-string body :object-type 'alist :array-type 'list))
           (token (or (alist-get "access_token" payload nil nil #'equal)
                      (alist-get 'access_token payload))))
      (unless (and (stringp token) (not (string-empty-p token)))
        (user-error "OAuth2 token response is missing access_token"))
      token)))

;;;###autoload
(defun courier-resolve-request (request &optional env-vars)
  "Resolve variables in REQUEST with optional ENV-VARS and return a new plist."
  (let* ((vars (courier-merge-vars env-vars (plist-get request :vars)))
         (resolved-vars (copy-tree vars t))
         (resolved (copy-sequence request))
         (body-type (courier--request-body-type request))
         (resolved-params
          (mapcar (lambda (param)
                    (cons (car param)
                          (courier-expand-template (cdr param) resolved-vars)))
                  (plist-get request :params))))
    (when-let* ((auth-query-param
                 (courier--resolve-auth-query-param (plist-get request :auth)
                                                    resolved-vars)))
      (unless (assoc-string (car auth-query-param) resolved-params nil)
        (setq resolved-params (append resolved-params (list auth-query-param)))))
    (plist-put resolved :body-type body-type)
    (plist-put resolved :params resolved-params)
    (plist-put resolved :url
               (let* ((base-url
                       (courier-expand-template (plist-get request :url) resolved-vars))
                      (params resolved-params))
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
               (pcase body-type
                 ('none "")
                 ('form-urlencoded
                  (courier--resolve-form-body (or (plist-get request :body) "") resolved-vars))
                 (_
                  (courier-expand-template (or (plist-get request :body) "") resolved-vars))))
    (plist-put resolved :body-file-path
               (when-let* ((path (plist-get request :body-file-path)))
                 (courier-expand-template path resolved-vars)))
    (plist-put resolved :body-file-content-type
               (when-let* ((content-type (plist-get request :body-file-content-type)))
                 (courier-expand-template content-type resolved-vars)))
    (plist-put resolved :body-parts
               (when-let* ((parts (plist-get request :body-parts)))
                 (courier--resolve-multipart-body-parts parts resolved-vars)))
    (plist-put resolved :resolved-vars resolved-vars)
    (courier--maybe-add-default-body-header resolved)
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
       (cons (downcase (plist-get auth :header))
             (courier-expand-template
              (plist-get auth :value)
              resolved-vars)))
      ('api_key
       (when (string= (plist-get auth :in) "header")
         (cons (downcase (courier-expand-template (plist-get auth :name)
                                                  resolved-vars))
               (courier-expand-template
                (plist-get auth :value)
                resolved-vars)))))))

(defvar courier-script-request nil
  "Request plist bound while running Courier request scripts.")

(defvar courier-script-response nil
  "Response plist bound while running Courier response scripts.")

(defvar courier-script-env-vars nil
  "Environment variable alist bound while running Courier request scripts.")

(defvar courier-script-phase nil
  "Current Courier script phase symbol.")

(defun courier--plist-like-p (value)
  "Check whether VALUE resembles a plist."
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

(defun courier--json-scalar-to-string (value)
  "Return VALUE converted to a runtime var string.
Signal `user-error' when VALUE is not a scalar."
  (cond
   ((stringp value) value)
   ((integerp value) (number-to-string value))
   ((floatp value) (number-to-string value))
   ((eq value t) "true")
   ((null value) "null")
   (t
    (user-error "Post-response JSON extraction must resolve to a scalar value"))))

(defun courier--json-path-segments (expr)
  "Return parsed JSON path segments for EXPR.
Supported paths look like `$.foo.bar'."
  (unless (and (stringp expr)
               (string-match-p "\\`\\$\\(?:\\.[A-Za-z_][A-Za-z0-9_-]*\\)+\\'" expr))
    (user-error "Unsupported JSON extract expression: %s" expr))
  (split-string (substring expr 2) "\\." t))

(defun courier--extract-json-response-var (response expr)
  "Extract a string value from RESPONSE JSON body using EXPR."
  (let* ((body (plist-get response :body-text))
         (data (condition-case err
                   (json-parse-string body
                                      :object-type 'alist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)
                 (error
                  (user-error "Failed to parse JSON response body: %s"
                              (error-message-string err)))))
         (value data))
    (dolist (segment (courier--json-path-segments expr))
      (unless (listp value)
        (user-error "JSON extract expression %s does not resolve" expr))
      (setq value (alist-get segment value nil nil #'string=))
      (when (null value)
        (user-error "JSON extract expression %s does not resolve" expr)))
    (courier--json-scalar-to-string value)))

(defun courier--extract-response-var (response rule)
  "Extract a runtime variable from RESPONSE using RULE."
  (pcase (plist-get rule :from)
    ('json
     (courier--extract-json-response-var response (plist-get rule :expr)))
    ('header
     (or (cdr (assoc-string (plist-get rule :expr)
                            (plist-get response :headers)
                            t))
         (user-error "Response header %s not found" (plist-get rule :expr))))
    ('status
     (number-to-string (or (plist-get response :status-code) 0)))
    (_
     (user-error "Unsupported post-response var source: %s"
                 (plist-get rule :from)))))

(defun courier--response-success-p (response)
  "Return non-nil when RESPONSE completed successfully."
  (let ((status (or (plist-get response :status-code) 0))
        (exit-code (or (plist-get response :exit-code) 0)))
    (and (= exit-code 0)
         (>= status 200)
         (< status 300))))

(defun courier--apply-post-response-vars (request response env-name)
  "Apply post-response vars from REQUEST to RESPONSE for ENV-NAME.
Return the updated RESPONSE plist."
  (let ((rules (plist-get request :post-response-vars)))
    (if (or (null rules)
            (not (courier--response-success-p response))
            (null (plist-get request :path)))
        response
      (let* ((collection-root (courier--collection-root (plist-get request :path)))
             (extracted nil))
        (dolist (rule rules)
          (push (cons (plist-get rule :name)
                      (courier--extract-response-var response rule))
                extracted))
        (setq extracted (nreverse extracted))
        (when collection-root
          (courier--write-runtime-vars
           collection-root
           env-name
           (courier-merge-vars
            (courier--read-runtime-vars collection-root env-name)
            extracted)))
        (plist-put response :post-response-vars extracted)))))

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
  "Return test result plists for RESPONSE.

TESTS supplies the assertions to evaluate."
  (mapcar (lambda (expr) (courier-run-test response expr)) tests))

;;;; HTTP transport

(defconst courier--write-out-template
  "%{http_code}\n%{size_download}\n%{time_total}\n"
  "Curl `--write-out' template used by Courier.")

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

Signal `user-error' when CHARSET is unsupported.  Let decoding failures
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
                  (split-string (buffer-string) "\n")))
         (status-raw (or (nth 0 lines) ""))
         (size-raw (or (nth 1 lines) ""))
         (seconds-raw (or (nth 2 lines) ""))
         (status (if (string-match-p "\\`[0-9]+\\'" status-raw)
                     (string-to-number status-raw)
                   0))
         (size (if (string-match-p "\\`[0-9]+\\'" size-raw)
                   (truncate (string-to-number size-raw))
                 0))
         (seconds (if (string-match-p
                       "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" seconds-raw)
                      (string-to-number seconds-raw)
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
        :request-body (make-temp-file "courier-" nil ".request-body")
        :meta (make-temp-file "courier-" nil ".meta")
        :stdout (generate-new-buffer " *courier-stdout*")
        :stderr (generate-new-buffer " *courier-stderr*")))

(defun courier--delete-temp-path (path)
  "Delete temp PATH if it exists."
  (when (and path (stringp path) (file-exists-p path))
    (delete-file path)))

;;;###autoload
(defun courier-build-curl-command (resolved-request header-file body-file meta-file
                                                    &optional request-body-file)
  "Build curl argv for RESOLVED-REQUEST.
Use HEADER-FILE, BODY-FILE, and META-FILE for curl output.

When REQUEST-BODY-FILE is non-nil, stage non-binary request bodies there so
transport failures do not leave the outgoing payload in BODY-FILE."
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
    (pcase (courier--request-body-type resolved-request)
      ('binary
       (when-let* ((path (plist-get resolved-request :body-file-path)))
         (setq command
               (append command (list "--data-binary" (format "@%s" path))))))
      ('multipart
       (dolist (part (plist-get resolved-request :body-parts))
         (setq command
               (append command
                       (list "-F"
                             (pcase (plist-get part :kind)
                               ('file
                                (concat (plist-get part :name)
                                        "=@"
                                        (plist-get part :path)
                                        (if-let* ((content-type
                                                   (plist-get part :content-type)))
                                            (format ";type=%s" content-type)
                                          "")))
                               (_
                                (concat (plist-get part :name)
                                        "="
                                        (or (plist-get part :value) "")))))))))
      (_
       (when-let* ((body (plist-get resolved-request :body)))
         (unless (string-empty-p body)
           (with-temp-file (or request-body-file body-file)
             (insert body))
           (setq command
                 (append command
                         (list "--data-binary"
                               (format "@%s" (or request-body-file body-file)))))))))
    (append command (list (plist-get resolved-request :url)))))

;;;###autoload
(defun courier-parse-response (header-file body-file meta-file stderr exit-code request)
  "Parse HEADER-FILE, BODY-FILE, META-FILE, STDERR, and EXIT-CODE for REQUEST."
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
  "Return a response plist for REQUEST.
Use BODY-FILE, EXIT-CODE, COMMAND, and ERR for error context."
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

(defun courier--oauth2-error-response (request token-response err)
  "Return an OAuth2 error response for REQUEST using TOKEN-RESPONSE and ERR."
  (list :request-path (plist-get request :path)
        :status-code (or (plist-get token-response :status-code) 0)
        :reason "OAuth2 Token Error"
        :headers (plist-get token-response :headers)
        :content-type (plist-get token-response :content-type)
        :charset (plist-get token-response :charset)
        :duration-ms (plist-get token-response :duration-ms)
        :size (plist-get token-response :size)
        :body-file (plist-get token-response :body-file)
        :body-text (plist-get token-response :body-text)
        :stderr (error-message-string err)
        :exit-code (or (plist-get token-response :exit-code) 0)
        :tests nil
        :command (plist-get token-response :command)))

(defun courier--response-processing-error-response (response command err)
  "Return RESPONSE annotated with a local processing error from ERR.
COMMAND becomes the recorded curl command for the returned response."
  (let ((updated (copy-tree response t)))
    (plist-put updated :reason "Response Processing Error")
    (plist-put updated :stderr (error-message-string err))
    (plist-put updated :tests nil)
    (plist-put updated :command command)))

(defun courier--request-preparation-error-response (request err)
  "Return a response plist for REQUEST preparation failure ERR."
  (list :request-path (plist-get request :path)
        :status-code 0
        :reason "Request Preparation Error"
        :headers nil
        :content-type nil
        :charset nil
        :duration-ms 0
        :size 0
        :body-file nil
        :body-text nil
        :stderr (error-message-string err)
        :exit-code 0
        :tests nil
        :command nil))

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
            (condition-case err
                (progn
                  (setq response
                        (courier--apply-post-response-vars
                         request
                         response
                         (plist-get request :env-name)))
                  (setq response
                        (courier--run-post-response-script
                         request
                         response
                         (plist-get request :env-vars)))
                  (plist-put response :tests
                             (courier-run-tests
                              response
                              (plist-get request :tests)))
                  (plist-put response :command command))
              (error
               (setq response
                     (courier--response-processing-error-response
                      response command err))))
            (when callback
              (funcall callback response)))
        (courier--delete-temp-path header-file)
        (courier--delete-temp-path meta-file)
        (courier--delete-temp-path
         (process-get process 'courier-request-body-file))
        (when (buffer-live-p stdout-buffer)
          (kill-buffer stdout-buffer))
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer))))))

(defun courier--send-resolved-request (resolved-request callback)
  "Send RESOLVED-REQUEST asynchronously and invoke CALLBACK with the response."
  (let* ((temps (courier--temp-files-for-request))
         (header-file (plist-get temps :header))
         (body-file (plist-get temps :body))
         (request-body-file (plist-get temps :request-body))
         (meta-file (plist-get temps :meta))
         (stdout-buffer (plist-get temps :stdout))
         (stderr-buffer (plist-get temps :stderr))
         (command (courier-build-curl-command
                   resolved-request
                   header-file
                   body-file
                   meta-file
                   request-body-file))
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
    (process-put process 'courier-request-body-file request-body-file)
    (process-put process 'courier-meta-file meta-file)
    (process-put process 'courier-temp-files (list header-file body-file meta-file))
    (process-put process 'courier-stdout-buffer stdout-buffer)
    (process-put process 'courier-stderr-buffer stderr-buffer)
    (process-put process 'courier-start-time (float-time))
    process))

(defvar-local courier--request nil
  "Resolved request plist associated with the current Courier response buffer.")

(defvar-local courier--process nil
  "In-flight curl process for the current Courier response buffer.")

(defvar-local courier--temp-files nil
  "Temp file paths associated with the current Courier response buffer.")

(defun courier--handoff-process-to-response-buffer (from-process to-process request)
  "Move response-buffer ownership from FROM-PROCESS to TO-PROCESS.

REQUEST becomes the active request associated with TO-PROCESS."
  (when (and (processp from-process)
             (processp to-process))
    (when-let* ((buffer (process-get from-process 'courier-response-buffer)))
    (process-put to-process 'courier-response-buffer buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq courier--request request
              courier--process to-process
              courier--temp-files (process-get to-process 'courier-temp-files)))))))

(defun courier--send-oauth2-request (resolved-request callback)
  "Send RESOLVED-REQUEST using OAuth2 client credentials, then invoke CALLBACK."
  (let* ((token-request (courier--oauth2-token-request resolved-request))
         token-process)
    (setq token-process
          (courier--send-resolved-request
           token-request
           (lambda (token-response)
             (condition-case err
                 (let* ((access-token (courier--oauth2-access-token token-response))
                        (main-request (copy-tree resolved-request t))
                        (headers (copy-tree (plist-get main-request :headers) t)))
                   (unless (assoc-string "authorization" headers nil)
                     (setq headers
                           (append headers
                                   (list (cons "authorization"
                                               (format "Bearer %s" access-token))))))
                   (plist-put main-request :headers headers)
                   (let ((main-process
                          (courier--send-resolved-request main-request callback)))
                     (courier--handoff-process-to-response-buffer
                      token-process main-process main-request)))
               (error
                (funcall callback
                         (courier--oauth2-error-response
                          resolved-request token-response err)))))))
    token-process))

;;;###autoload
(defun courier-send-request (resolved-request callback)
  "Send RESOLVED-REQUEST asynchronously and invoke CALLBACK with the response."
  (if (eq (plist-get (plist-get resolved-request :auth) :type) 'oauth2)
      (courier--send-oauth2-request resolved-request callback)
    (courier--send-resolved-request resolved-request callback)))

;;;###autoload
(defun courier-cancel-request (process)
  "Cancel Courier PROCESS and clean up its temp files."
  (when (process-live-p process)
    (process-put process 'courier-cancelled t)
    (delete-process process))
  (dolist (path (process-get process 'courier-temp-files))
    (courier--delete-temp-path path))
  (courier--delete-temp-path (process-get process 'courier-request-body-file))
  (when-let* ((buffer (process-get process 'courier-response-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (courier--response-mark-cancelled)))))

(defun courier--collection-name (&optional start)
  "Return a display name for the current collection around START."
  (if-let* ((config (courier--collection-config start))
            (name (plist-get config :name)))
      name
    (if-let* ((root (courier--collection-root start)))
        (file-name-nondirectory
         (directory-file-name root))
      "Courier")))

;;;; Response mode

(defvar-local courier--response nil
  "Response plist displayed in the current Courier response buffer.")

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

(defvar-local courier--response-content-start nil
  "Marker for the start of the current response buffer content region.")

(defvar-local courier--timeline-details-start nil
  "Marker for the start of the expanded Courier timeline details block.")

(defvar-local courier--timeline-details-end nil
  "Marker for the end of the expanded Courier timeline details block.")

(defun courier--response-content-start-position ()
  "Return the current response content start position."
  (if (markerp courier--response-content-start)
      (marker-position courier--response-content-start)
    (point-min)))

(defun courier--response-layout-ready-p ()
  "Return non-nil when the current response buffer has layout markers."
  (and (markerp courier--response-content-start)
       (markerp courier--timeline-details-start)
       (markerp courier--timeline-details-end)))

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

(defun courier--document-content-type-p (content-type)
  "Return non-nil when CONTENT-TYPE identifies a document body."
  (and content-type
       (string-match-p "\\`application/pdf\\'" content-type)))

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
     ((courier--document-content-type-p content-type) 'document)
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

(defun courier--response-display-stderr (response)
  "Return RESPONSE stderr trimmed for user-facing display."
  (let ((stderr (string-trim (or (plist-get response :stderr) ""))))
    (setq stderr
          (replace-regexp-in-string
           "\n*Process [^\n]+ stderr finished\\'" ""
           stderr))
    (cond
     ((string-match
       (rx string-start "curl: (" (+ digit) ") " (group (* anychar)) string-end)
       stderr)
      (match-string 1 stderr))
     ((string-prefix-p "curl: " stderr)
      (string-remove-prefix "curl: " stderr))
     (t
      stderr))))

(defun courier--response-error-hint (response)
  "Return a user-facing hint for RESPONSE transport failures, or nil."
  (let ((stderr (downcase (courier--response-display-stderr response))))
    (when (or (string-match-p "tlsv1 alert protocol version" stderr)
              (string-match-p "wrong version number" stderr)
              (string-match-p "server gave http response to https client" stderr))
      "TLS handshake failed. This often means the URL uses https:// but the target port only serves http://.\n")))

(defconst courier--response-local-error-prefixes
  '(("OAuth2 Token Error" . "OAuth2 token error")
    ("Request Preparation Error" . "Request preparation error")
    ("Response Processing Error" . "Response processing error")
    ("Parse Error" . "Parse error"))
  "User-facing message prefixes for local Courier response errors.")

(defun courier--response-local-error-prefix (reason)
  "Return the message prefix for local response error REASON, or nil."
  (cdr (assoc-string reason courier--response-local-error-prefixes t)))

(defun courier--response-display-error (response)
  "Return a user-facing error string for RESPONSE, or nil."
  (let* ((status (or (plist-get response :status-code) 0))
         (exit-code (or (plist-get response :exit-code) 0))
         (reason (string-trim (or (plist-get response :reason) "")))
         (stderr (courier--response-display-stderr response))
         (hint (courier--response-error-hint response))
         (prefix (courier--response-local-error-prefix reason)))
    (cond
     ((or (string-empty-p stderr)
          (and (= status 0) (= exit-code 0) (string-empty-p reason)))
      nil)
     (prefix
      (format "%s: %s\n" prefix stderr))
     ((and (= status 0) (/= exit-code 0))
      (concat
       (format "Request error: %s\n" stderr)
        hint))
     (t
      nil))))

(defun courier--response-reason (response)
  "Return the best available reason phrase for RESPONSE."
  (let* ((status (or (plist-get response :status-code) 0))
         (exit-code (or (plist-get response :exit-code) 0))
         (reason (string-trim (or (plist-get response :reason) ""))))
    (cond
     ((not (string-empty-p reason))
      reason)
     ((and (= status 0) (/= exit-code 0))
      "Request Error")
     (t
      (or (alist-get status courier--status-reason-phrases) "")))))

(defun courier--response-status-label (response)
  "Return a status label for RESPONSE."
  (let* ((status (or (plist-get response :status-code) 0))
         (reason (courier--response-reason response)))
    (if (string-empty-p reason)
        (number-to-string status)
      (if (= status 0)
          reason
        (format "%d %s" status reason)))))

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
  (let ((status (or (plist-get response :status-code) 0))
        (exit-code (or (plist-get response :exit-code) 0))
        (reason (string-trim (or (plist-get response :reason) ""))))
    (cond
     ((or (/= exit-code 0)
          (courier--response-local-error-prefix reason))
      'courier-response-status-error-face)
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

(defun courier--response-tab-count (tab response)
  "Return a count for response TAB using RESPONSE when appropriate."
  (pcase tab
    ('headers
     (length (plist-get response :headers)))
    ('tests
     (length (plist-get response :tests)))
    (_
     nil)))

(defun courier--label-with-count (label count)
  "Return LABEL decorated with COUNT when COUNT is positive."
  (if (and count (> count 0))
      (concat label
              (propertize "(" 'face 'default)
              (propertize (number-to-string count)
                          'face 'courier-response-tab-count-face)
              (propertize ")" 'face 'default))
    label))

(defun courier--response-tab-label (tab response)
  "Return the display text for response TAB using RESPONSE."
  (courier--label-with-count
   (courier--response-tab-display-name tab)
   (courier--response-tab-count tab response)))

(defun courier--response-view-state-label (response)
  "Return the current response view label for RESPONSE."
  (let* ((tab (or courier--response-tab 'response))
         (label (courier--response-tab-label tab response)))
    (propertize label
                'face 'courier-response-tab-active-face)))

(defun courier--ensure-response-layout ()
  "Ensure the current Courier response buffer has content markers."
  (unless (markerp courier--response-content-start)
    (setq courier--response-content-start (copy-marker (point-min))))
  (unless (markerp courier--timeline-details-start)
    (setq courier--timeline-details-start (copy-marker (point-min))))
  (unless (markerp courier--timeline-details-end)
    (setq courier--timeline-details-end (copy-marker (point-min))))
  (set-marker courier--response-content-start (point-min)))

(defun courier--replace-response-content ()
  "Replace the response content region in the current buffer."
  (courier--ensure-response-layout)
  (let ((inhibit-read-only t))
    (delete-region (marker-position courier--response-content-start) (point-max))
    (goto-char (marker-position courier--response-content-start))
    (set-marker courier--timeline-details-start (point-min))
    (set-marker courier--timeline-details-end (point-min))
    (courier--insert-response-tab courier--response courier--request)))

(defun courier--capture-response-window-state ()
  "Capture point and window offsets for the current response buffer."
  (let ((content-start (courier--response-content-start-position))
        (buffer (current-buffer)))
    (list
     :point-offset (max 0 (- (point) content-start))
     :windows
     (mapcar
      (lambda (window)
        (list window
              (max 0 (- (window-start window) content-start))
              (max 0 (- (window-point window) content-start))))
      (get-buffer-window-list buffer nil t)))))

(defun courier--restore-response-window-state (state)
  "Restore response buffer point and window STATE."
  (let* ((content-start (courier--response-content-start-position))
         (point-offset (plist-get state :point-offset))
         (windows (plist-get state :windows)))
    (goto-char (min (point-max) (+ content-start point-offset)))
    (dolist (entry windows)
      (pcase-let ((`(,window ,start-offset ,point-offset) entry))
        (when (window-live-p window)
          (set-window-start window
                            (min (point-max) (+ content-start start-offset))
                            t)
          (set-window-point window
                            (min (point-max) (+ content-start point-offset))))))))

(defun courier--call-preserving-response-window-state (fn)
  "Call FN while preserving response buffer point and window state."
  (let ((state (courier--capture-response-window-state)))
    (funcall fn)
    (courier--restore-response-window-state state)))

(defun courier--reset-response-window-state ()
  "Reset point and window start positions.
Move them to the top of the current response buffer."
  (goto-char (point-min))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when (window-live-p window)
      (set-window-start window (point-min) t)
      (set-window-point window (point-min)))))

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

(defun courier--response-summary-string (response)
  "Return the status, duration, and size summary string for RESPONSE."
  (concat
   (propertize (courier--response-status-label response)
               'face (courier--response-status-face response))
   courier--header-separator
   (courier--human-readable-duration (plist-get response :duration-ms))
   courier--header-separator
   (courier--human-readable-size (or (plist-get response :size) 0))))

(defun courier--response-history-indicator (timestamp index total)
  "Return a propertized history indicator for TIMESTAMP, INDEX, and TOTAL."
  (propertize (format "[%s %d/%d]" timestamp (1+ index) total)
              'face 'shadow))

(defun courier--response-header-line-with-history (response timestamp index total)
  "Return a history header line for RESPONSE at TIMESTAMP, INDEX, and TOTAL."
  (concat
   (courier--response-summary-string response)
   courier--header-separator
   (courier--response-history-indicator timestamp index total)))

(defun courier--header-summary-response ()
  "Return the response that should drive the current header summary."
  (if (and (eq courier--response-tab 'timeline)
           (numberp courier--history-index))
      (or (courier--current-history-response)
          courier--response)
    courier--response))

(defun courier--response-header-line-format (response)
  "Return the complete header line format for RESPONSE."
  (let* ((summary-response (or (courier--header-summary-response)
                               response))
         (view-label
          (courier--response-view-state-label summary-response))
         (summary
          (if (and (eq courier--response-tab 'timeline)
                   (numberp courier--history-index))
              (let* ((entry (courier--current-history-entry))
                     (timestamp (car entry)))
                (courier--response-header-line-with-history
                 summary-response timestamp courier--history-index
                 (length courier--history)))
            (courier--response-summary-string summary-response))))
    (concat " "
            (propertize "View:" 'face 'shadow)
            " "
            view-label
            "  "
            summary)))

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
      ((and (fboundp 'json-ts-mode)
            (fboundp 'treesit-ready-p)
            (treesit-ready-p 'json t))
       #'json-ts-mode)
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
          (or (courier--response-display-error response)
              (pcase view
                ('json
                 (if-let* ((body-text (plist-get response :body-text)))
                     (condition-case err
                         (concat (courier--pretty-json-body body-text) "\n")
                       (error
                        (concat
                         (format "[Invalid JSON body: %s]\n"
                                 (error-message-string err))
                         body-text
                         "\n")))
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
                ('document
                 (if-let* ((body-file (plist-get response :body-file)))
                     (format "Document body saved to %s\n" body-file)
                   "(empty)\n"))
                (_
                 "(empty)\n")))))
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
               (or (plist-get request :method)
                   (courier--response-request-method response))
               (or (plist-get request :url)
                   (courier--response-request-url response)))))))

(defun courier--insert-timeline-heading (title &optional count)
  "Insert timeline section heading TITLE with optional COUNT."
  (insert (propertize (courier--label-with-count title count)
                      'face 'courier-response-timeline-heading-face))
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
         (method (courier--response-request-method response))
         (url (courier--response-request-url response))
         (status-face (courier--response-status-face response))
         (summary-face (if selectedp
                           'courier-response-timeline-selected-face
                         'courier-response-timeline-entry-face)))
    (if selectedp
        (concat
         (propertize method
                     'face `(courier-response-method-face ,summary-face))
         (propertize " " 'face summary-face)
         (propertize url
                     'face `(courier-response-url-face ,summary-face))
         "\n")
      (concat
       (propertize status 'face `(,status-face ,summary-face))
      (propertize (format "  %s" timestamp)
                  'face `(shadow ,summary-face))
       "\n"
       (propertize method
                   'face `(courier-response-method-face ,summary-face))
       (propertize " " 'face summary-face)
       (propertize url
                   'face `(courier-response-url-face ,summary-face))
       "\n"
       (propertize (concat duration courier--header-separator size)
                   'face `(shadow ,summary-face))
       "\n"))))

(defun courier--response-request-method (response)
  "Return the request method associated with RESPONSE."
  (or (plist-get response :request-method)
      (plist-get courier--request :method)
      ""))

(defun courier--response-request-url (response)
  "Return the request URL associated with RESPONSE."
  (or (plist-get response :request-url)
      (plist-get courier--request :url)
      ""))

(defun courier--apply-string-face-properties (string buffer-start)
  "Copy face properties from STRING into the current buffer at BUFFER-START."
  (let ((position 0)
        (length (length string)))
    (while (< position length)
      (let* ((next (or (next-single-property-change position 'face string)
                       length))
             (face (get-text-property position 'face string)))
        (when face
          (put-text-property (+ buffer-start position)
                             (+ buffer-start next)
                             'face
                             face))
        (setq position next)))))

(defun courier--insert-response-body (response)
  "Insert the active response body for RESPONSE."
  (let ((view (courier--effective-body-view response))
        (body-file (plist-get response :body-file)))
    (pcase view
      ('image
       (cond
        ((and body-file (display-images-p))
         (condition-case err
             (progn
               (insert-image (create-image body-file))
               (insert "\n\n")
               (insert body-file "\n"))
           (error
            (insert (format "Image rendering failed: %s\n" (error-message-string err)))
            (insert (format "Image body saved to %s\n" body-file)))))
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
  (let ((headers (plist-get request :headers)))
    (courier--insert-timeline-heading "Headers" (length headers))
    (if headers
        (insert (courier--format-headers (list :headers headers)))
      (insert "No Headers found\n")))
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
  (let ((headers (plist-get response :headers)))
    (courier--insert-timeline-heading "Headers" (length headers))
    (if headers
      (insert (courier--format-headers (list :headers headers)))
      (insert "No Headers found\n")))
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

(defun courier--replace-selected-timeline-details ()
  "Replace only the expanded details block for the selected timeline entry."
  (unless (and (eq courier--response-tab 'timeline)
               (numberp courier--history-index)
               (markerp courier--timeline-details-start)
               (markerp courier--timeline-details-end))
    (user-error "No expanded Courier timeline details are available"))
  (let* ((history-entry (nth courier--history-index courier--history))
         (response (cdr history-entry))
         (request (or (plist-get response :request-snapshot)
                      courier--request))
         (timestamp (car history-entry)))
    (courier--call-preserving-response-window-state
     (lambda ()
       (let ((inhibit-read-only t))
         (delete-region (marker-position courier--timeline-details-start)
                        (marker-position courier--timeline-details-end))
         (goto-char (marker-position courier--timeline-details-start))
         (courier--insert-expanded-timeline-details response request timestamp)
         (insert "\n")
         (set-marker courier--timeline-details-end (point))
         (setq header-line-format
               (courier--response-header-line-format courier--response)))))))

(defun courier--insert-response-timeline ()
  "Insert the response timeline for the current request history."
  (set-marker courier--timeline-details-start (point-min))
  (set-marker courier--timeline-details-end (point-min))
  (if (null courier--history)
      (insert "No history yet.\n")
    (cl-loop for history-entry in courier--history
             for index from 0
             for response = (cdr history-entry)
             for request = (or (plist-get response :request-snapshot)
                               courier--request)
             for timestamp = (car history-entry)
             do
             (let* ((start (point))
                    (text (courier--response-timeline-entry-line
                           index timestamp response)))
               (insert text)
               (make-text-button
                start (point)
                'action #'courier--timeline-entry-button-action
               'mouse-face 'highlight
               'follow-link t
               'courier-history-index index
                'help-echo "RET, TAB, or mouse-1 to expand or collapse this response history entry")
               (put-text-property
                start (point) 'face
                (if (and (numberp courier--history-index)
                         (= index courier--history-index))
                    'courier-response-timeline-selected-face
                  'courier-response-timeline-entry-face))
               (courier--apply-string-face-properties text start))
             (insert "\n")
             (when (and (numberp courier--history-index)
                        (= index courier--history-index))
               (set-marker courier--timeline-details-start (point))
               (courier--insert-expanded-timeline-details response request timestamp)
               (insert "\n")
               (set-marker courier--timeline-details-end (point))))))

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
    (setq header-line-format
          (courier--response-header-line-format courier--response))
    (courier--replace-response-content)
    (courier--reset-response-window-state)))

(defun courier--refresh-response-content-only ()
  "Refresh only the current response content region."
  (if (courier--response-layout-ready-p)
      (courier--call-preserving-response-window-state
       (lambda ()
         (setq header-line-format
               (courier--response-header-line-format courier--response))
         (courier--replace-response-content)))
    (courier--refresh-response-display)))

(defun courier--refresh-for-response-body-change ()
  "Refresh after changing the active response body presentation."
  (if (eq courier--response-tab 'response)
      (courier--refresh-response-content-only)
    (progn
      (courier--set-response-tab-local 'response)
      (courier--refresh-response-display))))

(defun courier--history-push (response)
  "Push RESPONSE onto the history for the current response buffer."
  (push (cons (format-time-string "%Y-%m-%d %H:%M:%S") response)
        courier--history)
  (let ((max (max 0 courier-history-max)))
    (when (> (length courier--history) max)
      (let ((dropped (nthcdr max courier--history)))
        (setq courier--history (seq-take courier--history max))
        (dolist (entry dropped)
          (when-let* ((body-file (plist-get (cdr entry) :body-file)))
            (courier--delete-temp-path body-file))))))
  (setq courier--history-index nil))

(defun courier--history-render-current ()
  "Render the response at `courier--history-index'."
  (unless courier--history
    (user-error "No response history"))
  (courier--refresh-response-content-only))

(defun courier--response-cleanup ()
  "Clean up process and temp files for the current response buffer."
  (when (and (processp courier--process)
             (process-live-p courier--process))
    (courier-cancel-request courier--process))
  (dolist (path courier--temp-files)
    (when (and path (stringp path) (file-exists-p path))
      (delete-file path)))
  (dolist (entry courier--history)
    (when-let* ((body-file (plist-get (cdr entry) :body-file)))
      (courier--delete-temp-path body-file)))
  (when-let* ((body-file (plist-get courier--response :body-file)))
    (courier--delete-temp-path body-file)))

(define-derived-mode courier--response-mode special-mode "Courier-Response"
  "Major mode used for Courier response buffers."
  (setq-local truncate-lines nil)
  (unless (markerp courier--response-content-start)
    (setq-local courier--response-content-start (copy-marker (point-min))))
  (unless (markerp courier--timeline-details-start)
    (setq-local courier--timeline-details-start (copy-marker (point-min))))
  (unless (markerp courier--timeline-details-end)
    (setq-local courier--timeline-details-end (copy-marker (point-min))))
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
  (courier--refresh-for-response-body-change))

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
  (courier--refresh-for-response-body-change))

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
  (if (numberp courier--history-index)
      (courier--replace-selected-timeline-details)
    (courier--refresh-response-content-only))
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

(defun courier--response-jump-choices ()
  "Return completion choices for response tabs."
  (mapcar (lambda (tab)
            (cons
             (format "%s -- %s"
                     (courier--response-tab-display-name tab)
                     (pcase tab
                       ('response "view response body")
                       ('headers "view response headers")
                       ('timeline "inspect previous runs")
                       ('tests "view test results")
                       (_ "view response")))
             tab))
          courier--response-tabs))

;;;###autoload
(defun courier-response-jump-tab (tab)
  "Jump directly to response TAB."
  (interactive
   (let* ((choices (courier--response-jump-choices))
          (current-tab (or courier--response-tab 'response))
          (current
           (car (rassoc current-tab choices)))
          (selection (completing-read "Response view: "
                                      choices nil t nil nil current)))
     (list (cdr (assoc selection choices)))))
  (unless courier--response
    (user-error "No Courier response is available"))
  (courier--set-response-tab tab))

(defun courier-response-view-response ()
  "Switch to the Response view."
  (interactive)
  (courier--set-response-tab 'response))

(defun courier-response-view-headers ()
  "Switch to the Headers view."
  (interactive)
  (courier--set-response-tab 'headers))

(defun courier-response-view-timeline ()
  "Switch to the Timeline view."
  (interactive)
  (courier--set-response-tab 'timeline))

(defun courier-response-view-tests ()
  "Switch to the test results view."
  (interactive)
  (courier--set-response-tab 'tests))

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
  "Move point to the first button for which PREDICATE is non-nil."
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
  (courier--refresh-response-content-only))

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
         (body-file (plist-get response :body-file))
         (display-target buffer))
    (pcase view
      ('document
       (if (null body-file)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (special-mode)
               (insert "(empty)\n")))
         (setq display-target (find-file-noselect body-file))
         (with-current-buffer display-target
           (when (and (courier--document-content-type-p
                       (plist-get response :content-type))
                      (fboundp 'doc-view-mode))
             (doc-view-mode))
           (read-only-mode 1))))
      (_
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (erase-buffer)
           (pcase view
             ('image
              (special-mode)
              (cond
               ((null body-file)
                (insert "(empty)\n"))
               ((not (display-images-p))
                (insert "[Image display is unavailable in this Emacs session]\n\n")
                (insert (format "%s\n" body-file)))
               (t
                (condition-case err
                    (let ((image (create-image body-file)))
                      (insert-image image)
                      (insert (format "\n\n%s\n" body-file)))
                  (error
                   (insert (format "[Failed to render image: %s]\n\n%s\n"
                                   (error-message-string err)
                                   body-file)))))))
             (_
              (setq-local courier--response response)
              (setq-local courier--request request)
              (setq-local courier--body-view view)
              (insert (courier--format-body response))
              (funcall (courier--body-view-major-mode view))
              (setq buffer-read-only t)))))))
    (display-buffer display-target)))

(define-key courier--response-mode-map (kbd "RET") #'courier-response-activate)
(define-key courier--response-mode-map (kbd "<return>") #'courier-response-activate)
(define-key courier--response-mode-map (kbd "TAB") #'courier-response-context-tab)
(define-key courier--response-mode-map (kbd "<tab>") #'courier-response-context-tab)
(define-key courier--response-mode-map (kbd "C-c ?") #'courier-dispatch)
(define-key courier--response-mode-map (kbd "o") #'courier-response-open-body)
(define-key courier--response-mode-map (kbd "C-c C-j") #'courier-response-jump-tab)
(define-key courier--response-mode-map (kbd "g") #'courier-response-retry)
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

(defvar-local courier--request-divider-overlay nil
  "Overlay highlighting the current Courier request divider line.")

(defvar-local courier--request-divider-label-overlay nil
  "Overlay highlighting the active request section label in the divider.")

(defvar-local courier--params-source-buffer nil
  "Request buffer associated with the current Courier params editor.")

(defvar-local courier--request-model nil
  "Parsed request model backing the current Courier request buffer.")

(defvar-local courier--request-tab 'body
  "Current Courier request section shown in the active request buffer.")

(defvar-local courier--request-rendering-p nil
  "Non-nil while the current Courier request buffer is being re-rendered.")

(defvar-local courier--request-content-start nil
  "Marker for the start of the editable Courier request content region.")

(defvar-local courier--request-divider-width nil
  "Last rendered width for the current request divider line.")

(put 'courier--request-path 'permanent-local t)
(put 'courier--active-env 'permanent-local t)
(put 'courier--collection-root-hint 'permanent-local t)
(put 'courier--request-model 'permanent-local t)
(put 'courier--request-tab 'permanent-local t)
(put 'courier--request-content-start 'permanent-local t)

(defconst courier--request-primary-sections
  '(params body headers)
  "Primary navigation sections shown in Courier request buffers.")

(defconst courier--request-secondary-sections
  '(auth vars script tests)
  "Secondary request sections shown through the section jump command.")

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

(defface courier-request-nav-active-face
  '((t :inherit bold))
  "Face used for the active Courier request navigation section.")

(defface courier-request-divider-face
  '((t :inherit shadow))
  "Face used for the subtle divider between request header and editor area.")

(defconst courier--request-method-read-only-message
  "Courier manages the HTTP method token. Use C-c C-m to change it."
  "Read-only message shown when editing the request method token.")

(defconst courier--request-divider-read-only-message
  "Courier manages the request divider."
  "Read-only message shown when editing the request divider.")

(defconst courier-request-font-lock-keywords
  '(("^#.*$" . font-lock-comment-face)
    ("^\\([A-Z]+\\)\\s-+\\(\\S-+.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^\\([[:alnum:]_][^: \t]*\\):\\s-*\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face))
    ("^\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face))
    ("{{[^{}[:space:]]+}}"
     . font-lock-variable-name-face))
  "Font-lock keywords for `courier-request-mode'.")

(defconst courier-env-font-lock-keywords
  '(("^#.*$" . font-lock-comment-face)
    ("^\\([A-Za-z_][A-Za-z0-9_]*\\)\\(=\\)\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 'shadow)
     (3 font-lock-string-face)))
  "Font-lock keywords for `courier-env-mode'.")

(defun courier--mode-line-lighter ()
  "Return the mode-line lighter for `courier-request-mode'."
  (if courier--active-env
      (format "Courier[%s]" courier--active-env)
    "Courier"))

(defun courier--request-section-label (section)
  "Return the display label for request SECTION."
  (pcase section
    ('url "URL")
    ('headers "Headers")
    ('body (format "Body: %s"
                   (courier--body-type-label
                    (courier--request-body-type courier--request-model))))
    ('params "Parameters")
    ('auth (format "Authentication: %s"
                   (courier--auth-type-label
                    (courier--request-auth-type courier--request-model))))
    ('vars "Variables")
    ('script "Scripts")
    ('tests "Tests")
    (_ (capitalize (symbol-name section)))))

(defun courier--request-section-description (section)
  "Return a short description for request SECTION."
  (pcase section
    ('params "edit URL query parameters")
    ('body "edit request body")
    ('headers "edit HTTP headers")
    ('auth "edit authentication settings")
    ('vars "edit request and response vars")
    ('script "edit pre and post scripts")
    ('tests "edit assertions")
    (_ "edit section")))

(defun courier--request-jump-choices ()
  "Return an alist of request section jump choices."
  (mapcar (lambda (item)
            (cons (format "%s -- %s"
                          (courier--request-section-label item)
                          (courier--request-section-description item))
                  item))
          (append courier--request-primary-sections
                  courier--request-secondary-sections)))

(defun courier--request-all-sections ()
  "Return all request sections in display order."
  (append courier--request-primary-sections
          courier--request-secondary-sections))

(defun courier--request-current-section ()
  "Return the logical request section at point."
  (or courier--request-tab 'body))

(defun courier--request-effective-url (request)
  "Return REQUEST URL with structured params projected into the query string."
  (let ((url (or (plist-get request :url) "")))
    (if (string-empty-p url)
        ""
      (courier--update-request-url-query url (plist-get request :params)))))

(defun courier--request-display-name (path)
  "Return the display name for request PATH."
  (let ((request (if-let* ((buffer (courier--request-buffer-for-path path)))
                     (with-current-buffer buffer
                       (courier--sync-request-model)
                       (copy-tree courier--request-model t))
                   (courier-parse-file path))))
    (or (plist-get request :name)
        (file-name-base path))))

(defun courier--request-view-request-line (request)
  "Return the request line string for REQUEST."
  (format "%s %s"
          (or (plist-get request :method) courier-default-request-method)
          (courier--request-effective-url request)))

(defun courier--request-divider-target-width ()
  "Return the target width for the current request divider line."
  (or (when-let* ((window (get-buffer-window (current-buffer) t)))
        (window-body-width window))
      44))

(defun courier--request-divider-line ()
  "Return the divider line showing the active request section."
  (let* ((label (courier--request-section-label
                 (courier--request-current-section)))
         (target-width (max (+ (string-width label) 4)
                            (courier--request-divider-target-width)))
         (padding-width (- target-width (string-width label) 2))
         (left-width (max 1
                          (min 12
                               (/ padding-width 4))))
         (right-width (max 1 (- padding-width left-width)))
         (left-padding (make-string left-width ?-))
         (right-padding (make-string right-width ?-)))
    (concat (propertize left-padding 'face 'courier-request-divider-face)
            " "
            (propertize label 'face 'courier-request-nav-active-face)
            " "
            (propertize right-padding 'face 'courier-request-divider-face))))

(defun courier--set-request-read-only-region (start end message)
  "Protect buffer region from START to END with read-only MESSAGE."
  (when (< start end)
    (let ((inhibit-read-only t))
      (add-text-properties
       start end
       `(read-only ,message
         front-sticky (read-only)
         rear-nonsticky (read-only)
         help-echo ,message)))))

(defun courier--protect-request-method-prefix ()
  "Protect the request method token in the current buffer."
  (when-let* ((bounds (courier--current-method-bounds)))
    (pcase-let ((`(,start . ,method-end) bounds))
      (save-excursion
        (goto-char method-end)
        (courier--set-request-read-only-region
         start
         (if (eq (char-after) ?\s)
             (1+ method-end)
           method-end)
         courier--request-method-read-only-message)))))

(defun courier--protect-request-divider-region ()
  "Protect the request divider and spacer region in the current buffer."
  (when (markerp courier--request-content-start)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (courier--set-request-read-only-region
       (line-beginning-position)
       (marker-position courier--request-content-start)
       courier--request-divider-read-only-message))))

(defun courier--protect-request-scaffold ()
  "Protect non-editable request scaffold lines in the current buffer."
  (courier--protect-request-method-prefix)
  (courier--protect-request-divider-region))

(defun courier--delete-request-divider-overlays ()
  "Delete the current request divider overlays."
  (dolist (overlay (list courier--request-divider-overlay
                         courier--request-divider-label-overlay))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq courier--request-divider-overlay nil
        courier--request-divider-label-overlay nil))

(defun courier--refresh-request-divider-overlays ()
  "Refresh the request divider overlays in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-start line-end))
           (label (courier--request-section-label
                   (courier--request-current-section)))
           (label-index (string-match (regexp-quote label) line))
           (divider-overlay
            (or courier--request-divider-overlay
                (make-overlay line-start line-end nil t t))))
      (setq courier--request-divider-overlay divider-overlay)
      (move-overlay divider-overlay line-start line-end)
      (overlay-put divider-overlay 'evaporate t)
      (overlay-put divider-overlay 'priority 900)
      (overlay-put divider-overlay 'face 'courier-request-divider-face)
      (if label-index
          (let* ((label-start (+ line-start label-index))
                 (label-end (+ label-start (length label)))
                 (label-overlay
                  (or courier--request-divider-label-overlay
                      (make-overlay label-start label-end nil t t))))
            (setq courier--request-divider-label-overlay label-overlay)
            (move-overlay label-overlay label-start label-end)
            (overlay-put label-overlay 'evaporate t)
            (overlay-put label-overlay 'priority 901)
            (overlay-put label-overlay 'face 'courier-request-nav-active-face))
        (when (overlayp courier--request-divider-label-overlay)
          (delete-overlay courier--request-divider-label-overlay)
          (setq courier--request-divider-label-overlay nil))))))

(defun courier--refresh-request-divider-line ()
  "Refresh the request divider line in the current request buffer."
  (let ((divider (courier--request-divider-line))
        (inhibit-read-only t))
    (setq courier--request-divider-width (string-width divider))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (line-beginning-position) (line-end-position))
      (insert divider)))
  (courier--refresh-request-divider-overlays)
  (courier--protect-request-divider-region))

(defun courier--refresh-request-divider-after-window-change ()
  "Refresh the request divider after a current buffer window resize."
  (when (and (derived-mode-p 'courier-request-mode)
             (not courier--request-rendering-p)
             (get-buffer-window (current-buffer) t))
    (let ((target-width (courier--request-divider-target-width)))
      (unless (= target-width courier--request-divider-width)
        (let ((modified (buffer-modified-p)))
          (courier--refresh-request-divider-line)
          (set-buffer-modified-p modified))))))

(defun courier--request-section-placeholder (section)
  "Return placeholder text for empty request SECTION."
  (pcase section
    ('params
     (concat "# Query params\n"
             "# page = 1\n"
             "# per_page = 20\n"
             "# include = profile,roles\n"))
    ('headers "# No headers.\n")
    ('body
     (pcase (courier--request-body-type courier--request-model)
       ('none "# No body.\n")
       ('binary
        (concat "# [body]\n"
                "# type = \"binary\"\n"
                "# path = \"./payload.bin\"\n"
                "# content_type = \"application/octet-stream\"\n"))
       ('multipart
        (concat "# [body]\n"
                "# type = \"multipart\"\n"
                "\n"
                "# [[body.parts]]\n"
                "# name = \"avatar\"\n"
                "# kind = \"file\"\n"
                "# path = \"./avatar.png\"\n"
                "# content_type = \"image/png\"\n"))
       (_ "")))
    ('auth
     (concat "# [auth]\n"
             "# type = \"bearer\"\n"
             "# token = \"{{token}}\"\n"))
    ('vars
     (concat "# Request Vars\n"
             "# [vars]\n"
             "# token = \"courier-pigeon-token\"\n"
             "# tenant = \"night-shift\"\n"
             "\n"
             "# Pre-request Vars\n"
             "# [vars.pre_request]\n"
             "# request_id = \"courier-42\"\n"
             "\n"
             "# Post-response Vars\n"
             "# [[vars.post_response]]\n"
             "# name = \"session_token\"\n"
             "# from = \"json\"\n"
             "# expr = \"$.data.token\"\n"))
    ('script
     (concat "# Pre-request Script\n"
             "# pre_request = \"\"\"\n"
             "# (message \"before request\")\n"
             "# \"\"\"\n"
             "\n"
             "# Post-response Script\n"
             "# post_response = \"\"\"\n"
             "# (message \"after response\")\n"
             "# \"\"\"\n"))
    ('tests
     (concat "# tests = [\n"
             "#   \"status == 200\",\n"
             "#   \"header content-type contains json\",\n"
             "# ]\n"))
    (_ "")))

(defun courier--format-request-kv-lines (pairs)
  "Return PAIRS formatted as `key = value' lines."
  (if pairs
      (mapconcat (lambda (pair)
                   (format "%s = %s" (car pair) (cdr pair)))
                 pairs
                 "\n")
    ""))

(defun courier--request-effective-params (request)
  "Return the effective params for REQUEST.

Explicit Courier params win.  When no explicit params exist, fall back to the
request URL query string."
  (or (plist-get request :params)
      (courier--parse-url-query-params (or (plist-get request :url) ""))))

(defun courier--request-auth-lines (request)
  "Return editable TOML fragment for REQUEST auth."
  (string-join (courier--serialize-front-matter-auth-sections request) "\n\n"))

(defun courier--reset-request-buffer-state-for-path (path)
  "Reset request-local editor state when visiting a new request PATH."
  (unless (equal courier--request-path path)
    (setq-local courier--request-model nil)
    (setq-local courier--request-tab nil)
    (setq-local courier--request-content-start nil)
    (setq-local courier--active-env nil)
    (setq-local courier--collection-root-hint nil)))

(defun courier--request-tests-lines (request)
  "Return editable TOML fragment for REQUEST test rules."
  (let ((tests-request (courier--empty-request)))
    (plist-put tests-request :tests (plist-get request :tests))
    (string-join
     (courier--serialize-front-matter-root-sections tests-request)
     "\n\n")))

(defun courier--request-body-toml-fragment (request keys)
  "Return TOML fragment for body-related KEYS of REQUEST."
  (let ((body-request (courier--empty-request)))
    (dolist (key keys)
      (plist-put body-request key (plist-get request key)))
    (string-join
     (courier--serialize-front-matter-body-sections body-request)
     "\n\n")))

(defun courier--request-binary-body-lines (request)
  "Return editable text for binary body REQUEST metadata."
  (courier--request-body-toml-fragment
   request '(:body-type :body-file-path :body-file-content-type)))

(defun courier--request-multipart-body-lines (request)
  "Return editable text for multipart REQUEST body parts."
  (courier--request-body-toml-fragment
   request '(:body-type :body-parts)))

(defun courier--request-body-section-text (request)
  "Return editable text for REQUEST body section."
  (pcase (courier--request-body-type request)
    ('form-urlencoded
     (courier--format-request-kv-lines
      (courier--form-body-pairs-from-string (plist-get request :body))))
    ('binary
     (courier--request-binary-body-lines request))
    ('multipart
     (courier--request-multipart-body-lines request))
    ('none "")
    (_
     (or (plist-get request :body) ""))))

(defun courier--request-apply-plist (request props)
  "Apply PROPS plist entries onto REQUEST and return REQUEST."
  (while props
    (plist-put request (pop props) (pop props)))
  request)

(defun courier--parse-request-body-fragment (text expected-type)
  "Parse body section TEXT and require [body].type to equal EXPECTED-TYPE."
  (let* ((trimmed (string-trim (or text "")))
         (request (if (or (string-empty-p trimmed)
                          (courier--comment-only-section-p trimmed))
                      (courier--empty-request)
                    (courier--parse-front-matter trimmed 1
                                                 (courier--empty-request)))))
    (when (and (not (string-empty-p trimmed))
               (not (courier--comment-only-section-p trimmed))
               (not (eq (plist-get request :body-type) expected-type)))
      (user-error "Body section must declare [body].type = \"%s\""
                  (symbol-name expected-type)))
    request))

(defun courier--request-body-state (body-type text)
  "Return request body plist entries for BODY-TYPE from section TEXT."
  (pcase body-type
    ('none
     '(:body "" :body-parts nil :body-file-path nil :body-file-content-type nil))
    ('form-urlencoded
     (list :body
           (courier--form-body-string-from-pairs
            (courier--parse-request-kv-lines text "form body"))
           :body-parts nil
           :body-file-path nil
           :body-file-content-type nil))
    ('binary
     (let ((request (courier--parse-request-body-fragment text 'binary)))
       (list :body ""
             :body-parts nil
             :body-file-path (plist-get request :body-file-path)
             :body-file-content-type
             (plist-get request :body-file-content-type))))
    ('multipart
     (let ((request (courier--parse-request-body-fragment text 'multipart)))
       (list :body ""
             :body-parts (plist-get request :body-parts)
             :body-file-path nil
             :body-file-content-type nil)))
    (_
     (list :body text
           :body-parts nil
           :body-file-path nil
           :body-file-content-type nil))))

(defun courier--request-apply-body-state (request body-type text)
  "Apply body section TEXT for BODY-TYPE onto REQUEST and return REQUEST."
  (courier--request-apply-plist request
                                (courier--request-body-state body-type text)))

(defun courier--request-default-body-state (body-type)
  "Return default request body plist entries for BODY-TYPE."
  (pcase body-type
    ('binary
     (courier--request-body-state
      'binary
      (concat "[body]\n"
              "type = \"binary\"\n"
              "path = \"./payload.bin\"\n"
              "content_type = \"application/octet-stream\"")))
    ('multipart
     (courier--request-body-state
      'multipart
      (concat "[body]\n"
              "type = \"multipart\"\n\n"
              "[[body.parts]]\n"
              "name = \"field\"\n"
              "kind = \"text\"\n"
              "value = \"{{value}}\"")))
    (_
     (courier--request-body-state body-type ""))))

(defun courier--mime-type-for-extension (path)
  "Return a MIME type inferred from PATH."
  (or (when-let* ((extension (file-name-extension path))
                  (mime-type (cdr (assoc-string extension
                                                courier--mime-type-alist
                                                t))))
        mime-type)
      "application/octet-stream"))

(defun courier--request-attachment-base-directory ()
  "Return the base directory used for request attachment relative paths."
  (expand-file-name
   (or (and buffer-file-name (file-name-directory buffer-file-name))
       (courier--collection-root)
       default-directory)))

(defun courier--request-relative-attachment-path (path)
  "Return PATH relative to the current request attachment base directory."
  (file-relative-name (expand-file-name path)
                      (courier--request-attachment-base-directory)))

(defun courier--capf-quoted-value-bounds (regexp)
  "Return string bounds for REGEXP on the current line, or nil.

REGEXP must place the editable string contents in capture group 1."
  (let ((pos (point)))
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward regexp (line-end-position) t)
        (let ((start (match-beginning 1))
              (end (match-end 1)))
          (when (<= start pos end)
            (cons start end)))))))

(defun courier--content-type-capf ()
  "Return completion data for `content_type' values at point."
  (when-let* ((bounds (courier--capf-quoted-value-bounds
                       "^[ \t#]*content_type[ \t]*=[ \t]*\"\\([^\"]*\\)\"?")))
    (list (car bounds) (cdr bounds) courier--common-content-types)))

(defun courier--post-response-var-from-capf ()
  "Return completion data for `from' values in the vars section."
  (when (eq (courier--request-current-section) 'vars)
    (when-let* ((bounds (courier--capf-quoted-value-bounds
                         "^[ \t#]*from[ \t]*=[ \t]*\"\\([^\"]*\\)\"?")))
      (list (car bounds) (cdr bounds) courier--post-response-var-sources))))

(defun courier--tests-dsl-capf ()
  "Return completion data for test DSL strings in the current test section."
  (when (eq (courier--request-current-section) 'tests)
    (when-let* ((bounds (courier--capf-quoted-value-bounds
                         "^[ \t#]*\"\\([^\"]*\\)\"?\\(?:,[ \t]*\\)?$")))
      (list (car bounds) (cdr bounds) courier--tests-dsl-completions))))

(defun courier--ensure-request-layout ()
  "Ensure the current Courier request buffer has layout markers."
  (unless (markerp courier--request-content-start)
    (setq courier--request-content-start (copy-marker (point-min))))
  courier--request-content-start)

(defun courier--request-section-text (request section)
  "Return editable text for REQUEST SECTION."
  (let ((text
         (pcase section
           ('params
            (courier--format-request-kv-lines
             (courier--request-effective-params request)))
           ('headers
           (if-let* ((headers (plist-get request :headers)))
                (mapconcat (lambda (header)
                             (format "%s: %s" (car header) (cdr header)))
                           headers
                           "\n")
              ""))
           ('body
            (courier--request-body-section-text request))
           ('auth
            (courier--request-auth-lines request))
           ('vars
            (courier--request-vars-lines request))
           ('script
            (courier--request-script-lines request))
           ('tests
            (courier--request-tests-lines request))
           (_
            ""))))
    (if (string-empty-p text)
        (courier--request-section-placeholder section)
      text)))

(defun courier--request-content-start-position ()
  "Return the editable content start position in the current request buffer."
  (if (markerp courier--request-content-start)
      (marker-position courier--request-content-start)
    (save-excursion
      (goto-char (point-min))
      (forward-line 3)
      (point))))

(defun courier--parse-request-view-request-line (line)
  "Parse request LINE from the current rendered view."
  (let ((trimmed (string-trim-right line)))
    (cond
     ((string-match "^\\([A-Z]+\\)\\(?:\\s-+\\(.*\\)\\)?$" trimmed)
      (list (match-string 1 trimmed)
            (or (match-string 2 trimmed) "")))
     ((string-empty-p trimmed)
      (list courier-default-request-method ""))
     (t
      (user-error "Malformed request line: %s" line)))))

(defun courier--trim-trailing-empty-lines (string)
  "Return STRING without trailing empty lines."
  (replace-regexp-in-string "\n*\\'" "" (or string "")))

(defun courier--comment-only-section-p (text)
  "Return non-nil when TEXT consists only of comments or blank lines."
  (cl-every
   (lambda (line)
     (let ((trimmed (string-trim line)))
       (or (string-empty-p trimmed)
           (string-prefix-p "#" trimmed))))
   (split-string (or text "") "\n")))

(defun courier--parse-request-kv-lines (text label)
  "Parse `key = value' TEXT for LABEL into an alist."
  (let (pairs)
    (dolist (line (split-string (or text "") "\n"))
      (let ((trimmed (string-trim-right line)))
        (unless (or (string-empty-p trimmed)
                    (string-prefix-p "#" (string-trim-left trimmed)))
          (unless (string-match "\\`\\([^=[:space:]][^=]*\\)\\s-*=\\s-*\\(.*\\)\\'" trimmed)
            (user-error "Invalid %s line: %s" label trimmed))
          (push (cons (string-trim (match-string 1 trimmed))
                      (string-trim (match-string 2 trimmed)))
                pairs))))
    (nreverse pairs)))

(defun courier--parse-request-header-lines (text)
  "Parse header TEXT into a Courier header alist."
  (let (headers)
    (dolist (line (split-string (or text "") "\n"))
      (let ((trimmed (string-trim-right line)))
        (unless (or (string-empty-p trimmed)
                    (string-prefix-p "#" (string-trim-left trimmed)))
          (unless (string-match courier--header-line-regexp trimmed)
            (user-error "Malformed header: %s" trimmed))
          (push (cons (downcase (match-string 1 trimmed))
                      (match-string 2 trimmed))
                headers))))
    (nreverse headers)))

(defun courier--parse-request-auth-lines (text)
  "Parse auth section TEXT into a Courier auth plist or nil."
  (let* ((trimmed (string-trim (or text "")))
         (request (if (or (string-empty-p trimmed)
                          (courier--comment-only-section-p trimmed))
                      (courier--empty-request)
                    (courier--parse-front-matter trimmed 1
                                                 (courier--empty-request)))))
    (plist-get request :auth)))

(defun courier--parse-request-tests-lines (text)
  "Parse test section TEXT into Courier test expressions."
  (let* ((trimmed (string-trim (or text "")))
         (request (if (or (string-empty-p trimmed)
                          (courier--comment-only-section-p trimmed))
                      (courier--empty-request)
                    (courier--parse-front-matter trimmed 1
                                                 (courier--empty-request)))))
    (plist-get request :tests)))

(defun courier--request-vars-lines (request)
  "Return editable TOML fragment for REQUEST vars."
  (let (sections)
    (when-let* ((vars (plist-get request :vars)))
      (let ((lines (list "[vars]")))
        (dolist (pair vars)
          (push (format "%s = %s"
                        (car pair)
                        (courier--toml-quote-string (cdr pair)))
                lines))
        (push (format "# Request Vars\n%s"
                      (string-join (nreverse lines) "\n"))
              sections)))
    (when-let* ((vars (plist-get request :pre-request-vars)))
      (let ((lines (list "[vars.pre_request]")))
        (dolist (pair vars)
          (push (format "%s = %s"
                        (car pair)
                        (courier--toml-quote-string (cdr pair)))
                lines))
        (push (format "# Pre-request Vars\n%s"
                      (string-join (nreverse lines) "\n"))
              sections)))
    (when-let* ((rules (plist-get request :post-response-vars)))
      (dolist (rule rules)
        (let ((lines (list "[[vars.post_response]]")))
          (push (format "name = %s"
                        (courier--toml-quote-string
                         (or (plist-get rule :name) "")))
                lines)
          (push (format "from = %s"
                        (courier--toml-quote-string
                         (symbol-name (or (plist-get rule :from) 'json))))
                lines)
          (when-let* ((expr (plist-get rule :expr)))
            (push (format "expr = %s"
                          (courier--toml-quote-string expr))
                  lines))
          (push (format "# Post-response Vars\n%s"
                        (string-join (nreverse lines) "\n"))
                sections))))
    (string-join (nreverse sections) "\n\n")))

(defun courier--parse-request-vars-lines (text)
  "Parse request vars section TEXT into Courier vars data."
  (let* ((trimmed (string-trim (or text "")))
         (request (if (or (string-empty-p trimmed)
                          (courier--comment-only-section-p trimmed))
                      (courier--empty-request)
                    (courier--parse-front-matter trimmed 1
                                                 (courier--empty-request)))))
    (list :vars (plist-get request :vars)
          :pre-request-vars (plist-get request :pre-request-vars)
          :post-response-vars (plist-get request :post-response-vars))))

(defun courier--request-script-lines (request)
  "Return editable TOML fragment for REQUEST scripts."
  (let ((pre-request (plist-get request :pre-request-script))
        (post-response (plist-get request :post-response-script))
        blocks)
    (when (and pre-request
               (not (string-empty-p (string-trim pre-request))))
      (push (format "# Pre-request Script\npre_request = \"\"\"\n%s\n\"\"\""
                    pre-request)
            blocks))
    (when (and post-response
               (not (string-empty-p (string-trim post-response))))
      (push (format "# Post-response Script\npost_response = \"\"\"\n%s\n\"\"\""
                    post-response)
            blocks))
    (string-join (nreverse blocks) "\n\n")))

(defun courier--parse-request-script-lines (text)
  "Parse request script section TEXT into Courier script data."
  (let* ((trimmed (string-trim (or text "")))
         (request (if (or (string-empty-p trimmed)
                          (courier--comment-only-section-p trimmed))
                      (courier--empty-request)
                    (courier--parse-front-matter
                     (concat "[scripts]\n" trimmed)
                     1
                     (courier--empty-request)))))
    (list :pre-request-script (or (plist-get request :pre-request-script) "")
          :post-response-script (or (plist-get request :post-response-script) ""))))

(defun courier--toml-quote-string (string)
  "Return STRING serialized as a TOML basic string."
  (prin1-to-string (or string "")))

(defun courier--serialize-front-matter-root-sections (request)
  "Return root-level TOML sections for REQUEST."
  (let* ((settings (plist-get request :settings))
         (name (and (plist-get request :name)
                    (string-trim (plist-get request :name))))
         (timeout (plist-get settings :timeout))
         (follow-redirects-present (plist-member settings :follow-redirects))
         (follow-redirects (plist-get settings :follow-redirects))
         (tests (plist-get request :tests))
         sections)
    (when (and name (not (string-empty-p name)))
      (push (format "name = %s" (courier--toml-quote-string name)) sections))
    (when timeout
      (push (format "timeout = %d" timeout) sections))
    (when follow-redirects-present
      (push (format "follow_redirects = %s" (if follow-redirects "true" "false"))
            sections))
    (when tests
      (let ((lines (list "tests = [")))
        (dolist (test tests)
          (push (format "  %s," (courier--toml-quote-string test)) lines))
        (push "]" lines)
        (push (string-join (nreverse lines) "\n") sections)))
    (nreverse sections)))

(defun courier--serialize-front-matter-body-sections (request)
  "Return body-related TOML sections for REQUEST."
  (let ((body-type (courier--request-body-type request))
        (body-parts (plist-get request :body-parts))
        (body-file-path (plist-get request :body-file-path))
        (body-file-content-type (plist-get request :body-file-content-type))
        sections)
    (when body-type
      (let ((lines (list "[body]")))
        (push (format "type = %s"
                      (courier--toml-quote-string (symbol-name body-type)))
              lines)
        (when (and (eq body-type 'binary) body-file-path)
          (push (format "path = %s"
                        (courier--toml-quote-string body-file-path))
                lines))
        (when (and (eq body-type 'binary) body-file-content-type)
          (push (format "content_type = %s"
                        (courier--toml-quote-string body-file-content-type))
                lines))
        (push (string-join (nreverse lines) "\n") sections))
      (when (eq body-type 'multipart)
        (dolist (part body-parts)
          (let ((lines (list "[[body.parts]]")))
            (push (format "name = %s"
                          (courier--toml-quote-string
                           (or (plist-get part :name) "")))
                  lines)
            (push (format "kind = %s"
                          (courier--toml-quote-string
                           (symbol-name (or (plist-get part :kind) 'text))))
                  lines)
            (when-let* ((value (plist-get part :value)))
              (push (format "value = %s" (courier--toml-quote-string value))
                    lines))
            (when-let* ((path (plist-get part :path)))
              (push (format "path = %s" (courier--toml-quote-string path))
                    lines))
            (when-let* ((content-type (plist-get part :content-type)))
              (push (format "content_type = %s"
                            (courier--toml-quote-string content-type))
                    lines))
            (push (string-join (nreverse lines) "\n") sections)))))
    (nreverse sections)))

(defun courier--serialize-front-matter-auth-sections (request)
  "Return auth-related TOML sections for REQUEST."
  (let ((auth (plist-get request :auth))
        sections)
    (when auth
      (let ((lines (list "[auth]")))
        (push (format "type = %s"
                      (courier--toml-quote-string
                       (symbol-name (plist-get auth :type))))
              lines)
        (pcase (plist-get auth :type)
          ('none nil)
          ('bearer
           (push (format "token = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :token) "")))
                 lines))
          ('basic
           (push (format "username = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :username) "")))
                 lines)
           (push (format "password = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :password) "")))
                 lines))
          ('header
           (push (format "header = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :header) "")))
                 lines)
           (push (format "value = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :value) "")))
                 lines))
          ('api_key
           (push (format "in = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :in) "header")))
                 lines)
           (push (format "name = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :name) "")))
                 lines)
           (push (format "value = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :value) "")))
                 lines))
          ('oauth2
           (push (format "grant_type = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :grant-type) "client_credentials")))
                 lines)
           (push (format "token_url = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :token-url) "")))
                 lines)
           (push (format "client_id = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :client-id) "")))
                 lines)
           (push (format "client_secret = %s"
                         (courier--toml-quote-string
                          (or (plist-get auth :client-secret) "")))
                 lines)
           (when-let* ((scopes (plist-get auth :scopes)))
             (push (format "scopes = [%s]"
                           (mapconcat #'courier--toml-quote-string scopes ", "))
                   lines)))
          (_
           (user-error "Unsupported auth type: %s" (plist-get auth :type))))
        (push (string-join (nreverse lines) "\n") sections)))
    (nreverse sections)))

(defun courier--serialize-front-matter-vars-sections (request)
  "Return vars-related TOML sections for REQUEST."
  (let ((vars (plist-get request :vars))
        (pre-request-vars (plist-get request :pre-request-vars))
        (post-response-vars (plist-get request :post-response-vars))
        sections)
    (when vars
      (let ((lines (list "[vars]")))
        (dolist (pair vars)
          (push (format "%s = %s"
                        (car pair)
                        (courier--toml-quote-string (cdr pair)))
                lines))
        (push (string-join (nreverse lines) "\n") sections)))
    (when pre-request-vars
      (let ((lines (list "[vars.pre_request]")))
        (dolist (pair pre-request-vars)
          (push (format "%s = %s"
                        (car pair)
                        (courier--toml-quote-string (cdr pair)))
                lines))
        (push (string-join (nreverse lines) "\n") sections)))
    (when post-response-vars
      (dolist (rule post-response-vars)
        (let ((lines (list "[[vars.post_response]]")))
          (push (format "name = %s"
                        (courier--toml-quote-string
                         (or (plist-get rule :name) "")))
                lines)
          (push (format "from = %s"
                        (courier--toml-quote-string
                         (symbol-name (or (plist-get rule :from) 'json))))
                lines)
          (when-let* ((expr (plist-get rule :expr)))
            (push (format "expr = %s"
                          (courier--toml-quote-string expr))
                  lines))
          (push (string-join (nreverse lines) "\n") sections))))
    (nreverse sections)))

(defun courier--serialize-front-matter-script-sections (request)
  "Return script-related TOML sections for REQUEST."
  (let ((pre-request (plist-get request :pre-request-script))
        (post-response (plist-get request :post-response-script))
        sections)
    (when (or (and pre-request (not (string-empty-p (string-trim pre-request))))
              (and post-response (not (string-empty-p (string-trim post-response)))))
      (let ((lines (list "[scripts]")))
        (when (and pre-request
                   (not (string-empty-p (string-trim pre-request))))
          (when (string-match-p "\"\"\"" pre-request)
            (user-error "Pre-request script contains unsupported triple quotes"))
          (push (format "pre_request = \"\"\"\n%s\n\"\"\"" pre-request) lines))
        (when (and post-response
                   (not (string-empty-p (string-trim post-response))))
          (when (string-match-p "\"\"\"" post-response)
            (user-error "Post-response script contains unsupported triple quotes"))
          (push (format "post_response = \"\"\"\n%s\n\"\"\"" post-response) lines))
        (push (string-join (nreverse lines) "\n") sections)))
    (nreverse sections)))

(defun courier--serialize-front-matter (request)
  "Return TOML front matter for REQUEST, or nil when not needed."
  (let ((sections
         (append (courier--serialize-front-matter-root-sections request)
                 (courier--serialize-front-matter-body-sections request)
                 (courier--serialize-front-matter-auth-sections request)
                 (courier--serialize-front-matter-vars-sections request)
                 (courier--serialize-front-matter-script-sections request))))
    (when sections
      (concat "+++\n"
              (string-join sections "\n\n")
              "\n+++\n\n"))))

(defun courier--serialize-request (request)
  "Return REQUEST serialized to Courier v1 syntax."
  (let* ((front-matter (courier--serialize-front-matter request))
         (request-url (courier--update-request-url-query
                       (or (plist-get request :url) "")
                       (plist-get request :params)))
         (request-line (format "%s %s"
                               (or (plist-get request :method) courier-default-request-method)
                               request-url))
         (headers (mapcar (lambda (header)
                            (format "%s: %s" (car header) (cdr header)))
                          (plist-get request :headers)))
         (body (or (plist-get request :body) "")))
    (concat
     (or front-matter "")
     request-line "\n"
     (if headers
         (concat (string-join headers "\n") "\n")
       "")
     "\n"
     body)))

(defun courier--sync-request-model ()
  "Persist the currently visible request view back into `courier--request-model'."
  (when (and (derived-mode-p 'courier-request-mode)
             courier--request-model
             (not courier--request-rendering-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((request-line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
             (content-start (courier--request-content-start-position))
             (section-text (courier--trim-trailing-empty-lines
                            (buffer-substring-no-properties content-start (point-max)))))
        (pcase-let ((`(,method ,url) (courier--parse-request-view-request-line request-line)))
          (plist-put courier--request-model :method method)
          (plist-put courier--request-model :url url))
        (pcase courier--request-tab
          ('params
           (let ((params (courier--parse-request-kv-lines section-text "param")))
             (plist-put courier--request-model :params params)
             (plist-put courier--request-model :url
                        (car (courier--split-request-url
                              (or (plist-get courier--request-model :url) ""))))))
          ('headers
           (plist-put courier--request-model :headers
                      (courier--parse-request-header-lines section-text)))
         ('body
           (courier--request-apply-body-state
            courier--request-model
            (courier--request-body-type courier--request-model)
            section-text))
          ('auth
           (plist-put courier--request-model :auth
                      (courier--parse-request-auth-lines section-text)))
          ('vars
           (let ((parsed-vars (courier--parse-request-vars-lines section-text)))
             (plist-put courier--request-model :vars
                        (plist-get parsed-vars :vars))
             (plist-put courier--request-model :pre-request-vars
                        (plist-get parsed-vars :pre-request-vars))
             (plist-put courier--request-model :post-response-vars
                        (plist-get parsed-vars :post-response-vars))))
          ('script
           (let ((parsed-scripts (courier--parse-request-script-lines section-text)))
             (plist-put courier--request-model :pre-request-script
                        (plist-get parsed-scripts :pre-request-script))
             (plist-put courier--request-model :post-response-script
                        (plist-get parsed-scripts :post-response-script))))
          ('tests
           (plist-put courier--request-model :tests
                      (courier--parse-request-tests-lines section-text))))
        nil))))

(defun courier--render-request-buffer ()
  "Render the current Courier request buffer from `courier--request-model'."
  (unless courier--request-model
    (user-error "No Courier request model loaded"))
  (let ((inhibit-read-only t)
        (modified (buffer-modified-p))
        (request courier--request-model))
    (setq courier--request-rendering-p t)
    (unwind-protect
        (progn
          (setq courier--request-divider-width nil)
          (courier--ensure-request-layout)
          (erase-buffer)
          (insert (courier--request-view-request-line request) "\n")
          (let ((divider (courier--request-divider-line)))
            (setq courier--request-divider-width (string-width divider))
            (insert divider "\n"))
          (insert "\n")
          (set-marker courier--request-content-start (point))
          (courier--protect-request-scaffold)
          (courier--refresh-request-divider-overlays)
          (insert (courier--request-section-text request courier--request-tab))
          (goto-char (marker-position courier--request-content-start))
          (courier--refresh-method-overlay))
      (setq courier--request-rendering-p nil)
      (set-buffer-modified-p modified))))

(defun courier--refresh-request-content ()
  "Refresh only the current request section content region."
  (unless courier--request-model
    (user-error "No Courier request model loaded"))
  (courier--ensure-request-layout)
  (let ((inhibit-read-only t)
        (modified (buffer-modified-p)))
    (setq courier--request-rendering-p t)
    (unwind-protect
        (progn
          (courier--refresh-request-divider-line)
          (delete-region (marker-position courier--request-content-start) (point-max))
          (goto-char (marker-position courier--request-content-start))
          (insert (courier--request-section-text courier--request-model courier--request-tab))
          (goto-char (marker-position courier--request-content-start))
          (courier--refresh-method-overlay))
      (setq courier--request-rendering-p nil)
      (set-buffer-modified-p modified))))

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
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or start
                                           (courier--buffer-start-directory)))))
         (collections-root (file-name-as-directory (courier--collections-directory))))
    (cond
     ((file-in-directory-p directory collections-root)
      (let* ((relative (file-relative-name directory collections-root))
             (first-segment (car (split-string relative "/" t)))
             (candidate (and first-segment
                             (expand-file-name first-segment collections-root))))
        (when (and candidate
                   (file-directory-p candidate)
                   (file-exists-p
                    (expand-file-name courier--collection-marker-file candidate)))
          (file-name-as-directory candidate))))
     (t
      (when-let* ((root (locate-dominating-file directory courier--collection-marker-file)))
        (let ((normalized-root (file-name-as-directory (expand-file-name root))))
          (unless (file-equal-p normalized-root (courier--home-directory))
            normalized-root)))))))

(defun courier--home-directory ()
  "Return the normalized Courier home directory."
  (file-name-as-directory (expand-file-name courier-home-directory)))

(defun courier--collections-directory ()
  "Return the directory under Courier home for collections."
  (expand-file-name courier--collections-directory-name
                    (courier--home-directory)))

(defun courier--specs-directory ()
  "Return the directory under Courier home for copied API specs."
  (expand-file-name courier--specs-directory-name
                    (courier--home-directory)))

(defun courier--runtime-vars-root ()
  "Return the directory under Courier home for runtime variables."
  (expand-file-name courier--runtime-vars-directory-name
                    (expand-file-name courier--state-directory-name
                                      (courier--home-directory))))

(defun courier--runtime-vars-collection-key (collection-root)
  "Return a stable directory key for COLLECTION-ROOT."
  (if collection-root
      (let* ((normalized-root
              (file-name-as-directory (expand-file-name collection-root)))
             (name
              (or (courier--collection-name collection-root)
                  (file-name-nondirectory
                   (directory-file-name collection-root))))
             (digest (substring (secure-hash 'sha1 normalized-root) 0 12)))
        (format "%s-%s" (courier--slugify name) digest))
    "default"))

(defun courier--runtime-vars-directory (collection-root)
  "Return the runtime vars directory for COLLECTION-ROOT."
  (expand-file-name (courier--runtime-vars-collection-key collection-root)
                    (courier--runtime-vars-root)))

(defun courier--runtime-vars-file (collection-root env-name)
  "Return the runtime vars file path for COLLECTION-ROOT and ENV-NAME."
  (expand-file-name
   (format "%s.env" (or env-name "default"))
   (courier--runtime-vars-directory collection-root)))

(defun courier--decode-runtime-var-value (raw path line-number)
  "Decode runtime var RAW from PATH on LINE-NUMBER."
  (let ((trimmed (string-trim-left raw)))
    (if (string-prefix-p "\"" trimmed)
        (condition-case err
            (pcase-let ((`(,value . ,end) (read-from-string trimmed)))
              (unless (stringp value)
                (user-error
                 "Parse error in %s on line %d: runtime var value must be a string"
                 path line-number))
              (unless (string-empty-p (string-trim (substring trimmed end)))
                (user-error
                 "Parse error in %s on line %d: trailing characters after runtime var value"
                 path line-number))
              value)
          (error
           (user-error
            "Parse error in %s on line %d: invalid runtime var value (%s)"
            path line-number
            (error-message-string err))))
      (string-trim raw))))

(defun courier--read-env-like-file (path value-reader line-kind)
  "Read PATH as an env-like file using VALUE-READER for matched values.
LINE-KIND names the line type in malformed-line errors."
  (with-temp-buffer
    (insert-file-contents path)
    (let (vars)
      (while (not (eobp))
        (let* ((line-number (line-number-at-pos))
               (raw-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
               (trimmed (string-trim raw-line)))
          (cond
           ((or (string-empty-p trimmed)
                (string-prefix-p "#" trimmed))
            nil)
           ((string-match courier--env-line-regexp raw-line)
            (push (cons (match-string 1 raw-line)
                        (funcall value-reader
                                 (match-string 2 raw-line)
                                 path
                                 line-number))
                  vars))
           (t
            (user-error "Parse error in %s on line %d: malformed %s line"
                        path line-number line-kind))))
        (forward-line 1))
      (nreverse vars))))

(defun courier--read-runtime-vars (collection-root env-name)
  "Return runtime vars for COLLECTION-ROOT and ENV-NAME."
  (let ((path (courier--runtime-vars-file collection-root env-name)))
    (if (file-exists-p path)
        (courier--read-env-like-file
         path
         #'courier--decode-runtime-var-value
         "runtime var")
      nil)))

(defun courier--write-runtime-vars (collection-root env-name vars)
  "Write runtime VARS for COLLECTION-ROOT and ENV-NAME."
  (let ((directory (courier--runtime-vars-directory collection-root))
        (path (courier--runtime-vars-file collection-root env-name)))
    (make-directory directory t)
    (with-temp-file path
      (dolist (pair vars)
        (insert (car pair)
                "="
                (let ((print-escape-newlines t)
                      (print-escape-control-characters t))
                  (prin1-to-string (cdr pair)))
                "\n")))))

(defun courier--active-collection-root ()
  "Return the active Courier collection root for the current context, or nil."
  (or courier--collection-root-hint
      (courier--collection-root)))

(defun courier--current-request-error-snapshot ()
  "Return a best-effort current request snapshot for local error rendering."
  (let* ((path (and buffer-file-name (expand-file-name buffer-file-name)))
         (request (if courier--request-model
                      (copy-tree courier--request-model t)
                    (courier--empty-request path)))
         (line (save-excursion
                 (goto-char (point-min))
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
         (method (or (plist-get request :method) courier-default-request-method))
         (url (or (plist-get request :url) "")))
    (plist-put request :path path)
    (plist-put request :name (courier--buffer-request-name))
    (cond
     ((string-match "^\\([A-Z]+\\)\\(?:\\s-+\\(\\S-+\\)\\)?\\s-*$" line)
      (setq method (match-string 1 line)
            url (or (match-string 2 line) "")))
     ((string-match "^\\([A-Z]+\\)" line)
      (setq method (match-string 1 line))))
    (plist-put request :method method)
    (plist-put request :url url)
    request))

(defun courier--collection-config-path (&optional start)
  "Return the Courier config path for START, or nil."
  (when-let* ((root (courier--collection-root start)))
    (expand-file-name courier--collection-marker-file root)))

(defun courier--collection-config (&optional start)
  "Return Courier collection config for START, or nil.
The result is a plist with `:root', `:path', `:name', `:requests-dir',
`:env-dir', `:default-env', and `:defaults'."
  (when-let* ((path (courier--collection-config-path start))
              (root (file-name-directory path)))
    (let* ((data (courier--read-json-file path))
           (name (courier--json-string-field data 'name))
           (requests-dir (courier--json-string-field data 'requestsDir "requests"))
           (env-dir (courier--json-string-field data 'envDir "env"))
           (default-env (courier--json-string-field data 'defaultEnv))
           (defaults (courier--json-defaults-config data)))
      (list :root (file-name-as-directory root)
            :path path
            :name name
            :requests-dir requests-dir
            :env-dir env-dir
            :default-env default-env
            :defaults defaults))))

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
   ((and (derived-mode-p 'courier-request-mode)
         courier--request-path)
    courier--request-path)
   (t
    nil)))

(defun courier--management-root ()
  "Return the root directory used for Courier management commands."
  (cond
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
  (if courier--request-model
      (or (plist-get courier--request-model :name) "Untitled")
    "Untitled"))

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
  (concat "+++\n"
          "name = " (courier--toml-quote-string name) "\n"
          "\n"
          "[body]\n"
          "type = " (courier--toml-quote-string
                     (symbol-name courier-default-body-type)) "\n"
          "+++\n\n"
          courier-default-request-method " \n"
          "Accept: application/json\n"))

(defun courier--collection-template (root &optional name)
  "Return JSON text for a new Courier collection rooted at ROOT.

When NAME is non-nil, use it as the collection display name."
  (let ((json-encoding-pretty-print t))
    (concat
     (json-serialize
      `((name . ,(or name
                     (file-name-nondirectory
                      (directory-file-name (file-name-as-directory root)))))
        (requestsDir . "requests")
        (envDir . "env")))
     "\n")))

(defconst courier--collection-gitignore-template
  (concat
   "# Real secrets\n"
   "env/*.env\n"
   "!env/.env.example\n"
   "!env/*.env.example\n"
   "!env/*.example.env\n"
   "\n"
   "# Editor noise\n"
   ".dir-locals.el\n"
   "*~\n"
   "\\#*\\#\n"
   ".#*\n"
   "\n"
   "# OS noise\n"
   ".DS_Store\n")
  "Default .gitignore template for new Courier collections.")

(defun courier--create-collection (root &optional name)
  "Create a new Courier collection rooted at ROOT and return it.

When NAME is non-nil, store it in `courier.json' as the collection display
name."
  (let* ((collection-root (file-name-as-directory (expand-file-name root)))
         (existing-root (courier--collection-root collection-root))
         (config-path (expand-file-name courier--collection-marker-file collection-root))
         (gitignore-path (expand-file-name ".gitignore" collection-root)))
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
      (insert (courier--collection-template collection-root name)))
    (unless (file-exists-p gitignore-path)
      (with-temp-file gitignore-path
        (insert courier--collection-gitignore-template)))
    collection-root))

(defun courier--read-collection-name ()
  "Read a Courier collection name from the minibuffer."
  (let* ((candidates (courier--collection-candidates))
         (table (courier--completion-table
                 candidates
                 '(category . courier-collection)
                 '(group-function . courier--completion-group)
                 '(affixation-function . courier--completion-affixation))))
    (string-trim
     (completing-read "Collection: " table nil nil))))

(defun courier--collection-root-for-name (name)
  "Return the collection root path for collection NAME under Courier home."
  (when (string-empty-p name)
    (user-error "Collection name cannot be empty"))
  (when (string-match-p "[/\\]" name)
    (user-error "Collection name cannot contain path separators"))
  (expand-file-name name (courier--collections-directory)))

(defun courier--openapi-operation-name (method path operation)
  "Return a display name for METHOD PATH using OPERATION metadata."
  (or (courier--json-string-field operation 'summary)
      (courier--json-string-field operation 'operationId)
      (format "%s %s" method path)))

(defun courier--openapi-url-from-path (path)
  "Return a Courier URL template for OpenAPI PATH."
  (concat "{{base_url}}"
          (replace-regexp-in-string
           "{\\([^}]+\\)}"
           "{{\\1}}"
           path)))

(defun courier--openapi-file-directory (path requests-root)
  "Return the request directory for OpenAPI PATH under REQUESTS-ROOT."
  (let* ((segments (seq-remove
                    (lambda (segment)
                      (or (string-empty-p segment)
                          (string-prefix-p "{" segment)))
                    (split-string path "/" t))))
    (if segments
        (expand-file-name (mapconcat #'courier--slugify segments "/")
                          requests-root)
      requests-root)))

(defun courier--openapi-variable-name (string)
  "Return a variable-safe name derived from STRING."
  (let ((normalized (replace-regexp-in-string "[^[:alnum:]]+" "_" (downcase string))))
    (replace-regexp-in-string "\\`_+\\|_+\\'" "" normalized)))

(defun courier--openapi-content-body-type (content-type)
  "Map OpenAPI CONTENT-TYPE to a Courier body type symbol."
  (cond
   ((string-match-p "application/json" content-type) 'json)
   ((string-match-p "application/x-www-form-urlencoded" content-type) 'form-urlencoded)
   ((string-match-p "multipart/form-data" content-type) 'multipart)
   ((or (string-match-p "application/xml" content-type)
        (string-match-p "text/xml" content-type))
    'xml)
   ((string-match-p "text/" content-type) 'text)
   ((string-match-p "application/octet-stream" content-type) 'binary)
   (t nil)))

(defun courier--openapi-content-entry (request-body)
  "Return the first supported OpenAPI content entry from REQUEST-BODY."
  (when-let* ((content (courier--json-object-field request-body 'content)))
    (seq-find
     (lambda (entry)
       (courier--openapi-content-body-type (courier--json-key-name (car entry))))
     content)))

(defun courier--openapi-request-body-metadata (operation)
  "Return Courier body metadata derived from OpenAPI OPERATION."
  (when-let* ((request-body (courier--json-object-field operation 'requestBody))
              (entry (courier--openapi-content-entry request-body)))
    (let* ((content-type (courier--json-key-name (car entry)))
           (body-type (courier--openapi-content-body-type content-type)))
      (pcase body-type
        ('binary
         (list :body-type 'binary
               :body ""
               :body-file-path "./payload.bin"
               :body-file-content-type content-type))
        ('multipart
         (list :body-type 'multipart
               :body ""
               :body-parts '((:name "field" :kind text :value "{{value}}"))))
        ('form-urlencoded
         (list :body-type 'form-urlencoded
               :body "field=value"))
        ((or 'json 'xml 'text)
         (list :body-type body-type
               :body (pcase body-type
                       ('json "{}")
                       (_ ""))))
        (_ nil)))))

(defun courier--openapi-security-requirement (operation spec)
  "Return the active OpenAPI security requirement for OPERATION in SPEC."
  (let ((operation-security (alist-get 'security operation 'courier--missing)))
    (cond
     ((eq operation-security 'courier--missing)
      (alist-get 'security spec))
     ((null operation-security) nil)
     (t operation-security))))

(defun courier--openapi-security-schemes (spec)
  "Return OpenAPI security schemes from SPEC."
  (when-let* ((components (courier--json-object-field spec 'components)))
    (courier--json-object-field components 'securitySchemes)))

(defun courier--openapi-auth-metadata (operation spec)
  "Return Courier auth metadata for OpenAPI OPERATION in SPEC."
  (when-let* ((requirement-list (courier--openapi-security-requirement operation spec))
              (requirement (car requirement-list))
              (requirement-entry (car requirement))
              (scheme-name (courier--json-key-name
                            (if (consp requirement-entry)
                                (car requirement-entry)
                              requirement-entry)))
              (schemes (courier--openapi-security-schemes spec))
              (scheme-entry
               (seq-find
                (lambda (entry)
                  (string= (courier--json-key-name (car entry))
                           scheme-name))
                schemes))
              (scheme (cdr scheme-entry)))
    (let ((type (courier--json-string-field scheme 'type)))
      (pcase type
        ("http"
         (pcase (courier--json-string-field scheme 'scheme)
           ("bearer"
            '(:type bearer :token "{{token}}"))
           ("basic"
            '(:type basic :username "{{user}}" :password "{{password}}"))
           (_ nil)))
        ("apiKey"
         (list :type 'api_key
               :in (or (courier--json-string-field scheme 'in) "header")
               :name (or (courier--json-string-field scheme 'name) "X-API-Key")
               :value (format "{{%s}}"
                              (courier--openapi-variable-name
                               (or (courier--json-string-field scheme 'name)
                                   "api_key")))))
        ("oauth2"
         (when-let* ((flows (courier--json-object-field scheme 'flows))
                     (client-credentials
                      (courier--json-object-field flows 'clientCredentials))
                     (token-url (courier--json-string-field client-credentials 'tokenUrl)))
           (list :type 'oauth2
                 :grant-type "client_credentials"
                 :token-url token-url
                 :client-id "{{client_id}}"
                 :client-secret "{{client_secret}}"
                 :scopes (mapcar #'courier--json-key-name
                                 (mapcar #'car
                                         (or (courier--json-object-field client-credentials 'scopes)
                                             nil))))))
        (_ nil)))))

(defun courier--openapi-request-path (operation-name method path collection-root)
  "Return the output request path for OPERATION-NAME.

METHOD, PATH, and COLLECTION-ROOT determine the generated file location."
  (let* ((requests-root (courier--preferred-requests-root collection-root))
         (directory (courier--openapi-file-directory path requests-root))
         (file-name (format "%s.http"
                            (courier--slugify
                             (or operation-name
                                 (format "%s-%s" method path))))))
    (expand-file-name file-name directory)))

(defun courier--openapi-request-for-operation (collection-root spec path method operation)
  "Return a Courier request plist for OpenAPI OPERATION.
Use PATH, COLLECTION-ROOT, and SPEC to build the generated request."
  (let* ((method-name (upcase method))
         (operation-name (courier--openapi-operation-name method-name path operation))
         (request-path (courier--openapi-request-path operation-name method-name path collection-root))
         (request (courier--empty-request request-path))
         (body-metadata (courier--openapi-request-body-metadata operation))
         (auth-metadata (courier--openapi-auth-metadata operation spec)))
    (setq request (plist-put request :name operation-name))
    (setq request (plist-put request :method method-name))
    (setq request (plist-put request :url (courier--openapi-url-from-path path)))
    (setq request (plist-put request :auth auth-metadata))
    (while body-metadata
      (setq request (plist-put request (pop body-metadata)
                               (pop body-metadata))))
    request))

(defun courier--openapi-operation-report-lines (spec path method operation request)
  "Return import report lines for OpenAPI METHOD OPERATION and REQUEST.
Use SPEC and PATH when describing unsupported import details."
  (let (lines)
    (when (and (courier--openapi-security-requirement operation spec)
               (not (plist-get request :auth)))
      (push (format "- Unsupported auth scheme for %s %s"
                    (upcase method)
                    path)
            lines))
    (when (and (courier--json-object-field operation 'requestBody)
               (not (plist-get request :body-type)))
      (push (format "- Unsupported request body type for %s %s"
                    (upcase method)
                    path)
            lines))
    (nreverse lines)))

(defun courier--openapi-operations (spec)
  "Return a list of OpenAPI operations extracted from SPEC."
  (let (operations)
    (dolist (path-entry (or (alist-get 'paths spec) nil))
      (let ((path (courier--json-key-name (car path-entry)))
            (path-object (cdr path-entry)))
        (dolist (operation-entry path-object)
          (let ((method (courier--json-key-name (car operation-entry)))
                (operation (cdr operation-entry)))
            (when (member (upcase method) courier--allowed-methods)
              (push (list :path path :method method :operation operation) operations))))))
    (nreverse operations)))

(defun courier--openapi-collection-config (collection-name spec)
  "Return a Courier collection config alist for COLLECTION-NAME imported from SPEC."
  (let ((title (and (courier--json-object-field spec 'info)
                    (courier--json-string-field (courier--json-object-field spec 'info) 'title)))
        (servers (alist-get 'servers spec))
        defaults)
    (when-let* ((first-server (car servers))
                (server-url (and (listp first-server)
                                 (courier--json-string-field first-server 'url))))
      (setq defaults `((vars . ((base_url . ,server-url))))))
    `((name . ,(or title collection-name))
      (requestsDir . "requests")
      (envDir . "env")
      ,@(when defaults
          `((defaults . ,defaults))))))

(defun courier--copy-openapi-spec (spec-path collection-name)
  "Copy SPEC-PATH into Courier specs storage for COLLECTION-NAME."
  (let* ((target-directory (expand-file-name collection-name
                                             (courier--specs-directory)))
         (extension (downcase (or (file-name-extension spec-path) "json")))
         (target-path (expand-file-name (format "openapi.%s" extension)
                                        target-directory)))
    (make-directory target-directory t)
    (copy-file spec-path target-path t)
    target-path))

(defun courier--openapi-yaml-json-string (spec-path)
  "Return SPEC-PATH YAML converted to a JSON string."
  (unless (executable-find "ruby")
    (user-error "OpenAPI YAML import requires system Ruby with YAML support"))
  (with-temp-buffer
    (let ((stderr-path (make-temp-file "courier-openapi-yaml-stderr-")))
      (unwind-protect
          (let ((status
                 (call-process
                  "ruby" spec-path (list t stderr-path) nil
                  "-e"
                  "require 'yaml'; require 'json'; print JSON.generate(YAML.safe_load(ARGF.read, aliases: true))")))
            (unless (and (integerp status) (zerop status))
              (user-error "Failed to parse OpenAPI YAML: %s"
                          (with-temp-buffer
                            (insert-file-contents stderr-path)
                            (string-trim (buffer-string))))))
        (delete-file stderr-path)))
    (buffer-string)))

(defun courier--read-openapi-file (spec-path)
  "Return parsed OpenAPI data from SPEC-PATH."
  (pcase (downcase (or (file-name-extension spec-path) ""))
    ("json"
     (courier--read-json-file spec-path))
    ((or "yaml" "yml")
     (json-parse-string (courier--openapi-yaml-json-string spec-path)
                        :object-type 'alist
                        :array-type 'list))
    (_
     (user-error "Unsupported OpenAPI spec format: %s" spec-path))))

(defun courier--write-openapi-import-report (collection-name lines)
  "Write OpenAPI import report LINES for COLLECTION-NAME.

Return the report path."
  (let* ((target-directory (expand-file-name collection-name
                                             (courier--specs-directory)))
         (report-path (expand-file-name "import-report.org" target-directory)))
    (make-directory target-directory t)
    (with-temp-file report-path
      (insert "#+title: OpenAPI Import Report\n\n")
      (insert (format "* %s\n\n" collection-name))
      (if lines
          (insert (string-join lines "\n") "\n")
        (insert "- No unsupported items detected.\n")))
    report-path))

(defun courier--import-openapi-file (spec-path collection-name)
  "Import OpenAPI SPEC-PATH into COLLECTION-NAME.

Return a plist containing at least `:collection-root' and `:spec-path'."
  (let* ((spec (courier--read-openapi-file spec-path))
         (collection-root
          (courier--create-collection
           (courier--collection-root-for-name collection-name)
           collection-name))
         (config-path (expand-file-name courier--collection-marker-file collection-root))
         (spec-copy (courier--copy-openapi-spec spec-path collection-name))
         request-files
         report-lines)
    (courier--write-json-file
     config-path
     (courier--openapi-collection-config collection-name spec))
    (dolist (entry (courier--openapi-operations spec))
      (let* ((request
              (courier--openapi-request-for-operation
               collection-root
               spec
               (plist-get entry :path)
               (plist-get entry :method)
               (plist-get entry :operation)))
             (path (plist-get request :path)))
        (setq report-lines
              (append report-lines
                      (courier--openapi-operation-report-lines
                       spec
                       (plist-get entry :path)
                       (plist-get entry :method)
                       (plist-get entry :operation)
                       request)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path
          (insert (courier--serialize-request request)))
        (push path request-files)))
    (list :collection-root collection-root
          :spec-path spec-copy
          :report-path (courier--write-openapi-import-report
                        collection-name
                        report-lines)
          :request-files (nreverse request-files))))

(defun courier--openapi-default-collection-name (spec-path)
  "Return a default collection name derived from SPEC-PATH."
  (let* ((spec (courier--read-openapi-file spec-path))
         (info (courier--json-object-field spec 'info))
         (title (and info (courier--json-string-field info 'title))))
    (courier--slugify
     (or title
         (file-name-base spec-path)))))

;;;###autoload
(defun courier-import-openapi (spec-path collection-name)
  "Import OpenAPI SPEC-PATH into COLLECTION-NAME."
  (interactive
   (let* ((spec-path (read-file-name "OpenAPI file: " nil nil t nil
                                     (lambda (path)
                                       (or (file-directory-p path)
                                           (string-suffix-p ".json" path t)
                                           (string-suffix-p ".yaml" path t)
                                           (string-suffix-p ".yml" path t)))))
          (collection-name
           (read-string "Collection name: "
                        (courier--openapi-default-collection-name spec-path))))
     (list spec-path collection-name)))
  (let* ((result (courier--import-openapi-file spec-path collection-name))
         (request-count (length (plist-get result :request-files))))
    (message "Imported OpenAPI into %s (%d requests)"
             (abbreviate-file-name (plist-get result :collection-root))
             request-count)))

(defun courier--read-save-collection-root ()
  "Return the collection root used to save the current request buffer."
  (or courier--collection-root-hint
      (courier--collection-root)
      (let* ((name (courier--read-collection-name))
             (selected-root
              (or (when-let* ((candidate
                               (assoc name (courier--collection-candidates))))
                    (plist-get (cdr candidate) :root))
                  (let ((root (courier--collection-root-for-name name)))
                    (courier--create-collection root name)))))
        (file-name-as-directory (expand-file-name selected-root)))))

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

(defun courier--read-save-request-path (request-root request-name)
  "Read the request path under REQUEST-ROOT for REQUEST-NAME."
  (let* ((default-name (file-name-base (courier--request-file-name request-name)))
         (typed-name (string-trim
                      (read-string "Request file name: " default-name)))
         (file-name (if (string-suffix-p ".http" typed-name)
                        typed-name
                      (concat typed-name ".http"))))
    (when (string-empty-p typed-name)
      (user-error "Request file name cannot be empty"))
    (when (string-match-p "[/\\]" file-name)
      (user-error "Request file name cannot contain path separators"))
    (expand-file-name file-name request-root)))

(defun courier--write-request-name (buffer name)
  "Ensure BUFFER has Courier request NAME in its active model."
  (with-current-buffer buffer
    (if courier--request-model
        (progn
          (courier--sync-request-model)
          (plist-put courier--request-model :name name)
          (courier--render-request-buffer))
      (user-error "Courier request model is not initialized"))))

(defun courier--migrate-request-artifacts (old-path new-path &optional new-name)
  "Move Courier state from OLD-PATH to NEW-PATH.
When NEW-NAME is non-nil, also update any live response buffer name metadata."
  (when-let* ((response-buffer (courier--response-buffer-for-path old-path)))
    (when (buffer-live-p response-buffer)
      (with-current-buffer response-buffer
        (plist-put courier--request :path new-path)
        (when new-name
          (plist-put courier--request :name new-name))
        (rename-buffer (courier--response-buffer-name courier--request) t)))))

(defun courier--open-collection-label (collection-root)
  "Return the display label for COLLECTION-ROOT in open pickers."
  (courier--collection-name collection-root))

(defun courier--open-request-display-path (path collection-root)
  "Return the display path for request PATH in COLLECTION-ROOT."
  (let* ((request-root (or (courier--collection-requests-root collection-root)
                           collection-root))
         (relative-path (file-relative-name path request-root)))
    (file-name-sans-extension relative-path)))

(defun courier--request-candidate (path collection-root)
  "Return a completion candidate for request PATH in COLLECTION-ROOT."
  (let* ((label (courier--open-request-display-path path collection-root))
         (collection-name (courier--open-collection-label collection-root)))
    (cons (propertize label
                      'courier-group collection-name)
          (list :kind 'request
                :path path
                :collection-root collection-root))))

(defun courier--request-candidates (collection-root)
  "Return completion candidates for Courier request files in COLLECTION-ROOT."
  (mapcar (lambda (path)
            (courier--request-candidate path collection-root))
          (courier--request-files collection-root)))

(defun courier--env-file-candidate (entry collection-root)
  "Return a completion candidate for env file ENTRY in COLLECTION-ROOT."
  (let* ((name (car entry))
         (path (cdr entry))
         (relative-path (file-relative-name path collection-root))
         (collection-name (courier--open-collection-label collection-root))
         (label name)
         (annotation relative-path))
    (cons (propertize label
                      'courier-group collection-name
                      'courier-annotation annotation)
          (list :kind 'env-file
                :path path
                :collection-root collection-root))))

(defun courier--env-file-candidates (entries collection-root)
  "Return completion candidates for env file ENTRIES in COLLECTION-ROOT."
  (let ((sorted-entries
         (sort (copy-sequence entries)
               (lambda (left right)
                 (if (string= (car left) (car right))
                     (string< (cdr left) (cdr right))
                   (string< (car left) (car right)))))))
    (mapcar (lambda (entry)
              (courier--env-file-candidate entry collection-root))
            sorted-entries)))

(defun courier--discover-collection-roots (&optional base-directory)
  "Return Courier collection roots discoverable under BASE-DIRECTORY.

When BASE-DIRECTORY is nil, discover collections directly under the Courier
home `collections/' directory."
  (let* ((base (file-name-as-directory
                (expand-file-name
                 (or base-directory
                     (courier--collections-directory)))))
         roots)
    (when (file-directory-p base)
      (dolist (entry (directory-files base t directory-files-no-dot-files-regexp t))
        (when (and (file-directory-p entry)
                   (file-exists-p
                    (expand-file-name courier--collection-marker-file entry)))
          (push (file-name-as-directory entry) roots))))
    (sort roots #'string<)))

(defun courier--open-collection-roots ()
  "Return Courier collections available to `courier-open'."
  (let* ((active (courier--active-collection-root))
         (discovered (courier--discover-collection-roots))
         roots)
    (when active
      (push (file-name-as-directory (expand-file-name active)) roots))
    (dolist (root discovered)
      (push root roots))
    (delete-dups
     (nreverse roots))))

(defun courier--open-env-entries (collection-root)
  "Return environment entries discoverable in COLLECTION-ROOT for `courier-open'."
  (let (entries)
    (dolist (path (directory-files-recursively collection-root "\\.env\\'"))
      (let ((file-name (file-name-nondirectory path)))
        (when (courier--env-file-name-p file-name)
          (push (cons (courier--env-name-for-file file-name) path) entries))))
    (sort entries
          (lambda (left right)
            (string< (format "%s:%s" (car left) (cdr left))
                     (format "%s:%s" (car right) (cdr right)))))))

(defun courier--open-candidates ()
  "Return grouped completion candidates for `courier-open'."
  (let (candidates)
    (dolist (collection-root (courier--open-collection-roots))
      (setq candidates
            (append candidates
                    (courier--request-candidates collection-root))))
    candidates))

(defun courier--open-env-candidates ()
  "Return grouped completion candidates for `courier-open-env'."
  (let (candidates)
    (dolist (collection-root (courier--open-collection-roots))
      (setq candidates
            (append candidates
                    (courier--env-file-candidates
                     (courier--open-env-entries collection-root)
                     collection-root))))
    candidates))

(defun courier--completion-group (candidate transform)
  "Return the completion group title for CANDIDATE.
TRANSFORM is ignored; it is part of the completion metadata protocol."
  (if transform
      candidate
    (or (get-text-property 0 'courier-group candidate)
        "Other")))

(defun courier--completion-affixation (candidates)
  "Return affixation metadata for completion CANDIDATES."
  (mapcar
   (lambda (candidate)
     (list candidate
           ""
           (if-let* ((annotation (get-text-property 0 'courier-annotation candidate)))
               (propertize (format "  %s" annotation) 'face 'shadow)
             "")))
   candidates))

(defun courier--completion-table (candidates &rest metadata)
  "Return a completion table for CANDIDATES carrying METADATA.

CANDIDATES is an alist whose car values are the strings presented to the user.
METADATA entries follow `completion-metadata'."
  (let ((labels (mapcar #'car candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          (cons 'metadata metadata)
        (complete-with-action action labels string pred)))))

(defun courier--collection-candidate (root)
  "Return a completion candidate for Courier collection ROOT."
  (let ((label (courier--collection-name root)))
    (cons (propertize label
                      'courier-group "Collections"
                      'courier-annotation (abbreviate-file-name root))
          (list :kind 'collection
                :root root))))

(defun courier--collection-candidates ()
  "Return completion candidates for discoverable Courier collections."
  (mapcar #'courier--collection-candidate
          (courier--open-collection-roots)))

(defun courier--set-active-env (name)
  "Set the current request buffer environment to NAME."
  (setq courier--active-env name)
  (force-mode-line-update)
  (message "Courier environment set to %s." courier--active-env))

(defun courier--open-request-file (path)
  "Open Courier request file at PATH in a normal file-visiting buffer."
  (let ((buffer (find-file-noselect path)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'courier-request-mode)
        (courier-request-mode)))
    buffer))

(defun courier--open-env-file (path)
  "Open Courier env file at PATH in a normal file-visiting buffer."
  (let ((buffer (find-file-noselect path)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'courier-env-mode)
        (when (courier--buffer-env-file-p path)
          (courier-env-mode))))
    buffer))

(defun courier--handle-open-selection (selection candidates)
  "Handle SELECTION from CANDIDATES."
  (pcase (plist-get (cdr (assoc selection candidates)) :kind)
    ('request
     (courier--open-request-file
      (plist-get (cdr (assoc selection candidates)) :path)))
    (_
     (user-error "Unknown Courier selection: %s" selection))))

(defun courier--handle-open-env-selection (selection candidates)
  "Handle env file SELECTION from CANDIDATES."
  (pcase (plist-get (cdr (assoc selection candidates)) :kind)
    ('env-file
     (courier--open-env-file
      (plist-get (cdr (assoc selection candidates)) :path)))
    (_
     (user-error "Unknown Courier env selection: %s" selection))))

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
  (let ((names (courier--available-env-names entries))
        (selected (courier--selected-env-name-no-prompt entries)))
    (cond
     ((null names) nil)
     (selected
      (setq courier--active-env selected)
      (force-mode-line-update)
      selected)
     (t
      (setq courier--active-env (courier--read-env-name names))
      (force-mode-line-update)
      courier--active-env))))

(defun courier--selected-env-name-no-prompt (entries)
  "Return the selected environment name from ENTRIES without prompting."
  (let ((names (courier--available-env-names entries)))
    (cond
     ((null names) nil)
     ((and courier--active-env
           (member courier--active-env names))
      courier--active-env)
     (t
      (courier--preferred-env-name entries)))))

(defun courier--env-vars-for-name (entries env-name)
  "Return merged environment variables from ENTRIES for ENV-NAME."
  (let ((merged nil))
    (dolist (entry entries)
      (when (string= (car entry) env-name)
        (setq merged
              (courier-merge-vars merged
                                  (courier-parse-env-file (cdr entry))))))
    merged))

(defun courier--current-env-selection ()
  "Return the current environment selection as `(ENV-NAME . VARS)'."
  (let* ((entries (courier--available-env-entries))
         (env-name (courier--selected-env-name entries))
         (collection-root (courier--active-collection-root))
         (env-vars (if env-name
                       (courier--env-vars-for-name entries env-name)
                     nil))
         (runtime-vars (when collection-root
                         (courier--read-runtime-vars collection-root env-name))))
    (cons env-name
          (courier-merge-vars env-vars runtime-vars))))

(defun courier--placeholder-name-at-point ()
  "Return the `{{name}}' placeholder under point, or nil."
  (let ((position (point))
        match)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (not match)
                  (re-search-forward "{{\\([^{}[:space:]]+\\)}}" (line-end-position) t))
        (when (and (<= (match-beginning 0) position)
                   (<= position (match-end 0)))
          (setq match (match-string-no-properties 1)))))
    match))

(defun courier--definition-key-at-point ()
  "Return the `key = value' definition key at point, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*")
      (when (and (<= (match-beginning 1) (point))
                 (<= (point) (match-end 1)))
        (match-string-no-properties 1)))))

(defun courier--xref-identifier-at-point ()
  "Return the Courier variable identifier under point, or nil."
  (or (courier--placeholder-name-at-point)
      (courier--definition-key-at-point)))

(defun courier--xref-file-location (path regexp summary)
  "Return an xref item for PATH matching REGEXP with SUMMARY, or nil."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (let ((line (line-number-at-pos (match-beginning 0)))
              (column (save-excursion
                        (goto-char (match-beginning 0))
                        (current-column))))
          (xref-make summary
                     (xref-make-file-location path line column)))))))

(defun courier--request-local-var-section (name request)
  "Return the request-local section symbol defining NAME in REQUEST, or nil."
  (cond
   ((assoc-string name (plist-get request :pre-request-vars) nil)
    'pre-request-vars)
   ((assoc-string name (plist-get request :vars) nil)
    'vars)
   (t
    nil)))

(defun courier--request-local-var-xref (name section)
  "Return a request-buffer xref for NAME defined in SECTION, or nil."
  (let ((buffer (current-buffer))
        (heading (pcase section
                   ('pre-request-vars "[vars.pre_request]")
                   ('vars "[vars]")
                   (_ (user-error "Unsupported request var section: %s" section)))))
    (courier--request-jump-section 'vars)
    (save-excursion
      (goto-char (courier--request-content-start-position))
      (when (re-search-forward (format "^%s$" (regexp-quote heading)) nil t)
        (forward-line 1)
        (let ((section-start (point))
              (section-end (or (and (re-search-forward "^\\[" nil t)
                                    (match-beginning 0))
                               (point-max))))
          (goto-char section-start)
          (when (re-search-forward (format "^%s\\s-*="
                                           (regexp-quote name))
                                   section-end t)
            (xref-make (format "%s (%s)"
                               name
                               (pcase section
                                 ('pre-request-vars "request vars.pre_request")
                                 ('vars "request vars")))
                       (xref-make-buffer-location buffer (match-beginning 0)))))))))

(defun courier--env-var-xref (entries env-name name)
  "Return an env-file xref for NAME from ENTRIES and ENV-NAME, or nil."
  (let ((regexp (format "^\\s-*%s\\s-*=" (regexp-quote name))))
    (seq-some
     (lambda (entry)
       (when (string= (car entry) env-name)
         (courier--xref-file-location
          (cdr entry)
          regexp
          (format "%s (%s env)" name env-name))))
     entries)))

(defun courier--defaults-var-xref (path name summary)
  "Return a defaults xref for NAME in config PATH with SUMMARY, or nil."
  (when (assoc-string name
                      (plist-get (courier--json-defaults-config
                                  (courier--read-json-file path))
                                 :vars)
                      nil)
    (courier--xref-file-location
     path
     (format "^[[:space:]]*\"%s\"[[:space:]]*:"
             (regexp-quote name))
     summary)))

(defun courier--request-default-var-xref (name request)
  "Return a defaults xref for NAME visible to REQUEST, or nil."
  (let* ((request-path (plist-get request :path))
         (collection-root
          (or (and request-path (courier--collection-root request-path))
              (courier--active-collection-root)))
         (request-directory
          (or (and request-path (file-name-directory request-path))
              default-directory))
         (directories (reverse
                       (courier--request-default-directories
                        collection-root
                        request-directory))))
    (or
     (seq-some
      (lambda (directory)
        (let ((config-path (expand-file-name courier--collection-marker-file directory)))
          (when (file-exists-p config-path)
            (courier--defaults-var-xref
             config-path
             name
             (format "%s (%s defaults)"
                     name
                     (abbreviate-file-name directory))))))
      directories)
     (when-let* ((config-path (and collection-root
                                   (courier--collection-config-path collection-root))))
       (courier--defaults-var-xref
        config-path
        name
        (format "%s (%s defaults)"
                name
                (courier--collection-name collection-root)))))))

(defun courier--request-xref-definitions (name)
  "Return xref definitions for NAME from the current request buffer."
  (courier--sync-request-model)
  (let* ((request courier--request-model)
         (local-section (courier--request-local-var-section name request)))
    (cond
     (local-section
      (when-let* ((xref (courier--request-local-var-xref name local-section)))
        (list xref)))
     (t
      (let* ((entries (courier--available-env-entries))
             (env-name (courier--selected-env-name-no-prompt entries)))
        (seq-filter
         #'identity
         (list (and env-name
                    (courier--env-var-xref entries env-name name))
               (courier--request-default-var-xref name request))))))))

(defun courier--env-buffer-xref-definitions (name)
  "Return xref definitions for NAME from the current env buffer."
  (let* ((path (and buffer-file-name (expand-file-name buffer-file-name)))
         (regexp (format "^\\s-*%s\\s-*=" (regexp-quote name)))
         (self (and path
                    (courier--xref-file-location
                     path regexp
                     (format "%s (%s)"
                             name
                             (file-name-nondirectory path)))))
         (collection-root (and path (courier--collection-root path))))
    (seq-filter
     #'identity
     (list self
           (and collection-root
                (courier--defaults-var-xref
                 (courier--collection-config-path collection-root)
                 name
                 (format "%s (%s defaults)"
                         name
                         (courier--collection-name collection-root))))))))

(defun courier--xref-backend ()
  "Return the Courier xref backend when appropriate for the current buffer."
  (when (or (derived-mode-p 'courier-request-mode)
            (derived-mode-p 'courier-env-mode))
    'courier))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql courier)))
  "Return the Courier identifier under point."
  (courier--xref-identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql courier)) identifier)
  "Return Courier xref definitions for IDENTIFIER."
  (cond
   ((derived-mode-p 'courier-request-mode)
    (courier--request-xref-definitions identifier))
   ((derived-mode-p 'courier-env-mode)
    (courier--env-buffer-xref-definitions identifier))
   (t
    nil)))

(defun courier--prepared-current-request ()
  "Return the current request after env selection and pre-request scripts."
  (courier--sync-request-model)
  (let* ((parsed-request (if courier--request-model
                             (copy-tree courier--request-model t)
                           (courier-parse-buffer)))
         (defaulted-request (courier--apply-request-defaults parsed-request))
         (env-selection (courier--current-env-selection))
         (env-name (car env-selection))
         (env-vars (cdr env-selection))
         (base-vars (courier-merge-vars
                     (plist-get defaulted-request :default-vars)
                     env-vars))
         (request-with-vars
          (plist-put defaulted-request :vars
                     (courier-merge-vars
                      (plist-get defaulted-request :vars)
                      (plist-get defaulted-request :pre-request-vars))))
         (request (courier--run-pre-request-script request-with-vars base-vars)))
    (plist-put request :env-name env-name)
    (plist-put request :env-vars base-vars)
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

(defun courier--preview-insert-label (label)
  "Insert preview LABEL with highlighting."
  (insert (propertize label 'face 'font-lock-keyword-face)))

(defun courier--preview-insert-section (title)
  "Insert preview section TITLE with highlighting."
  (insert (propertize title 'face 'font-lock-function-name-face) "\n"))

(defun courier--preview-request (request argv)
  "Render REQUEST preview with ARGV into the preview buffer."
  (let ((buffer (courier--preview-buffer))
        (method (plist-get request :method)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (courier--preview-insert-label "Name:    ")
        (insert (or (plist-get request :name) "(none)") "\n")
        (courier--preview-insert-label "Method:  ")
        (insert (propertize method 'face (courier--method-overlay-face method)) "\n")
        (courier--preview-insert-label "URL:     ")
        (insert (propertize (plist-get request :url) 'face 'link) "\n\n")
        (courier--preview-insert-section "Headers:")
        (if-let* ((headers (plist-get request :headers)))
            (dolist (header headers)
              (insert "  "
                      (propertize (car header) 'face 'font-lock-variable-name-face)
                      ": " (cdr header) "\n"))
          (insert (propertize "  (none)\n" 'face 'shadow)))
        (insert "\n")
        (courier--preview-insert-section "Body:")
        (if (string-empty-p (or (plist-get request :body) ""))
            (insert (propertize "  (empty)\n" 'face 'shadow))
          (insert (plist-get request :body) "\n"))
        (insert "\n")
        (courier--preview-insert-section "Curl argv:")
        (dolist (arg argv)
          (insert "  " (propertize arg 'face 'font-lock-string-face) "\n"))
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
  (if courier--request-model
      (plist-get courier--request-model :method)
    (save-excursion
      (when (courier--goto-request-line)
        (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
          (match-string-no-properties 1))))))

(defun courier--method-overlay-face (method)
  "Return the overlay face used for request METHOD."
  (cond
   ((equal method "GET") 'courier-request-method-get-face)
   ((equal method "POST") 'courier-request-method-post-face)
   ((member method '("PUT" "PATCH" "DELETE")) 'courier-request-method-write-face)
   (t 'courier-request-method-default-face)))

(defun courier--current-method-bounds ()
  "Return `(START . END)' for the current request method token, or nil."
  (if courier--request-model
      (save-excursion
        (goto-char (point-min))
        (beginning-of-line)
        (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
          (cons (match-beginning 1) (match-end 1))))
    (save-excursion
      (when (courier--goto-request-line)
        (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+.*\\)?$")
          (cons (match-beginning 1) (match-end 1)))))))

(defun courier--after-change-refresh-method-overlay (&rest _args)
  "Refresh the Courier request method overlay."
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
        (overlay-put overlay 'help-echo
                     (format "Courier method: %s. Use C-c C-m to change it."
                             method)))
    (courier--delete-method-overlay)))

(defun courier--current-request-url-bounds ()
  "Return `(START . END)' for the current request URL token, or nil."
  (if courier--request-model
      (save-excursion
        (goto-char (point-min))
        (beginning-of-line)
        (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+\\(\\S-+\\)\\)?\\s-*$")
          (when (match-beginning 2)
            (cons (match-beginning 2) (match-end 2)))))
    (save-excursion
      (when (courier--goto-request-line)
        (beginning-of-line)
        (when (looking-at "^\\([A-Z]+\\)\\(?:\\s-+\\(\\S-+\\)\\)?\\s-*$")
          (when (match-beginning 2)
            (cons (match-beginning 2) (match-end 2))))))))

(defun courier--current-request-url ()
  "Return the current request URL token.
Signal `user-error' when the current request line has no URL."
  (if-let* ((bounds (courier--current-request-url-bounds)))
      (buffer-substring-no-properties (car bounds) (cdr bounds))
    (user-error "Current request URL is empty")))

(defun courier--replace-current-request-url (url)
  "Replace the current request URL token with URL."
  (if courier--request-model
      (progn
        (courier--sync-request-model)
        (plist-put courier--request-model :url url)
        (courier--render-request-buffer))
    (unless (courier--goto-request-line)
      (user-error "No Courier request in this buffer"))
    (beginning-of-line)
    (cond
     ((looking-at "^\\([A-Z]+\\)\\(\\s-+\\)\\(\\S-+\\)\\(\\s-*\\)$")
      (replace-match url t t nil 3))
     ((looking-at "^\\([A-Z]+\\)\\(\\s-*\\)$")
      (goto-char (match-end 1))
      (insert " " url))
     (t
      (user-error "Cannot replace the current request URL")))))

(defun courier--split-request-url (url)
  "Split URL into `(BASE QUERY FRAGMENT)'."
  (let* ((fragment-pos (string-match "#" url))
         (fragment (and fragment-pos (substring url fragment-pos)))
         (without-fragment (if fragment-pos
                               (substring url 0 fragment-pos)
                             url))
         (query-pos (string-match "\\?" without-fragment))
         (base (if query-pos
                   (substring without-fragment 0 query-pos)
                 without-fragment))
         (query (and query-pos
                     (substring without-fragment (1+ query-pos)))))
    (list base query fragment)))

(defun courier--decode-query-component (value)
  "Decode query component VALUE."
  (url-unhex-string (replace-regexp-in-string "\\+" "%20" value t t)))

(defun courier--parse-url-query-params (url)
  "Return decoded query params from URL as an alist."
  (pcase-let ((`(,_base ,query ,_fragment) (courier--split-request-url url)))
    (if (or (null query) (string-empty-p query))
        nil
      (mapcar
       (lambda (part)
         (pcase-let ((`(,raw-key ,raw-value)
                      (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" part)
                          (list (match-string 1 part) (match-string 2 part))
                        (list part ""))))
           (cons (courier--decode-query-component raw-key)
                 (courier--decode-query-component raw-value))))
       (split-string query "&" t)))))

(defun courier--encode-url-query-params (params)
  "Encode PARAMS alist into a query string."
  (mapconcat
   (lambda (param)
     (format "%s=%s"
             (url-hexify-string (car param))
             (url-hexify-string (cdr param))))
   params "&"))

(defun courier--update-request-url-query (url params)
  "Return URL with PARAMS replacing its query string."
  (pcase-let ((`(,base ,_query ,fragment) (courier--split-request-url url)))
    (concat base
            (if params
                (concat "?" (courier--encode-url-query-params params))
              "")
            (or fragment ""))))

(defun courier--format-params-editor-lines (params)
  "Return editable text for PARAMS."
  (if params
      (mapconcat (lambda (param)
                   (format "%s = %s" (car param) (cdr param)))
                 params
                 "\n")
    ""))

(defun courier--parse-params-editor-buffer ()
  "Parse the current Courier params editor buffer into an alist."
  (let (params)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim-right
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line))
            (unless (string-match "\\`\\([^=[:space:]][^=]*\\)\\s-*=\\s-*\\(.*\\)\\'" line)
              (user-error "Invalid params line: %s" line))
            (let ((name (string-trim (match-string 1 line)))
                  (value (string-trim (match-string 2 line))))
              (when (string-empty-p name)
                (user-error "Param name cannot be empty"))
              (push (cons name value) params))))
        (forward-line 1)))
    (nreverse params)))

(defun courier--params-editor-buffer-name (source-buffer)
  "Return the params editor buffer name for SOURCE-BUFFER."
  (format "*courier-params: %s*" (buffer-name source-buffer)))

;;;###autoload
(define-derived-mode courier-request-params-mode text-mode "Courier-Params"
  "Major mode for editing Courier URL query params."
  (setq-local header-line-format
              " Edit query params as key = value. C-c C-c applies. C-c C-k cancels. "))

(define-key courier-request-params-mode-map (kbd "C-c C-c") #'courier-request-params-apply)
(define-key courier-request-params-mode-map (kbd "C-c C-k") #'courier-request-params-cancel)

(defun courier--buffer-env-file-p (&optional path)
  "Return non-nil when PATH or the current buffer visits a Courier env file."
  (when-let* ((file (and (or path buffer-file-name)
                         (expand-file-name (or path buffer-file-name))))
              ((courier--env-file-name-p (file-name-nondirectory file)))
              (env-dir (courier--collection-env-dir file)))
    (file-equal-p (file-name-directory file)
                  (file-name-as-directory env-dir))))

;;;###autoload
(defun courier-env-file-p (&optional path)
  "Return non-nil when PATH or the current buffer visits a Courier env file."
  (courier--buffer-env-file-p path))

;;;###autoload
(define-derived-mode courier-env-mode text-mode "Courier-Env"
  "Major mode for editing Courier environment files."
  (setq-local font-lock-defaults '(courier-env-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (add-hook 'xref-backend-functions #'courier--xref-backend nil t))

;;;###autoload
(defun courier-request-params-apply ()
  "Apply the current params editor contents back to the source request URL."
  (interactive)
  (unless (derived-mode-p 'courier-request-params-mode)
    (user-error "Not in a Courier params editor"))
  (let ((params (courier--parse-params-editor-buffer))
        (source-buffer courier--params-source-buffer)
        (editor-buffer (current-buffer)))
    (unless (buffer-live-p source-buffer)
      (user-error "Source Courier request buffer is no longer live"))
    (with-current-buffer source-buffer
      (courier--sync-request-model)
      (if courier--request-model
          (progn
            (plist-put courier--request-model :params params)
            (plist-put courier--request-model :url
                       (car (courier--split-request-url
                             (or (plist-get courier--request-model :url) ""))))
            (courier--render-request-buffer))
        (save-excursion
          (courier--replace-current-request-url
           (courier--update-request-url-query
            (courier--current-request-url)
            params))))
      (set-buffer-modified-p t))
    (when-let* ((window (get-buffer-window editor-buffer)))
      (quit-window t window))
    (when (buffer-live-p editor-buffer)
      (kill-buffer editor-buffer))
    (message "Courier params applied.")))

;;;###autoload
(defun courier-request-params-cancel ()
  "Close the current Courier params editor without applying edits."
  (interactive)
  (unless (derived-mode-p 'courier-request-params-mode)
    (user-error "Not in a Courier params editor"))
  (when-let* ((window (get-buffer-window (current-buffer))))
    (quit-window t window))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun courier-request-edit-params ()
  "Edit the current request URL query params in a dedicated buffer."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (params (if courier--request-model
                     (copy-tree (courier--request-effective-params courier--request-model) t)
                   (courier--parse-url-query-params (courier--current-request-url))))
         (buffer (get-buffer-create
                  (courier--params-editor-buffer-name source-buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (courier--format-params-editor-lines params))
        (unless params
          (insert "# Add one param per line: key = value\n"))
        (goto-char (point-min))
        (courier-request-params-mode)
        (setq-local courier--params-source-buffer source-buffer)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(define-derived-mode courier-request-mode text-mode "Courier"
  "Major mode for editing Courier request files."
  (let ((current-path (and buffer-file-name (expand-file-name buffer-file-name))))
    (courier--reset-request-buffer-state-for-path current-path)
    (setq-local courier--request-path current-path))
  (setq-local courier--request-divider-width nil)
  (setq-local mode-name '(:eval (courier--mode-line-lighter)))
  (setq-local font-lock-defaults '(courier-request-font-lock-keywords))
  (setq-local header-line-format nil)
  (setq-local outline-regexp "^[A-Z]+ ")
  (setq-local revert-buffer-function #'courier-request-revert-buffer)
  (add-hook 'xref-backend-functions #'courier--xref-backend nil t)
  (add-hook 'window-configuration-change-hook
            #'courier--refresh-request-divider-after-window-change nil t)
  (add-hook 'completion-at-point-functions #'courier--tests-dsl-capf nil t)
  (add-hook 'completion-at-point-functions #'courier--post-response-var-from-capf nil t)
  (add-hook 'completion-at-point-functions #'courier--content-type-capf nil t)
  (setq-local courier--collection-root-hint
              (or courier--collection-root-hint
                  (courier--collection-root)))
  (add-hook 'after-change-functions
            #'courier--after-change-refresh-method-overlay nil t)
  (add-hook 'change-major-mode-hook #'courier--delete-method-overlay nil t)
  (add-hook 'change-major-mode-hook #'courier--delete-request-divider-overlays nil t)
  (courier--refresh-method-overlay)
  (unless courier--active-env
    (when-let* ((preferred-env (courier--preferred-env-name
                                (courier--available-env-entries))))
      (setq-local courier--active-env preferred-env)))
  (unless courier--request-model
    (setq-local courier--request-model (courier--parse-buffer-for-editor)))
  (setq-local courier--request-tab (or courier--request-tab 'body))
  (courier--render-request-buffer))

;;;###autoload
(defun courier-request-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert the current Courier request buffer from disk."
  (interactive)
  (unless buffer-file-name
    (user-error "Current Courier request has no file to revert"))
  (setq-local courier--request-model (courier--parse-file-for-editor buffer-file-name))
  (courier--render-request-buffer)
  (set-buffer-modified-p nil)
  (message "Courier request reverted."))

;;;###autoload
(defun courier-request-validate ()
  "Parse and validate the current Courier request buffer."
  (interactive)
  (courier--sync-request-model)
  (let ((request (copy-tree courier--request-model t)))
    (courier-validate-request request)
    (message "Courier request is valid.")))

;;;###autoload
(defun courier-request-preview ()
  "Preview the resolved Courier request in a read-only buffer."
  (interactive)
  (let* ((resolved (courier--resolved-current-request))
         (header-file (make-temp-file "courier-preview-" nil ".headers"))
         (body-file (make-temp-file "courier-preview-" nil ".body"))
         (request-body-file (make-temp-file "courier-preview-" nil ".request-body"))
         (meta-file (make-temp-file "courier-preview-" nil ".meta")))
    (unwind-protect
        (courier--preview-request
         resolved
         (courier-build-curl-command
          resolved header-file body-file meta-file request-body-file))
      (dolist (path (list header-file body-file request-body-file meta-file))
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
  (courier--sync-request-model)
  (plist-put courier--request-model :method method)
  (courier--render-request-buffer)
  (message "Courier method set to %s." method))

(defun courier--read-request-body-type ()
  "Read a request body type for the current Courier request buffer."
  (intern
   (completing-read "Body type: "
                    (mapcar #'symbol-name courier--allowed-body-types)
                    nil t nil nil
                    (symbol-name
                     (courier--request-body-type
                      (or courier--request-model
                          (courier--parse-buffer-for-editor)))))))

(defun courier--request-set-body-type (body-type)
  "Set the current request body type to BODY-TYPE."
  (unless (memq body-type courier--allowed-body-types)
    (user-error "Unsupported body type: %s" body-type))
  (courier--sync-request-model)
  (plist-put courier--request-model :body-type body-type)
  (courier--request-apply-plist
   courier--request-model
   (courier--request-default-body-state body-type))
  (setq courier--request-tab 'body)
  (courier--refresh-request-content)
  (set-buffer-modified-p t)
  (message "Courier body type set to %s." (courier--body-type-label body-type)))

;;;###autoload
(defun courier-request-attach-file (path)
  "Attach file PATH to the current request body."
  (interactive
   (list
    (read-file-name "Attach file: "
                    (courier--request-attachment-base-directory)
                    nil t)))
  (let ((absolute-path (expand-file-name path)))
    (when (file-directory-p absolute-path)
      (user-error "Cannot attach a directory"))
    (courier--sync-request-model)
    (let* ((current-body-type (courier--request-body-type courier--request-model))
           (target-body-type
            (if (memq current-body-type '(binary multipart))
                current-body-type
              (intern
               (completing-read "Attach as body type: "
                                '("binary" "multipart")
                                nil t nil nil "binary"))))
           (relative-path (courier--request-relative-attachment-path absolute-path))
           (mime-type (courier--mime-type-for-extension absolute-path)))
      (pcase target-body-type
        ('binary
         (plist-put courier--request-model :body-type 'binary)
         (plist-put courier--request-model :body "")
         (plist-put courier--request-model :body-parts nil)
         (plist-put courier--request-model :body-file-path relative-path)
         (plist-put courier--request-model :body-file-content-type mime-type))
        ('multipart
         (plist-put courier--request-model :body-type 'multipart)
         (plist-put courier--request-model :body "")
         (plist-put courier--request-model :body-file-path nil)
         (plist-put courier--request-model :body-file-content-type nil)
         (plist-put courier--request-model :body-parts
                    (append (when (eq current-body-type 'multipart)
                              (copy-tree (plist-get courier--request-model :body-parts) t))
                            (list (list :name (file-name-base absolute-path)
                                        :kind 'file
                                        :path relative-path
                                        :content-type mime-type)))))
        (_
         (user-error "Attach file requires binary or multipart body type")))
      (setq courier--request-tab 'body)
      (courier--refresh-request-content)
      (set-buffer-modified-p t)
      (message "Courier attached %s." (file-name-nondirectory absolute-path)))))

(defun courier--read-response-var-rule ()
  "Read a response var rule from the minibuffer."
  (let* ((source (completing-read "Response var source: "
                                  courier--post-response-var-sources
                                  nil t nil nil "json"))
         (name (string-trim (read-string "Response var name: ")))
         (expr (pcase source
                 ("json"
                  (string-trim (read-string "JSON expr: ")))
                 ("header"
                  (string-trim (read-string "Header name: ")))
                 (_ nil))))
    (list source name expr)))

(defun courier--request-add-response-var (source name &optional expr)
  "Append a post-response var rule using SOURCE, NAME, and EXPR."
  (when (string-empty-p name)
    (user-error "Response var name must not be empty"))
  (when (and (member source '("json" "header"))
             (or (null expr) (string-empty-p expr)))
    (user-error "%s response vars require expr"
                (capitalize source)))
  (courier--sync-request-model)
  (plist-put
   courier--request-model :post-response-vars
   (append (copy-tree (plist-get courier--request-model :post-response-vars) t)
           (list (append (list :name name
                               :from (intern source))
                         (when (and expr (not (string-empty-p expr)))
                           (list :expr expr))))))
  (setq courier--request-tab 'vars)
  (courier--refresh-request-content)
  (set-buffer-modified-p t)
  (message "Courier response var %s added." name))

;;;###autoload
(defun courier-request-section-type ()
  "Run the type-switching command appropriate for the current section.
In the Body section, switch body type.  In the Auth section, switch auth
type.  In the Vars section, add a response var rule."
  (interactive)
  (pcase (courier--request-current-section)
    ('body
     (courier--request-set-body-type
      (courier--read-request-body-type)))
    ('auth
     (courier--request-set-auth-type
      (courier--read-request-auth-type)))
    ('vars
     (pcase-let ((`(,source ,name ,expr) (courier--read-response-var-rule)))
       (courier--request-add-response-var source name expr)))
    ('tests
     (user-error "No section action for Tests.  Use completion-at-point"))
    (section
     (user-error "No section action for %s"
                 (courier--request-section-label section)))))

(defun courier--read-request-auth-type ()
  "Read an auth type for the current Courier request buffer."
  (intern
   (completing-read "Auth type: "
                    (mapcar #'symbol-name courier--allowed-auth-types)
                    nil t nil nil
                    (symbol-name
                     (courier--request-auth-type
                      (or courier--request-model
                          (courier--parse-buffer-for-editor)))))))

(defun courier--request-set-auth-type (auth-type)
  "Set the current request auth type to AUTH-TYPE."
  (unless (memq auth-type courier--allowed-auth-types)
    (user-error "Unsupported auth type: %s" auth-type))
  (courier--sync-request-model)
  (plist-put courier--request-model :auth
             (pcase auth-type
               ('none
                '(:type none))
               ('bearer
                (list :type 'bearer
                      :token "{{token}}"))
               ('basic
                (list :type 'basic
                      :username "{{user}}"
                      :password "{{password}}"))
               ('header
                (list :type 'header
                      :header "X-API-Key"
                      :value "{{token}}"))
               ('api_key
                (list :type 'api_key
                      :in "header"
                      :name "x-api-key"
                      :value "{{token}}"))
               ('oauth2
                (list :type 'oauth2
                      :grant-type "client_credentials"
                      :token-url "{{token_url}}"
                      :client-id "{{client_id}}"
                      :client-secret "{{client_secret}}"
                      :scopes '("read")))
               (_
                (user-error "Unsupported auth type: %s" auth-type))))
  (setq courier--request-tab 'auth)
  (courier--refresh-request-content)
  (set-buffer-modified-p t)
  (message "Courier auth type set to %s." (courier--auth-type-label auth-type)))

(defun courier--request-jump-section (section)
  "Show request SECTION in the current Courier buffer."
  (unless (memq section (courier--request-all-sections))
    (user-error "Unknown Courier section: %s" section))
  (courier--sync-request-model)
  (setq courier--request-tab section)
  (courier--refresh-request-content))

;;;###autoload
(defun courier-request-jump-section (section)
  "Jump to request SECTION in the current Courier buffer."
  (interactive
   (let* ((choices (courier--request-jump-choices))
          (current-section (courier--request-current-section))
          (current (and (memq current-section
                              (append courier--request-primary-sections
                                      courier--request-secondary-sections))
                        (car (rassoc current-section choices))))
          (selection (completing-read "Request section: " choices nil t nil nil current)))
     (list (cdr (assoc selection choices)))))
  (courier--request-jump-section section))

;;;###autoload
(defun courier-request-save-buffer ()
  "Save the current Courier request buffer.
Unsaved request buffers choose a collection on first save."
  (interactive)
  (courier--sync-request-model)
  (if buffer-file-name
      (progn
        (write-region (courier--serialize-request courier--request-model) nil buffer-file-name nil 'silent)
        (set-buffer-modified-p nil))
    (let* ((collection-root (courier--read-save-collection-root))
           (request-root (courier--preferred-requests-root collection-root))
           (request-name (courier--buffer-request-name))
           (path (courier--read-save-request-path request-root request-name)))
      (make-directory request-root t)
      (when (and (file-exists-p path)
                 (not (equal (expand-file-name path)
                             (and buffer-file-name (expand-file-name buffer-file-name))))
                 (not (y-or-n-p
                       (format "Overwrite existing request %s? "
                               (abbreviate-file-name path)))))
        (user-error "Courier save aborted"))
      (set-visited-file-name path)
      (setq-local default-directory (file-name-directory path))
      (setq-local courier--request-path path)
      (setq-local courier--collection-root-hint collection-root)
      (write-region (courier--serialize-request courier--request-model) nil path nil 'silent)
      (set-buffer-modified-p nil)
      (message "Courier request saved to %s" path))))

;;;###autoload
(defun courier-request-send ()
  "Send the current Courier request buffer with curl."
  (interactive)
  (let ((resolved
         (condition-case err
             (courier--resolved-current-request)
           (error
            (let* ((request (courier--current-request-error-snapshot))
                   (response (courier--request-preparation-error-response
                              request err))
                   (response-buffer (courier--display-response-buffer request)))
              (with-current-buffer response-buffer
                (courier--render-response response request)))
            nil))))
    (when resolved
      (let ((path (plist-get resolved :path)))
        (when-let* ((process (courier--in-flight-process-for-path path)))
          (unless (yes-or-no-p "Request in progress.  Cancel and resend? ")
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
                    courier--temp-files
                    (process-get process 'courier-temp-files)))))))))

;;;###autoload
(defun courier-open ()
  "Open another Courier request."
  (interactive)
  (let ((candidates (courier--open-candidates)))
    (unless candidates
      (user-error "No Courier requests found"))
    (let* ((table (courier--completion-table
                   candidates
                   '(category . courier-open)
                   '(group-function . courier--completion-group)
                   '(affixation-function . courier--completion-affixation)))
           (selection
            (completing-read
             "Courier open: "
             table nil t)))
      (courier--handle-open-selection selection candidates))))

;;;###autoload
(defun courier-open-env ()
  "Open a Courier environment file."
  (interactive)
  (let ((candidates (courier--open-env-candidates)))
    (unless candidates
      (user-error "No Courier environment files found"))
    (let* ((table (courier--completion-table
                   candidates
                   '(category . courier-open-env)
                   '(group-function . courier--completion-group)
                   '(affixation-function . courier--completion-affixation)))
           (selection
            (completing-read
             "Courier env: "
             table nil t)))
      (courier--handle-open-env-selection selection candidates))))

;;;###autoload
(defun courier-new-request ()
  "Create a new unsaved Courier request draft."
  (interactive)
  (let* ((name (courier--next-untitled-request-name))
         (origin-root (courier--collection-root))
         (buffer (generate-new-buffer (courier--draft-buffer-name name))))
    (with-current-buffer buffer
      (setq-local default-directory (or origin-root default-directory))
      (insert (courier--request-skeleton name))
      (courier-request-mode)
      (setq-local courier--collection-root-hint origin-root)
      (goto-char (point-min))
      (end-of-line))
    (switch-to-buffer buffer)
    (message "Courier draft created. Save it into a collection with C-x C-s.")))

;;;###autoload
(defun courier-create-collection (name)
  "Create a Courier collection named NAME under Courier home."
  (interactive
   (list (string-trim (read-string "Create Courier collection: "))))
  (make-directory (courier--collections-directory) t)
  (let* ((root (courier--collection-root-for-name name))
         (collection-root (courier--create-collection root name)))
    (message "Courier collection created at %s" collection-root)
    collection-root))

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
    (message "Courier folder created at %s" directory)))

;;;###autoload
(defun courier-rename-request (name)
  "Rename the current Courier request to NAME.
This renames the request file and updates its front matter name."
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
        (unless (derived-mode-p 'courier-request-mode)
          (courier-request-mode))
        (courier--write-request-name buffer name)
        (unless (equal old-path new-path)
          (rename-file old-path new-path)
          (set-visited-file-name new-path t))
        (setq-local courier--request-path new-path)
        (courier-request-save-buffer))
      (unless (equal old-path new-path)
        (courier--migrate-request-artifacts old-path new-path name))
      (when (and created-buffer-p (buffer-live-p buffer))
        (kill-buffer buffer))
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
        (unless (derived-mode-p 'courier-request-mode)
          (courier-request-mode))
        (set-visited-file-name new-path t)
        (setq-local courier--request-path new-path)
        (courier-request-save-buffer))
      (courier--migrate-request-artifacts old-path new-path)
      (when (and created-buffer-p (buffer-live-p buffer))
        (kill-buffer buffer))
      (message "Courier request moved to %s" new-path))))

;;;###autoload
(transient-define-prefix courier-request-menu ()
  "Show request actions for the current Courier buffer."
  [["Run"
   ("c" "Send" courier-request-send)
   ("p" "Preview" courier-request-preview)
   ("l" "Validate" courier-request-validate)]
   ["Edit"
    ("m" "Method" courier-request-set-method)
    ("t" "Section action" courier-request-section-type)
    ("f" "Attach file" courier-request-attach-file)
    ("e" "Environment" courier-request-switch-env)
    ("s" "Save" courier-request-save-buffer)]
   ["Navigate"
    ("o" "Open request" courier-open)
    ("E" "Open env file" courier-open-env)]
   ["Manage"
    ("n" "New request" courier-new-request)
    ("r" "Rename" courier-rename-request)
    ("F" "New folder" courier-new-folder)
    ("C" "New collection" courier-create-collection)]])

;;;###autoload
(transient-define-prefix courier-response-menu ()
  "Show response actions for the current Courier buffer."
  [["View"
    ("r" "Response" courier-response-view-response)
    ("h" "Headers" courier-response-view-headers)
    ("t" "Timeline" courier-response-view-timeline)
    ("T" "Tests" courier-response-view-tests)
    ("j" "Jump view" courier-response-jump-tab)]
   ["Body"
    ("v" "Body view" courier-response-set-view)
    ("V" "Raw/auto" courier-response-toggle-pretty)
    ("o" "Open body" courier-response-open-body)]
   ["Run"
    ("g" "Retry" courier-response-retry)
    ("k" "Cancel" courier-response-cancel)]
   ["Timeline"
    ("c" "Clear timeline" courier-response-clear-timeline)]])

;;;###autoload
(defun courier-dispatch ()
  "Show the Courier action menu for the current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'courier-request-mode)
    (transient-setup 'courier-request-menu))
   ((derived-mode-p 'courier--response-mode)
    (transient-setup 'courier-response-menu))
   (t
    (user-error "No Courier action menu is available in this buffer"))))

(define-key courier-request-mode-map (kbd "C-c C-c") #'courier-request-send)
(define-key courier-request-mode-map (kbd "C-c ?") #'courier-dispatch)
(define-key courier-request-mode-map (kbd "C-c C-e") #'courier-request-switch-env)
(define-key courier-request-mode-map (kbd "C-c C-f") #'courier-request-attach-file)
(define-key courier-request-mode-map (kbd "C-c C-j") #'courier-request-jump-section)
(define-key courier-request-mode-map (kbd "C-c C-t") #'courier-request-section-type)
(define-key courier-request-mode-map (kbd "C-c C-l") #'courier-request-validate)
(define-key courier-request-mode-map (kbd "C-c C-m") #'courier-request-set-method)
(define-key courier-request-mode-map (kbd "C-c C-n") #'courier-new-request)
(define-key courier-request-mode-map (kbd "C-c C-o") #'courier-open)
(define-key courier-request-mode-map (kbd "C-c C-p") #'courier-request-preview)
(define-key courier-request-mode-map (kbd "C-c C-r") #'courier-rename-request)
(define-key courier-request-mode-map [remap save-buffer] #'courier-request-save-buffer)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . courier-request-mode))

;;;###autoload
(add-to-list 'magic-fallback-mode-alist '(courier-env-file-p . courier-env-mode))

(provide 'courier)

;;; courier.el ends here
