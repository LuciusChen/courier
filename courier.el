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

(defcustom courier-default-body-type 'json
  "Default body type used for new Courier requests.

This affects request-side editing and send-time behavior when a request does
not declare an explicit `[body].type` in its front matter."
  :type '(choice (const none)
                 (const json)
                 (const xml)
                 (const text)
                 (const form-urlencoded))
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

(defconst courier--allowed-body-types
  '(none json xml text form-urlencoded)
  "Supported Courier request body types.")

(defconst courier--allowed-auth-types
  '(none bearer basic header)
  "Supported Courier request auth types.")

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

(defface courier-response-tab-inactive-face
  '((t :inherit shadow))
  "Face used for inactive Courier response navigation tabs.")

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
  '((((background dark)) :background "#1f2227")
    (((background light)) :background "#eceff4")
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
  '((((background dark)) :background "#2a313a")
    (((background light)) :background "#dbe5f2")
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
  "^\\[\\([A-Za-z_][A-Za-z0-9_]*\\)\\]\\s-*$"
  "Regexp used to parse supported Courier TOML tables.")

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
        :params nil
        :vars nil
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
        (auth nil)
        (vars nil)
        (tests nil)
        (pre-request nil)
        (post-response nil))
    (while (< index (length lines))
      (let* ((line (nth index lines))
             (line-number (+ start-line index))
             (trimmed (string-trim line)))
        (cond
         ((or (string-empty-p trimmed)
              (string-prefix-p "#" trimmed))
          (setq index (1+ index)))
         ((string-match courier--toml-table-regexp trimmed)
          (setq current-table (match-string 1 trimmed))
          (when (member current-table seen-tables)
            (courier--parse-error line-number "Duplicate TOML table: [%s]" current-table))
          (unless (member current-table '("auth" "vars" "scripts" "body"))
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
                 (_
                  (courier--parse-error line-number "Unsupported [auth] key: %s" key))))
              ("vars"
               (unless (stringp value)
                 (courier--parse-error line-number "[vars] values must be strings"))
               (setq vars (append vars (list (cons key value)))))
              ("body"
               (pcase key
                 ("type"
                  (setq body-type (intern value)))
                 (_
                  (courier--parse-error line-number "Unsupported [body] key: %s" key))))
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
    (when body-type
      (unless (memq body-type courier--allowed-body-types)
        (courier--parse-error start-line "Invalid [body].type: %s" body-type)))
    (when auth
      (let ((type (plist-get auth :type)))
        (unless (memq type '(bearer basic header))
          (courier--parse-error start-line "Invalid [auth].type: %s" type))
        (pcase type
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
             (courier--parse-error start-line "[auth] header auth requires value"))))))
    (setq request (plist-put request :settings settings))
    (setq request (plist-put request :body-type body-type))
    (setq request (plist-put request :auth auth))
    (setq request (plist-put request :vars vars))
    (setq request (plist-put request :tests tests))
    (setq request (plist-put request :pre-request-script pre-request))
    (setq request (plist-put request :post-response-script post-response))
    request))

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
        (settings (plist-get request :settings))
        (body-type (or (plist-get request :body-type)
                       courier-default-body-type))
        (body (or (plist-get request :body) "")))
    (unless (and (stringp method) (member method courier--allowed-methods))
      (user-error "Invalid HTTP method: %s" method))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error "Request URL must be present"))
    (unless (memq body-type courier--allowed-body-types)
      (user-error "Invalid body type: %s" body-type))
    (when (and (eq body-type 'none)
               (not (string-empty-p (string-trim body))))
      (user-error "Body type `none` cannot have a body"))
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
  (let ((body (or body "")))
    (if (string-empty-p body)
        nil
      (mapcar
       (lambda (part)
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
         (headers (plist-get request :headers)))
    (when (and (not (string-empty-p body))
               (not (assoc-string "content-type" headers nil)))
      (when-let* ((content-type (courier--default-content-type-for-body-type body-type)))
        (plist-put request :headers
                   (append headers (list (cons "content-type" content-type))))))
    request))

;;;###autoload
(defun courier-resolve-request (request &optional env-vars)
  "Resolve variables in REQUEST with optional ENV-VARS and return a new plist."
  (let* ((vars (courier-merge-vars env-vars (plist-get request :vars)))
         (resolved-vars (courier--resolve-vars vars))
         (resolved (copy-sequence request))
         (body-type (courier--request-body-type request)))
    (plist-put resolved :body-type body-type)
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
               (pcase body-type
                 ('none "")
                 ('form-urlencoded
                  (courier--resolve-form-body (or (plist-get request :body) "") resolved-vars))
                 (_
                  (courier-expand-template (or (plist-get request :body) "") resolved-vars))))
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

(defun courier--response-view-prefix (response)
  "Return the header-line view prefix for RESPONSE."
  (concat
   (propertize (courier--response-tab-label courier--response-tab response)
               'face 'courier-response-tab-active-face)
   "  "
   (propertize ">>" 'face 'courier-response-tab-inactive-face)))

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
  "Reset point and window starts to the top of the current response buffer."
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

(defun courier--response-header-line-format (response)
  "Return the complete header line format for RESPONSE."
  (let* ((summary-response (or (courier--header-summary-response)
                               response))
         (view-prefix
          (courier--response-view-prefix summary-response))
         (summary
          (if (and (eq courier--response-tab 'timeline)
                   (numberp courier--history-index))
              (let* ((entry (courier--current-history-entry))
                     (timestamp (car entry)))
                (courier--response-header-line-with-history
                 summary-response timestamp courier--history-index
                 (length courier--history)))
            (courier--response-header-line summary-response))))
    (concat " " view-prefix courier--header-separator summary)))

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
         (method (or (plist-get response :request-method)
                     (plist-get courier--request :method)
                     ""))
         (url (or (plist-get response :request-url)
                  (plist-get courier--request :url)
                  ""))
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
                   'face 'courier-response-method-face)
       " "
       (propertize url
                   'face 'courier-response-url-face)
       "\n"
       (propertize (concat duration courier--header-separator size)
                   'face 'shadow)
       "\n"))))

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
               (put-text-property start (point) 'face 'default)
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
      (setq courier--history (seq-take courier--history max))))
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
      (delete-file path))))

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
         (body-file (plist-get response :body-file)))
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
(define-key courier--response-mode-map (kbd "]") #'courier-response-next-tab)
(define-key courier--response-mode-map (kbd "[") #'courier-response-prev-tab)
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

(defvar-local courier--params-source-buffer nil
  "Request buffer associated with the current Courier params editor.")

(defvar-local courier--request-model nil
  "Parsed request model backing the current Courier request buffer.")

(defvar-local courier--request-tab 'body
  "Current Courier request section shown in the active request buffer.")

(defvar-local courier--request-rendering-p nil
  "Non-nil while the current Courier request buffer is being re-rendered.")

(put 'courier--request-path 'permanent-local t)
(put 'courier--active-env 'permanent-local t)
(put 'courier--collection-root-hint 'permanent-local t)
(put 'courier--request-model 'permanent-local t)
(put 'courier--request-tab 'permanent-local t)

(defconst courier--request-primary-sections
  '(params body headers)
  "Primary navigation sections shown in Courier request buffers.")

(defconst courier--request-secondary-sections
  '(auth vars tests pre-request post-response)
  "Secondary navigation sections hidden behind `>>' in request buffers.")

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
  '((t :inherit bold :underline t))
  "Face used for the active Courier request navigation section.")

(defface courier-request-nav-inactive-face
  '((t :inherit shadow))
  "Face used for inactive Courier request navigation sections.")

(defface courier-request-nav-more-face
  '((t :inherit shadow :weight bold))
  "Face used for the `>>' marker in Courier request navigation.")

(defface courier-request-divider-face
  '((t :inherit shadow))
  "Face used for the subtle divider between request header and editor area.")

(defconst courier-request-font-lock-keywords
  '(("^\\([A-Z]+\\)\\s-+\\(\\S-+.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^\\([^:# \t][^:]*\\):\\s-*\\(.*\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face))
    ("^\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=\\s-*\\(.*\\)$"
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

(defun courier--request-section-label (section)
  "Return the display label for request SECTION."
  (pcase section
    ('url "URL")
    ('headers "Headers")
    ('body (format "Body(%s)"
                   (courier--body-type-label
                    (courier--request-body-type courier--request-model))))
    ('params "Params")
    ('auth (format "Auth(%s)"
                   (courier--auth-type-label
                    (courier--request-auth-type courier--request-model))))
    ('vars "Vars")
    ('tests "Tests")
    ('pre-request "Pre-request")
    ('post-response "Post-response")
    (_ (capitalize (symbol-name section)))))

(defun courier--request-nav-item (section active)
  "Return a propertized request navigation label for SECTION.
ACTIVE controls whether the active navigation face is used."
  (propertize (courier--request-section-label section)
              'face (if active
                        'courier-request-nav-active-face
                      'courier-request-nav-inactive-face)))

(defun courier--request-jump-choices ()
  "Return an alist of request section jump choices."
  (mapcar (lambda (item)
            (cons (courier--request-section-label item) item))
          (append courier--request-primary-sections
                  courier--request-secondary-sections)))

(defun courier--request-all-sections ()
  "Return all request sections in display order."
  (append courier--request-primary-sections
          courier--request-secondary-sections))

(defun courier--request-current-section ()
  "Return the logical request section at point."
  (or courier--request-tab 'body))

(defun courier--request-header-line-format ()
  "Return the request navigation header line for the current buffer."
  (let* ((current (courier--request-current-section))
         (primary (mapconcat
                   (lambda (section)
                     (courier--request-nav-item section (eq current section)))
                   courier--request-primary-sections
                   "  "))
         (secondary
          (when (memq current courier--request-secondary-sections)
            (concat " "
                    (courier--request-nav-item current t)))))
    (concat " " primary "  "
            (propertize ">>" 'face 'courier-request-nav-more-face)
            (or secondary ""))))

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
          (or (plist-get request :url) "")))

(defun courier--request-divider-line ()
  "Return the subtle divider line between request header and section view."
  (propertize "----------------------------------------"
              'face 'courier-request-divider-face))

(defun courier--request-section-placeholder (section)
  "Return placeholder text for empty request SECTION."
  (pcase section
    ('params "# No params.\n")
    ('headers "# No headers.\n")
    ('body
     (if (eq (courier--request-body-type courier--request-model) 'none)
         "# No body.\n"
       ""))
    ('auth
     (concat "# Auth section\n"
             "# Supported types: none, bearer, basic, header\n"))
    ('vars "# No variables.\n")
    ('tests "# No tests.\n")
    ('pre-request "# Empty pre-request script.\n")
    ('post-response "# Empty post-response script.\n")
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

Explicit Courier params win. When no explicit params exist, fall back to the
request URL query string."
  (or (plist-get request :params)
      (courier--parse-url-query-params (or (plist-get request :url) ""))))

(defun courier--request-auth-lines (auth)
  "Return editable text for AUTH."
  (pcase (plist-get auth :type)
    ('none
     "type = none")
    ('bearer
     (format "type = bearer\ntoken = %s" (or (plist-get auth :token) "")))
    ('basic
     (format "type = basic\nusername = %s\npassword = %s"
             (or (plist-get auth :username) "")
             (or (plist-get auth :password) "")))
    ('header
     (format "type = header\nheader = %s\nvalue = %s"
             (or (plist-get auth :header) "")
             (or (plist-get auth :value) "")))
    (_
     "")))

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
            (pcase (courier--request-body-type request)
              ('form-urlencoded
               (courier--format-request-kv-lines
                (courier--form-body-pairs-from-string (plist-get request :body))))
              ('none "")
              (_
               (or (plist-get request :body) ""))))
           ('auth
            (courier--request-auth-lines (plist-get request :auth)))
           ('vars
            (courier--format-request-kv-lines (plist-get request :vars)))
           ('tests
            (string-join (plist-get request :tests) "\n"))
           ('pre-request
            (or (plist-get request :pre-request-script) ""))
           ('post-response
            (or (plist-get request :post-response-script) ""))
           (_
            ""))))
    (if (string-empty-p text)
        (courier--request-section-placeholder section)
      text)))

(defun courier--request-content-start-position ()
  "Return the editable content start position in the current request buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 3)
    (point)))

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
  "Return non-nil when TEXT only contains comments or blank lines."
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
  (let* ((pairs (courier--parse-request-kv-lines text "auth"))
         (type (cdr (assoc-string "type" pairs nil))))
    (cond
     ((null pairs) nil)
     ((string-empty-p (or type ""))
      (user-error "Auth section requires `type = ...'"))
     ((string= type "none")
      nil)
     ((string= type "bearer")
      (list :type 'bearer
            :token (or (cdr (assoc-string "token" pairs nil)) "")))
     ((string= type "basic")
      (list :type 'basic
            :username (or (cdr (assoc-string "username" pairs nil)) "")
            :password (or (cdr (assoc-string "password" pairs nil)) "")))
     ((string= type "header")
      (list :type 'header
            :header (or (cdr (assoc-string "header" pairs nil)) "")
            :value (or (cdr (assoc-string "value" pairs nil)) "")))
     (t
      (user-error "Unsupported auth type: %s" type)))))

(defun courier--toml-quote-string (string)
  "Return STRING serialized as a TOML basic string."
  (prin1-to-string (or string "")))

(defun courier--serialize-front-matter (request)
  "Return TOML front matter for REQUEST, or nil when not needed."
  (let* ((settings (plist-get request :settings))
         (name (and (plist-get request :name)
                    (string-trim (plist-get request :name))))
         (timeout (plist-get settings :timeout))
         (follow-redirects-present (plist-member settings :follow-redirects))
         (follow-redirects (plist-get settings :follow-redirects))
         (body-type (courier--request-body-type request))
         (auth (plist-get request :auth))
         (vars (plist-get request :vars))
         (tests (plist-get request :tests))
         (pre-request (plist-get request :pre-request-script))
         (post-response (plist-get request :post-response-script))
         root-entries
         table-sections)
    (when (and name (not (string-empty-p name)))
      (push (format "name = %s" (courier--toml-quote-string name)) root-entries))
    (when timeout
      (push (format "timeout = %d" timeout) root-entries))
    (when follow-redirects-present
      (push (format "follow_redirects = %s" (if follow-redirects "true" "false")) root-entries))
    (when tests
      (let ((lines (list "tests = [")))
        (dolist (test tests)
          (push (format "  %s," (courier--toml-quote-string test)) lines))
        (push "]" lines)
        (push (string-join (nreverse lines) "\n") root-entries)))
    (when body-type
      (push (format "[body]\ntype = %s"
                    (courier--toml-quote-string (symbol-name body-type)))
            table-sections))
    (when auth
      (let ((lines (list "[auth]")))
        (push (format "type = %s"
                      (courier--toml-quote-string
                       (symbol-name (plist-get auth :type))))
              lines)
        (pcase (plist-get auth :type)
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
          (_
           (user-error "Unsupported auth type: %s" (plist-get auth :type))))
        (push (string-join (nreverse lines) "\n") table-sections)))
    (when vars
      (let ((lines (list "[vars]")))
        (dolist (pair vars)
          (push (format "%s = %s"
                        (car pair)
                        (courier--toml-quote-string (cdr pair)))
                lines))
        (push (string-join (nreverse lines) "\n") table-sections)))
    (when (or (and pre-request (not (string-empty-p (string-trim pre-request))))
              (and post-response (not (string-empty-p (string-trim post-response)))))
      (let ((lines (list "[scripts]")))
        (when (and pre-request
                   (not (string-empty-p (string-trim pre-request))))
          (when (string-match-p "\"\"\"" pre-request)
            (user-error "pre_request script contains unsupported triple quotes"))
          (push (format "pre_request = \"\"\"\n%s\n\"\"\"" pre-request) lines))
        (when (and post-response
                   (not (string-empty-p (string-trim post-response))))
          (when (string-match-p "\"\"\"" post-response)
            (user-error "post_response script contains unsupported triple quotes"))
          (push (format "post_response = \"\"\"\n%s\n\"\"\"" post-response) lines))
        (push (string-join (nreverse lines) "\n") table-sections)))
    (let ((sections (append (nreverse root-entries)
                            (nreverse table-sections))))
      (when sections
      (concat "+++\n"
              (string-join sections "\n\n")
              "\n+++\n\n")))))

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
           (let ((body-type (courier--request-body-type courier--request-model)))
             (plist-put courier--request-model :body
                        (pcase body-type
                          ('none "")
                          ('form-urlencoded
                           (courier--form-body-string-from-pairs
                            (courier--parse-request-kv-lines section-text "form body")))
                          (_
                           section-text)))))
          ('auth
           (plist-put courier--request-model :auth
                      (courier--parse-request-auth-lines section-text)))
          ('vars
           (plist-put courier--request-model :vars
                      (courier--parse-request-kv-lines section-text "variable")))
          ('tests
           (plist-put courier--request-model :tests
                      (cl-loop for line in (split-string section-text "\n")
                               for trimmed = (string-trim-right line)
                               unless (or (string-empty-p trimmed)
                                          (string-prefix-p "#" (string-trim-left trimmed)))
                               collect trimmed)))
          ('pre-request
           (plist-put courier--request-model :pre-request-script
                      (if (courier--comment-only-section-p section-text)
                          ""
                        section-text)))
          ('post-response
           (plist-put courier--request-model :post-response-script
                      (if (courier--comment-only-section-p section-text)
                          ""
                        section-text))))))))

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
          (erase-buffer)
          (insert (courier--request-view-request-line request) "\n")
          (insert (courier--request-divider-line) "\n")
          (insert "\n")
          (insert (courier--request-section-text request courier--request-tab))
          (goto-char (courier--request-content-start-position))
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
  "Ensure BUFFER contains Courier request NAME in its active model."
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
  (courier--sync-request-model)
  (let* ((parsed-request (if courier--request-model
                             (copy-tree courier--request-model t)
                           (courier-parse-buffer)))
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
       (cons (downcase (plist-get auth :header))
             (plist-get auth :value))))))

(defun courier--request-for-export (request)
  "Return the current REQUEST prepared for export."
  (courier-validate-request request)
  (if courier--export-interpolate
      (courier-resolve-request request (plist-get request :env-vars))
    (let ((export (copy-tree request t)))
      (plist-put export :body-type (courier--request-body-type request))
      (plist-put export :url (courier--export-source-url request))
      (when-let* ((auth-header
                   (courier--export-source-auth-header (plist-get request :auth))))
        (unless (assoc-string (car auth-header) (plist-get export :headers) nil)
          (plist-put export :headers
                     (append (plist-get export :headers)
                             (list auth-header)))))
      (when (eq (courier--request-body-type export) 'none)
        (plist-put export :body ""))
      (courier--maybe-add-default-body-header export)
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

(define-derived-mode courier-request-params-mode text-mode "Courier-Params"
  "Major mode for editing Courier URL query params."
  (setq-local header-line-format
              " Edit query params as key = value. C-c C-c applies. C-c C-k cancels. "))

(define-key courier-request-params-mode-map (kbd "C-c C-c") #'courier-request-params-apply)
(define-key courier-request-params-mode-map (kbd "C-c C-k") #'courier-request-params-cancel)

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
  "Close the current Courier params editor without applying changes."
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
  (setq-local mode-name '(:eval (courier--mode-line-lighter)))
  (setq-local font-lock-defaults '(courier-request-font-lock-keywords))
  (setq-local header-line-format '(:eval (courier--request-header-line-format)))
  (setq-local outline-regexp "^[A-Z]+ ")
  (setq-local revert-buffer-function #'courier-request-revert-buffer)
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
  (courier--sync-request-model)
  (plist-put courier--request-model :method method)
  (courier--render-request-buffer)
  (courier--refresh-overview-buffers)
  (message "Courier method set to %s." method))

;;;###autoload
(defun courier-request-set-body-type (body-type)
  "Set the current request body type to BODY-TYPE."
  (interactive
   (list
    (intern
     (completing-read "Body type: "
                      (mapcar #'symbol-name courier--allowed-body-types)
                      nil t nil nil
                      (symbol-name
                       (courier--request-body-type
                        (or courier--request-model
                            (courier--parse-buffer-for-editor))))))))
  (unless (memq body-type courier--allowed-body-types)
    (user-error "Unsupported body type: %s" body-type))
  (courier--sync-request-model)
  (plist-put courier--request-model :body-type body-type)
  (when (eq body-type 'none)
    (plist-put courier--request-model :body ""))
  (setq courier--request-tab 'body)
  (courier--render-request-buffer)
  (set-buffer-modified-p t)
  (message "Courier body type set to %s." (courier--body-type-label body-type)))

;;;###autoload
(defun courier-request-set-auth-type (auth-type)
  "Set the current request auth type to AUTH-TYPE."
  (interactive
   (list
    (intern
     (completing-read "Auth type: "
                      (mapcar #'symbol-name courier--allowed-auth-types)
                      nil t nil nil
                      (symbol-name
                       (courier--request-auth-type
                        (or courier--request-model
                            (courier--parse-buffer-for-editor))))))))
  (unless (memq auth-type courier--allowed-auth-types)
    (user-error "Unsupported auth type: %s" auth-type))
  (courier--sync-request-model)
  (plist-put courier--request-model :auth
             (pcase auth-type
               ('none nil)
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
               (_
                (user-error "Unsupported auth type: %s" auth-type))))
  (setq courier--request-tab 'auth)
  (courier--render-request-buffer)
  (set-buffer-modified-p t)
  (message "Courier auth type set to %s." (courier--auth-type-label auth-type)))

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

(defun courier--request-jump-section (section)
  "Show request SECTION in the current Courier buffer."
  (unless (memq section (courier--request-all-sections))
    (user-error "Unknown Courier section: %s" section))
  (courier--sync-request-model)
  (setq courier--request-tab section)
  (courier--render-request-buffer))

;;;###autoload
(defun courier-request-jump-section (section)
  "Jump to request SECTION in the current Courier buffer."
  (interactive
   (let* ((choices (courier--request-jump-choices))
          (current-section (courier--request-current-section))
          (current (and (memq current-section
                              (append courier--request-primary-sections
                                      courier--request-secondary-sections))
                        (courier--request-section-label current-section)))
          (selection (completing-read "Jump to section: " choices nil t nil nil current)))
     (list (cdr (assoc selection choices)))))
  (courier--request-jump-section section))

;;;###autoload
(defun courier-request-next-section ()
  "Jump to the next primary request section."
  (interactive)
  (let* ((current (courier--request-current-section))
         (sections courier--request-primary-sections)
         (index (or (cl-position current sections)
                    -1)))
    (courier--request-jump-section
     (nth (mod (1+ index) (length sections)) sections))))

;;;###autoload
(defun courier-request-prev-section ()
  "Jump to the previous primary request section."
  (interactive)
  (let* ((current (courier--request-current-section))
         (sections courier--request-primary-sections)
         (index (or (cl-position current sections)
                    1)))
    (courier--request-jump-section
     (nth (mod (1- index) (length sections)) sections))))

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
           (path (courier--unique-request-path request-root request-name)))
      (make-directory request-root t)
      (set-visited-file-name path)
      (setq-local default-directory (file-name-directory path))
      (setq-local courier--request-path path)
      (setq-local courier--collection-root-hint collection-root)
      (write-region (courier--serialize-request courier--request-model) nil path nil 'silent)
      (set-buffer-modified-p nil)
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
        (unless (derived-mode-p 'courier-request-mode)
          (courier-request-mode))
        (set-visited-file-name new-path t)
        (setq-local courier--request-path new-path)
        (courier-request-save-buffer))
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
(transient-define-prefix courier-request-menu ()
  "Show request actions for the current Courier buffer."
  [["Run"
   ("c" "Send" courier-request-send)
   ("p" "Preview" courier-request-preview)
   ("l" "Validate" courier-request-validate)]
   ["Edit"
    ("m" "Method" courier-request-set-method)
    ("B" "Body type" courier-request-set-body-type)
    ("A" "Auth type" courier-request-set-auth-type)
    ("j" "Jump section" courier-request-jump-section)
    ("e" "Environment" courier-request-switch-env)
    ("s" "Save" courier-request-save-buffer)]
   ["Navigate"
    ("o" "Open" courier-open)
    ("b" "Overview" courier-overview)]
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
   ["Timeline"
    ("c" "Clear timeline" courier-response-clear-timeline)]
   ["Navigate"
    ("]" "Next view" courier-response-next-tab)
    ("[" "Prev view" courier-response-prev-tab)]])

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
(define-key courier-request-mode-map (kbd "C-c C-j") #'courier-request-jump-section)
(define-key courier-request-mode-map (kbd "C-c C-l") #'courier-request-validate)
(define-key courier-request-mode-map (kbd "C-c C-m") #'courier-request-set-method)
(define-key courier-request-mode-map (kbd "C-c C-n") #'courier-new-request)
(define-key courier-request-mode-map (kbd "C-c C-o") #'courier-open)
(define-key courier-request-mode-map (kbd "C-c C-p") #'courier-request-preview)
(define-key courier-request-mode-map (kbd "C-c C-r") #'courier-rename-request)
(define-key courier-request-mode-map (kbd "C-c [") #'courier-request-prev-section)
(define-key courier-request-mode-map (kbd "C-c ]") #'courier-request-next-section)
(define-key courier-request-mode-map [remap save-buffer] #'courier-request-save-buffer)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . courier-request-mode))

(provide 'courier)

;;; courier.el ends here
