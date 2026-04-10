;;; courier.el --- Curl-backed HTTP client -*- lexical-binding: t; -*-

;; Author: Lucius Chen <chenyh572@gmail.com>
;; URL: https://github.com/LuciusChen/courier
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))

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

(require 'json)
(require 'seq)
(require 'subr-x)

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

(defvar courier--request-metadata (make-hash-table :test 'equal)
  "Hash table of request metadata keyed by absolute request path.")

(defconst courier--body-views
  '(auto json html xml javascript raw hex base64 image)
  "Supported Courier response body views.")

;;;; Parsing

(defconst courier--allowed-methods
  '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS")
  "Allowed HTTP methods in request files.")

(defconst courier--directive-regexp
  "^\\(#\\)\\s-*\\(@[[:alnum:]-]+\\)\\(?:\\s-+\\(.*\\)\\)?$"
  "Regexp used to parse directive lines.")

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
      ("@test"
       (when (string-empty-p value)
         (courier--parse-error line-number "Invalid @test directive"))
       (plist-put request :tests
                  (append (plist-get request :tests) (list value))))
      (_
       (courier--parse-error line-number "Unknown directive: %s" key))))
  request)

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
                       :vars nil
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
            (setq request (courier--parse-directive line-number line request))
            (forward-line 1))
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
(defun courier-scan-env-files (dir &optional stop-dir)
  "Walk upward from DIR and return `(NAME . PATH)' env entries.
When STOP-DIR is non-nil, do not scan above that directory."
  (let ((directories nil)
        (current (file-name-as-directory (expand-file-name dir)))
        (limit (and stop-dir
                    (file-name-as-directory (expand-file-name stop-dir))))
        results)
    (while current
      (push current directories)
      (if (and limit
               (file-equal-p current limit))
          (setq current nil)
        (let* ((parent (file-name-directory (directory-file-name current))))
          (setq current (unless (or (null parent)
                                    (string= parent current))
                          parent)))))
    (dolist (directory directories)
      (dolist (file (directory-files directory nil nil t))
        (when (courier--env-file-name-p file)
          (setq results
                (append results
                        (list (cons (courier--env-name-for-file file)
                                    (expand-file-name file directory))))))))
    (sort results
          (lambda (left right)
            (string< (format "%s:%s" (car left) (cdr left))
                     (format "%s:%s" (car right) (cdr right)))))))

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
    (plist-put resolved :url
               (courier-expand-template (plist-get request :url) resolved-vars))
    (plist-put resolved :headers
               (mapcar (lambda (header)
                         (cons (car header)
                               (courier-expand-template (cdr header) resolved-vars)))
                       (plist-get request :headers)))
    (plist-put resolved :body
               (courier-expand-template (or (plist-get request :body) "") resolved-vars))
    (plist-put resolved :resolved-vars resolved-vars)
    resolved))

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
   ((>= size 1048576) (format "%.1f MB" (/ size 1048576.0)))
   ((>= size 1024) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%d B" size))))

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
  "Decode BODY-FILE using CHARSET with Courier fallbacks."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally body-file)
    (let* ((bytes (buffer-string))
           (coding-systems
            (delq nil
                  (list (and charset
                             (ignore-errors (coding-system-from-name charset)))
                        'utf-8
                        'undecided))))
      (catch 'decoded
        (dolist (coding-system coding-systems)
          (condition-case nil
              (throw 'decoded (decode-coding-string bytes coding-system))
            (error nil)))
        nil))))

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
         (size (or (plist-get meta :size)
                   (if (file-exists-p body-file)
                       (file-attribute-size (file-attributes body-file))
                     0)))
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
      (plist-put response :tests
                 (courier-run-tests response (plist-get request :tests)))
      response)))

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
          (courier-overview-refresh))))))

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
    (file-name-nondirectory
     (directory-file-name (or (courier--collection-root start)
                              (courier--request-search-root))))))

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
  (let* ((request (courier--request-object-for-path path))
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
          :updated-at (courier--request-updated-at-string path)
          :parse-error (plist-get request :parse-error))))

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
    (insert line)
    (when-let* ((parse-error (plist-get entry :parse-error)))
      (insert (format "  ! %s" parse-error)))
    (insert "\n")
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
  "Explicit body view for the current Courier response buffer.
Nil keeps compatibility with the older pretty/raw toggle behavior.")

(defvar-local courier--history nil
  "Response history for the current Courier response buffer.
Each entry is a cons cell of the form `(TIMESTAMP . RESPONSE)'.")

(defvar-local courier--history-index nil
  "Index into `courier--history' currently displayed, or nil for latest.")

(defun courier--section-underline (title)
  "Return a section heading string for TITLE."
  (format "%s\n" title))

(defun courier--section-symbol (name)
  "Return the invisibility symbol used for section NAME."
  (intern (format "courier-section-%s" (downcase name))))

(defun courier--hide-section (symbol)
  "Hide section content associated with SYMBOL."
  (add-to-invisibility-spec symbol))

(defun courier--show-section (symbol)
  "Show section content associated with SYMBOL."
  (remove-from-invisibility-spec symbol))

(defun courier--section-hidden-p (symbol)
  "Return non-nil when section SYMBOL is hidden."
  (and (listp buffer-invisibility-spec)
       (memq symbol buffer-invisibility-spec)))

(defun courier--insert-section (name collapsed body)
  "Insert section NAME with COLLAPSED state and BODY string."
  (let* ((symbol (courier--section-symbol name))
         (title-start (point)))
    (insert (courier--section-underline name))
    (add-text-properties
     title-start (1- (point))
     `(courier-section ,name
                       courier-section-symbol ,symbol
                       mouse-face highlight
                       face bold))
    (let ((content-start (point)))
      (insert body)
      (unless (or (string-empty-p body) (string-suffix-p "\n" body))
        (insert "\n"))
      (insert "\n")
      (add-text-properties content-start (point) `(invisible ,symbol)))
    (if collapsed
        (courier--hide-section symbol)
      (courier--show-section symbol))))

(defun courier--section-symbol-at-point ()
  "Return the section symbol for the section at point."
  (or (get-text-property (point) 'courier-section-symbol)
      (save-excursion
        (beginning-of-line)
        (while (and (not (bobp))
                    (not (get-text-property (point) 'courier-section-symbol)))
          (forward-line -1))
        (get-text-property (point) 'courier-section-symbol))))

(defun courier--section-match (name)
  "Return the property match object for section NAME."
  (save-excursion
    (goto-char (point-min))
    (text-property-search-forward 'courier-section name)))

(defun courier--section-content-bounds (name)
  "Return `(START END SYMBOL HIDDENP)' for section NAME."
  (when-let* ((match (courier--section-match name)))
    (let* ((title-start (prop-match-beginning match))
           (symbol (get-text-property title-start 'courier-section-symbol))
           (hiddenp (courier--section-hidden-p symbol))
           (content-start (save-excursion
                            (goto-char title-start)
                            (forward-line 1)
                            (point)))
           (content-end (save-excursion
                          (goto-char content-start)
                          (if-let* ((next-match
                                     (text-property-search-forward 'courier-section nil nil t)))
                              (prop-match-beginning next-match)
                            (point-max)))))
      (list content-start content-end symbol hiddenp))))

(defun courier--previous-section-position (position)
  "Return the previous section heading position before POSITION."
  (let ((previous nil))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) position)
        (when (get-text-property (point) 'courier-section)
          (setq previous (point)))
        (forward-line 1)))
    previous))

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

(defun courier--response-header-line (response)
  "Return a header line string for RESPONSE."
  (format " %s %s | %d ms | %s | %s | %s "
          (or (plist-get response :status-code) 0)
          (or (plist-get response :reason) "")
          (or (plist-get response :duration-ms) 0)
          (courier--human-readable-size (or (plist-get response :size) 0))
          (or (plist-get response :content-type) "unknown")
          (courier--body-view-label response)))

(defun courier--response-header-line-with-history (response timestamp index total)
  "Return a history header line for RESPONSE at TIMESTAMP, INDEX, and TOTAL."
  (format "%s  [%s %d/%d]"
          (courier--response-header-line response)
          timestamp
          (1+ index)
          total))

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

(defun courier--format-summary (response)
  "Return a summary section string for RESPONSE."
  (format "Status: %s %s\nDuration: %d ms\nSize: %s\nContent-Type: %s\n"
          (or (plist-get response :status-code) 0)
          (or (plist-get response :reason) "")
          (or (plist-get response :duration-ms) 0)
          (courier--human-readable-size (or (plist-get response :size) 0))
          (or (cdr (assoc-string "content-type" (plist-get response :headers) nil))
              "unknown")))

(defun courier--format-headers (response)
  "Return formatted headers for RESPONSE."
  (if-let* ((headers (plist-get response :headers)))
      (concat
       (mapconcat (lambda (header)
                    (format "%s: %s" (car header) (cdr header)))
                  headers
                  "\n")
       "\n")
    "(none)\n"))

(defun courier--format-body (response)
  "Return formatted body text for RESPONSE."
  (pcase (courier--effective-body-view response)
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
     "(empty)\n")))

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

(defun courier--format-trace (response request)
  "Return a trace section string for RESPONSE and REQUEST."
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

(defun courier--insert-response-sections (response request)
  "Insert rendered sections for RESPONSE and REQUEST."
  (let* ((tests (plist-get response :tests))
         (tests-failed (seq-some (lambda (result)
                                   (not (plist-get result :passed)))
                                 tests)))
    (courier--insert-section "Summary" nil (courier--format-summary response))
    (courier--insert-section "Headers" t (courier--format-headers response))
    (courier--insert-section "Body" nil (courier--format-body response))
    (courier--insert-section "Tests" (not tests-failed) (courier--format-tests response))
    (courier--insert-section "Trace" t (courier--format-trace response request))))

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
  (let* ((index (or courier--history-index 0))
         (entry (nth index courier--history))
         (timestamp (car entry))
         (response (cdr entry))
         (inhibit-read-only t)
         (request courier--request)
         (history courier--history)
         (history-index courier--history-index)
         (temp-files courier--temp-files)
         (body-view courier--body-view))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--response response
          courier--request request
          courier--process nil
          courier--body-pretty t
          courier--body-view body-view
          courier--temp-files temp-files
          courier--history history
          courier--history-index history-index)
    (setq header-line-format
          (if history-index
              (courier--response-header-line-with-history
               response timestamp index (length history))
            (courier--response-header-line response)))
    (courier--insert-response-sections response request)
    (goto-char (point-min))))

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
  (setq-local truncate-lines t)
  (setq-local buffer-invisibility-spec nil)
  (add-hook 'kill-buffer-hook #'courier--response-cleanup nil t))

(defun courier--response-buffer-name (request)
  "Return a response buffer name for REQUEST."
  (format "*courier: %s*"
          (or (plist-get request :name)
              (format "%s %s"
                      (plist-get request :method)
                      (plist-get request :url)))))

(defun courier--response-show-sending (request)
  "Render a sending state for REQUEST in the current buffer."
  (let ((inhibit-read-only t)
        (history courier--history)
        (temp-files courier--temp-files)
        (body-view courier--body-view))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--request request
          courier--response nil
          courier--process nil
          courier--body-pretty t
          courier--body-view body-view
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
         (body-view courier--body-view))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--response response
          courier--request request
          courier--process nil
          courier--body-pretty t
          courier--body-view body-view
          courier--temp-files prev-temp-files
          courier--history prev-history
          courier--history-index nil)
    (courier--history-push response)
    (courier--record-request-metadata request response)
    (setq header-line-format (courier--response-header-line response))
    (courier--insert-response-sections response request)
    (goto-char (point-min))))

(defun courier--rerender-body-section ()
  "Re-render the body section in the current Courier response buffer."
  (let* ((inhibit-read-only t)
         (bounds (courier--section-content-bounds "Body")))
    (when bounds
      (pcase-let ((`(,start ,end ,symbol ,hiddenp) bounds))
        (delete-region start end)
        (goto-char start)
        (let ((content-start (point)))
          (insert (courier--format-body courier--response))
          (unless (or (eobp)
                      (eq (char-before) ?\n))
            (insert "\n"))
          (insert "\n")
          (add-text-properties content-start (point) `(invisible ,symbol)))
        (if hiddenp
            (courier--hide-section symbol)
          (courier--show-section symbol))))))

;;;###autoload
(defun courier-response-toggle-section ()
  "Toggle the visibility of the section at point."
  (interactive)
  (if-let* ((symbol (courier--section-symbol-at-point)))
      (if (courier--section-hidden-p symbol)
          (courier--show-section symbol)
        (courier--hide-section symbol))
    (user-error "No Courier section at point")))

;;;###autoload
(defun courier-response-next-section ()
  "Move point to the next section heading."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (if-let* ((match (text-property-search-forward 'courier-section nil nil t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      (user-error "No next section"))))

;;;###autoload
(defun courier-response-prev-section ()
  "Move point to the previous section heading."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (if-let* ((position (courier--previous-section-position (point))))
        (goto-char position)
      (goto-char start)
      (user-error "No previous section"))))

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
  (setq header-line-format
        (if courier--history-index
            (let* ((entry (nth courier--history-index courier--history))
                   (timestamp (car entry)))
              (courier--response-header-line-with-history
               courier--response timestamp courier--history-index
               (length courier--history)))
          (courier--response-header-line courier--response)))
  (courier--rerender-body-section))

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
  (setq header-line-format
        (if courier--history-index
            (let* ((entry (nth courier--history-index courier--history))
                   (timestamp (car entry)))
              (courier--response-header-line-with-history
               courier--response timestamp courier--history-index
               (length courier--history)))
          (courier--response-header-line courier--response)))
  (courier--rerender-body-section))

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

;;;###autoload
(defun courier-response-history-prev ()
  "Show the previous older response from history."
  (interactive)
  (unless courier--history
    (user-error "No response history"))
  (let* ((max-index (1- (length courier--history)))
         (next-index (if courier--history-index
                         (1+ courier--history-index)
                       1)))
    (when (> next-index max-index)
      (user-error "No older responses"))
    (setq courier--history-index next-index)
    (courier--history-render-current)))

;;;###autoload
(defun courier-response-history-next ()
  "Show the next newer response from history."
  (interactive)
  (unless courier--history
    (user-error "No response history"))
  (unless (and courier--history-index
               (> courier--history-index 0))
    (user-error "Already at latest response"))
  (setq courier--history-index (1- courier--history-index))
  (when (= courier--history-index 0)
    (setq courier--history-index nil))
  (courier--history-render-current))

(define-key courier--response-mode-map (kbd "TAB") #'courier-response-toggle-section)
(define-key courier--response-mode-map (kbd "RET") #'courier-response-open-body)
(define-key courier--response-mode-map (kbd "n") #'courier-response-next-section)
(define-key courier--response-mode-map (kbd "o") #'courier-response-open-body)
(define-key courier--response-mode-map (kbd "p") #'courier-response-prev-section)
(define-key courier--response-mode-map (kbd "g") #'courier-response-retry)
(define-key courier--response-mode-map (kbd "v") #'courier-response-set-view)
(define-key courier--response-mode-map (kbd "V") #'courier-response-toggle-pretty)
(define-key courier--response-mode-map (kbd "[") #'courier-response-history-prev)
(define-key courier--response-mode-map (kbd "]") #'courier-response-history-next)
(define-key courier--response-mode-map (kbd "C-c C-k") #'courier-response-cancel)

;;;; Request mode

(defvar-local courier--request-path nil
  "Path associated with the current Courier request buffer.")

(defvar-local courier--active-env nil
  "Name of the active Courier environment for the current request buffer.")

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
    (courier--request-search-root)))

(defun courier--request-search-root ()
  "Return the root directory used to discover Courier request files."
  (let ((start (courier--buffer-start-directory)))
    (or (courier--collection-requests-root start)
        (locate-dominating-file start ".git")
        start)))

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
   (buffer-file-name
    (expand-file-name buffer-file-name))
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
      (courier--management-root)
      default-directory))

(defun courier--slugify-name (name)
  "Return a filesystem-friendly slug for NAME."
  (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                            (downcase (string-trim name))))

(defun courier--request-file-name (name)
  "Return a Courier request filename for NAME."
  (concat (courier--slugify-name name) ".http"))

(defun courier--request-display-name (path)
  "Return the display name for request PATH."
  (let ((request (courier--request-object-for-path path)))
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

(defun courier--request-object-for-path (path)
  "Return Courier request data for PATH.
When PATH is already visited in a Courier request buffer, use the live buffer
contents. If parsing fails, return a placeholder request plist."
  (condition-case err
      (if-let* ((buffer (courier--request-buffer-for-path path)))
          (with-current-buffer buffer
            (courier-parse-buffer))
        (courier-parse-file path))
    (error
     (list :path path
           :name (file-name-base path)
           :method "ERR"
           :headers nil
           :body ""
           :vars nil
           :tests nil
           :settings nil
           :parse-error (error-message-string err)))))

(defun courier--request-candidate (path root)
  "Return a completion candidate for request PATH under ROOT."
  (let* ((request (courier--request-object-for-path path))
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
    (if (courier--collection-root directory)
        (courier--collection-env-entries directory)
      (courier-scan-env-files directory))))

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

(defun courier--resolved-current-request ()
  "Return the validated and resolved request for the current buffer."
  (let* ((request (courier-parse-buffer))
         (_validated (courier-validate-request request))
         (env-selection (courier--current-env-selection))
         (env-name (car env-selection))
         (env-vars (cdr env-selection))
         (resolved (courier-resolve-request request env-vars)))
    (plist-put resolved :env-name env-name)))

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
  "Move point to the Courier request line and return non-nil on success."
  (goto-char (point-min))
  (while (and (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (or (string-empty-p line)
                    (string-prefix-p "#" line))))
    (forward-line 1))
  (when (and (not (eobp))
             (looking-at courier--request-line-regexp))
    t))

(defun courier--current-method ()
  "Return the current request method from the buffer, or nil."
  (save-excursion
    (when (courier--goto-request-line)
      (match-string-no-properties 1))))

;;;###autoload
(define-derived-mode courier-request-mode text-mode "Courier"
  "Major mode for editing Courier request files."
  (setq-local mode-name '(:eval (courier--mode-line-lighter)))
  (setq-local font-lock-defaults '(courier-request-font-lock-keywords))
  (setq-local outline-regexp "^# @\\|^[A-Z]+ ")
  (setq-local courier--request-path (and buffer-file-name (expand-file-name buffer-file-name)))
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
      (user-error "No Courier request line found"))
    (replace-match method t t nil 1))
  (courier--refresh-overview-buffers)
  (message "Courier method set to %s." method))

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
  (let ((candidates (courier--open-candidates)))
    (unless candidates
      (user-error "No Courier requests or environments found"))
    (let ((completion-extra-properties
           '(:group-function courier--completion-group))
          (selection
           (completing-read
            (format "Courier open (%s): "
                    (abbreviate-file-name (courier--request-search-root)))
            candidates nil t)))
      (courier--handle-open-selection selection candidates))))

;;;###autoload
(defun courier-find-request ()
  "Compatibility alias for `courier-open'."
  (interactive)
  (courier-open))

;;;###autoload
(defun courier-new-request (name)
  "Create a new Courier request file named after NAME.
The file is placed in the current Courier management directory. When inside a
collection, creation respects the configured `requestsDir'. The HTTP method
defaults to GET; change it in the buffer after creation."
  (interactive (list (read-string "Request name: ")))
  (let* ((directory (courier--ensure-directory-under-root
                     (courier--management-directory)
                     (courier--management-root)))
         (filename (expand-file-name (courier--request-file-name name) directory)))
    (when (file-exists-p filename)
      (user-error "File already exists: %s" filename))
    (find-file filename)
    (insert (concat "# @name " name "\n"
                    "GET \n"
                    "Accept: application/json\n"))
    (save-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (courier--refresh-overview-buffers)
    (message "Courier request created at %s" filename)))

;;;###autoload
(defun courier-new-folder (name)
  "Create a Courier folder named NAME in the current management directory."
  (interactive (list (read-string "Folder name: ")))
  (let* ((base-directory (courier--management-directory))
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

(define-key courier-request-mode-map (kbd "C-c C-c") #'courier-request-send)
(define-key courier-request-mode-map (kbd "C-c C-b") #'courier-overview)
(define-key courier-request-mode-map (kbd "C-c C-e") #'courier-request-switch-env)
(define-key courier-request-mode-map (kbd "C-c C-l") #'courier-request-validate)
(define-key courier-request-mode-map (kbd "C-c C-m") #'courier-request-set-method)
(define-key courier-request-mode-map (kbd "C-c C-n") #'courier-new-request)
(define-key courier-request-mode-map (kbd "C-c C-o") #'courier-open)
(define-key courier-request-mode-map (kbd "C-c C-p") #'courier-request-preview)
(define-key courier-request-mode-map (kbd "C-c C-r") #'courier-rename-request)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . courier-request-mode))

(provide 'courier)

;;; courier.el ends here
