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
(defun courier-scan-env-files (dir)
  "Walk upward from DIR and return `(NAME . PATH)' env entries."
  (let ((directories nil)
        (current (file-name-as-directory (expand-file-name dir)))
        results)
    (while current
      (push current directories)
      (let* ((parent (file-name-directory (directory-file-name current))))
        (setq current (unless (or (null parent)
                                  (string= parent current))
                        parent))))
    (dolist (directory directories)
      (dolist (file (directory-files directory nil nil t))
        (when (courier--env-file-name-p file)
          (setq results
                (append results
                        (list (cons (courier--env-name-for-file file)
                                    (expand-file-name file directory))))))))
    results))

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

(defvar-local courier--history nil
  "Response history for the current Courier response buffer.
Each entry is a cons cell of the form `(TIMESTAMP . RESPONSE)'.")

(defvar-local courier--history-index nil
  "Index into `courier--history' currently displayed, or nil for latest.")

(defun courier--section-underline (title)
  "Return a section heading string for TITLE."
  (concat "── " title " "
          (make-string (max 2 (- 58 (length title))) ?─)
          "\n"))

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
     `(courier-section ,name courier-section-symbol ,symbol mouse-face highlight))
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

(defun courier--pretty-json-body (body-text)
  "Pretty-print BODY-TEXT as JSON and return the rendered string."
  (with-temp-buffer
    (insert body-text)
    (json-pretty-print-buffer)
    (buffer-string)))

(defun courier--response-header-line (response)
  "Return a header line string for RESPONSE."
  (format "%s %s │ %dms │ %s │ %s"
          (or (plist-get response :status-code) 0)
          (or (plist-get response :reason) "")
          (or (plist-get response :duration-ms) 0)
          (courier--human-readable-size (or (plist-get response :size) 0))
          (or (plist-get response :content-type) "unknown")))

(defun courier--response-header-line-with-history (response timestamp index total)
  "Return a history header line for RESPONSE at TIMESTAMP, INDEX, and TOTAL."
  (format "%s  [%s %d/%d]"
          (courier--response-header-line response)
          timestamp
          (1+ index)
          total))

(defun courier--format-summary (response)
  "Return a summary section string for RESPONSE."
  (format "Status:       %s %s\nDuration:     %dms\nSize:         %s\nContent-Type: %s\n"
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
  (cond
   ((plist-get response :body-text)
    (let ((body-text (plist-get response :body-text)))
      (concat
       (if (and courier--body-pretty
                (courier--json-content-type-p (plist-get response :content-type)))
           (condition-case _err
               (courier--pretty-json-body body-text)
             (error body-text))
         body-text)
       "\n")))
   ((plist-get response :body-file)
    (format "Body saved to %s\n" (plist-get response :body-file)))
   (t
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
         (temp-files courier--temp-files))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--response response
          courier--request request
          courier--process nil
          courier--body-pretty t
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
        (temp-files courier--temp-files))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--request request
          courier--response nil
          courier--process nil
          courier--body-pretty t
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
         (prev-temp-files courier--temp-files))
    (courier--response-mode)
    (erase-buffer)
    (setq courier--response response
          courier--request request
          courier--process nil
          courier--body-pretty t
          courier--temp-files prev-temp-files
          courier--history prev-history
          courier--history-index nil)
    (courier--history-push response)
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
  (setq courier--body-pretty (not courier--body-pretty))
  (courier--rerender-body-section))

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
(define-key courier--response-mode-map (kbd "n") #'courier-response-next-section)
(define-key courier--response-mode-map (kbd "p") #'courier-response-prev-section)
(define-key courier--response-mode-map (kbd "g") #'courier-response-retry)
(define-key courier--response-mode-map (kbd "v") #'courier-response-toggle-pretty)
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

(defun courier--available-env-entries ()
  "Return available environment entries for the current request buffer."
  (when-let* ((path (or courier--request-path
                        (and buffer-file-name (expand-file-name buffer-file-name))))
              (directory (file-name-directory path)))
    (courier-scan-env-files directory)))

(defun courier--available-env-names (entries)
  "Return unique environment names from ENTRIES."
  (delete-dups (mapcar #'car entries)))

(defun courier--read-env-name (names)
  "Prompt for an environment name from NAMES."
  (completing-read "Environment: " names nil t nil nil courier--active-env))

(defun courier--selected-env-name (entries)
  "Return the selected environment name from ENTRIES."
  (let ((names (courier--available-env-names entries)))
    (cond
     ((null names) nil)
     ((and courier--active-env
           (member courier--active-env names))
      courier--active-env)
     ((= (length names) 1)
      (setq courier--active-env (car names))
      (force-mode-line-update))
     (t
      (setq courier--active-env (courier--read-env-name names))
      (force-mode-line-update)))))

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

(defun courier--resolved-current-request ()
  "Return the validated and resolved request for the current buffer."
  (let* ((request (courier-parse-buffer))
         (_validated (courier-validate-request request))
         (env-vars (courier--current-env-vars)))
    (courier-resolve-request request env-vars)))

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

;;;###autoload
(define-derived-mode courier-request-mode text-mode "Courier"
  "Major mode for editing Courier request files."
  (setq-local mode-name '(:eval (courier--mode-line-lighter)))
  (setq-local font-lock-defaults '(courier-request-font-lock-keywords))
  (setq-local outline-regexp "^# @\\|^[A-Z]+ ")
  (setq-local courier--request-path (and buffer-file-name (expand-file-name buffer-file-name))))

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
    (setq courier--active-env (courier--read-env-name names))
    (force-mode-line-update)
    (message "Courier environment set to %s." courier--active-env)))

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
(defun courier-new-request (name)
  "Create a new Courier request file named after NAME.
The file is placed in the directory of the current buffer's file,
or `default-directory' if there is no file.  The HTTP method
defaults to GET; change it in the buffer after creation."
  (interactive (list (read-string "Request name: ")))
  (let* ((directory (if buffer-file-name
                       (file-name-directory buffer-file-name)
                     default-directory))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                                         (downcase (string-trim name))))
         (filename (expand-file-name (concat slug ".http") directory)))
    (when (file-exists-p filename)
      (user-error "File already exists: %s" filename))
    (find-file filename)
    (insert (concat "# @name " name "\n"
                    "GET \n"
                    "Accept: application/json\n"))
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)))

(define-key courier-request-mode-map (kbd "C-c C-c") #'courier-request-send)
(define-key courier-request-mode-map (kbd "C-c C-e") #'courier-request-switch-env)
(define-key courier-request-mode-map (kbd "C-c C-l") #'courier-request-validate)
(define-key courier-request-mode-map (kbd "C-c C-p") #'courier-request-preview)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . courier-request-mode))

(provide 'courier)

;;; courier.el ends here
