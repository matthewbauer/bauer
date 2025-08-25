;;; monet.el  --- Claude Code MCP over websockets   -*- lexical-binding:t -*-

;; Author: Stephen Molitor <stevemolitor@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0") (websocket "1.15"))
;; Keywords: tools, ai
;; URL: https://github.com/stevemolitor/monet

;;; Commentary:
;; Implement Claude Code IDE support in Emacs using websockets + MCP.

;;; Code:

;;; Dependencies
(require 'cl-lib)
(require 'diff)
(require 'ediff)
(require 'json)
(require 'project)
(require 'subr-x) ; For when-let*
(require 'websocket)

;;; Customization
(defgroup monet nil
  "Monet - Claude Code MCP over websockets."
  :group 'external
  :prefix "monet-")

(defcustom monet-log-buffer-name "*Monet Log*"
  "Name of the buffer to use for logging Claude communication."
  :type 'string
  :group 'monet)

(defcustom monet-prefix-key "C-c m"
  "Prefix key for Monet mode commands.
This should be a key sequence like \\[monet-prefix-key].
Set to nil to not bind any prefix key."
  :type '(choice (string :tag "Key sequence")
                 (const :tag "No prefix key" nil))
  :group 'monet)

(defgroup monet-tool nil
  "Tool configuration for Monet."
  :group 'monet
  :prefix "monet-")

(defcustom monet-diff-tool 'monet-simple-diff-tool
  "Function to use for creating diff displays.
The function should have the signature:
  (old-file-path new-file-path new-file-contents on-accept on-quit)
where ON-ACCEPT and ON-QUIT are no-argument callbacks.
It should return the created diff buffer."
  :type 'function
  :group 'monet-tool)

(defcustom monet-diff-cleanup-tool 'monet-simple-diff-cleanup-tool
  "Function to use for cleaning up diff displays.
The function should have the signature:
  (diff-buffer)
where DIFF-BUFFER is the buffer created by `monet-diff-tool'."
  :type 'function
  :group 'monet-tool)

(defgroup monet-ediff nil
  "Customization options for the Monet ediff tool."
  :group 'monet
  :prefix "monet-")

(defcustom monet-ediff-split-window-direction 'horizontal
  "Direction for splitting ediff windows in Monet.
When set to 'horizontal, windows are split side-by-side.
When set to 'vertical, windows are split top-and-bottom."
  :type '(choice (const :tag "Split horizontally (side-by-side)" horizontal)
                 (const :tag "Split vertically (top-and-bottom)" vertical))
  :group 'monet-ediff)

(defcustom monet-ediff-accept-key "y"
  "Key binding for accepting changes in ediff.
This key will be bound in the ediff control buffer to accept the changes."
  :type 'string
  :group 'monet-ediff)

(defcustom monet-ediff-quit-key "q"
  "Key binding for quitting ediff without saving.
This key will be bound in the ediff control buffer to quit without saving changes."
  :type 'string
  :group 'monet-ediff)

(defcustom monet-simple-diff-accept-key "y"
  "Key binding for accepting changes in simple diff mode.
This key will be bound in the diff buffer to accept the changes."
  :type 'string
  :group 'monet-tool)

(defcustom monet-simple-diff-quit-key "q"
  "Key binding for quitting simple diff mode without saving.
This key will be bound in the diff buffer to quit without saving changes."
  :type 'string
  :group 'monet-tool)

(defcustom monet-hide-diff-when-irrelevant nil
  "When non-nil, hide diff buffers when editing unrelated files.
Diff buffers will only be shown when editing files within the
associated Claude session directory or the file that initiated
the diff request."
  :type 'boolean
  :group 'monet-tool)

(defcustom monet-get-current-selection-tool 'monet-default-get-current-selection-tool
  "Function to use for getting the current text selection.
The function should have the signature:
  () -> MCP response
Returns the current selection or cursor position in the active editor.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-get-latest-selection-tool 'monet-default-get-latest-selection-tool
  "Function to use for getting the latest text selection.
The function should have the signature:
  () -> MCP response
Returns the latest text selection from any file.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-open-file-tool 'monet-default-open-file-tool
  "Function to use for opening files.
The function should have the signature:
  (uri) -> MCP response
where URI is a file path or file:// URI.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-save-document-tool 'monet-default-save-document-tool
  "Function to use for saving documents.
The function should have the signature:
  (uri) -> MCP response
where URI is the file URI or path to save.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-check-document-dirty-tool 'monet-default-check-document-dirty-tool
  "Function to use for checking if a document has unsaved changes.
The function should have the signature:
  (uri) -> MCP response
where URI is the file URI or path to check.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-get-open-editors-tool 'monet-default-get-open-editors-tool
  "Function to use for getting the list of open editors.
The function should have the signature:
  () -> MCP response
Returns the list of currently open files in the editor.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-get-workspace-folders-tool 'monet-default-get-workspace-folders-tool
  "Function to use for getting workspace folders.
The function should have the signature:
  () -> MCP response
Returns the list of workspace folders (project directories).
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

(defcustom monet-diagnostics-tool 'monet-flymake-flycheck-diagnostics-tool
  "Function to use for getting diagnostics.
The function should have the signature:
  (&optional uri) -> MCP response
where URI is an optional file URI or path to get diagnostics for.
If URI is nil, gets diagnostics for all open files.
The default implementation collects from Flymake and Flycheck.
The MCP response should be a list of content objects."
  :type 'function
  :group 'monet-tool)

;;; Constants
(defconst monet-version "0.0.1")
(defconst monet--port-min 10000 "Minimum port number for WebSocket server.")
(defconst monet--port-max 65535 "Maximum port number for WebSocket server.")
(defconst monet--protocol-version "2024-11-05" "MCP protocol version supported.")
(defconst monet--selection-delay 0.05 "Delay in seconds before sending selection update.")
(defconst monet--initial-notification-delay 0.01 "Delay before sending initial notifications after connection.")

;;; Data structures
(cl-defstruct monet--session
  "MCP session for a Claude instance."
  key
  server
  client
  directory
  port
  initialized                           ; Whether handshake is complete
  auth-token                            ; UUID auth token for validation
  opened-diffs                          ; Hash table of active diff sessions keyed by tab-name
  deferred-responses                    ; Hash table of deferred responses keyed by unique-key
  )

;;; Utility Functions
(defun monet--generate-uuid ()
  "Generate a UUID v4 string."
  (let ((uuid (make-string 36 ?-)))
    ;; Generate random hex characters
    (dotimes (i 36)
      (unless (memq i '(8 13 18 23))    ; Skip dash positions
        (let ((char (if (= i 14)
                        4               ; Version 4 UUID
                      (if (= i 19)
                          ;; Variant bits: 10xx
                          (let ((val (random 16)))
                            (if (< val 4) (+ val 8)
                              (if (< val 8) (+ val 8)
                                (if (< val 12) val
                                  (- val 4)))))
                        (random 16)))))
          (aset uuid i (aref "0123456789abcdef" char)))))
    uuid))

(defun monet--shuffle-list (l)
  "Randomly shuffle list L."
  (let* ((v (apply #'vector l))
         (n (length v)))
    (dotimes (i (1- n))
      (cl-rotatef (aref v i) (aref v (+ i (random (- n i))))))
    (append v nil)))

(defun monet--port-available-p (port)
  "Check if PORT is available for binding."
  (condition-case nil
      (let ((test-process
             (make-network-process
              :name "test"
              :server t
              :host 'local
              :service port
              :family 'ipv4)))
        (delete-process test-process)
        t)
    (error nil)))

(defun monet--find-free-port ()
  "Find a free port in the allowed range."
  (let* ((ports (number-sequence monet--port-min monet--port-max))
         ;; Shuffle ports for randomness
         (shuffled-ports (monet--shuffle-list ports)))
    ;; Try ports until we find a free one
    (cl-loop for port in shuffled-ports
             when (monet--port-available-p port)
             return port
             finally (error "No free ports available"))))

(defun monet--remove-lockfile (port)
  "Remove lock file for PORT."
  (when port
    (let* ((dir (expand-file-name "~/.claude/ide/"))
           (file (expand-file-name (format "%d.lock" port) dir)))
      (when (file-exists-p file)
        (condition-case err
            (delete-file file)
          (error
           (error "Error removing lockfile %s: %s" file (error-message-string err))))))))

(defun monet--create-lockfile (folder port auth-token session-key)
  "Create lock file for claude running in FOLDER for PORT with AUTH-TOKEN and SESSION-KEY."
  (condition-case err
      (let* ((dir (expand-file-name "~/.claude/ide/"))
             (file (expand-file-name (format "%d.lock" port) dir))
             (content (json-encode
                       `((pid . ,(emacs-pid))
                         (workspaceFolders . ,(vector folder))
                         (ideName . ,(format "Emacs (%s)" session-key))
                         (transport . "ws")
                         (authToken . ,auth-token)))))
        ;; Ensure directory exists
        (make-directory dir t)
        ;; Write lock file
        (with-temp-file file
          (insert content))
        ;; Make it readable
        (set-file-modes file 420))
    (error
     (error "Error creating lockfile: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(defun monet--get-mime-type (file-path)
  "Return the MIME type for FILE-PATH based on its extension."
  (let ((extension (file-name-extension file-path)))
    (pcase extension
      ;; Text files
      ("txt" "text/plain")
      ("md" "text/markdown")
      ("org" "text/org")
      ("rst" "text/x-rst")
      ;; Programming languages
      ("el" "text/x-elisp")
      ("lisp" "text/x-lisp")
      ("scm" "text/x-scheme")
      ("clj" "text/x-clojure")
      ("py" "text/x-python")
      ("js" "text/javascript")
      ("jsx" "text/jsx")
      ("ts" "text/typescript")
      ("tsx" "text/tsx")
      ("java" "text/x-java")
      ("c" "text/x-c")
      ("cpp" "text/x-c++")
      ("cc" "text/x-c++")
      ("cxx" "text/x-c++")
      ("h" "text/x-c")
      ("hpp" "text/x-c++")
      ("cs" "text/x-csharp")
      ("go" "text/x-go")
      ("rs" "text/x-rust")
      ("rb" "text/x-ruby")
      ("php" "text/x-php")
      ("swift" "text/x-swift")
      ("kt" "text/x-kotlin")
      ("scala" "text/x-scala")
      ("r" "text/x-r")
      ("lua" "text/x-lua")
      ("perl" "text/x-perl")
      ("pl" "text/x-perl")
      ;; Web files
      ("html" "text/html")
      ("htm" "text/html")
      ("xml" "text/xml")
      ("css" "text/css")
      ("scss" "text/x-scss")
      ("sass" "text/x-sass")
      ("less" "text/x-less")
      ;; Config files
      ("json" "application/json")
      ("yaml" "text/yaml")
      ("yml" "text/yaml")
      ("toml" "text/x-toml")
      ("ini" "text/x-ini")
      ("conf" "text/plain")
      ("config" "text/plain")
      ;; Shell scripts
      ("sh" "text/x-sh")
      ("bash" "text/x-bash")
      ("zsh" "text/x-zsh")
      ("fish" "text/x-fish")
      ;; Build files
      ("makefile" "text/x-makefile")
      ("cmake" "text/x-cmake")
      ;; Documentation
      ("tex" "text/x-latex")
      ("latex" "text/x-latex")
      ;; Default
      (_ "text/plain"))))


;;; Session Management
(defvar monet--sessions (make-hash-table :test 'equal)
  "Hash table mapping claude code buffer names to ide websocket sessions.

Values are instances of the `monet--session' structure.

Keys can actually be any unique string, not just buffer names. Using a
key that is not a buffer name is useful for creating a websocket server
in Emacs to connect to an claude process running outside Emacs." )

(defvar monet--selection-timer nil
  "Timer for debouncing selection updates.")

(defvar monet--ping-timer nil
  "Timer for sending periodic keepalive pings.")

(defun monet--find-session-by-client (client)
  "Find the session that owns the CLIENT websocket."
  (seq-find (lambda (session)
              (eq (monet--session-client session) client))
            (hash-table-values monet--sessions)))

(defun monet--store-deferred-response (session unique-key id)
  "Store a deferred response for UNIQUE-KEY with request ID in SESSION."
  (when (and session unique-key id)
    (let ((deferred-responses (monet--session-deferred-responses session)))
      (puthash unique-key id deferred-responses))))

(defun monet--complete-deferred-response (unique-key result)
  "Complete a deferred response for UNIQUE-KEY with RESULT.
Searches all sessions for the deferred response."
  (let ((completed nil)
        (sessions-checked 0))
    (maphash (lambda (_key session)
               (setq sessions-checked (1+ sessions-checked))
               (unless completed
                 (let* ((deferred-responses (monet--session-deferred-responses session))
                        (request-id (gethash unique-key deferred-responses)))
                   (when request-id
                     ;; Found the deferred response
                     (let ((client (monet--session-client session)))
                       (if client
                           (progn
                             ;; Send the response - wrap in content field like normal responses
                             (monet--send-response client request-id `((content . ,result)))
                             ;; Remove from deferred responses
                             (remhash unique-key deferred-responses)
                             (setq completed t))
                         (error "No client found for session")))))))
             monet--sessions)))

(defun monet--cleanup-diff (tab-name session)
  "Clean up diff session for TAB-NAME in SESSION."
  (let ((opened-diffs (monet--session-opened-diffs session)))
    (when-let ((diff-context (gethash tab-name opened-diffs)))
      ;; Pass the whole context to the cleanup function
      (funcall monet-diff-cleanup-tool diff-context)
      ;; Remove from opened diffs
      (remhash tab-name opened-diffs))))

(defun monet--status-message (msg)
  "Display a temporary status message MSG that disappears after 2 seconds."
  (message msg)
  (run-with-idle-timer 2 nil (lambda () (message nil))))

(defun monet--ping (client)
  "Send tools/list_changed notification as keepalive to CLIENT."
  (monet--send-notification
   client
   "notifications/tools/list_changed"))

(defun monet--start-ping-timer (session)
  "Start periodic ping timer for SESSION."
  (monet--stop-ping-timer) ; Stop any existing timer
  (setq monet--ping-timer
        (run-with-timer 30 30 ; Start after 30s, repeat every 30s
                        (lambda ()
                          (when-let ((client (monet--session-client session)))
                            (when (websocket-openp client)
                              (monet--ping client)))))))

(defun monet--stop-ping-timer ()
  "Stop the ping timer."
  (when monet--ping-timer
    (cancel-timer monet--ping-timer)
    (setq monet--ping-timer nil)))

(defun monet--get-session (key)
  "Get the Monet session for KEY, or nil if not found."
  (gethash key monet--sessions))

;;; Websockets Communication
;;;; Message sending functions
(defun monet--send-response (ws id result)
  "Send successful response with ID and RESULT to WS."
  (let* ((data `((jsonrpc . "2.0")
                 (id . ,id)
                 (result . ,result)))
         (response (json-encode data)))
    (websocket-send-text ws response)))

(defun monet--send-notification (client method &optional params)
  "Send notification with METHOD and PARAMS to CLIENT.
If PARAMS is not provided, uses an empty hash table."
  (when client
    (let* ((session (monet--find-session-by-client client)))
      ;; Only send if session is initialized (unless it's the initialization-related notification)
      (when (or (and session (monet--session-initialized session))
                (string-prefix-p "notifications/" method))
        (let* ((params (or params (make-hash-table :test 'equal)))
               (data `((jsonrpc . "2.0")
                       (method . ,method)
                       (params . ,params)))
               (notification (json-encode data)))
          (condition-case err
              (websocket-send-text client notification)
            (error
             (error "Error sending notification: %s" (error-message-string err)))))))))

(defun monet--send-error (ws id code message &optional data)
  "Send error response with ID, CODE, MESSAGE and optional DATA to WS."
  (let* ((error-data `((jsonrpc . "2.0")
                       (id . ,id)
                       (error . ((code . ,code)
                                 (message . ,message)
                                 ,@(when data `((data . ,data)))))))
         (response (json-encode error-data)))
    (websocket-send-text ws response)))


;;;; Message receiving functions
(defun monet--on-message (session ws frame)
  "Handle JSON-RPC messsage from WS FRAME for SESSION."
  (condition-case err
      (let* ((payload (websocket-frame-text frame))
             (message (condition-case json-err
                          (json-read-from-string payload)
                        (error
                         (error "JSON parse error: %s" (error-message-string json-err)))))
             (id (alist-get 'id message))
             (method (alist-get 'method message))
             (params (alist-get 'params message)))

        (pcase method
          ;; Protocol initialization
          ("initialize"
           (monet--handle-initialize session ws id params))
          ;; Tool listing
          ("tools/list"
           (monet--handle-tools-list session ws id params))
          ;; Tool invocation
          ("tools/call"
           (monet--handle-tools-call session ws id params))
          ;; Prompts listing (empty for now)
          ("prompts/list"
           (monet--send-response ws id '((prompts . []))))
          ;; Resources listing
          ("resources/list"
           (monet--handle-resources-list session ws id params))
          ;; Resources read
          ("resources/read"
           (monet--handle-resources-read session ws id params))
          ;; IDE connected notification
          ("ide_connected"
           (monet--handle-ide-connected session ws id params))
          ;; Notifications initialized
          ("notifications/initialized"
           ;; This is a notification - don't send response
           nil)
          ;; Unknown method
          (_
           ;; Only send error response if this is a request (has an id)
           (when id
             (monet--send-error ws id -32601 (format "Method not found: %s" method)))
           (message "Method not found: %s" method))))
    (error
     (message "Error handling MCP message: %s" (error-message-string err)))))


;;;; WebSocket server functions
(defun monet--on-open-server (session ws)
  "Handle WebSocket WS open for SESSION.

Put client WS in SESSION structure, so we can use it to send messages to
claude later."
  (setf (monet--session-client session) ws))

(defun monet--on-close-server (session _ws)
  "Handle WebSocket WS close for SESSION.

Remove SESSION from `monet--sessions'."
  (let ((key (monet--session-key session))
        (port (monet--session-port session)))
    ;; Remove lockfile
    (monet--remove-lockfile port)
    ;; Remove session
    (remhash key monet--sessions)
    ;; Remove hooks if no more sessions
    (when (= 0 (hash-table-count monet--sessions))
      (remove-hook 'post-command-hook #'monet--track-selection-change)
      (remove-hook 'post-command-hook #'monet--track-diff-visibility)
      ;; Cancel visibility timer if active
      (when monet--diff-visibility-timer
        (cancel-timer monet--diff-visibility-timer)
        (setq monet--diff-visibility-timer nil)))))

(defun monet--on-error-server (session _ws action error)
  "Handle WebSocket error for SESSION with WS during ACTION with ERROR."
  (message "WebSocket error on port %d during %s: %s"
           (monet--session-port session)
           action
           (error-message-string error)))

(defun monet-start-server-in-directory (key directory)
  "Start websocker server for claude process with KEY running in DIR.

Returns the session object."
  (let* ((dir (expand-file-name directory))
         (port (monet--find-free-port))
         (auth-token (monet--generate-uuid))
         (session (make-monet--session
                   :key key
                   :directory dir
                   :port port
                   :initialized nil
                   :auth-token auth-token
                   :opened-diffs (make-hash-table :test 'equal)
                   :deferred-responses (make-hash-table :test 'equal))))
    (condition-case err
        (let ((server (websocket-server
                       port
                       :host 'local
                       :on-open (lambda (ws)
                                  (monet--on-open-server session ws))
                       :on-close (lambda (ws)
                                   (monet--on-close-server session ws))
                       :on-message (lambda (ws frame)
                                     (monet--on-message session ws frame))
                       :on-error (lambda (ws action error)
                                   (monet--on-error-server session ws action error))
                       :protocol '("mcp"))))
          ;; Put server in the session
          (setf (monet--session-server session) server)

          ;; Create lock file with auth token and session key
          (monet--create-lockfile dir port (monet--session-auth-token session) key)

          ;; Store session
          (puthash key session monet--sessions)

          ;; Register hooks for selection tracking
          (monet-register-hooks)

          (message "Started monet websocket server on port %d" port)
          ;; Return session
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

(defun monet-start-server-function (key directory)
  "Start a Monet server with KEY in DIRECTORY.

Returns the process environment variables needed to start a Claude
process that will connect to the Claude server as a list."
  (let* ((session (monet-start-server-in-directory key default-directory))
         (port (monet--session-port session)))
    `("ENABLE_IDE_INTEGRATION=t" ,(format "CLAUDE_CODE_SSE_PORT=%d" port))))

;;; MCP Protocol Handlers
;;;; Initialization handlers
(defun monet--handle-initialize (session ws id _params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (when-let ((client (monet--session-client session)))
    ;; Send initialize response
    (monet--send-response
     ws id
     `((protocolVersion . ,monet--protocol-version)
       (capabilities . ((tools . ((listChanged . t)))
                        (prompts . ((listChanged . t)))
                        (resources . ((subscribe . :json-false)
                                      (listChanged . :json-false)))))
       (serverInfo . ((name . "monet.el")
                      (version . ,monet-version)))))

    ;; Mark session as initialized
    (setf (monet--session-initialized session) t)

    ;; Send tools/list_changed notification immediately
    (monet--send-notification
     client
     "notifications/tools/list_changed")))

(defun monet--handle-ide-connected (session ws id _params)
  "Handle ide_connected notification from Claude Code.

SESSION is the monet session.
WS is the websocket connection.
ID is the request ID.
_PARAMS are the notification parameters (unused).

This is sent when Claude Code has successfully connected to the IDE."
  ;; Start the ping timer to keep connection alive
  (monet--start-ping-timer session)

  ;; Don't send response - this is a notification, not a request
  ;; Notifications don't have an id and don't expect a response

  ;; Send initial selection state after a short delay
  (when-let ((client (monet--session-client session)))
    (run-with-timer monet--initial-notification-delay nil
                    (lambda ()
                      ;; Send current selection if we have a file buffer
                      (monet--send-selection client)))))

;;;; Resource handlers
(defun monet--handle-resources-list (_session ws id params)
  "Handle resources/list request with ID and PARAMS from WS for SESSION."
  (let* ((cursor (alist-get 'cursor params))
         (resources (monet--get-file-resources cursor)))
    (monet--send-response
     ws id
     `((resources . ,(vconcat resources))
       ,@(when cursor `((nextCursor . ,cursor)))))))

(defun monet--handle-resources-read (_session ws id params)
  "Handle resources/read request with ID and PARAMS from WS for SESSION."
  (let* ((uri (alist-get 'uri params))
         (file-path (when (string-prefix-p "file://" uri)
                      (substring uri 7))))
    (if (and file-path (file-exists-p file-path))
        (let ((content (with-temp-buffer
                         (insert-file-contents file-path)
                         (buffer-string))))
          (monet--send-response
           ws id
           `((contents . ,(vector `((uri . ,uri)
                                    (text . ,content)
                                    (mimeType . ,(monet--get-mime-type file-path))))))))
      (monet--send-error
       ws id -32602
       (format "Resource not found: %s" uri)))))

;;;; Tool handlers
(defun monet--get-tool-handler (name)
  "Return the handler function for tool NAME."
  (pcase name
    ("getCurrentSelection" #'monet--tool-get-current-selection-handler)
    ("openDiff" #'monet--tool-open-diff-handler)
    ("closeAllDiffTabs" #'monet--tool-close-all-diff-tabs-handler)
    ("close_tab" #'monet--tool-close-tab-handler)
    ("openFile" #'monet--tool-open-file-handler)
    ("saveDocument" #'monet--tool-save-document-handler)
    ("checkDocumentDirty" #'monet--tool-check-document-dirty-handler)
    ("getOpenEditors" #'monet--tool-get-open-editors-handler)
    ("getWorkspaceFolders" #'monet--tool-get-workspace-folders-handler)
    ("getDiagnostics" #'monet--tool-get-diagnostics-handler)
    ("getLatestSelection" #'monet--tool-get-latest-selection-handler)
    (_ nil)))

;; Tool handler adapters (convert MCP protocol to tool function calls)
(defun monet--tool-get-current-selection-handler (_params _session)
  "MCP handler for getCurrentSelection tool.
_PARAMS and _SESSION are unused."
  (funcall monet-get-current-selection-tool))

(defun monet--tool-get-latest-selection-handler (_params _session)
  "MCP handler for getLatestSelection tool.
_PARAMS and _SESSION are unused."
  (funcall monet-get-latest-selection-tool))
(defun monet--tool-open-diff-handler (params session)
  "MCP handler for openDiff tool.

PARAMS contains old_file_path, new_file_path, new_file_contents, tab_name.
SESSION is the MCP session.
Returns deferred response indicator."
  (let* ((old-file-path (alist-get 'old_file_path params))
         (new-file-path (alist-get 'new_file_path params))
         (new-file-contents (alist-get 'new_file_contents params))
         (tab-name (alist-get 'tab_name params))
         ;; Capture the current buffer's file that initiated this diff
         (initiating-file (when buffer-file-name
                            (expand-file-name buffer-file-name)))
         ;; Define accept callback
         (on-accept
          (lambda (final-contents)
            (interactive)
            ;; Use the passed final-contents parameter instead of closure value
            ;; Defer the response until after Emacs is idle
            (run-with-idle-timer 0 nil
                                 (lambda ()
                                   ;; Send FILE_SAVED response
                                   (monet--complete-deferred-response
                                    tab-name
                                    (list (list (cons 'type "text")
                                                (cons 'text "FILE_SAVED"))
                                          (list (cons 'type "text")
                                                (cons 'text final-contents))))))
            ;; Update the selection after a short delay
            (when-let ((client (monet--session-client session)))
              (run-with-timer 0.1 nil
                              (lambda ()
                                (monet--send-selection client))))))
         ;; Define quit callback
         (on-quit
          (lambda ()
            (interactive)
            ;; Send DIFF_REJECTED response
            (monet--complete-deferred-response
             tab-name
             (list (list (cons 'type "text")
                         (cons 'text "DIFF_REJECTED"))
                   (list (cons 'type "text")
                         (cons 'text tab-name))))
            ;; Schedule cleanup of the diff after sending response
            (run-with-timer 0.2 nil
                            (lambda ()
                              ;; Clean up the diff
                              (monet--cleanup-diff tab-name session)
                              ;; Ping and update selection
                              (when-let ((client (monet--session-client session)))
                                (monet--ping client)
                                (monet--send-selection client))))))
         (opened-diffs (monet--session-opened-diffs session))
         ;; Create the diff using the configured diff tool
         (diff-context (condition-case err
                           (funcall monet-diff-tool old-file-path new-file-path
                                    new-file-contents on-accept on-quit)
                         (error "Diff tool failed: %s" (error-message-string err)))))

    ;; Enhance the diff context with session information
    (when diff-context
      (setf (alist-get 'initiating-file diff-context) initiating-file)
      (setf (alist-get 'session-directory diff-context) (monet--session-directory session))
      (setf (alist-get 'tab-name diff-context) tab-name))

    ;; Store the full context in hash table
    (puthash tab-name diff-context opened-diffs)

    ;; Update diff visibility if feature is enabled
    (when monet-hide-diff-when-irrelevant
      (run-with-idle-timer 0.01 nil #'monet--update-diff-visibility))

    ;; Return deferred response indicator
    `((deferred . t)
      (unique-key . ,tab-name))))

(defun monet--tool-close-all-diff-tabs-handler (_params session)
  "MCP handler for closeAllDiffTabs tool.
_PARAMS is unused.
SESSION is the MCP session."
  (monet--close-all-diff-tabs session))

(defun monet--tool-close-tab-handler (params session)
  "MCP handler for close_tab tool.
PARAMS contains tab_name.
SESSION is the MCP session."
  (let ((tab-name (alist-get 'tab_name params))
        (current-window-before (selected-window))
        (current-buffer-before (current-buffer)))
    (let ((result (monet--close-tab tab-name session)))
      result)))

(defun monet--tool-open-file-handler (params _session)
  "MCP handler for openFile tool.
PARAMS contains uri.
_SESSION is unused."
  (let ((uri (alist-get 'uri params)))
    (funcall monet-open-file-tool uri)))

(defun monet--tool-save-document-handler (params _session)
  "MCP handler for saveDocument tool.
PARAMS contains uri.
_SESSION is unused."
  (let ((uri (alist-get 'uri params)))
    (funcall monet-save-document-tool uri)))

(defun monet--tool-check-document-dirty-handler (params _session)
  "MCP handler for checkDocumentDirty tool.
PARAMS contains uri.
_SESSION is unused."
  (let ((uri (alist-get 'uri params)))
    (funcall monet-check-document-dirty-tool uri)))

(defun monet--tool-get-open-editors-handler (_params _session)
  "MCP handler for getOpenEditors tool.
_PARAMS and _SESSION are unused."
  (funcall monet-get-open-editors-tool))

(defun monet--tool-get-workspace-folders-handler (_params _session)
  "MCP handler for getWorkspaceFolders tool.
_PARAMS and _SESSION are unused."
  (funcall monet-get-workspace-folders-tool))

(defun monet--tool-get-diagnostics-handler (params _session)
  "MCP handler for getDiagnostics tool.
PARAMS may contain optional uri.
_SESSION is unused."
  (let ((uri (alist-get 'uri params)))
    (funcall monet-diagnostics-tool uri)))

(defun monet--handle-tools-list (_session ws id _params)
  "Handle tools/list request with ID and PARAMS from WS for SESSION."
  (monet--send-response
   ws id
   `((tools . ,(monet--get-tools-list)))))

(defun monet--handle-tools-call (session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (handler (monet--get-tool-handler tool-name)))
    (if handler
        (condition-case err
            (let ((result (funcall handler arguments session)))
              ;; Check if this is a deferred response
              (if (and (listp result) (alist-get 'deferred result))
                  (let ((unique-key (alist-get 'unique-key result)))
                    ;; Store the deferred response
                    (monet--store-deferred-response session unique-key id)
                    ;; Don't send response yet - it will be sent later
                    nil)
                ;; Normal response - wrap in content field and send immediately
                (monet--send-response ws id `((content . ,result)))))
          (error
           (monet--send-error
            ws id -32603
            (format "Error in tool %s: %s" tool-name (error-message-string err)))))
      (monet--send-error
       ws id -32601
       (format "Tool not found: %s" tool-name)))))

(defun monet-default-get-current-selection-tool ()
  "Default implementation of getCurrentSelection tool."
  (let ((selection-data (monet--get-selection)))
    (if selection-data
        (list `((type . "text")
                (text . ,(json-encode selection-data))))
      (list `((type . "text")
              (text . ,(json-encode '((text . "")
                                      (filePath . "")
                                      (selection . ((start . ((line . 0) (character . 0)))
                                                    (end . ((line . 0) (character . 0)))
                                                    (isEmpty . t)))))))))))

(defun monet-default-get-latest-selection-tool ()
  "Default implementation to get the latest text selection from any file."
  ;; Find the most recently selected buffer with a selection
  (let ((selection-data nil)
        (latest-time 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (use-region-p)
                   (> (buffer-modified-tick) latest-time))
          (setq latest-time (buffer-modified-tick))
          (setq selection-data (monet--get-selection)))))
    ;; If no selection found, return empty
    (if selection-data
        (list `((type . "text")
                (text . ,(json-encode selection-data))))
      (list `((type . "text")
              (text . ,(json-encode '((text . "")
                                      (filePath . "")
                                      (selection . ((start . ((line . 0) (character . 0)))
                                                    (end . ((line . 0) (character . 0)))
                                                    (isEmpty . t)))))))))))

;;; MCP Over Websockets Implementation
(defun monet--get-tools-list ()
  "Return the list of available MCP tools."
  (vector
   `((name . "getCurrentSelection")
     (description . "Get the current text selection or cursor position in the active editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "openFile")
     (description . "Open a file in the editor. <important>Do NOT use emacs or emacsclient to open a file. Use this IDE openFile tool to open a file in the editor / Emacs / IDE.")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to open")))))
                     (required . ["uri"]))))
   `((name . "openDiff")
     (description . "Open a diff view")
     (inputSchema . ((type . "object")
                     (properties . ((old_file_path . ((type . "string")))
                                    (new_file_path . ((type . "string")))
                                    (new_file_contents . ((type . "string")))
                                    (tab_name . ((type . "string")))))
                     (required . ["old_file_path" "new_file_path" "new_file_contents"]))))
   `((name . "closeAllDiffTabs")
     (description . "Close all diff tabs")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "close_tab")
     (description . "Close a tab")
     (inputSchema . ((type . "object")
                     (properties . ((tab_name . ((type . "string")))))
                     (required . ["tab_name"]))))
   `((name . "getDiagnostics")
     (description . "Get diagnostics for a file. <important>Unless told otherwise, do NOT use external commands, typecheckers, or lint tools to get diagnostics, errors, or warnings. Use this getDiagnostics IDE tool</important>")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string"))))))))
   `((name . "getOpenEditors")
     (description . "Get the list of currently open files in the editor")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "getWorkspaceFolders")
     (description . "Get the list of workspace folders (project directories)")
     (inputSchema . ((type . "object")
                     (properties . ()))))
   `((name . "checkDocumentDirty")
     (description . "Check if a document has unsaved changes")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to check")))))
                     (required . ["uri"]))))
   `((name . "saveDocument")
     (description . "Save a document to disk")
     (inputSchema . ((type . "object")
                     (properties . ((uri . ((type . "string")
                                            (description . "The file URI or path to save")))))
                     (required . ["uri"]))))
   `((name . "getLatestSelection")
     (description . "Get the latest text selection from any file")
     (inputSchema . ((type . "object")
                     (properties . ()))))))

;; Resource definitions
(defun monet--get-file-resources (&optional cursor)
  "Get list of file resources.
CURSOR is for pagination support (not implemented yet).
Returns a list of resource objects."
  (let ((resources '()))
    ;; Add open file buffers as resources
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push `((uri . ,(concat "file://" file))
                (name . ,(file-name-nondirectory file))
                (description . ,(format "Open file in buffer: %s" (buffer-name buffer)))
                (mimeType . ,(monet--get-mime-type file)))
              resources)))
    ;; Add recent files if available
    (when (boundp 'recentf-list)
      (dolist (file (seq-take recentf-list 20)) ; Limit to 20 recent files
        (when (and (file-exists-p file)
                   ;; Don't duplicate already open files
                   (not (find-buffer-visiting file)))
          (push `((uri . ,(concat "file://" file))
                  (name . ,(file-name-nondirectory file))
                  (description . "Recent file")
                  (mimeType . ,(monet--get-mime-type file)))
                resources))))
    ;; Add project files if available (limit for performance)
    (when-let* ((project (project-current))
                (root (project-root project)))
      ;; Get a sample of project files
      (let ((project-files (project-files project)))
        (dolist (file (seq-take project-files 50)) ; Limit to 50 project files
          (let ((full-path (expand-file-name file root)))
            (when (and (file-regular-p full-path)
                       ;; Don't duplicate already listed files
                       (not (cl-find-if (lambda (r)
                                          (string= (alist-get 'uri r)
                                                   (concat "file://" full-path)))
                                        resources)))
              (push `((uri . ,(concat "file://" full-path))
                      (name . ,(file-name-nondirectory full-path))
                      (description . "Project file")
                      (mimeType . ,(monet--get-mime-type full-path)))
                    resources))))))
    (nreverse resources)))

;; Hooks
(defun monet-register-hooks ()
  "Register hooks for MCP functionality."
  (add-hook 'post-command-hook #'monet--track-selection-change)
  (add-hook 'post-command-hook #'monet--track-diff-visibility))

;;; Diff Visibility Management
(defun monet--is-buffer-relevant-to-session-p (buffer session-directory initiating-file)
  "Check if BUFFER is relevant to a session.
BUFFER is relevant if its file is under SESSION-DIRECTORY or matches INITIATING-FILE."
  (when-let ((file-name (buffer-file-name buffer)))
    (let ((expanded-file (expand-file-name file-name)))
      (or
       ;; File is under session directory
       (and session-directory
            (string-prefix-p (file-name-as-directory session-directory)
                             expanded-file))
       ;; File matches the initiating file
       (and initiating-file
            (string= expanded-file initiating-file))))))

(defun monet--should-show-diff-p (diff-context current-buffer)
  "Determine if diff should be visible based on CURRENT-BUFFER.
DIFF-CONTEXT contains the diff information including session context."
  (let ((diff-buffer (alist-get 'diff-buffer diff-context))
        (session-directory (alist-get 'session-directory diff-context))
        (initiating-file (alist-get 'initiating-file diff-context)))
    (or
     ;; Current buffer IS the diff buffer
     (eq current-buffer diff-buffer)
     ;; Current buffer is relevant to the session
     (monet--is-buffer-relevant-to-session-p current-buffer
                                              session-directory
                                              initiating-file))))

(defun monet--hide-diff-window (diff-buffer)
  "Hide the window displaying DIFF-BUFFER if visible."
  (when-let ((window (get-buffer-window diff-buffer)))
    ;; Store the window's buffer in a property for later restoration
    (with-current-buffer diff-buffer
      (setq-local monet--hidden-window window))
    ;; Switch the window to show scratch buffer or previous buffer
    (with-selected-window window
      (switch-to-prev-buffer))))

(defun monet--show-diff-window (diff-buffer)
  "Show DIFF-BUFFER in a window, restoring previous window if possible."
  (if-let ((window (get-buffer-window diff-buffer)))
      ;; Already visible, do nothing
      nil
    ;; Try to restore to previous window or create new one
    (let ((prev-window (with-current-buffer diff-buffer
                         (and (boundp 'monet--hidden-window)
                              monet--hidden-window))))
      (if (and prev-window (window-live-p prev-window))
          ;; Restore to previous window
          (set-window-buffer prev-window diff-buffer)
        ;; Display in new window
        (display-buffer diff-buffer)))))

(defun monet--update-diff-visibility ()
  "Update visibility of all diff buffers based on current buffer context.
Only runs when `monet-hide-diff-when-irrelevant' is non-nil."
  (when monet-hide-diff-when-irrelevant
    (let ((current-buf (current-buffer)))
      ;; Check all sessions for active diffs
      (maphash
       (lambda (_key session)
         (let ((opened-diffs (monet--session-opened-diffs session)))
           (maphash
            (lambda (_tab-name diff-context)
              (let ((diff-buffer (alist-get 'diff-buffer diff-context)))
                (when (and diff-buffer (buffer-live-p diff-buffer))
                  (if (monet--should-show-diff-p diff-context current-buf)
                      (monet--show-diff-window diff-buffer)
                    (monet--hide-diff-window diff-buffer)))))
            opened-diffs)))
       monet--sessions))))

(defvar monet--diff-visibility-timer nil
  "Timer for debouncing diff visibility updates.")

(defun monet--track-diff-visibility ()
  "Track buffer changes and update diff visibility with debouncing.
This is called from post-command-hook."
  (when (and monet-hide-diff-when-irrelevant
             ;; Only run if there are active diffs
             (cl-some (lambda (session)
                        (> (hash-table-count (monet--session-opened-diffs session)) 0))
                      (hash-table-values monet--sessions)))
    ;; Cancel any existing timer
    (when monet--diff-visibility-timer
      (cancel-timer monet--diff-visibility-timer))
    ;; Set new timer for debounced update
    (setq monet--diff-visibility-timer
          (run-with-idle-timer 0.1 nil #'monet--update-diff-visibility))))

;;; Selection Tracking
(defun monet--get-selection ()
  "Return current selection information."
  (when buffer-file-name
    (let* ((file-path buffer-file-name)
           (point-pos (point))
           (start-pos (if (use-region-p) (region-beginning) point-pos))
           (end-pos (if (use-region-p) (region-end) point-pos))
           (text (if (use-region-p)
                     (buffer-substring-no-properties start-pos end-pos)
                   ""))
           (start-line (1- (line-number-at-pos start-pos)))
           (end-line (1- (line-number-at-pos end-pos)))
           (start-col (save-excursion
                        (goto-char start-pos)
                        (current-column)))
           (end-col (save-excursion
                      (goto-char end-pos)
                      (current-column)))
           (selection `((start . ((line . ,start-line)
                                  (character . ,start-col)))
                        (end . ((line . ,end-line)
                                (character . ,end-col)))
                        (isEmpty . ,(if (use-region-p) :json-false t))))
           (file-url (concat "file://" file-path)))
      `((text . ,text)
        (filePath . ,file-path)
        (fileUrl . ,file-url)
        (selection . ,selection)))))

(defun monet--track-selection-change ()
  "Track selection change in file buffers."
  (when (and buffer-file-name
             ;; Only track if we have initialized sessions
             (cl-some (lambda (session)
                        (and (monet--session-initialized session)
                             (monet--session-client session)))
                      (hash-table-values monet--sessions)))
    ;; Cancel any existing timer
    (when monet--selection-timer
      (cancel-timer monet--selection-timer))
    ;; Set new timer for debounced update
    (setq monet--selection-timer
          (run-with-timer monet--selection-delay nil
                          #'monet--send-selection-to-all))))

(defun monet--send-selection (client)
  "Send current selection to CLIENT if buffer has file and is live."
  (when (and buffer-file-name
             (buffer-live-p (current-buffer)))
    (let ((selection (monet--get-selection)))
      (when selection
        (monet--send-notification
         client
         "selection_changed"
         selection)))))

(defun monet--send-selection-to-all ()
  "Send selection update to all initialized sessions."
  (when buffer-file-name
    (let* ((cursor-pos (point))
           (current-state (if (use-region-p)
                              `(,cursor-pos ,(region-beginning) ,(region-end))
                            `(,cursor-pos ,cursor-pos ,cursor-pos)))
           (selection (monet--get-selection)))
      (maphash (lambda (_key session)
                 (when (and (monet--session-initialized session)
                            (monet--session-client session))
                   (monet--send-notification
                    (monet--session-client session)
                    "selection_changed"
                    selection)))
               monet--sessions))))

;;; Tools

;; Minor mode for diff tool keybindings to ensure compatibility with evil-mode
(defvar monet-diff-mode-map (make-sparse-keymap)
  "Keymap for monet-diff-mode.")

(define-minor-mode monet-diff-mode
  "Minor mode for monet diff tool keybindings.
This mode ensures keybindings work correctly even when evil-mode is active."
  :init-value nil
  :lighter " MonetDiff"
  :keymap monet-diff-mode-map
  :group 'monet)

(defun monet--setup-diff-keybindings (accept-key quit-key on-accept on-quit)
  "Set up diff keybindings ensuring evil-mode compatibility.
ACCEPT-KEY and QUIT-KEY are the keys to bind.
ON-ACCEPT and ON-QUIT are the callbacks to execute."
  ;; Clear any existing bindings in our mode map
  (setcdr monet-diff-mode-map nil)

  ;; Define our keybindings in the minor mode map
  (define-key monet-diff-mode-map (kbd accept-key) on-accept)
  (define-key monet-diff-mode-map (kbd quit-key) on-quit)

  ;; Enable our minor mode with high precedence
  (monet-diff-mode 1)

  ;; If evil-mode is active, also define the keys for the appropriate states
  (when (and (boundp 'evil-mode) evil-mode)
    ;; Define for normal state (most common in diff buffers)
    (when (fboundp 'evil-local-set-key)
      (evil-local-set-key 'normal (kbd accept-key) on-accept)
      (evil-local-set-key 'normal (kbd quit-key) on-quit)
      ;; Also define for motion state (sometimes used in special buffers)
      (evil-local-set-key 'motion (kbd accept-key) on-accept)
      (evil-local-set-key 'motion (kbd quit-key) on-quit))

    ;; For ediff control buffer, ensure we're in emacs state
    (when (string-match-p "\\*Ediff Control Panel\\*" (buffer-name))
      (when (fboundp 'evil-emacs-state)
        (evil-emacs-state)))))

(defun monet-simple-diff-tool (old-file-path new-file-path new-file-contents on-accept on-quit)
  "Create a simple, read- diff display using diff-mode.

OLD-FILE-PATH is the path to the original file.
NEW-FILE-PATH is the path where changes will be saved (often same as old).
NEW-FILE-CONTENTS is the proposed new content.
ON-ACCEPT is a function called when user accepts (no arguments).
ON-QUIT is a function called when user quits (no arguments).

Returns the diff context object for later used by the cleanup tool."
  (unless (and old-file-path new-file-contents)
    (error "Missing required parameters"))
  (let ((file-exists (file-exists-p old-file-path))
        (old-temp-buffer (generate-new-buffer
                          (format " *%s-old*" (file-name-nondirectory old-file-path))))
        (new-temp-buffer (generate-new-buffer
                          (format " *%s-new*" (file-name-nondirectory old-file-path)))))
    ;; Fill the old temp buffer
    (with-current-buffer old-temp-buffer
      (when file-exists
        (insert-file-contents old-file-path))
      ;; Mark as not modified since this is a temporary buffer
      (set-buffer-modified-p nil))
    ;; Fill the new temp buffer
    (with-current-buffer new-temp-buffer
      (insert new-file-contents)
      ;; Mark as not modified since this is a temporary buffer
      (set-buffer-modified-p nil))
    ;; Create the diff
    (let* ((diff-buffer-name (format "*Diff: %s*" (file-name-nondirectory old-file-path)))
           (diff-buffer (get-buffer-create diff-buffer-name t))
           (switches `("-u" "--label" ,old-file-path "--label" ,(or new-file-path old-file-path)))
           ;; syntax highlighting
           (diff-font-lock-syntax 'hunk-also)
           (diff-font-lock-prettify t)
           ;; diff-no-select will try to add extra labels if diff-use-labels is t or 'check; we're supplying or own labels
           (diff-use-labels nil))
      ;; Create diff via diff-no-select
      (diff-no-select old-temp-buffer new-temp-buffer switches t diff-buffer)
      ;; Configure the diff buffer
      (with-current-buffer diff-buffer
        ;; Set the default directory to help with file resolution
        (setq default-directory (file-name-directory old-file-path))
        ;; Store file paths for diff-mode to use
        (setq-local diff-vc-backend nil) ; Not using VC
        (setq-local diff-default-directory default-directory)
        ;; Set diff-font-lock-syntax to 'hunk-also BEFORE calling diff-mode
        (setq-local diff-font-lock-syntax 'hunk-also)
        ;; Re-initialize diff-mode with our settings
        (diff-mode)

        ;; Set up keybindings using our minor mode for evil-mode compatibility
        (monet--setup-diff-keybindings
         monet-simple-diff-accept-key
         monet-simple-diff-quit-key
         (lambda ()
           (interactive)
           (funcall on-accept new-file-contents))
         (lambda ()
           (interactive)
           (funcall on-quit)))

        ;; Call on-quit on quit window
        (add-hook 'quit-window-hook (lambda () (funcall on-quit)) nil t))

      ;; Display the diff buffer and switch to it
      ;; Note: visibility management will be handled by the post-command-hook
      ;; after the context is enhanced with session info in the handler
      (let ((diff-window (display-buffer diff-buffer
                                         '((display-buffer-pop-up-window)))))
        (when diff-window
          (select-window diff-window)))

      ;; Help message
      (message "Type %s in the diff buffer to accept changes, or %s to reject"
               monet-simple-diff-accept-key
               monet-simple-diff-quit-key)

      ;; Return a context object for consistency with new interface
      `((diff-buffer . ,diff-buffer)
        (old-temp-buffer . ,old-temp-buffer)
        (new-temp-buffer . ,new-temp-buffer)
        (new-contents . ,new-file-contents)
        (old-file-path . ,old-file-path)))))

(defun monet-simple-diff-cleanup-tool (diff-context)
  "Clean up diff session using DIFF-CONTEXT.

DIFF-CONTEXT is a context object containing diff-buffer and temp buffers."
  (let ((diff-buffer (alist-get 'diff-buffer diff-context))
        (old-temp-buffer (alist-get 'old-temp-buffer diff-context))
        (new-temp-buffer (alist-get 'new-temp-buffer diff-context))
        (kill-buffer-query-functions nil))
    (when (and diff-buffer (buffer-live-p diff-buffer))
      ;; Mark as not modified
      (with-current-buffer diff-buffer
        (set-buffer-modified-p nil))
      ;; Kill the diff buffer and close its window
      (let ((diff-window (get-buffer-window diff-buffer))
            (original-window (selected-window)))
        (when diff-window
          ;; If we're in the diff window, switch to another window first
          (when (eq (selected-window) diff-window)
            ;; Try to select previous window
            (other-window -1))
          ;; Delete the diff window
          (delete-window diff-window)))
      (kill-buffer diff-buffer)
      ;; Kill the new temporary buffer
      (when (and new-temp-buffer (buffer-live-p new-temp-buffer))
        (with-current-buffer new-temp-buffer
          (set-buffer-modified-p nil))
        (kill-buffer new-temp-buffer))
      ;; Kill the old temporary buffer
      (when (and old-temp-buffer (buffer-live-p old-temp-buffer))
        (with-current-buffer old-temp-buffer
          (set-buffer-modified-p nil))
        (kill-buffer old-temp-buffer)))))

(defun monet-ediff-tool (old-file-path new-file-path new-file-contents on-accept on-quit)
  "Show a diff using ediff.

OLD-FILE-PATH is the path to the original file.
NEW-FILE-PATH is the path where changes will be saved (often same as old).
NEW-FILE-CONTENTS is the proposed new content.
ON-ACCEPT is a function called when user accepts (no arguments).
ON-QUIT is a function called when user quits (no arguments).

Returns the diff context object."
  (let* (;; Get the old and new buffers
         (old-buffer (find-file-noselect old-file-path))
         (new-buffer (generate-new-buffer "*monet-ediff-new*"))

         ;; Save current buffer
         (original-buffer (current-buffer))

         ;; Get a suitable buffer for ediff display (avoid *claude* and similar special buffers)
         (ediff-buffer (if (string-match-p "^\\*claude" (buffer-name))
                           ;; Find the most recent file buffer, or use scratch
                           (or (car (seq-filter
                                     (lambda (b)
                                       (and (buffer-file-name b)
                                            (not (string-prefix-p " " (buffer-name b)))))
                                     (buffer-list)))
                               (get-buffer-create "*scratch*"))
                         (current-buffer)))

         ;; Switch to the suitable buffer before capturing window config
         (window-config (with-current-buffer ediff-buffer
                          (current-window-configuration)))
         (original-point (with-current-buffer ediff-buffer
                           (point)))

         ;; Create the context alist that will hold our state
         (context (list (cons 'control-buffer nil) ; Will be set in startup hook
                        (cons 'new-buffer new-buffer)
                        (cons 'old-buffer old-buffer)
                        (cons 'window-config window-config)
                        (cons 'original-buffer original-buffer)
                        (cons 'ediff-buffer ediff-buffer)
                        (cons 'original-point original-point)
                        (cons 'new-contents new-file-contents)
                        (cons 'old-file-path old-file-path)))

         ;; Single frame ediff display
         (ediff-window-setup-function #'ediff-setup-windows-plain)

         ;; Horizontal or vertical ediff windows
         (ediff-split-window-function (if (eq monet-ediff-split-window-direction 'vertical)
                                          #'split-window-vertically
                                        #'split-window-horizontally))

         ;; Set ediff message with configured keys
         (ediff-brief-message-string (format " Type %s to accept changes, %s to quit, ? for help"
                                             monet-ediff-accept-key
                                             monet-ediff-quit-key)))

    ;; Put the new contexts into the new bufer
    (with-current-buffer new-buffer
      (insert new-file-contents)
      ;; Set the major mode based on the file extension for syntax highlighting
      (let ((buffer-file-name new-file-path))
        (set-auto-mode)))

    ;; Switch to the ediff-buffer
    (switch-to-buffer ediff-buffer)

    ;; Start ediff session with a setup hook that captures the control buffer
    (ediff-buffers
     old-buffer
     new-buffer
     (list (lambda ()
             ;; Put the control buffer in the context
             (setf (alist-get 'control-buffer context) (current-buffer))

             ;; Store the on-accept and on-quit callbacks in local buffer variables
             (setq-local on-accept-callback on-accept)
             (setq-local on-quit-callback on-quit)

             ;; Set up keybindings using our minor mode for evil-mode compatibility
             (monet--setup-diff-keybindings
              monet-ediff-accept-key
              monet-ediff-quit-key
              (lambda ()
                (interactive)
                ;; Get the edited content from buffer B
                (let ((edited-content
                       (with-current-buffer ediff-buffer-B
                         (buffer-substring-no-properties (point-min) (point-max)))))
                  (funcall on-accept-callback edited-content)))
              (lambda ()
                (interactive)
                (funcall on-quit-callback)))

             ;; Jump to the first difference
             (ignore-errors (ediff-next-difference)))))

    ;; We'll cleanup the mess
    (remove-hook 'ediff-quit-hook #'ediff-cleanup-mess t)

    ;; Return the context object
    context))

(defun monet-ediff-cleanup-tool (context)
  "Clean up ediff session using CONTEXT object."
  (let ((control-buffer (alist-get 'control-buffer context))
        (window-config (alist-get 'window-config context))
        (ediff-buffer (alist-get 'ediff-buffer context))
        (original-buffer (alist-get 'original-buffer context)))
    ;; Quit ediff if control buffer exists
    (when (and control-buffer (buffer-live-p control-buffer))
      (with-current-buffer control-buffer
        ;; bypass ediff confirmation
        (let ((ctl-buf (current-buffer))
              (ctl-frm (selected-frame))
              (minibuffer-auto-raise t))
          (setq this-command 'ediff-quit) ; bug#38219
          (set-buffer ctl-buf)
          (let ((ediff-keep-variants t))
            (ediff-really-quit nil)
            (ediff-cleanup-mess)))))

    ;; Restore window configuration immediately after ediff quits
    ;; Don't try to kill any buffers - let ediff handle that
    (when window-config
      (set-window-configuration window-config))

    ;; Switch back to original buffers and restore position
    (let ((original-point (alist-get 'original-point context))
          (original-mark (alist-get 'original-mark context))
          (new-buffer (alist-get 'new-buffer context)))
      (when (and ediff-buffer (buffer-live-p ediff-buffer))
        (switch-to-buffer ediff-buffer)
        (when original-point
          (goto-char original-point)))
      ;; Kill the temporary buffer after ediff cleanup
      (when (and new-buffer (buffer-live-p new-buffer))
        (kill-buffer new-buffer)))

    ;; switch back to original buffer if different from the ediff buffer
    (when (not (eq ediff-buffer original-buffer))
      (switch-to-buffer original-buffer))))

(defun monet--close-all-diff-tabs (session)
  "Close all diff tabs created by Claude.
SESSION is the monet session containing opened diffs."
  (let ((closed-count 0))
    (when session
      ;; Close all diffs for this session
      (let ((opened-diffs (monet--session-opened-diffs session)))
        (maphash (lambda (tab-name _diff-info)
                   (monet--cleanup-diff tab-name session)
                   (setq closed-count (1+ closed-count)))
                 opened-diffs)))
    ;; Return success with actual count
    (list `((type . "text")
            (text . ,(format "CLOSED_%d_DIFF_TABS" closed-count))))))

(defun monet--close-tab (tab-name session)
  "Close a tab/buffer by TAB-NAME.
TAB-NAME is the name of the tab/buffer to close.
SESSION is the monet session for tracking opened diffs."
  (if tab-name
      (let* ((opened-diffs (when session
                             ;; Handle closing by tab name (check if this is a diff tab first)
                             (monet--session-opened-diffs session)))
             (diff-info (when opened-diffs
                          (gethash tab-name opened-diffs))))
        (if diff-info
            ;; This is a diff tab - clean it up
            (progn
              ;; Clean up the diff
              (monet--cleanup-diff tab-name session)
              ;; Update the selection
              (when-let ((client (monet--session-client session)))
                (run-with-timer 0.1 nil
                                (lambda ()
                                  ;; Send the selection
                                  (monet--send-selection client))))
              (list `((type . "text")
                      (text . "TAB_CLOSED"))))
          ;; Not a diff - DON'T close regular buffers!
          ;; This is likely Claude trying to close a file buffer, which we should ignore
          (list `((type . "text")
                (text . "TAB_NOT_DIFF")))))
  ;; No tab name provided
  (t
   (list `((type . "text")
           (text . "NO_TAB_SPECIFIED"))))))

(defun monet-default-open-file-tool (uri)
  "Default implementation to open a file specified by URI.
URI can be a file path or file:// URI.
Returns success or error response."
  (condition-case err
      (let* (;; Handle file:// URIs
             (file-path (if (string-prefix-p "file://" uri)
                            (substring uri 7)
                          uri)))
        ;; Expand and normalize the file path
        (setq file-path (expand-file-name file-path))
        ;; Check if file exists
        (unless (file-exists-p file-path)
          (error "File not found: %s" file-path))
        ;; Open the file - find-file will display it appropriately
        ;; If the file is already open in a window, it will switch to that window
        ;; Otherwise it will use the current window
        (find-file file-path)
        ;; Return success with file information
        (list `((type . "text")
                (text . "FILE_OPENED"))))
    (error
     (list `((type . "text")
             (text . ,(format "Error opening file: %s" (error-message-string err))))))))

(defun monet-default-save-document-tool (uri)
  "Default implementation to save a document to disk.
URI is the file URI or path to save.
Returns success or error response."
  (let* ((file-path (if (string-prefix-p "file://" uri)
                        (substring uri 7)
                      uri))
         (buffer (find-buffer-visiting file-path)))
    (if buffer
        (with-current-buffer buffer
          (save-buffer)
          (list `((type . "text")
                  (text . ,(json-encode `((saved . t)))))))
      (list `((type . "text")
              (text . ,(json-encode `((saved . :json-false)
                                      (error . "File not open")))))))))

(defun monet-default-check-document-dirty-tool (uri)
  "Default implementation to check if a document has unsaved changes.
URI is the file URI or path to check.
Returns dirty status response."
  (let* ((file-path (if (string-prefix-p "file://" uri)
                        (substring uri 7)
                      uri))
         (buffer (find-buffer-visiting file-path))
         (is-dirty (if buffer
                       (buffer-modified-p buffer)
                     nil)))
    (list `((type . "text")
            (text . ,(json-encode `((isDirty . ,is-dirty))))))))

(defun monet-default-get-open-editors-tool ()
  "Default implementation to get list of currently open file editors.
Returns MCP-formatted response with editors list."
  (let ((editors '()))
    ;; Iterate through all buffers
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (let* ((file-path (buffer-file-name buffer))
               (file-name (file-name-nondirectory file-path))
               (file-uri (concat "file://" file-path)))
          (push `((uri . ,file-uri)
                  (name . ,file-name)
                  (path . ,file-path))
                editors))))
    ;; Return MCP-formatted response
    (list `((type . "text")
            (text . ,(json-encode `((editors . ,(nreverse editors)))))))))

(defun monet-default-get-workspace-folders-tool ()
  "Default implementation to get list of workspace folders/project roots.
Returns MCP-formatted response with folders list."
  (let ((folders '())
        (seen-dirs (make-hash-table :test 'equal)))

    ;; First, add current project root if available
    (when-let ((project (project-current)))
      (let* ((project-root (project-root project))
             (folder-name (file-name-nondirectory (directory-file-name project-root)))
             (folder-uri (concat "file://" project-root)))
        (unless (gethash project-root seen-dirs)
          (puthash project-root t seen-dirs)
          (push `((uri . ,folder-uri)
                  (name . ,folder-name))
                folders))))

    ;; Then add project roots for all open files
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (let* ((file-path (buffer-file-name buffer))
               (file-dir (file-name-directory file-path))
               (project (project-current nil file-dir))
               (root-dir (if project
                             (project-root project)
                           file-dir)))
          (unless (gethash root-dir seen-dirs)
            (puthash root-dir t seen-dirs)
            (let* ((folder-name (file-name-nondirectory (directory-file-name root-dir)))
                   (folder-uri (concat "file://" root-dir)))
              (push `((uri . ,folder-uri)
                      (name . ,folder-name))
                    folders))))))

    ;; Return MCP-formatted response
    (list `((type . "text")
            (text . ,(json-encode `((folders . ,(nreverse folders)))))))))

(defun monet-flymake-flycheck-diagnostics-tool (&optional uri)
  "Default implementation to get diagnostics for a file or all open files.
URI is optional file URI or path to get diagnostics for.
If URI is nil, gets diagnostics for all open files."
  (let* ((file-path (when uri
                      (if (string-prefix-p "file://" uri)
                          (substring uri 7)
                        uri)))
         (diagnostics-by-file '()))

    ;; Iterate through all buffers
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer)
                 (or (null file-path)
                     (string= file-path (buffer-file-name buffer))))
        (let ((file-diagnostics '())
              (file-uri (concat "file://" (buffer-file-name buffer))))
          (with-current-buffer buffer

            ;; Collect Flymake diagnostics
            (when (bound-and-true-p flymake-mode)
              (dolist (diag (flymake-diagnostics))
                (let* ((beg (flymake-diagnostic-beg diag))
                       (end (flymake-diagnostic-end diag))
                       (type (flymake-diagnostic-type diag))
                       (text (flymake-diagnostic-text diag))
                       (start-line (1- (line-number-at-pos beg)))
                       (end-line (1- (line-number-at-pos end)))
                       (start-char (save-excursion
                                     (goto-char beg)
                                     (current-column)))
                       (end-char (save-excursion
                                   (goto-char end)
                                   (current-column)))
                       (severity (cond
                                  ((eq type :error) "Error")
                                  ((eq type :warning) "Warning")
                                  ((eq type :note) "Information")
                                  (t "Hint"))))
                  (push `((range . ((start . ((line . ,start-line)
                                              (character . ,start-char)))
                                    (end . ((line . ,end-line)
                                            (character . ,end-char)))))
                          (severity . ,severity)
                          (message . ,text)
                          (source . "flymake"))
                        file-diagnostics))))

            ;; Collect Flycheck diagnostics
            (when (and (bound-and-true-p flycheck-mode)
                       (boundp 'flycheck-current-errors)
                       flycheck-current-errors)
              (dolist (err flycheck-current-errors)
                (let* ((line (1- (or (flycheck-error-line err) 1)))
                       (col (or (flycheck-error-column err) 0))
                       (level (flycheck-error-level err))
                       (msg (flycheck-error-message err))
                       (checker (symbol-name (flycheck-error-checker err)))
                       (severity (cond
                                  ((eq level 'error) "Error")
                                  ((eq level 'warning) "Warning")
                                  ((eq level 'info) "Information")
                                  (t "Hint"))))
                  (push `((range . ((start . ((line . ,line)
                                              (character . ,col)))
                                    (end . ((line . ,line)
                                            (character . ,(+ col 1))))))
                          (severity . ,severity)
                          (message . ,msg)
                          (source . ,checker))
                        file-diagnostics))))

            ;; Add file diagnostics if any found
            (when file-diagnostics
              (push `((uri . ,file-uri)
                      (diagnostics . ,(vconcat (nreverse file-diagnostics))))
                    diagnostics-by-file))))))

    ;; Return response to Claude
    (let ((json-str (if diagnostics-by-file
                        (json-encode (vconcat (nreverse diagnostics-by-file)))
                      "[]")))
      (list `((type . "text")
              (text . ,json-str))))))

;;; Logging
;;;; Logging configuration
(defvar monet--logging-enabled nil
  "Whether to enable logging of Claude communication.")

;;;; Logging functions
(defun monet--log-message (direction data json-string)
  "Log message DATA in DIRECTION with both elisp sexp and JSON formats.
DIRECTION is a string indicating message direction.
DATA is the elisp data structure.
JSON-STRING is the JSON representation."
  (when monet--logging-enabled
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
          (log-buffer (get-buffer-create monet-log-buffer-name)))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert "----------\n")
        (insert (format "[%s] %s ELISP: %S\n" timestamp direction data))
        (insert (format "[%s] %s JSON: %s\n" timestamp direction json-string))
        (insert "----------\n")
        ;; Auto-scroll if the log buffer is visible in any window
        (when-let ((window (get-buffer-window log-buffer)))
          (with-selected-window window
            (goto-char (point-max))))))))

;;;; Logging advice functions
(defun monet--advice-send-response (ws id result)
  "Log outgoing response before sending.
WS is the websocket connection.
ID is the request ID.
RESULT is the response result."
  (let* ((data `((jsonrpc . "2.0")
                 (id . ,id)
                 (result . ,result)))
         (response (json-encode data)))
    (monet--log-message ">>> SEND-RESPONSE" data response)))

(defun monet--advice-send-notification (client method &optional params)
  "Log outgoing notification before sending.
CLIENT is the websocket client connection.
METHOD is the notification method.
PARAMS are optional notification parameters."
  (when client
    (let* ((params (or params (make-hash-table :test 'equal)))
           (data `((jsonrpc . "2.0")
                   (method . ,method)
                   (params . ,params)))
           (notification (json-encode data)))
      (monet--log-message ">>> SEND-NOTIFICATION" data notification))))

(defun monet--advice-send-error (ws id code message &optional data)
  "Log outgoing error before sending.
WS is the websocket connection.
ID is the request ID.
CODE is the error code.
MESSAGE is the error message.
DATA is optional error data."
  (let* ((error-data `((jsonrpc . "2.0")
                       (id . ,id)
                       (error . ((code . ,code)
                                 (message . ,message)
                                 ,@(when data `((data . ,data)))))))
         (response (json-encode error-data)))
    (monet--log-message ">>> SEND-ERROR" error-data response)))

(defun monet--advice-on-message (session ws frame)
  "Log incoming message after receiving.
SESSION is the monet session.
WS is the websocket connection.
FRAME is the websocket frame containing the message."
  (condition-case err
      (let* ((payload (websocket-frame-text frame))
             (data (condition-case json-err
                       (json-read-from-string payload)
                     (error
                      ;; If JSON parsing fails, still log the raw payload
                      `((error . ,(format "JSON parse error: %s" (error-message-string json-err)))
                        (raw-payload . ,payload))))))
        (monet--log-message "<<< RECV" data payload))
    (error
     ;; If anything goes wrong with logging, don't interfere with the original function
     (monet--log-message "<<< RECV-ERROR"
                         `((error . ,(format "Logging error: %s" (error-message-string err))))
                         "N/A"))))

;; Logging control commands
(defun monet-enable-logging ()
  "Enable logging of Claude communication by installing advice functions."
  (interactive)
  (setq monet--logging-enabled t)
  ;; Install advice for outgoing messages
  (advice-add 'monet--send-response :before #'monet--advice-send-response)
  (advice-add 'monet--send-notification :before #'monet--advice-send-notification)
  (advice-add 'monet--send-error :before #'monet--advice-send-error)
  ;; Install advice for incoming messages
  (advice-add 'monet--on-message :before #'monet--advice-on-message)
  (message "Monet logging enabled"))

(defun monet-disable-logging ()
  "Disable logging of Claude communication by removing advice functions."
  (interactive)
  (setq monet--logging-enabled nil)
  ;; Remove advice for outgoing messages
  (advice-remove 'monet--send-response #'monet--advice-send-response)
  (advice-remove 'monet--send-notification #'monet--advice-send-notification)
  (advice-remove 'monet--send-error #'monet--advice-send-error)
  ;; Remove advice for incoming messages
  (advice-remove 'monet--on-message #'monet--advice-on-message)
  (message "Monet logging disabled"))

;;; User Commands

(defun monet--make-unique-key (base-key)
  "Generate a unique key based on BASE-KEY.
If BASE-KEY already exists in sessions, append <2>, <3>, etc."
  (if (not (gethash base-key monet--sessions))
      base-key
    (let ((counter 2)
          (unique-key))
      (while (gethash (setq unique-key (format "%s<%d>" base-key counter))
                      monet--sessions)
        (setq counter (1+ counter)))
      unique-key)))

(defun monet--get-session-context ()
  "Get the session context (key . directory) for the current buffer.
Returns a cons cell of (base-key . directory).
Uses project root if in a project, otherwise uses file directory or `default-directory'."
  (let ((project (project-current)))
    (if project
        ;; In a project - use project name and root
        (let* ((project-root (project-root project))
               (project-name (file-name-nondirectory
                              (directory-file-name project-root))))
          (cons project-name project-root))
      ;; Not in a project
      (let ((directory (if buffer-file-name
                           (file-name-directory buffer-file-name)
                         default-directory)))
        (cons (file-name-nondirectory
               (directory-file-name directory))
              directory)))))

(defun monet-start-server (&optional arg)
  "Start a Monet MCP websocket server.
With prefix ARG, prompt for directory to use.
Otherwise, use project root if in a project, or current file's directory."
  (interactive "P")
  (let* ((context (if arg
                      ;; With prefix arg - prompt for directory
                      (let ((dir (read-directory-name "Directory for Monet server: "
                                                      nil nil t)))
                        (cons (file-name-nondirectory
                               (directory-file-name dir))
                              dir))
                    ;; No prefix arg - use automatic context
                    (monet--get-session-context)))
         (base-key (car context))
         (directory (cdr context))
         (unique-key (monet--make-unique-key base-key)))
    (if (gethash unique-key monet--sessions)
        (error "Failed to generate unique key for %s" base-key)
      (let* ((session (monet-start-server-in-directory unique-key directory))
             (port (monet--session-port session)))
        (message "Started Monet server '%s' in %s, listening on port %d" unique-key directory port)))))

(defun monet-stop-server (key)
  "Stop the websocket server for.

When called interactively, prompt for KEY with completion from available
sessions. KEY is the session identifier."
  (interactive
   (list
    (if (zerop (hash-table-count monet--sessions))
        (error "No active Monet sessions")
      (completing-read "Stop server for key: "
                       (hash-table-keys monet--sessions)
                       nil t))))
  (if-let* ((session (gethash key monet--sessions))
            (server (monet--session-server session))
            (port (monet--session-port session)))
      (progn
        ;; Remove lockfile before closing server
        (monet--remove-lockfile port)
        ;; Close the server
        (websocket-server-close server)
        ;; Remove the session
        (remhash key monet--sessions))
    (message "No websocket server running for key %s" key)))

(defun monet-list-sessions ()
  "List all active Monet MCP sessions."
  (interactive)
  (if (hash-table-empty-p monet--sessions)
      (message "No active Monet sessions")
    (let ((session-info '()))
      (maphash
       (lambda (key session)
         (let* ((dir (monet--session-directory session))
                (port (monet--session-port session))
                (client-connected (if (monet--session-client session) "Yes" "No"))
                (initialized (if (monet--session-initialized session) "Yes" "No")))
           (push (format "%-20s Port: %-5d Connected: %-3s Initialized: %s  Directory: %s"
                         key port client-connected initialized dir)
                 session-info)))
       monet--sessions)
      (with-current-buffer (get-buffer-create "*Monet Sessions*")
        (erase-buffer)
        (insert "Active Monet Sessions:\n")
        (insert "======================\n\n")
        (dolist (info (nreverse session-info))
          (insert info "\n"))
        (insert (format "\nTotal sessions: %d\n" (hash-table-count monet--sessions)))
        (goto-char (point-min))
        (view-mode 1)
        (display-buffer (current-buffer))))))

(defun monet-stop-all-servers ()
  "Stop all running monet websocket servers."
  (interactive)
  (maphash
   (lambda (key _session)
     (monet-stop-server key))
   monet--sessions)
  ;; Cancel selection timer if active
  (when monet--selection-timer
    (cancel-timer monet--selection-timer)
    (setq monet--selection-timer nil))
  ;; Cancel visibility timer if active
  (when monet--diff-visibility-timer
    (cancel-timer monet--diff-visibility-timer)
    (setq monet--diff-visibility-timer nil))
  ;; Stop ping timer
  (monet--stop-ping-timer))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'monet-stop-all-servers)

;;; Minor Mode
(defvar monet-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'monet-start-server)
    (define-key map "q" #'monet-stop-server)
    (define-key map "Q" #'monet-stop-all-servers)
    (define-key map "l" #'monet-list-sessions)
    (define-key map "L" #'monet-enable-logging)
    (define-key map "D" #'monet-disable-logging)
    map)
  "Keymap for Monet mode commands under the prefix key.")

(defun monet--make-mode-map ()
  "Create the mode map with the configured prefix key."
  (let ((map (make-sparse-keymap)))
    (when monet-prefix-key
      (define-key map (kbd monet-prefix-key) monet-command-map))
    map))

(defvar monet-mode-map (monet--make-mode-map)
  "Keymap for Monet mode.")

;;;###autoload
(define-minor-mode monet-mode
  "Minor mode for Monet - Claude Code MCP integration.
When enabled, provides key bindings for managing Monet sessions.

\\{monet-mode-map}"
  :lighter " Monet"
  :keymap monet-mode-map
  :global t
  :group 'monet
  (if monet-mode
      (progn
        ;; Update keymap in case prefix key changed
        (setq monet-mode-map (monet--make-mode-map))
        (message "Monet mode enabled. Use %s for commands."
                 (or monet-prefix-key "M-x monet-")))
    (message "Monet mode disabled.")))

;;; provide monet
(provide 'monet)

;;; monet.el ends here
