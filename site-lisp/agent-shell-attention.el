;;; agent-shell-attention.el --- Mode-line attention tracker for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.0.0
;; URL: https://github.com/ultronozm/agent-shell-attention.el
;; Package-Requires: ((emacs "29.1") (agent-shell "0.24.2"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `agent-shell-attention-mode' instruments the `agent-shell' prompt
;; flow to keep track of buffers that are waiting for user input.  It
;; displays a clickable `AS:n' indicator in the mode line, provides a
;; command `agent-shell-attention-jump' to visit pending buffers, and
;; can optionally issue notifications via a user-defined function.
;;
;; It also includes a renderer for an `AS:n/m' pending/active indicator.
;;
;; Enable with (agent-shell-attention-mode).
;;
;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'map)

(declare-function notifications-notify "notifications")
(declare-function agent-shell--on-request "agent-shell")
(declare-function agent-shell--send-command "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")
(declare-function agent-shell--stop-reason-description "agent-shell")
(declare-function acp-send-request "acp")

(defgroup agent-shell-attention nil
  "Mode-line tally and minibuffer notifications for `agent-shell' buffers."
  :group 'agent-shell)

;;; Basic customizations

(defcustom agent-shell-attention-awaiting-stop-reasons '("end_turn")
  "Stop reasons that indicate the agent expects further input.
Each entry should be a string that matches the ACP stopReason field."
  :type '(repeat string)
  :group 'agent-shell-attention)

(defcustom agent-shell-attention-lighter " AS:%d"
  "String shown in the mode line indicator.
The token `%d' is replaced with the number of buffers waiting for
attention.  Set to nil to hide the lighter entirely."
  :type '(choice (const :tag "Hide indicator" nil)
                 string)
  :group 'agent-shell-attention)

(defcustom agent-shell-attention-message-prefix "[agent-shell]"
  "Prefix used for minibuffer notifications."
  :type 'string
  :group 'agent-shell-attention)

;;; Notifications

(defun agent-shell-attention-notify-default (_buffer title body)
  "Send TITLE/BODY notification via `notifications-notify'."
  (when (require 'notifications nil t)
    (notifications-notify
     :app-name "agent-shell"
     :title title
     :body body)))

(defcustom agent-shell-attention-notify-function nil
  "Function used to emit optional notifications.

When non-nil, it is called with three arguments: BUFFER, TITLE, and
BODY.  Set this to nil to disable notifications entirely.

The helper `agent-shell-attention-notify-default' uses
`notifications-notify'."
  :type '(choice (const :tag "Disable notifications" nil)
                 (const :tag "Use built-in notifications"
                        agent-shell-attention-notify-default)
                 function))

;;; Display-buffer

(defun agent-shell-attention--window-state-has-buffer (state buffer)
  "Return non-nil when window-state STATE shows BUFFER."
  (when (and state (fboundp 'window-state-buffers))
    (condition-case nil
        (let ((buffers (window-state-buffers state)))
          (and buffers
               (or (memq buffer buffers)
                   (member (buffer-name buffer) buffers))))
      (error nil))))

(defun agent-shell-attention--tab-displays-buffer-p (tab buffer)
  "Return non-nil when TAB's window state shows BUFFER."
  (let ((state (alist-get 'ws tab)))
    (and state
         (agent-shell-attention--window-state-has-buffer state buffer))))

(defun agent-shell-attention-display-buffer-across-tabs (buffer _alist)
  "Display BUFFER by reusing an existing window across tabs in this frame.

Return the window that shows BUFFER, or nil to let other display actions
run.  If `tab-bar-mode' is enabled and BUFFER is visible in another tab
of the current frame, switch to that tab and reuse its window."
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'tab-bar-tabs)
             (fboundp 'tab-bar-select-tab))
    (let* ((frame (selected-frame))
           (tabs (condition-case nil
                     (tab-bar-tabs frame)
                   (wrong-number-of-arguments
                    (tab-bar-tabs))
                   (error nil)))
           (current (when (fboundp 'tab-bar--current-tab)
                      (tab-bar--current-tab)))
           (current-index (or (and current
                                   (cl-position current tabs :test #'equal))
                              0))
           (target-index (cl-position-if
                          (lambda (tab)
                            (agent-shell-attention--tab-displays-buffer-p
                             tab buffer))
                          tabs))
           (target-window nil))
      (when (numberp target-index)
        (cond
         ((= target-index current-index)
          (setq target-window (get-buffer-window buffer frame)))
         (t
          (condition-case nil
              (let ((inhibit-redisplay t))
                (tab-bar-select-tab (1+ target-index))
                (setq target-window (get-buffer-window buffer frame)))
            (error
             (setq target-window nil)))))
        (when (window-live-p target-window)
          target-window)))))

(defcustom agent-shell-attention-display-buffer-action
  '((display-buffer-reuse-window
     agent-shell-attention-display-buffer-across-tabs
     display-buffer-same-window))
  "Display action used by `agent-shell-attention-jump'.

This is passed as ACTION to `pop-to-buffer'.  The default prefers an
existing window showing the buffer in the current frame.  If
`tab-bar-mode' is enabled it will also look across tabs in the same
frame."
  :type 'sexp)

(defvar agent-shell-attention--pending (make-hash-table :test #'eq)
  "Table of `agent-shell-mode' buffers awaiting user input.")

(defvar agent-shell-attention--busy (make-hash-table :test #'eq)
  "Table of `agent-shell-mode' buffers with in-flight requests.
Values are integer reference counts.")

;;; Mode-line

(defcustom agent-shell-attention-render-function
  #'agent-shell-attention-render-pending
  "Function used to render the attention indicator.
Called with PENDING-COUNT and ACTIVE-COUNT.  Renderers may ignore the
second argument if they only care about pending buffers.

Return a string suitable for `format-mode-line', or nil/empty string
for no indicator."
  :type '(choice (const :tag "Pending only (AS:n)" agent-shell-attention-render-pending)
                 (const :tag "Pending and busy (AS:n/m)" agent-shell-attention-render-active)
                 function))

(defcustom agent-shell-attention-active-lighter " AS:%d/%d"
  "Mode-line template used when showing pending and active counts.

The first `%d' is replaced with the number of pending buffers; the second
`%d' is replaced with the number of buffers that are either pending or
currently busy.

Used by `agent-shell-attention-render-active'."
  :type 'string)

(defcustom agent-shell-attention-show-zeros nil
  "When non-nil, show the indicator even when counts are zero.
Applies to both pending-only and pending+active renderers."
  :type 'boolean)

(defcustom agent-shell-attention-jump-show-groups nil
  "When non-nil, group `prefix-arg' jump candidates by status.

Grouping relies on completion UIs honoring completion metadata
`group-function'.  When nil, candidates are still ordered with pending
buffers first and are annotated with status tags."
  :type 'boolean)

(defun agent-shell-attention--mode-line-list (value)
  "Return VALUE as a list suitable for mode-line variables."
  (cond
   ((null value) nil)
   ((listp value) value)
   (t (list value))))

(defun agent-shell-attention--call-renderer (pending-count active-count)
  "Call `agent-shell-attention-render-function' safely.
PENDING-COUNT and ACTIVE-COUNT are passed to the renderer."
  (condition-case err
      (funcall agent-shell-attention-render-function pending-count active-count)
    (error
     (message "agent-shell-attention render error: %s" err)
     "")))

(defun agent-shell-attention--indicator ()
  "Return the rendered indicator for pending agent shells."
  (let* ((entries (agent-shell-attention--pending-live-entries))
         (count (length entries))
         (active-count (agent-shell-attention--compute-active-count count)))
    (agent-shell-attention--call-renderer count active-count)))

(defvar agent-shell-attention-mode-line-construct
  '(:eval (agent-shell-attention--indicator))
  "Mode-line construct for `agent-shell-attention-mode'.")

(put 'agent-shell-attention-mode-line-construct 'risky-local-variable t)

(defvar agent-shell-attention--mode-line
  agent-shell-attention-mode-line-construct
  "Interned mode-line entry reused in `mode-line-misc-info'.")

(defun agent-shell-attention-tab-bar-format ()
  "Return a `tab-bar-format' entry that renders the indicator.
Useful when users want only this indicator in the tab bar instead of
mirroring the entire `global-mode-string'."
  `((global menu-item ,(format-mode-line
                        agent-shell-attention-mode-line-construct)
            ignore)))

(defcustom agent-shell-attention-indicator-location 'mode-line-misc-info
  "Where to install the attention indicator.

When set to `mode-line-misc-info', the indicator lives in
`mode-line-misc-info'.  When set to `global-mode-string', it lives in
`global-mode-string' (which some setups also show in the tab bar via
`tab-bar-format-global')."
  :type '(choice (const :tag "Mode line misc info" mode-line-misc-info)
                 (const :tag "Global mode string" global-mode-string))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (bound-and-true-p agent-shell-attention-mode)
           (agent-shell-attention--apply-indicator-location)))
  :group 'agent-shell-attention)

(defun agent-shell-attention--apply-indicator-location ()
  "Apply `agent-shell-attention-indicator-location' to the mode line."
  (let ((indicator agent-shell-attention--mode-line))
    (setq mode-line-misc-info
          (agent-shell-attention--mode-line-list mode-line-misc-info))
    (setq global-mode-string
          (agent-shell-attention--mode-line-list global-mode-string))
    (setq-default mode-line-misc-info
                  (agent-shell-attention--mode-line-list
                   (default-value 'mode-line-misc-info)))
    (setq-default global-mode-string
                  (agent-shell-attention--mode-line-list
                   (default-value 'global-mode-string)))
    ;; Remove from both lists first to avoid duplicates.
    (setq mode-line-misc-info (delq indicator mode-line-misc-info))
    (setq global-mode-string (delq indicator global-mode-string))
    (setq-default mode-line-misc-info
                  (delq indicator (default-value 'mode-line-misc-info)))
    (setq-default global-mode-string
                  (delq indicator (default-value 'global-mode-string)))
    (pcase agent-shell-attention-indicator-location
      ('mode-line-misc-info
       (unless (member indicator mode-line-misc-info)
         (setq mode-line-misc-info
               (append mode-line-misc-info (list indicator))))
       (unless (member indicator (default-value 'mode-line-misc-info))
         (setq-default mode-line-misc-info
                       (append (default-value 'mode-line-misc-info)
                               (list indicator)))))
      ('global-mode-string
       (unless (member indicator global-mode-string)
         (setq global-mode-string
               (append global-mode-string (list indicator))))
       (unless (member indicator (default-value 'global-mode-string))
         (setq-default global-mode-string
                       (append (default-value 'global-mode-string)
                               (list indicator))))))
    (force-mode-line-update t)))

;;; Pending buffer bookkeeping

(defun agent-shell-attention--pending-live-entries ()
  "Return alist of live buffers needing attention.
Each entry is of the form (BUFFER . ENTRY), where ENTRY is the value
stored in `agent-shell-attention--pending'.  This call also purges stale
entries for dead buffers."
  (let ((entries nil)
        (stale nil))
    (maphash (lambda (buffer entry)
               (if (buffer-live-p buffer)
                   (push (cons buffer entry) entries)
                 (push buffer stale)))
             agent-shell-attention--pending)
    (dolist (buffer stale)
      (remhash buffer agent-shell-attention--pending)
      (remhash buffer agent-shell-attention--busy))
    (nreverse entries)))

(defun agent-shell-attention--pending-entry-label (entry)
  "Return the descriptive label stored in ENTRY."
  (cond
   ((plistp entry) (plist-get entry :label))
   ((consp entry) (car entry))
   (t entry)))

(defun agent-shell-attention--pending-entry-timestamp (entry)
  "Return the timestamp stored in ENTRY or 0 if missing."
  (cond
   ((plistp entry) (or (plist-get entry :timestamp) 0.0))
   ((consp entry) (cdr entry))
   (t 0.0)))

(defun agent-shell-attention--busy-live-buffers ()
  "Return a list of live buffers currently marked busy.

This call also purges stale entries for dead buffers."
  (let ((buffers nil)
        (stale nil))
    (maphash (lambda (buffer count)
               (if (and (buffer-live-p buffer)
                        (integerp count)
                        (> count 0))
                   (push buffer buffers)
                 (push buffer stale)))
             agent-shell-attention--busy)
    (dolist (buffer stale)
      (remhash buffer agent-shell-attention--busy)
      (unless (buffer-live-p buffer)
        (remhash buffer agent-shell-attention--pending)))
    (nreverse buffers)))

(defun agent-shell-attention--compute-active-count (pending-count)
  "Return number of unique buffers that are pending or busy.x"
  (let ((count pending-count))
    (dolist (buffer (agent-shell-attention--busy-live-buffers))
      (unless (gethash buffer agent-shell-attention--pending)
        (setq count (1+ count))))
    count))

(defun agent-shell-attention--buffer-selected-p (buffer)
  "Return non-nil if BUFFER is currently shown in the selected window."
  (let ((window (selected-window)))
    (and (window-live-p window)
         (eq buffer (window-buffer window)))))

(defun agent-shell-attention--should-notify-buffer (buffer)
  "Return non-nil when BUFFER warrants notifications or lighter updates."
  (not (agent-shell-attention--buffer-selected-p buffer)))

(defun agent-shell-attention--awaiting-p (stop-reason)
  "Return non-nil when STOP-REASON should mark a buffer as awaiting input."
  (and stop-reason
       (member stop-reason agent-shell-attention-awaiting-stop-reasons)))

(defun agent-shell-attention--describe-stop (stop-reason)
  "Return human-readable text for STOP-REASON."
  (if (and stop-reason (fboundp 'agent-shell--stop-reason-description))
      (agent-shell--stop-reason-description stop-reason)
    (or stop-reason "Finished")))

(defun agent-shell-attention--extract-message (object)
  "Return best-effort message string from OBJECT."
  (cond
   ((stringp object) object)
   ((null object) nil)
   (t (or (ignore-errors (map-elt object 'message))
          (ignore-errors (map-elt object 'code))))))

(defun agent-shell-attention--maybe-notify (buffer title body)
  "Show notification for BUFFER with TITLE and BODY when configured."
  (when agent-shell-attention-notify-function
    (condition-case err
        (funcall agent-shell-attention-notify-function buffer title body)
      (error
       (message "agent-shell-attention notification error: %s" err)))))

(defun agent-shell-attention--message (buffer text)
  "Display TEXT as a minibuffer notification for BUFFER."
  (when (and (buffer-live-p buffer)
             (agent-shell-attention--should-notify-buffer buffer))
    (let* ((name (buffer-name buffer))
           (title (format "%s agent" name)))
      (message "%s %s: %s" agent-shell-attention-message-prefix name text)
      (agent-shell-attention--maybe-notify buffer title text))))

(defun agent-shell-attention--on-buffer-killed ()
  "Buffer-local hook to keep tracking tables tidy."
  (let ((buffer (current-buffer))
        (changed nil))
    (when (gethash buffer agent-shell-attention--pending)
      (remhash buffer agent-shell-attention--pending)
      (setq changed t))
    (when (gethash buffer agent-shell-attention--busy)
      (remhash buffer agent-shell-attention--busy)
      (setq changed t))
    (when changed
      (force-mode-line-update t))))

(defun agent-shell-attention--clear-buffer (buffer)
  "Remove BUFFER from the pending table and refresh the mode line."
  (let ((had-pending (gethash buffer agent-shell-attention--pending)))
    (when had-pending
      (remhash buffer agent-shell-attention--pending))
    (when (and had-pending
               (buffer-live-p buffer)
               (not (gethash buffer agent-shell-attention--pending))
               (not (gethash buffer agent-shell-attention--busy)))
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook
                     #'agent-shell-attention--on-buffer-killed t)))
    (when had-pending
      (force-mode-line-update t))))

(defun agent-shell-attention--maybe-clear-current ()
  "Clear the currently selected buffer if it no longer needs attention."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((permissions (agent-shell-attention--permission-pending-p (current-buffer))))
      ;; Clear as soon as no outstanding permissions remain.
      (unless permissions
        (agent-shell-attention--clear-buffer (current-buffer))))))

;;; Navigation UI

(defun agent-shell-attention--entry-label (buffer entry)
  "Return label for BUFFER with ENTRY text."
  (format "%s — %s"
          (buffer-name buffer)
          (agent-shell-attention--pending-entry-label entry)))

(defun agent-shell-attention--entry-older-p (entry-a entry-b)
  "Return non-nil when ENTRY-A is older than ENTRY-B."
  (< (agent-shell-attention--pending-entry-timestamp (cdr entry-a))
     (agent-shell-attention--pending-entry-timestamp (cdr entry-b))))

(defun agent-shell-attention--active-entry-records ()
  "Return ordered candidate records for active agent-shell buffers.

Each record is a list (BUFFER ENTRY STATUS), where STATUS is either
`pending' (awaiting user response) or `busy' (in-flight request)."
  (let* ((pending (copy-sequence (agent-shell-attention--pending-live-entries)))
         (pending-sorted (sort pending #'agent-shell-attention--entry-older-p))
         (busy-only (cl-remove-if
                     (lambda (buffer)
                       (gethash buffer agent-shell-attention--pending))
                     (agent-shell-attention--busy-live-buffers)))
         (busy-sorted (sort busy-only
                            (lambda (a b)
                              (string-lessp (buffer-name a) (buffer-name b)))))
         (records nil))
    (dolist (entry pending-sorted)
      (push (list (car entry) (cdr entry) 'pending) records))
    (dolist (buffer busy-sorted)
      (push (list buffer nil 'busy) records))
    (nreverse records)))

(defun agent-shell-attention--unique-candidates-with-status (records)
  "Return an alist of (DISPLAY . (BUFFER . STATUS)) built from RECORDS.

RECORDS is a list of (BUFFER ENTRY STATUS).  DISPLAY strings are made
unique if multiple entries would otherwise collide."
  (let ((seen (make-hash-table :test #'equal))
        (candidates nil))
    (dolist (record records)
      (pcase-let ((`(,buffer ,entry ,status) record))
        (let* ((label (pcase status
                        ('busy (buffer-name buffer))
                        (_ (agent-shell-attention--entry-label buffer entry))))
               (count (gethash label seen 0))
               (unique (if (zerop count)
                           label
                         (format "%s <%d>" label (1+ count)))))
          (puthash label (1+ count) seen)
          (push (cons unique (cons buffer status)) candidates))))
    (nreverse candidates)))

(defun agent-shell-attention--completion-tag (status)
  "Return a visually distinguished completion tag for STATUS."
  (pcase status
    ('pending (propertize "awaiting" 'face 'success))
    ('busy (propertize "busy" 'face 'shadow))
    (_ (propertize "active" 'face 'shadow))))

(defun agent-shell-attention--completion-annotation (display status-table)
  "Return completion annotation string for DISPLAY using STATUS-TABLE."
  (let ((status (gethash display status-table)))
    (concat " [" (agent-shell-attention--completion-tag status) "]")))

(defun agent-shell-attention--completion-table (candidates)
  "Return completion table for CANDIDATES with ordering and annotations.

CANDIDATES is an alist of (DISPLAY . (BUFFER . STATUS))."
  (let* ((displays (mapcar #'car candidates))
         (order (let ((table (make-hash-table :test #'equal))
                      (index 0))
                  (dolist (display displays)
                    (puthash display index table)
                    (setq index (1+ index)))
                  table))
         (status-table (let ((table (make-hash-table :test #'equal)))
                          (dolist (candidate candidates)
                            (puthash (car candidate) (cddr candidate) table))
                          table))
         (annotation (lambda (display)
                       (agent-shell-attention--completion-annotation display status-table)))
         (affixation (lambda (completions)
                       (mapcar (lambda (completion)
                                 (let* ((display completion))
                                   (list display ""
                                         (agent-shell-attention--completion-annotation
                                          display status-table))))
                               completions)))
         (group (lambda (display transform)
                  (if transform
                      display
                    (pcase (gethash display status-table)
                      ('pending "Awaiting response")
                      ('busy "Busy")
                      (_ "Active")))))
         (sorter (lambda (completions)
                   (sort (copy-sequence completions)
                         (lambda (a b)
                           (let* ((ia (gethash a order most-positive-fixnum))
                                  (ib (gethash b order most-positive-fixnum)))
                             (if (/= ia ib)
                                 (< ia ib)
                               (string-lessp a b))))))))
    (completion-table-with-metadata
     displays
     (append
      `((category . agent-shell-attention)
        (display-sort-function . ,sorter)
        (annotation-function . ,annotation)
        (affixation-function . ,affixation))
      (when agent-shell-attention-jump-show-groups
        `((group-function . ,group)))))))

(defun agent-shell-attention--jump-to-buffer (buffer)
  "Switch to BUFFER, clearing pending state when appropriate."
  (when (buffer-live-p buffer)
    (unless (agent-shell-attention--permission-pending-p buffer)
      (agent-shell-attention--clear-buffer buffer))
    (pop-to-buffer buffer agent-shell-attention-display-buffer-action)))

(defun agent-shell-attention--build-menu (entries)
  "Create popup menu for ENTRIES.
ENTRIES is an alist of (BUFFER . ENTRY)."
  (easy-menu-create-menu
   "Agent Shell Attention"
   (mapcar (lambda (entry)
             (vector (agent-shell-attention--entry-label (car entry)
                                                         (cdr entry))
                     `(agent-shell-attention--jump-to-buffer ',(car entry))
                     t))
           entries)))

;;;###autoload
(defun agent-shell-attention-jump (&optional prompt)
  "Jump to an agent-shell buffer awaiting your response.

With PROMPT (prefix argument), use `completing-read' to select from all
active agent-shell buffers, ordered with pending buffers first and
busy buffers after.

Without PROMPT, jump to the oldest pending buffer."
  (interactive "P")
  (if prompt
      (let* ((records (agent-shell-attention--active-entry-records)))
        (if (null records)
            (message "No active agent-shell buffers")
          (let* ((candidates (agent-shell-attention--unique-candidates-with-status records))
                 (table (agent-shell-attention--completion-table candidates))
                 (choice (completing-read "Agent shell: " table nil t))
                 (entry (assoc choice candidates))
                 (buffer (and entry (cadr entry))))
            (when (buffer-live-p buffer)
              (agent-shell-attention--jump-to-buffer buffer)))))
    (let* ((entries (copy-sequence (agent-shell-attention--pending-live-entries)))
           (sorted (sort entries #'agent-shell-attention--entry-older-p)))
      (if (null sorted)
          (message "No agent-shell buffers awaiting replies")
        (agent-shell-attention--jump-to-buffer (caar sorted))))))

(defun agent-shell-attention-open-menu (&optional event)
  "Show pending agent-shell buffers in a popup menu.
EVENT is the mouse event used to trigger the menu."
  (interactive (list last-nonmenu-event))
  (let ((entries (agent-shell-attention--pending-live-entries)))
    (if (null entries)
        (message "No agent-shell buffers awaiting replies")
      (popup-menu (agent-shell-attention--build-menu entries) event))))

(defvar agent-shell-attention--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'agent-shell-attention-open-menu)
    (define-key map [mode-line mouse-1] #'ignore)
    (define-key map [mode-line down-mouse-3] #'agent-shell-attention-open-menu)
    map)
  "Keymap used to make the mode-line indicator clickable.")

(defun agent-shell-attention-render-pending (pending-count _active-count)
  "Render only PENDING-COUNT using `agent-shell-attention-lighter'.

ACTIVE-COUNT is accepted for API compatibility but ignored."
  (if (or (null agent-shell-attention-lighter)
          (and (zerop pending-count)
               (not agent-shell-attention-show-zeros)))
      ""
    (let ((label (format agent-shell-attention-lighter pending-count)))
      (propertize label
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: list agent-shell buffers awaiting reply"
                  'local-map agent-shell-attention--mode-line-map))))

(defun agent-shell-attention-render-active (pending-count active-count)
  "Render pending PENDING-COUNT plus ACTIVE-COUNT with the active lighter."
  (let* ((active (or active-count pending-count))
         (nonzero (or (> pending-count 0) (> active 0)))
         (show (and agent-shell-attention-active-lighter
                    (or nonzero agent-shell-attention-show-zeros))))
    (when show
      (let ((label (format agent-shell-attention-active-lighter pending-count active)))
        (propertize label
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: list agent-shell buffers awaiting reply"
                    'local-map agent-shell-attention--mode-line-map)))))

;;; Agent-shell integration

(defun agent-shell-attention--ensure-mode-line-entry ()
  "Ensure our indicator is present in both current and default mode line."
  (agent-shell-attention--apply-indicator-location))

(defun agent-shell-attention--mark-busy (buffer)
  "Mark BUFFER as busy (with an in-flight request)."
  (when (buffer-live-p buffer)
    (let ((count (gethash buffer agent-shell-attention--busy 0)))
      (puthash buffer (1+ (if (and (integerp count) (> count 0)) count 0))
               agent-shell-attention--busy))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                #'agent-shell-attention--on-buffer-killed nil t))
    (agent-shell-attention--ensure-mode-line-entry)
    (force-mode-line-update t)))

(defun agent-shell-attention--clear-busy (buffer)
  "Clear one busy mark for BUFFER, removing it when the refcount hits 0."
  (when (gethash buffer agent-shell-attention--busy)
    (let* ((count (gethash buffer agent-shell-attention--busy 0))
           (new-count (1- (if (integerp count) count 1))))
      (if (> new-count 0)
          (puthash buffer new-count agent-shell-attention--busy)
        (remhash buffer agent-shell-attention--busy)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (unless (gethash buffer agent-shell-attention--pending)
              (remove-hook 'kill-buffer-hook
                           #'agent-shell-attention--on-buffer-killed t)))))))
  (force-mode-line-update t))

(defun agent-shell-attention--tool-call-awaits-permission-p (tool-call)
  "Return non-nil when TOOL-CALL still requires a user decision."
  (when tool-call
    (let ((permission-id (map-elt tool-call :permission-request-id))
          (status (map-elt tool-call :status)))
      (and permission-id
           (or (null status)
               (equal status "pending"))))))

(defun agent-shell-attention--permission-pending-p (buffer)
  "Return non-nil when BUFFER still has outstanding permission prompts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (let ((state (and (boundp 'agent-shell--state)
                          agent-shell--state)))
          (when state
            (let ((tool-calls (map-elt state :tool-calls)))
              (when tool-calls
                (condition-case nil
                    (catch 'pending
                      (map-do (lambda (_tool-call-id tool-call)
                                (when (agent-shell-attention--tool-call-awaits-permission-p tool-call)
                                  (throw 'pending t)))
                              tool-calls)
                      nil)
                  (error nil))))))))))

(cl-defun agent-shell-attention--mark-buffer (buffer label &key force)
  "Mark BUFFER as waiting for user input with LABEL description.

When FORCE is non-nil, mark the buffer even if it's currently selected."
  (when (buffer-live-p buffer)
    (let ((should-track (or force
                            (agent-shell-attention--should-notify-buffer buffer))))
      (when should-track
        (unless (gethash buffer agent-shell-attention--pending)
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook
                      #'agent-shell-attention--on-buffer-killed nil t)))
        (puthash buffer (cons label (float-time)) agent-shell-attention--pending)
        (agent-shell-attention--ensure-mode-line-entry)
        (force-mode-line-update t)))))

(defun agent-shell-attention--handle-success (buffer response)
  "Process successful agent RESPONSE for BUFFER."
  (let* ((stop-reason (map-elt response 'stopReason))
         (description (agent-shell-attention--describe-stop stop-reason))
         (awaiting (agent-shell-attention--awaiting-p stop-reason)))
    (when (agent-shell-attention--should-notify-buffer buffer)
      (agent-shell-attention--message buffer description))
    (when awaiting
      (agent-shell-attention--mark-buffer buffer description))))

(defun agent-shell-attention--handle-failure (buffer error raw-message)
  "Handle agent failure for BUFFER using ERROR and RAW-MESSAGE."
  (let ((details (or (agent-shell-attention--extract-message error)
                     (agent-shell-attention--extract-message raw-message)
                     "Request failed")))
    (when (agent-shell-attention--should-notify-buffer buffer)
      (agent-shell-attention--message buffer details))))

(defun agent-shell-attention--request-label (request)
  "Return descriptive label for REQUEST when it needs user input."
  (pcase (map-elt request 'method)
    ("session/request_permission"
     (let* ((tool-call (ignore-errors
                         (map-nested-elt request '(params toolCall))))
            (title (or (and tool-call (map-elt tool-call 'title))
                       "Permission required"))
            (kind (and tool-call (map-elt tool-call 'kind))))
       (concat "Permission: " title
               (if kind (format " (%s)" kind) ""))))
    (_ nil)))

(defun agent-shell-attention--handle-request (state request)
  "Record that REQUEST in STATE is awaiting a user response."
  (let ((buffer (and state (map-elt state :buffer))))
    (when buffer
      (let ((label (agent-shell-attention--request-label request)))
        (when label
          (when (agent-shell-attention--should-notify-buffer buffer)
            (agent-shell-attention--message buffer label))
          (agent-shell-attention--mark-buffer buffer label :force t))))))

(defun agent-shell-attention--decorate-request (buffer request-args)
  "Wrap REQUEST-ARGS with our completion hooks for BUFFER."
  (let* ((on-success (plist-get request-args :on-success))
         (on-failure (plist-get request-args :on-failure)))
    (setq request-args
          (plist-put request-args :on-success
                     (lambda (response)
                       (agent-shell-attention--clear-busy buffer)
                       (agent-shell-attention--handle-success buffer response)
                       (when on-success
                         (funcall on-success response)))))
    (setq request-args
          (plist-put request-args :on-failure
                     (lambda (error raw-message)
                       (agent-shell-attention--clear-busy buffer)
                       (agent-shell-attention--handle-failure
                        buffer error raw-message)
                       (when on-failure
                         (funcall on-failure error raw-message)))))
    request-args))

(defun agent-shell-attention--around-send-command (orig-fn &rest args)
  "Advice around `agent-shell--send-command' ORIG-FN with ARGS.

Intercept completions to track buffers awaiting user input."
  (let ((shell (plist-get args :shell)))
    (if (not (and shell (map-elt shell :buffer)))
        (apply orig-fn args)
      (let ((buffer (map-elt shell :buffer)))
        (agent-shell-attention--clear-buffer buffer)
        (agent-shell-attention--mark-busy buffer)
        (condition-case err
            (cl-letf* ((orig-request (symbol-function #'acp-send-request))
                       ((symbol-function #'acp-send-request)
                        (lambda (&rest request-args)
                          (apply orig-request
                                 (agent-shell-attention--decorate-request
                                  buffer
                                  request-args)))))
              (apply orig-fn args))
          (error
           (agent-shell-attention--clear-busy buffer)
           (signal (car err) (cdr err))))))))

(defun agent-shell-attention--around-on-request (orig-fn &rest args)
  "Advice around `agent-shell--on-request' ORIG-FN with ARGS.

Catch permission prompts and mark buffers awaiting input."
  (let ((state (plist-get args :state))
        (request (plist-get args :request)))
    (when (and state request)
      (agent-shell-attention--handle-request state request))
    (apply orig-fn args)))

(cl-defun agent-shell-attention--after-permission-response (&rest args)
  "Clear permission-pending marker after user responds.
ARGS are provided by `agent-shell--send-permission-response'."
  (let ((state (plist-get args :state)))
    (when state
      (let ((buffer (map-elt state :buffer)))
        (when (buffer-live-p buffer)
          (unless (agent-shell-attention--permission-pending-p buffer)
            (agent-shell-attention--clear-buffer buffer))
          (force-mode-line-update t))))))

;;; Minor mode

(defun agent-shell-attention--enable ()
  "Enable hooks and mode-line integration."
  (require 'agent-shell)
  (agent-shell-attention--ensure-mode-line-entry)
  (add-hook 'buffer-list-update-hook
            #'agent-shell-attention--maybe-clear-current)
  (advice-add #'agent-shell--send-command :around
              #'agent-shell-attention--around-send-command)
  (advice-add #'agent-shell--on-request :around
              #'agent-shell-attention--around-on-request)
  (advice-add #'agent-shell--send-permission-response :after
              #'agent-shell-attention--after-permission-response)
  (force-mode-line-update t))

(defun agent-shell-attention--disable ()
  "Disable hooks and clear all pending/busy markers."
  ;; Remove from both possible locations.
  (let ((indicator agent-shell-attention--mode-line))
    (setq mode-line-misc-info
          (agent-shell-attention--mode-line-list mode-line-misc-info))
    (setq global-mode-string
          (agent-shell-attention--mode-line-list global-mode-string))
    (setq-default mode-line-misc-info
                  (agent-shell-attention--mode-line-list
                   (default-value 'mode-line-misc-info)))
    (setq-default global-mode-string
                  (agent-shell-attention--mode-line-list
                   (default-value 'global-mode-string)))
    (setq mode-line-misc-info (delq indicator mode-line-misc-info))
    (setq global-mode-string (delq indicator global-mode-string))
    (setq-default mode-line-misc-info
                  (delq indicator (default-value 'mode-line-misc-info)))
    (setq-default global-mode-string
                  (delq indicator (default-value 'global-mode-string))))
  (remove-hook 'buffer-list-update-hook
               #'agent-shell-attention--maybe-clear-current)
  (advice-remove #'agent-shell--send-command
                 #'agent-shell-attention--around-send-command)
  (advice-remove #'agent-shell--on-request
                 #'agent-shell-attention--around-on-request)
  (advice-remove #'agent-shell--send-permission-response
                 #'agent-shell-attention--after-permission-response)
  (maphash (lambda (buffer _)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (remove-hook 'kill-buffer-hook
                              #'agent-shell-attention--on-buffer-killed t))))
           agent-shell-attention--pending)
  (maphash (lambda (buffer _)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (remove-hook 'kill-buffer-hook
                              #'agent-shell-attention--on-buffer-killed t))))
           agent-shell-attention--busy)
  (clrhash agent-shell-attention--pending)
  (clrhash agent-shell-attention--busy)
  (force-mode-line-update t))

(defvar agent-shell-attention-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used while `agent-shell-attention-mode' is active.")

;;;###autoload
(define-minor-mode agent-shell-attention-mode
  "Global notifications for agent-shell buffers.
When enabled, display a tally of agent shells needing user input and
emit minibuffer notifications whenever an agent hands back control."
  :global t
  :group 'agent-shell-attention
  :lighter nil
  :keymap agent-shell-attention-mode-map
  (if agent-shell-attention-mode
      (agent-shell-attention--enable)
    (agent-shell-attention--disable)))

(provide 'agent-shell-attention)

;;; agent-shell-attention.el ends here
