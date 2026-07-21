;;; mode-line-notify.el --- lightweight notifications in the mode line -*- lexical-binding: t; -*-

(defvar mode-line-notify--data nil
  "List of pending notifications.
Each notification is a plist with :message and :buffer.")

(defun mode-line-notify--display-buffer-in-existing-tab (buffer alist)
  "Display BUFFER by reusing a tab that already shows it.
Like `display-buffer-in-tab', but never creates a new tab, so
`display-buffer' can fall through to the next action function."
  (when (and (fboundp 'tab-bar-get-buffer-tab)
             (tab-bar-get-buffer-tab buffer (alist-get 'reusable-frames alist)))
    (display-buffer-in-tab buffer alist)))

(defun mode-line-notify--goto-buffer (buffer)
  "Go to BUFFER, reusing any window, tab, or frame that already shows it.
Uses `display-buffer-reuse-window' across all frames, then a tab-bar
tab already containing BUFFER, and falls back to displaying it in the
selected window."
  (if (buffer-live-p (get-buffer buffer))
      (pop-to-buffer buffer
                     '((display-buffer-reuse-window
                        mode-line-notify--display-buffer-in-existing-tab
                        display-buffer-same-window)
                       (reusable-frames . 0)))
    (message "Notification buffer no longer exists")))

(defun mode-line-notify-open ()
  "Dismiss the first pending notification and go to its buffer."
  (interactive)
  (when mode-line-notify--data
    (let ((buffer (plist-get (car mode-line-notify--data) :buffer)))
      (setq mode-line-notify--data (cdr mode-line-notify--data))
      (force-mode-line-update t)
      (when buffer
        (mode-line-notify--goto-buffer buffer)))))

(defvar mode-line-notify-indicator
  '(:eval (when mode-line-notify--data
            (propertize (format " %s! " (length mode-line-notify--data))
                        'face 'error
                        'mouse-face 'mode-line-highlight
                        'help-echo (plist-get (car mode-line-notify--data) :title)
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] 'mode-line-notify-open)
                                     map)))))

(defun mode-line-notify-send (&rest params)
  "Send a notification to the mode line."
  (interactive)
  ;; ignore already displayed windows
  (unless (or
	   (and (plist-get params :buffer) (get-buffer-window (plist-get params :buffer) 'visible))
           (seq-find (lambda (data)
                       (equal (plist-get data :title) (plist-get params :title)))
                     mode-line-notify--data))
    (setq mode-line-notify--data (append mode-line-notify--data (list params)))
    (force-mode-line-update t)))

(defun mode-line-buffer-change (_frame)
  (setq mode-line-notify--data
	(seq-remove (lambda (data) (and (plist-get data :buffer)
					(get-buffer-window (plist-get data :buffer) 'visible))) mode-line-notify--data)))

(define-minor-mode mode-line-notify
  "Display notification indicator in the mode line."
  :global t
  :group 'mode-line

  (if mode-line-notify
      (progn
        (setq-default mode-line-misc-info
                      (push '(:eval mode-line-notify-indicator) mode-line-misc-info))
        (add-hook 'window-buffer-change-functions 'mode-line-buffer-change))
    (progn
      (setq-default mode-line-misc-info
                    (delete '(:eval mode-line-notify-indicator) mode-line-misc-info))
      (remove-hook 'window-buffer-change-functions 'mode-line-buffer-change))))

(provide 'mode-line-notify)
