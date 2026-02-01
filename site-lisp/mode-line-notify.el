;;; mode-line-notify.el --- lightweight notifications in the mode line -*- lexical-binding: t; -*-

(defvar mode-line-notify--data nil
  "List of pending notifications.
Each notification is a plist with :message and :buffer.")

(defvar mode-line-notify-indicator
  '(:eval (when mode-line-notify--data
            (propertize (format " %s! " (length mode-line-notify--data))
                        'face '(:foreground "red" :weight bold)
                        'mouse-face 'mode-line-highlight
                        'help-echo (plist-get (car mode-line-notify--data) :title)
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1]
                                                 (lambda ()
                                                   (interactive)
                                                   (when mode-line-notify--data
                                                     (let ((buffer (plist-get (car mode-line-notify--data) :buffer)))
                                                       (setq mode-line-notify--data (cdr mode-line-notify--data))
                                                       (force-mode-line-update t)
                                                       (when buffer
                                                         (switch-to-buffer buffer))))))
                                     map)))))

(defun mode-line-notify-send (&rest params)
  "Send a notification to the mode line."
  (interactive)
  (unless (or (equal (current-buffer) (plist-get params :buffer))
              (seq-find (lambda (data)
                          (equal (plist-get data :title) (plist-get params :title)))
                        mode-line-notify--data))
    (setq mode-line-notify--data (append mode-line-notify--data (list params)))
    (force-mode-line-update t)))

(defun mode-line-buffer-change (_frame)
  (seq-remove (lambda (data) (equal (plist-get data :buffer) (current-buffer))) mode-line-notify--data))

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
