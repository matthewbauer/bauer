;;; ghostel-toggle.el --- Toggle between a Ghostel terminal and other buffers  -*- lexical-binding: t; -*-

;; Author: ported from vterm-toggle (jixiuf <jixiuf@qq.com>)
;; Keywords: ghostel ghostty terminals
;; Version: 0.0.1
;; URL: https://github.com/jixiuf/vterm-toggle
;; Package-Requires: ((emacs "27.1") (ghostel "0.16"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;;
;; A port of `vterm-toggle' (https://github.com/jixiuf/vterm-toggle) to the
;; Ghostel terminal.  Provides `ghostel-toggle', which toggles between a
;; Ghostel terminal buffer and whatever buffer you are editing, with minimum
;; distortion of your window configuration:
;;
;; o Starts a Ghostel terminal if none exists.
;; o When done in the terminal you are returned to the window configuration
;;   you had before you toggled to it.
;; o `ghostel-toggle-cd' additionally issues a `cd' to the directory of the
;;   buffer you toggled from (even over TRAMP, when the host matches).
;;
;; Where the terminal window is placed (e.g. a bottom popup) is delegated to
;; `display-buffer', so this composes with packages like popper or a custom
;; `display-buffer-alist' rather than hard-coding a window layout.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'ghostel)

;;
;; Customization
;;

(defgroup ghostel-toggle nil
  "Toggle between a Ghostel terminal and other buffers."
  :group 'ghostel)

(defcustom ghostel-toggle-show-hook nil
  "Hook run after switching to a Ghostel buffer."
  :group 'ghostel-toggle
  :type 'hook)

(defcustom ghostel-toggle-hide-hook nil
  "Hook run before hiding a Ghostel buffer."
  :group 'ghostel-toggle
  :type 'hook)

(defcustom ghostel-toggle-fullscreen-p nil
  "Open the Ghostel buffer fullscreen (deleting other windows) or not."
  :group 'ghostel-toggle
  :type 'boolean)

(defcustom ghostel-toggle-scope nil
  "Scope of the terminal buffers `ghostel-toggle' considers.
`project'   limit the scope to the current project.
`frame'     limit the scope to the current frame.
`dedicated' use a single dedicated Ghostel buffer."
  :group 'ghostel-toggle
  :type '(radio
          (const :tag "all" nil)
          (const :tag "project" project)
          (const :tag "frame" frame)
          (const :tag "dedicated" dedicated)))

(defcustom ghostel-toggle-project-root t
  "Create a new Ghostel buffer at the project root directory.
Only has an effect when the effective scope is `project' (see
`ghostel-toggle-scoped' and `ghostel-toggle-scope')."
  :group 'ghostel-toggle
  :type 'boolean)

(defcustom ghostel-toggle-scoped nil
  "When non-nil, scope `ghostel-toggle' to the current project.

This is the single switch most users want: enabling it makes the toggle
commands behave as if `ghostel-toggle-scope' were `project', and in addition
makes show/hide project-aware.  Concretely:

- Toggling reuses the running terminal whose project root matches the
  current buffer's project, and creates one only when none exists.
- A visible terminal belonging to another project is ignored, so a toggle
  issued from project B never hides or reuses project A's terminal.

When nil, `ghostel-toggle-scope' is honored as-is.  When non-nil it takes
precedence over `ghostel-toggle-scope' (the effective scope is `project')."
  :group 'ghostel-toggle
  :type 'boolean)

(defcustom ghostel-toggle-cd-auto-create-buffer t
  "When non-nil, `ghostel-toggle-cd' may create a new Ghostel buffer.
Otherwise it reuses the most recent suitable Ghostel buffer."
  :group 'ghostel-toggle
  :type 'boolean)

(defcustom ghostel-toggle-reset-window-configuration-after-exit 'kill-window-only
  "Whether to reset the window configuration after a Ghostel buffer is killed."
  :group 'ghostel-toggle
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Reset window configuration after exit" t)
          (const :tag "Kill window only" kill-window-only)))

(defcustom ghostel-toggle-hide-method 'delete-window
  "How to hide the Ghostel buffer when toggling it away."
  :group 'ghostel-toggle
  :type '(choice
          (const :tag "Focus another window without closing the terminal" nil)
          (const :tag "Reset window configuration" reset-window-configuration)
          (const :tag "Bury all Ghostel buffers" bury-all-ghostel-buffer)
          (const :tag "Quit window" quit-window)
          (const :tag "Delete window" delete-window)))

(defcustom ghostel-toggle-display-action
  '((display-buffer-reuse-window display-buffer-at-bottom)
    (window-height . 0.3))
  "Default `display-buffer' ACTION used when showing a new Ghostel buffer.
This is only the fallback: any matching `display-buffer-alist' entry
\(for example one installed by popper for \"*ghostel*\") still takes
precedence, since `display-buffer-alist' is consulted before this action."
  :group 'ghostel-toggle
  :type 'sexp)

(defcustom ghostel-toggle-togglable-buffer-functions nil
  "List of predicates deciding whether a buffer is togglable.
Each function takes a buffer and returns non-nil if that buffer should be
managed by `ghostel-toggle'.  All must return non-nil (in addition to the
`ghostel-mode' check) for the buffer to be considered togglable."
  :group 'ghostel-toggle
  :type '(repeat function))

;;
;; State
;;

(defvar ghostel-toggle--window-configuration nil
  "Window configuration saved before a Ghostel buffer was shown.")

(defvar ghostel-toggle--dedicated-buffer nil
  "The dedicated Ghostel buffer, when `ghostel-toggle-scope' is `dedicated'.")

(defvar-local ghostel-toggle--dedicated-p nil
  "Non-nil in a buffer that is the dedicated Ghostel buffer.")

(defvar ghostel-toggle--buffer-list nil
  "The list of Ghostel buffers managed by `ghostel-toggle', in creation order.")

(defvar-local ghostel-toggle--cd-cmd nil
  "The pending \"cd\" command string for the current Ghostel buffer.")

;;
;; Predicates
;;

(defun ghostel-toggle--effective-scope ()
  "Return the scope `ghostel-toggle' should use right now.
`ghostel-toggle-scoped' is a convenience override: when it is non-nil the
effective scope is `project'; otherwise `ghostel-toggle-scope' is returned
unchanged.  All scope-sensitive code consults this instead of reading the
variables directly, so the two options stay in sync."
  (if ghostel-toggle-scoped 'project ghostel-toggle-scope))

(defun ghostel-toggle-togglable-buffer-p (buffer)
  "Return non-nil if BUFFER is a Ghostel buffer managed by `ghostel-toggle'."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                'ghostel-mode)
       (cl-every (lambda (fn) (funcall fn buffer))
                 ghostel-toggle-togglable-buffer-functions)))

(defun ghostel-toggle--at-prompt-p (&optional _buffer)
  "Return non-nil when the terminal is believed to be at a shell prompt.
Ghostel does not expose a public prompt-position API, so this is a
best-effort predicate that optimistically returns t."
  t)

;;
;; Toggle commands
;;

;;;###autoload
(defun ghostel-toggle (&optional args)
  "Toggle between a Ghostel terminal and the buffer you are editing.
With \\[universal-argument] ARGS while in a terminal, switch to the
main \"*ghostel*\" buffer; with \\[universal-argument] elsewhere, show
the terminal with the opposite of `ghostel-toggle-fullscreen-p'."
  (interactive "P")
  (cond
   ((or (ghostel-toggle-togglable-buffer-p (current-buffer))
        (and (ghostel-toggle--get-window)
             ghostel-toggle-hide-method))
    (if (equal (prefix-numeric-value args) 1)
        (ghostel-toggle-hide)
      (ghostel)))
   ((equal (prefix-numeric-value args) 1)
    (ghostel-toggle-show))
   ((equal (prefix-numeric-value args) 4)
    (let ((ghostel-toggle-fullscreen-p (not ghostel-toggle-fullscreen-p)))
      (ghostel-toggle-show)))))

;;;###autoload
(defun ghostel-toggle-cd (&optional args)
  "Toggle the Ghostel terminal and `cd' to the current buffer's directory.
ARGS behaves as in `ghostel-toggle'."
  (interactive "P")
  (cond
   ((or (ghostel-toggle-togglable-buffer-p (current-buffer))
        (and (ghostel-toggle--get-window)
             ghostel-toggle-hide-method))
    (if (equal (prefix-numeric-value args) 1)
        (ghostel-toggle-hide)
      (ghostel-toggle-show t)))
   ((equal (prefix-numeric-value args) 1)
    (ghostel-toggle-show t))
   ((equal (prefix-numeric-value args) 4)
    (let ((ghostel-toggle-fullscreen-p (not ghostel-toggle-fullscreen-p)))
      (ghostel-toggle-show t)))))

(defun ghostel-toggle-hide (&optional _args)
  "Hide the Ghostel buffer according to `ghostel-toggle-hide-method'."
  (interactive "P")
  (or (ghostel-toggle-togglable-buffer-p (current-buffer))
      (select-window (ghostel-toggle--get-window)))
  (run-hooks 'ghostel-toggle-hide-hook)
  (cond
   ((eq ghostel-toggle-hide-method 'reset-window-configuration)
    (when ghostel-toggle--window-configuration
      (set-window-configuration ghostel-toggle--window-configuration)))
   ((eq ghostel-toggle-hide-method 'bury-all-ghostel-buffer)
    (ghostel-toggle--bury-all))
   ((eq ghostel-toggle-hide-method 'quit-window)
    (quit-window))
   ((eq ghostel-toggle-hide-method 'delete-window)
    (if (window-deletable-p)
        (delete-window)
      (ghostel-toggle--bury-all)))
   ((not ghostel-toggle-hide-method)
    (let ((buf (ghostel-toggle--recent-other-buffer)))
      (when buf
        (if (get-buffer-window buf)
            (select-window (get-buffer-window buf))
          (if ghostel-toggle-fullscreen-p
              (switch-to-buffer buf)
            (switch-to-buffer-other-window buf))))))))

(defun ghostel-toggle--get-window ()
  "Return a live window currently showing a togglable Ghostel buffer, or nil.
When the effective scope is `project' (see `ghostel-toggle-scoped'), only
windows whose buffer belongs to the current project are considered, so a
terminal from another project is neither hidden nor reused from here."
  (let ((root (and (eq (ghostel-toggle--effective-scope) 'project)
                   (ghostel-toggle--project-root))))
    (cl-find-if
     (lambda (w)
       (let ((buf (window-buffer w)))
         (and (ghostel-toggle-togglable-buffer-p buf)
              (or (null root)
                  (equal (with-current-buffer buf (ghostel-toggle--project-root))
                         root)))))
     (window-list))))

(defun ghostel-toggle--bury-all ()
  "Bury all Ghostel buffers, in order."
  (dolist (buf (buffer-list))
    (when (ghostel-toggle-togglable-buffer-p buf)
      (bury-buffer buf))))

;;
;; Show
;;

(defun ghostel-toggle-cd-show (&optional args)
  "Switch to an idle Ghostel buffer and insert a cd command, or create one.
Optional argument ARGS inverts the cd behavior."
  (interactive "P")
  (ghostel-toggle-show (not args)))

(defun ghostel-toggle-show (&optional make-cd)
  "Show a Ghostel terminal buffer.
With non-nil MAKE-CD, also `cd' to the current buffer's directory."
  (interactive "P")
  (let* ((shell-buffer (ghostel-toggle--get-buffer
                        make-cd (not ghostel-toggle-cd-auto-create-buffer)))
         (dir (expand-file-name default-directory))
         cd-cmd cur-host ghostel-dir ghostel-host)
    (if (ignore-errors (file-remote-p dir))
        (with-parsed-tramp-file-name dir nil
          (setq cur-host host)
          (setq dir localname))
      (setq cur-host (system-name)))
    (setq cd-cmd (concat " cd " (shell-quote-argument dir)))
    (if shell-buffer
        (progn
          (when (and (not (ghostel-toggle-togglable-buffer-p (current-buffer)))
                     (not (get-buffer-window shell-buffer)))
            (setq ghostel-toggle--window-configuration
                  (current-window-configuration)))
          (if ghostel-toggle-fullscreen-p
              (progn
                (delete-other-windows)
                (switch-to-buffer shell-buffer))
            (if (ghostel-toggle-togglable-buffer-p (current-buffer))
                (switch-to-buffer shell-buffer nil t)
              (pop-to-buffer shell-buffer)))
          (with-current-buffer shell-buffer
            (when (ghostel-toggle-togglable-buffer-p shell-buffer)
              (setq ghostel-toggle--cd-cmd cd-cmd)
              (if (ignore-errors (file-remote-p default-directory))
                  (with-parsed-tramp-file-name default-directory nil
                    (setq ghostel-dir localname)
                    (setq ghostel-host host))
                (setq ghostel-dir (expand-file-name default-directory))
                (setq ghostel-host (system-name)))
              (when (and make-cd
                         (equal ghostel-host cur-host)
                         (not (equal (directory-file-name ghostel-dir)
                                     (directory-file-name dir))))
                ;; Clear any partially typed line, then send the cd.
                (ghostel-send-key "a" "ctrl")
                (ghostel-send-key "k" "ctrl")
                (sleep-for 0.01)
                (if (ghostel-toggle--at-prompt-p shell-buffer)
                    (ghostel-toggle-insert-cd)
                  (message "You can insert `%s' with M-x ghostel-toggle-insert-cd."
                           ghostel-toggle--cd-cmd))))
            (run-hooks 'ghostel-toggle-show-hook)))
      (unless (ghostel-toggle-togglable-buffer-p (current-buffer))
        (setq ghostel-toggle--window-configuration (current-window-configuration)))
      (with-current-buffer (setq shell-buffer (ghostel-toggle--new))
        (ghostel-toggle--wait-prompt)
        (when ghostel-toggle-fullscreen-p
          (delete-other-windows))
        (run-hooks 'ghostel-toggle-show-hook)))
    shell-buffer))

(defun ghostel-toggle--wait-prompt ()
  "Block briefly until the current Ghostel buffer has produced output."
  (let ((wait-ms 0))
    (cl-loop until (or (> (length (string-trim
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))
                          0)
                       (> wait-ms 3000))
             do (sleep-for 0.01)
                (setq wait-ms (+ wait-ms 10)))))

;;;###autoload
(defun ghostel-toggle-insert-cd ()
  "Insert a cd command in the Ghostel buffer toggled to with `ghostel-toggle-cd'."
  (interactive)
  (if (ghostel-toggle-togglable-buffer-p (current-buffer))
      (when ghostel-toggle--cd-cmd
        (ghostel-send-string ghostel-toggle--cd-cmd)
        (ghostel-send-key "return"))
    (call-interactively #'ghostel-toggle-cd-show)))

;;
;; Buffer creation and selection
;;

(defun ghostel-toggle--display-action ()
  "Return the `display-buffer' action used to show a new Ghostel buffer."
  (if ghostel-toggle-fullscreen-p
      '(display-buffer-same-window)
    ghostel-toggle-display-action))

(defun ghostel-toggle--new (&optional buffer-name)
  "Create and return a new Ghostel terminal buffer named BUFFER-NAME.
Mirrors the buffer setup performed by the `ghostel' command, but lets the
caller control the buffer name, the working directory and the display
action so the result integrates with `ghostel-toggle's window handling."
  (ghostel--load-module t)
  (let* ((default-directory default-directory)
         (name (or buffer-name ghostel-buffer-name))
         (display-action (ghostel-toggle--display-action))
         project-root buffer)
    (when (and ghostel-toggle-project-root
               (eq (ghostel-toggle--effective-scope) 'project))
      (setq project-root (ghostel-toggle--project-root))
      (when project-root
        (setq default-directory project-root)))
    (setq buffer (ghostel--create name display-action))
    (with-current-buffer buffer
      (setq ghostel--managed-buffer-name (buffer-name))
      (setq ghostel--buffer-identity (buffer-name))
      (ghostel--start-process))
    buffer))

(defun ghostel-toggle--get-buffer (&optional make-cd ignore-prompt-p)
  "Return a Ghostel buffer to switch to, honoring the effective scope.
The effective scope comes from `ghostel-toggle--effective-scope' (which
respects `ghostel-toggle-scoped').  MAKE-CD and IGNORE-PROMPT-P are forwarded
to the recent-buffer search."
  (let ((scope (ghostel-toggle--effective-scope)))
    (cond
     ((eq scope 'dedicated)
      (ghostel-toggle--get-dedicated-buffer))
     ((eq scope 'project)
      (ghostel-toggle--recent-ghostel-buffer
       make-cd ignore-prompt-p (ghostel-toggle--project-root)))
     (t
      (ghostel-toggle--recent-ghostel-buffer make-cd ignore-prompt-p)))))

(defun ghostel-toggle--get-dedicated-buffer ()
  "Return the dedicated Ghostel buffer, creating it if necessary."
  (if (buffer-live-p ghostel-toggle--dedicated-buffer)
      ghostel-toggle--dedicated-buffer
    (setq ghostel-toggle--dedicated-buffer (ghostel-toggle--new))
    (with-current-buffer ghostel-toggle--dedicated-buffer
      (ghostel-toggle--wait-prompt)
      (setq ghostel-toggle--dedicated-p t)
      ghostel-toggle--dedicated-buffer)))

(defun ghostel-toggle--not-in-other-frame (frame buf)
  "Return non-nil unless BUF is displayed only in a frame other than FRAME."
  (let ((win (get-buffer-window buf t)))
    (if win
        (eq frame (window-frame win))
      t)))

(defun ghostel-toggle--recent-ghostel-buffer (&optional make-cd ignore-prompt-p dir)
  "Return the most recent suitable Ghostel buffer, or nil.
MAKE-CD restricts to buffers on the same host whose shell is at a prompt
\(unless IGNORE-PROMPT-P).  DIR restricts to a project root when
`ghostel-toggle-scope' is `project'."
  (let ((shell-buffer)
        (curbuf (current-buffer))
        (curframe (window-frame))
        (scope (ghostel-toggle--effective-scope))
        buffer-host ghostel-host)
    (if (ignore-errors (file-remote-p default-directory))
        (with-parsed-tramp-file-name default-directory nil
          (setq buffer-host host))
      (setq buffer-host (system-name)))
    (cl-loop for buf in (buffer-list) do
             (when (and (ghostel-toggle-togglable-buffer-p buf)
                        (not (eq curbuf buf))
                        (not (buffer-local-value 'ghostel-toggle--dedicated-p buf))
                        (or (not scope)
                            (and (eq scope 'frame)
                                 (ghostel-toggle--not-in-other-frame curframe buf))
                            (and (eq scope 'project)
                                 (equal (with-current-buffer buf
                                          (ghostel-toggle--project-root))
                                        dir))))
               (cond
                (make-cd
                 (with-current-buffer buf
                   (if (ignore-errors (file-remote-p default-directory))
                       (with-parsed-tramp-file-name default-directory nil
                         (setq ghostel-host host))
                     (setq ghostel-host (system-name)))
                   (when (and (or ignore-prompt-p
                                  (ghostel-toggle--at-prompt-p buf))
                              (equal buffer-host ghostel-host))
                     (setq shell-buffer buf))))
                (t (setq shell-buffer buf))))
             until shell-buffer)
    shell-buffer))

(defun ghostel-toggle--project-root ()
  "Return the current project root, or nil."
  (require 'project)
  (let ((proj (project-current)))
    (when proj
      (project-root proj))))

(defun ghostel-toggle-buffer-name-by-project (_title)
  "Return \"*ghostel[PROJECT]*\" for the current buffer's project.
Ignores the terminal title; a `ghostel-buffer-name-function' that names each
terminal after its project, so the project-scoped toggling of
`ghostel-toggle-scoped' keeps one stably named terminal per project.
Falls back to the directory's base name when outside any project."
  (require 'project)
  (format "*ghostel[%s]*"
          (if-let* ((proj (project-current)))
              (project-name proj)
            (file-name-nondirectory
             (directory-file-name default-directory)))))

;; Name terminals by project by default, matching `ghostel-toggle-scoped's
;; one-terminal-per-project model.  Only override ghostel's stock namer, so an
;; explicit user choice of `ghostel-buffer-name-function' still wins.
(when (eq ghostel-buffer-name-function #'ghostel-buffer-name-by-title)
  (setq ghostel-buffer-name-function #'ghostel-toggle-buffer-name-by-project))

(defun ghostel-toggle--recent-other-buffer (&optional _args)
  "Return the most recently viewed non-Ghostel, non-internal buffer."
  (let (shell-buffer)
    (cl-loop for buf in (buffer-list) do
             (when (and (not (ghostel-toggle-togglable-buffer-p buf))
                        (not (char-equal ?\s (aref (buffer-name buf) 0))))
               (setq shell-buffer buf))
             until shell-buffer)
    shell-buffer))

;;
;; Lifecycle hooks
;;

(defun ghostel-toggle--exit-hook ()
  "Clean up windows when a managed Ghostel buffer is killed."
  (when (ghostel-toggle-togglable-buffer-p (current-buffer))
    (setq ghostel-toggle--buffer-list
          (delq (current-buffer) ghostel-toggle--buffer-list))
    (if (eq ghostel-toggle-reset-window-configuration-after-exit 'kill-window-only)
        (cond
         ((eq (window-deletable-p) 'frame)
          (delete-frame))
         ((eq (window-deletable-p) t)
          (delete-window))
         (t
          (quit-window)))
      (when (and ghostel-toggle-reset-window-configuration-after-exit
                 ghostel-toggle--window-configuration)
        (set-window-configuration ghostel-toggle--window-configuration)))))

(add-hook 'kill-buffer-hook #'ghostel-toggle--exit-hook)

(defun ghostel-toggle--mode-hook ()
  "Track newly created Ghostel buffers in `ghostel-toggle--buffer-list'."
  (when (ghostel-toggle-togglable-buffer-p (current-buffer))
    (add-to-list 'ghostel-toggle--buffer-list (current-buffer))))

(add-hook 'ghostel-mode-hook #'ghostel-toggle--mode-hook)

(dolist (buf (buffer-list))
  (when (ghostel-toggle-togglable-buffer-p buf)
    (add-to-list 'ghostel-toggle--buffer-list buf t)))

;;
;; Cycling
;;

(defun ghostel-toggle--switch (direction offset)
  "Switch among managed Ghostel buffers.
DIRECTION is `forward' or `backward'; OFFSET is how many buffers to skip."
  (if ghostel-toggle--buffer-list
      (let ((len (length ghostel-toggle--buffer-list))
            (index (cl-position (current-buffer) ghostel-toggle--buffer-list)))
        (if index
            (let ((target (if (eq direction 'forward)
                              (mod (+ index offset) len)
                            (mod (- index offset) len))))
              (switch-to-buffer (nth target ghostel-toggle--buffer-list)))
          (switch-to-buffer (car ghostel-toggle--buffer-list))))
    (call-interactively #'ghostel)))

;;;###autoload
(defun ghostel-toggle-forward (&optional offset)
  "Switch to the next Ghostel buffer, skipping OFFSET buffers."
  (interactive "P")
  (ghostel-toggle--switch 'forward (or offset 1)))

;;;###autoload
(defun ghostel-toggle-backward (&optional offset)
  "Switch to the previous Ghostel buffer, skipping OFFSET buffers."
  (interactive "P")
  (ghostel-toggle--switch 'backward (or offset 1)))

(provide 'ghostel-toggle)

;;; ghostel-toggle.el ends here
