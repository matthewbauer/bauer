(defun get-packages (script)
  (require 'use-package)

  (defvar package-list '())

  ;; (defun register-package (name &optional noerror)
  ;;   (add-to-list 'package-list name))

  ;; (advice-add 'use-package-load-name :before #'register-package)

  (add-to-list 'use-package-keywords :builtin)
  (defun use-package-handler/:builtin (name keyword arg rest state)
    (use-package-process-keywords name rest state))
  (add-to-list 'use-package-keywords :name)
  (defun use-package-handler/:name (name keyword arg rest state)
    (use-package-process-keywords name rest state))

  (defun register-package (name &rest args)
    (unless (or (member :disabled args)
                (member :builtin args))
      (when (member :name args)
        (setq name (plist-get args :name)))
      (add-to-list 'package-list name)))

  (advice-add 'use-package :before #'register-package)

  (defmacro define-hook-helper (&rest args))
  (defmacro create-hook-helper (&rest args))

  (defun never-run (x name keyword arg rest state)
    (let ((body (use-package-process-keywords name rest
                  (plist-put state :deferred t)))
          (name-string (use-package-as-string name)))
      (dolist (command (delete-dups (plist-get state :commands)))
        (unless (or
                 (string= (symbol-name command) "create-hook-helper")
                 (string= (symbol-name command) "define-hook-helper"))
          (fset command (lambda (&rest args)))))
      body))
  (advice-add 'use-package-handler/:defer :around #'never-run)

  (defun load-name-noerror (orig-fun name &optional noerror) t)

  (advice-add 'use-package-load-name :around #'load-name-noerror)

  (setq use-package-always-demand t)
  (setq use-package-always-defer nil)

  (load script)

  (require 'json)
  (princ (json-encode package-list))

  package-list)
