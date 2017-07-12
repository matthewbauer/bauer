;;; use-package-list.el --- List use-package declarations in config file

;; Copyright (C) 2017 Matthew Bauer
;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Git: https://github.com/matthewbauer/eshell-extras
;; Version: 0.01
;; Created: 2017-06-05
;; Keywords: eshell, prompt

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defun use-package-list (script)
  "Count use-package declarations listed in SCRIPT."

  (require 'use-package)

  (defvar package-list '())

  (add-to-list 'use-package-keywords :builtin)
  (defun use-package-handler/:builtin (name keyword arg rest state)
    (use-package-process-keywords name rest state))
  (add-to-list 'use-package-keywords :name)
  (defun use-package-handler/:name (name keyword arg rest state)
    (use-package-process-keywords name rest state))

  (advice-add 'use-package
              :before (lambda (name &rest args)
                        (unless (or (member :disabled args)
                                    (member :builtin args))
                          (when (member :name args)
                            (setq name (plist-get args :name)))
                          (add-to-list 'package-list name))))

  (defmacro define-hook-helper (&rest args))
  (defmacro create-hook-helper (&rest args))

  (advice-add 'use-package-handler/:defer
              :around (lambda (x name keyword arg rest state)
                        (let ((body (use-package-process-keywords name rest
                                      (plist-put state :deferred t)))
                              (name-string (use-package-as-string name)))
                          (dolist (command (delete-dups (plist-get state :commands)))
                            (unless (or
                                     (string= (symbol-name command) "create-hook-helper")
                                     (string= (symbol-name command) "define-hook-helper"))
                              (fset command (lambda (&rest args)))))
                          body)))

  (advice-add 'use-package-load-name :override #'ignore)

  (load script)

  (require 'json)

  (princ (json-encode package-list))

  package-list)

(provide 'use-package-list)
;;; use-package-list.el ends here
