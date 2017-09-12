;;; use-package-list.el --- List use-package declarations in config file

;; Copyright (C) 2017 Matthew Bauer
;; Author: Matthew Bauer <mjbauer95@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'json)
(require 'use-package)

(defun use-package-handler/:name (name _ __ rest state)
  "Name keyword for use-package.
Specifies package name (not the name used to require).

NAME the name of the keyword
REST rest of args
STATE use-package state var"
  (use-package-process-keywords name rest state))
(add-to-list 'use-package-keywords :name)

(defun use-package-list (script)
  "Count use-package declarations listed in SCRIPT."

  (defvar package-list '())

  (advice-add 'use-package
              :before (lambda (name &rest args)
                        (unless (or (member :disabled args)
                                    (and (not (member :name args))
                                         (member :ensure args)
                                         (not (alist-get :ensure args))))
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
                          (dolist (command
                                   (delete-dups (plist-get state :commands)))
                            (unless (or
                                     (string= (symbol-name command)
                                              "create-hook-helper")
                                     (string= (symbol-name command)
                                              "define-hook-helper"))
                              (fset command (lambda (&rest args)))))
                          body)))

  (advice-add 'use-package-load-name :override #'ignore)

  (load script)

  (princ (json-encode package-list))

  package-list)

(provide 'use-package-list)
;;; use-package-list.el ends here
