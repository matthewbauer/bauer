;;; set-defaults.el --- Custom defaults for custom

;; Copyright (C) 2018 Matthew Bauer

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Version: 1.0
;; Homepage: https://github.com/matthewbauer/bauer
;; Package-Requires: ((emacs "24.4"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides Emacs Lisp code to deal with setting defaults for an Emacs
;; distribution, in a similar way to ‘custom-set-variable’. The
;; ‘standard-value’ is set to ensure that Emacs recognizes the new
;; default.

;;; Code:

;; install advice to prevent custom-declare-variable from overwriting defaults
;; below
(require 'custom)
(advice-add 'custom-declare-variable
            :around (lambda (orig-fun symbol default doc &rest args)
                      (apply orig-fun symbol
                             (cond ((car (get symbol 'standard-value)))
                                   (default))
                             doc args)))

;; This code is based off of custom-set-variables’ defun.
(defun set-defaults (&rest args)
  "Set defaults in the same way as ’custom-set-variables’.
ARGS are a list in the form of (SYMBOL VALUE)."
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry))))
      (unless (or (get symbol 'standard-value)
                  (memq (get symbol 'custom-autoload) '(nil noset)))
        ;; This symbol needs to be autoloaded, even just for a `set'.
        (custom-load-symbol symbol))))
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry)))
           (value (nth 1 entry))
           ;; (now (nth 2 entry))
           (requests (nth 3 entry))
           ;; (comment (nth 4 entry))
           )
      ;; (custom-push-theme 'theme-value symbol 'user 'set value)
      (when requests
        (put symbol 'custom-requests requests)
        (mapc 'require requests))

      (put symbol 'default-value (list value))
      ;; (put symbol 'saved-value (list value))
      (put symbol 'standard-value (list value))
      ;; (put symbol 'force-value t)

      (let ((set (or (get symbol 'custom-set) 'custom-set-default)))
        (funcall set symbol (eval value))))))

(defun append-envs (sep &rest env)
  "Append environment variables with SEP from ENV alist."
  (dolist (x env)
    (setenv (car x) (string-join (cons (getenv (car x))
				       (cadr x)) sep))))

(defun set-envs (&rest env)
  "Set environment variables from ENV alist."
  (dolist (x env)
    (setenv (car x) (cadr x))))

(defun set-paths (&rest args)
  "Set paths from ARGS as default values.
verifies path exists"
  (dolist (entry args)
    (let ((path (nth 1 entry)))
      (when (file-exists-p path)
        (set-defaults entry)))))

(provide 'set-defaults)
;;; set-defaults.el ends here
