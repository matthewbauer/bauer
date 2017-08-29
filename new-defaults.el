;;; new-defaults.el --- Set new defaults for Emacs

;;; Commentary:

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'custom)
(require 'nadvice)

;; install advice to prevent custom-declare-variable from overwriting defaults
;; below
(advice-add 'custom-declare-variable
            :around (lambda (orig-fun symbol default doc &rest args)
                      (apply orig-fun symbol
                             (cond ((car (get symbol 'standard-value)))
                                   (default))
                             doc args)))

(defun setenvs (&rest env)
  "Set environment variables from ENV alist."
  (dolist (x env)
    (setenv (car x) (car (cdr x)))))

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

(defun set-paths (&rest args)
  "Set paths from ARGS as default values.
verifies path exists"
  (dolist (entry args)
    (let ((path (nth 1 entry)))
      (unless (file-exists-p path)
        (error "Path %s not found" path))))
  (apply 'set-defaults args))

(provide 'new-defaults)
;;; new-defaults.el ends here
