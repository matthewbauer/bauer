;;; company-eshell-history.el --- Eshell history backend for company

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'company)
(require 'em-hist)
(require 's)
(require 'dash)

(with-no-warnings
  (require 'cl))

;;;###autoload
(defun company-eshell-history (command &optional arg &rest _)
  "Company eshell history backend.

COMMAND - the company command
ARG - the company arg"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(provide 'company-eshell-history)
;;; company-eshell-history.el ends here
