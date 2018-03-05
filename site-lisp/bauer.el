;;; bauer.el --- Helper functions for use with Bauer IDE


;;; Commentary:
;; Some helper functions that are not in any other packages go here.
;; Eventually this should be published on MELPA as it’s own package.
;; Also look for replacements to these functions that are already in
;; MELPA.

;;; Code:

(require 'subr-x)
(require 'dash nil t)

(defgroup bauer nil "Options for Bauer IDE."
  :group 'emacs)

(defun update-font-spec-size (spec n &optional increment)
  "Update font SPEC to N. If INCREMENT is non-nil then increase by N."
  (string-join
   (-update-at 7 (lambda (i) (number-to-string
                         (if increment (+ (string-to-number i) n) n)))
               (split-string spec "-")) "-"))

(defun update-font-size (n &optional increment)
  "Update font size to N. If INCREMENT is non-nil then increase by N."
  (set-frame-font
   (update-font-spec-size (frame-parameter nil 'font) n increment)))

(defcustom user-symbols '(org-agenda-files
                          org-default-notes-file
                          org-capture-templates
                          erc-nick erc-nickserv-passwords
                          erc-autojoin-channels-alist
                          erc-server
                          smtpmail-smtp-server
                          smtpmail-smtp-service
                          smtpmail-smtp-user
                          user-full-name user-mail-address
                          gnus-secondary-select-methods)
  "Symbols that the user should customize for themselves."
  :type '(repeat symbol)
  :group 'bauer)

(defun user-customize ()
  "Provides an easy way for users to setup their configuration.
Run this to input your personal settings."
  (interactive)
  (custom-buffer-create (mapcar (lambda (x) (custom-load-symbol x)
                                  `(,x custom-variable)) user-symbols)
                        "*User customize*"))

(provide 'bauer)

;;; bauer.el ends here
