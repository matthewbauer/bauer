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

;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun increment-number-at-point (&optional num)
  "Increment number at point by 1.
Works with numerical arguments, too.
With a negative argument (just M--), uses -1.
With a universal argument (just C-u), ask by how much."
  (interactive "P")
  (save-excursion
    (when (zerop (skip-chars-backward "-0123456789."))
      (skip-syntax-forward "-"))
    (or (looking-at "-?[0123456789.]+")
	(error "No number at point"))
    (cond ((null num)
	   (setq num 1))
	  ((eq num '-)
	   (setq num -1))
	  ((listp num)
	   (setq num (read-number "Increment by how much? " 1))))
    (replace-match (number-to-string (+ num (string-to-number (match-string 0)))))))

(defun increment-numbers-in-region (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "" end t)
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

(defcustom bauer-dir (expand-file-name ".local/share" (getenv "HOME"))
  "Directory of installation."
  :group 'bauer
  :type 'string)
(defcustom bauer-org (expand-file-name "README.org" bauer-dir)
  "Org file."
  :group 'bauer
  :type 'string)

;; (defvar output-directory (expand-file-name ".nix-profile" (getenv "HOME")))

(defun bauer-find-config ()
  "Edit README.org"
  (interactive)
  (find-file bauer-org))

(defun autofun (sym file)
  (autoload sym file)
  sym)

(provide 'bauer)

;;; bauer.el ends here
