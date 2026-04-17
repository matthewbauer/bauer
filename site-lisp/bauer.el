;;; bauer.el --- Helper functions for use with Bauer IDE -*- lexical-binding: t; -*-

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

(defun update-font-size (n &optional increment)
  "Update font size to N. If INCREMENT is non-nil then increase by N."
  (set-frame-font
   (let ((spec (frame-parameter nil 'font))
         (resize (lambda (current)
                   (number-to-string
                    (if increment (+ (string-to-number (or current "0")) n) n)))))
     (cond
      ;; XLFD: leading "-" and 15 dash-separated fields; pixelsize is index 7.
      ((and (string-prefix-p "-" spec)
            (let ((fields (split-string spec "-")))
              (when (>= (length fields) 15)
                (setf (nth 7 fields) (funcall resize (nth 7 fields)))
                (string-join fields "-")))))
      ;; Fontconfig with explicit size attribute: "Family:size=12" (or mixed with other attrs).
      ((string-match "\\(:size=\\)\\([0-9]+\\(?:\\.[0-9]+\\)?\\)" spec)
       (replace-match (concat (match-string 1 spec) (funcall resize (match-string 2 spec)))
                      t t spec))
      ;; Fontconfig shorthand: "Family-Size" (size is trailing numeric token).
      ((string-match "\\`\\(.+\\)-\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\'" spec)
       (concat (match-string 1 spec) "-" (funcall resize (match-string 2 spec))))
      ;; No size found; append as fontconfig shorthand.
      (t (concat spec "-" (funcall resize nil)))))))

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
                          gnus-select-method gnus-secondary-select-methods
                          display-buffer-alist
                          initial-major-mode
                          message-send-mail-function
                          ring-bell-function
                          send-mail-function
                          visible-bell
                          zoneinfo-style-world-list
                          webjump-sites
                          browse-url-browser-function
                          )
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

(defcustom bauer-dir (expand-file-name ".local/share/bauer" (getenv "HOME"))
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

(provide 'bauer)

;;; bauer.el ends here
