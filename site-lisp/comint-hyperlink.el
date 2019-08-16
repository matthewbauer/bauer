;;; comint-hyperlink.el --- Create hyperlinks in comint for SGR URL control sequences

;; Copyright (C) 2019 Matthew Bauer

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Created: 15 Aug 2019
;; Keywords: comint, shell, processes, hypermedia, terminals
;; Version: 0.1.1
;; Homepage: https://github.com/matthewbauer/comint-hyperlink
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This file is free software...

;;; Commentary:

;; A filter for comint.el to make URLs clickable from control
;; sequences. Coreutils outputs these when you run ‘ls --hyperlink’.

;; See
;; https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
;; for more information.

;;; Usage:

;;; Usage is pretty straightforward, just require this Elisp file and
;;; add comint-hyperlink-process-output to
;;; comint-input-filter-functions. For vanilla Emacs, this looks like
;;; adding the below snippet to your init.el file.

;; (require 'comint-hyperlink)
;; (add-to-list 'comint-output-filter-functions
;;              'comint-hyperlink-process-output)

;;; Alternatively if you use use-package, this looks like the following.

;; (use-package comint-hyperlink
;;   :commands (comint-hyperlink-process-output)
;;   :init (add-to-list 'comint-output-filter-functions
;;                   'comint-hyperlink-process-output))

;;; Code:

(require 'comint)
(require 'button)
(require 'url-util)

(defvar comint-hyperlink-control-seq-regexp
  "\e\\]8;;\\([^\a\e]*\\)[\a\e]\\(?:\\\\\\)?\\([^\e]*\\)\e]8;;[\a\e]\\(?:\\\\\\)?")

(defgroup comint-hyperlink nil
  "Comint hyperlink handling"
  :group 'comint)

(defcustom comint-hyperlink-action 'comint-hyperlink-find-file
  "Action to use in comint-hyperlink.el."
  :group 'comint-hyperlink
  :type 'function
  :options '(comint-hyperlink-browse-url comint-hyperlink-find-file))

(defun comint-hyperlink-find-file (url)
  "Find file when clicking on a file:// URL.

Falls back to ‘browse-url’."
  (cond
   ((string-match-p "^file://" url)
    (find-file
     (replace-regexp-in-string "^file:///?[^/]+" "" url)))
   (t (comint-hyperlink-browse-url url))))

(defun comint-hyperlink-browse-url (url)
  "Use ‘browse-url’ to open the URL."
  ;; Need to strip hostname from file urls
  (browse-url
   (replace-regexp-in-string "^file:///?[^/]+" "file://" url)))

(define-button-type 'comint-hyperlink
  'follow-link t
  'action (lambda (x) (funcall comint-hyperlink-action
			       (button-get x 'comint-hyperlink-url))))

;;;###autoload
(defun comint-hyperlink-process-output (&optional _)
  "Convert SGR control sequences of comint into clickable text properties.

This is a good function to put in
`comint-output-filter-functions'."
  (interactive)
  (let ((start-marker (if (and (markerp comint-last-output-start)
			       (eq (marker-buffer comint-last-output-start)
				   (current-buffer))
			       (marker-position comint-last-output-start))
			  comint-last-output-start
			(point-min-marker)))
	(end-marker (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward comint-hyperlink-control-seq-regexp end-marker t)
	(let ((url (match-string 1)) (text (match-string 2))
	      start)
	  (delete-region (match-beginning 0) (point))
	  (setq start (point))
	  (insert-button text
			 'type 'comint-hyperlink
			 'comint-hyperlink-url (url-unhex-string url)))))))

(provide 'comint-hyperlink)
;;; comint-hyperlink.el ends here
