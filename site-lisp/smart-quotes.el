;;; smart-quotes.el --- Smart Quotes minor mode for GNU Emacs

;; Copyright (C) 2007-2011 Gareth Rees
;; Copyright (C) 2011-2013 Reuben Thomas

;; Author: Gareth Rees <gdr@garethrees.org>
;; Created: 2007-10-20
;; Keywords: abbrev

;; Smart Quotes mode is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;;; Commentary:
;;
;; Smart Quotes is a minor mode that rebinds the ' and " keys to
;; insert left and right quotation marks according to the context
;; before point.

;;; Code:

(defgroup smart-quotes nil
  "Minor mode for inserting left and right quotes."
  :group 'editing)

;;;###autoload
(defcustom smart-quotes-mode nil
  "Toggle smart-quotes-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `smart-quotes-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "1.2"
  :type 'boolean
  :group 'smart-quotes
  :require 'smart-quotes)

(defcustom smart-quotes-left-context "^\\|\\s-\\|\\s(\\|[‘“]"
  "Regular expression matching the context preceding point in
which a left quotation mark will be inserted; in other
contexts, a right quotation mark will be inserted."
  :type 'regexp
  :group 'smart-quotes)

(defcustom smart-quotes-reverse-quotes t
  "If non-nil, reverse a preceding quote instead of inserting a
quote of the same kind."
  :type 'boolean
  :group 'smart-quotes)

(defun smart-quotes-insert-single (&optional noreverse)
  "Insert U+2018 LEFT SINGLE QUOTATION MARK if point is preceded
by `smart-quotes-left-context'; U+2019 RIGHT SINGLE QUOTATION MARK
otherwise.  If `smart-quotes-reverse-quotes' is true, and point is
preceded by a single left or right quote, reverse its direction
instead of inserting another.  A prefix ARG prevents reversal."
  (interactive "P")
  (insert-char
   (or (if (and (not noreverse) smart-quotes-reverse-quotes)
           (if (= (preceding-char) #x2018)
               (progn (delete-char -1) #x2019)
             (if (= (preceding-char) #x2019)
                 (progn (delete-char -1) #x2018))))
       (if (looking-back smart-quotes-left-context) #x2018 #x2019))))

(defun smart-quotes-insert-double (&optional noreverse)
  "Insert U+201C LEFT DOUBLE QUOTATION MARK if point is preceded
by `smart-quotes-left-context'; U+201D RIGHT DOUBLE QUOTATION
MARK otherwise.  If `smart-quotes-reverse-quotes' is true, and
point is preceded by a double left or right quote, reverse its
direction instead of inserting another.  A prefix ARG prevents
reversal."
  (interactive "P")
  (insert-char
   (or (if (and (not noreverse) smart-quotes-reverse-quotes)
           (if (= (preceding-char) #x201C)
               (progn (delete-char -1) #x201D)
             (if (= (preceding-char) #x201D)
                 (progn (delete-char -1) #x201C))))
       (if (looking-back smart-quotes-left-context) #x201C #x201D))))

;;;###autoload
(define-minor-mode smart-quotes-mode
  "Minor mode that makes the ' and \" keys insert left and right
quotation marks automatically according to the context before point;
see `smart-quotes-insert-single' and `smart-quotes-insert-double'.
With no argument, this command toggles Smart Quotes mode.
With a prefix argument ARG, turn Smart Quotes minor mode on if ARG
is positive, otherwise turn it off."
  :lighter (:eval (string ? (decode-char 'ucs #x201C)
                          (decode-char 'ucs #x201D)))
  :keymap '(("'" . smart-quotes-insert-single)
            ("\"" . smart-quotes-insert-double)))

;;;###autoload
(defun turn-on-smart-quotes ()
  "Unconditionally turn on Smart Quotes mode."
  (smart-quotes-mode 1))

;;;###autoload
(defun turn-off-smart-quotes ()
  "Unconditionally turn off Smart Quotes mode."
  (smart-quotes-mode -1))

(custom-add-option 'text-mode-hook 'turn-on-smart-quotes)

;;;###autoload
(defun smart-quotes-smarten ()
  "Turn quotes into smart quotes in region or buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward "['\"]" nil t)
        (let ((single (string= (match-string 0) "'")))
          (replace-match "")
          (if single (smart-quotes-insert-single)
            (smart-quotes-insert-double)))))))

(provide 'smart-quotes)

;;; smart-quotes.el ends here
