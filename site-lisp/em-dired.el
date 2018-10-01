;;; em-dired.el --- Dired features in Eshell

;; Copyright (C) 2017 Matthew Bauer
;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Version: 0.01
;; Created: 2017-06-05
;; Keywords: convenience tools terminals
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://github.com/matthewbauer/bauer

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library provides utilities for working with directories in eshell.  They
;; aim to make eshell integrate into Emacs better.

;; This makes eshell’s cd behave like dired where each new directory is a new
;; buffer.

;; Installation

;; It is recommended to install this as a package.
;; Type M-x followed by:
;; package-install<RET>eshell-extras

;; Usage

;; use-package:
;; (use-package eshell-extras
;;   :after eshell)

;;; Code:

(require 'eshell)
(require 'em-dirs)

(defgroup em-dired nil
  "Dired features in eshell mode"
  :group 'eshell)

;;;###autoload
(defun em-dired (&rest args)
  "Load a new eshell for the current directory.
ARGS anything else Eshell needs."
  (interactive "P")
  (em-dired-new)
  (eshell))

;;;###autoload
(defun em-dired-new (&rest args)
  "Load a new eshell for the current directory.
ARGS anything else Eshell needs."
  (interactive "P")
  (setq eshell-buffer-name
        (concat "*eshell<" (expand-file-name default-directory) ">*")))

(defun em-dired-cd (&rest args)
  "Open each directory in a new buffer like dired.
ARGS to open if none provided assume HOME dir."
  (let* ((path (car (eshell-flatten-list args)))
         (curdir (eshell/pwd))
         (newdir (or path "~")))
    (unless (equal curdir newdir)
      (let ((default-directory
              (concat (directory-file-name (expand-file-name newdir curdir))
                      "/")))
        (if (file-directory-p default-directory)
            (let ((buffer
                   (get-buffer-create
                    (concat "*eshell<" default-directory ">*"))))
              (pop-to-buffer-same-window buffer)
              (unless (derived-mode-p 'eshell-mode)
                (eshell-mode)))
          (error "%s is not a directory" default-directory))))
    (run-hooks 'eshell-directory-change-hook)
    nil))
(put 'eshell/cd 'eshell-no-numeric-conversions t)

(require 'subr-x)

(defun em-dired-ls-find-file-at-point (point)
  "RET on Eshell's `ls' output to open files.
POINT is the point that the file is available at."
  (interactive "d")
  (find-file (string-trim-right
              (buffer-substring-no-properties
               (previous-single-property-change point 'help-echo)
               (next-single-property-change point 'help-echo)))))

(defun em-dired-ls-find-file-at-mouse-click (event)
  "Middle click on Eshell's `ls' output to open files.
EVENT refers to the mouse event that triggers the click.
 From Patrick Anderson via the wiki."
  (interactive "e")
  (em-dired-ls-find-file-at-point (posn-point (event-end event))))

(defvar em-dired-ls-keymap--clickable
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'em-dired-ls-find-file-at-point)
    (define-key map (kbd "RET") 'em-dired-ls-find-file-at-point)
    (define-key map (kbd "<mouse-1>") 'em-dired-ls-find-file-at-mouse-click)
    (define-key map (kbd "<mouse-2>") 'em-dired-ls-find-file-at-mouse-click)
    map))

(defun em-dired-ls-decorated-name--clickable (filename)
  "Modify Eshell's `ls' to let you click or RET on file names to open them.
FILENAME is the file text that we will add properties to."
  (add-text-properties 0 (length (car filename))
                       (list 'help-echo "RET, mouse-2: visit this file"
                             'mouse-face 'highlight
                             'keymap em-dired-ls-keymap--clickable)
                       (car filename))
  filename)

;;;###autoload
(define-minor-mode em-dired-mode
  "Toggle Eshell Dir Extras mode."
  :group 'eshell
  :lighter " Edired"
  (if em-dired-mode
      (progn
        (advice-add 'eshell :before 'em-dired-new)
        (advice-add 'eshell/cd :override 'em-dired-cd)
        (advice-add 'eshell-ls-decorated-name
                    :after 'em-dired-ls-decorated-name--clickable)
        )
    (advice-remove 'eshell 'em-dired-new)
    (advice-remove 'eshell/cd 'em-dired-cd)
    (advice-remove 'eshell-ls-decorated-name
                   'em-dired-ls-decorated-name--clickable)
    ))

(provide 'em-dired)
;;; em-dired.el ends here
