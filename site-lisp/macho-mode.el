;;; macho-mode.el --- Show symbols in Mach-O files

;; Copyright (C) 2018 Matthew Bauer

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/matthewbauer/bauer
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

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

;; Provides a simple viewer for Mach-O executables using objdump.

;;; Code:

(defvar-local macho-mode nil)

(defun macho-setup-default ()
  "Make `macho-mode' get called automatically for binaries."
  (add-to-list 'magic-mode-alist '("\xFE\xED\xFA\xCE" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xFE\xED\xFA\xCF" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xCE\xFA\xED\xFE" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xCF\xFA\xED\xFE" . macho-mode)))

(defvar macho-mode-command "objdump -D \"%s\""
  "The shell command to use for `macho-mode'.")

;;;###autoload
(defun macho-mode ()
  "Read Mach-O executable symbols."
  (interactive)
  (let ((inhibit-read-only t))
    (if macho-mode
        (progn
          (erase-buffer)
          (insert-file-contents (buffer-file-name))
          (setq macho-mode nil))
      (setq macho-mode t)
      (erase-buffer)
      (insert (shell-command-to-string
               (format macho-mode-command (buffer-file-name)))))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(provide 'macho-mode)
;;; macho-mode.el ends here
