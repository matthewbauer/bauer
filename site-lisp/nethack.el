;;; nethack.el --- Run nethack in a buffer

;; Copyright (C) 2018 Matthew Bauer

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/matthewbauer/bauer
;; Version: 1.0

;; This file is not part of GNU Emacs.

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

;; Provides a simple function to start nethack in a terminal.

;;; Code:

(require 'term)

(defgroup nethack nil
  "Nethack"
  :group 'processes)

(defcustom nethack-executable "nethack"
  "Executable to use to run nethack."
  :group 'nethack
  :type 'string)

;;;###autoload
(defun nethack ()
  "Start nethack in terminal."
  (interactive)
  (set-buffer (make-term "nethack" nethack-executable))
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer "*nethack*"))

(provide 'nethack)
;;; nethack.el ends here
