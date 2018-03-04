;;; nethack -- Run nethack in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'term)

(defgroup nethack nil
  "Nethack"
  :group 'processes)

(defcustom nethack-executable "nethack"
  "Executable to use to run nethack."
  :group 'nethack
  :type 'string)

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
