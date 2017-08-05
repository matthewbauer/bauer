(require 'font-lock)

(defvar nix-fontify-phases
  '("building"
    "unpacking sources"
    "configuring"
    "installing"
    "post-intallation fixup"))

(defvar nix-font-lock-keywords
  (append
   (mapcar (lambda (arg)
             `(,(concat "\\<" arg "\\>") 0 font-lock-keyword-face))
           nix-fontify-phases)
   '(("\\<\\(warning\\):" 1 font-lock-warning-face)
     ("\\<\\(error\\):" 1 font-lock-warning-face)
     ("\\<\\(replacing old\\) ‘\\(.*\\)’"
      (1 font-lock-type-face)
      (2 font-lock-constant-face))
     ("\\<\\(installing\\) ‘\\(.*\\)’"
      (1 font-lock-type-face)
      (2 font-lock-constant-face))
     ("\\<\\(building path(s)\\) ‘\\(.*\\)’"
      (1 font-lock-variable-name-face)
      (2 font-lock-string-face))
     ("\\<\\(Loading\\) \\(.*\\) \\((source)...\\)"
      (1 font-lock-type-face)
      (2 font-lock-string-face)
      (3 font-lock-keyword-face))
     ("\\<at \\([a-zA-Z./]+\\):\\([0-9]+\\):\\([0-9]+\\)"
      (1 font-lock-string-face)
      (2 font-lock-constant-face)
      (3 font-lock-constant-face))
     ("\\<  \\(.*\.drv\\)"
      (1 font-lock-string-face))
     ("\\<\\(these derivations will be built:\\)"
      (1 font-lock-builtin-face))
     ("\\<\\(these derivations will be fetched:\\)"
      (1 font-lock-builtin-face))
     ("\\<\\(created [0-9]+ symlinks in user environment\\)"
      (1 font-lock-function-name-face))
     ;; ("source root is")
     ;; ("unpacking source archive")
     ("no configure script, doing nothing" 0 font-lock-warning-face)
     ;; ("\\([a-zA-Z-.]\\): ")
     )))

;;;###autoload
(define-minor-mode nix-fontify-mode
  "Font lock nix-env, nix-build, etc."
  :lighter " NixFont"
  (if nix-fontify-mode
      (font-lock-add-keywords nil nix-font-lock-keywords)
    (font-lock-remove-keywords nil nix-font-lock-keywords)))

(provide 'nix-fontify)
;;; nix-fontify.el ends here
