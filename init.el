;; [[file:~/.local/share/bauer/README.org::*=init.el=][=init.el=:1]]
(load
  (expand-file-name "settings.el" user-emacs-directory) t)
(package-initialize)
(defvar bauer-dir
        (expand-file-name ".emacs.d" (getenv "HOME")))
(defvar bauer-org
        (expand-file-name "README.org" bauer-dir))
(add-to-list 'load-path
             (expand-file-name "site-lisp" bauer-dir))
(unless (file-exists-p
          (expand-file-name "README.el" bauer-dir))
  (let ((default-directory bauer-dir))
    (autoload 'org-babel-tangle-file "ob-tangle")
    (org-babel-tangle-file bauer-org
                          "README.el"
                          "emacs-lisp")))
(load (expand-file-name "README.el" bauer-dir) t)
;; =init.el=:1 ends here
