;;: pcomplete-extra.el -

;;; Commentary:

;;; Code:

(require 'pcomplete)

(defvar cabal-commands '("update" "install" "help"
			 "info" "list" "fetch" "user-config"
			 "get" "init" "configure" "build"
			 "clean" "run" "repl" "test" "bench"
			 "check" "sdist" "upload" "report"
			 "freeze" "gen-bounds" "outdated"
			 "doctest" "haddock" "hscolour"
			 "copy" "register" "reconfigure" "sandbox"
			 "exec" "new-build" "new-configure" "new-repl"
			 "new-run" "new-test" "new-bench" "new-freeze"
			 "new-haddock"))

(defvar cabal-global-flags '("-h" "--help" "-V" "--version"
			     "--numeric-version" "--require-sandbox"
			     "--no-require-sandbox" "--ignore-sandbox"
			     "--ignore-expiry" "--enable-nix" "--disable-nix"))

(defun cabal--pcomplete-flags ()
  "Complete flags to the Nix command."
  (while (pcomplete-match "^-" 0)
    (pcomplete-here cabal-global-flags)))

;;;###autoload
(defun pcomplete/cabal ()
  "Completions for cabal."
  (cabal--pcomplete-flags)
  (pcomplete-here cabal-commands)
  (cabal--pcomplete-flags))

(provide 'pcomplete-extra)
;;; pcomplete-extra.el ends here
