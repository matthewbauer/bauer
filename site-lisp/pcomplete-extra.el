;;: pcomplete-extra.el -

;;; Commentary:

;;; Code:

(require 'pcomplete)

;;;###autoload
(defun pcomplete/cabal ()
  "Completion for `cabal'."
  (pcomplete-here (pcomplete-from-help "cabal --help" :argument "[[:alnum:]-]+"))
  (let ((subcmd (pcomplete-arg 1)))
    (while (pcase subcmd
             ((guard (string-prefix-p "-" (pcomplete-arg)))
              (pcomplete-here (pcomplete-from-help (format "cabal %s --help" subcmd))))))))

;;;###autoload
(defun pcomplete/ghc ()
  "Completion for `ghc'."
  (pcomplete-here (process-lines "ghc" "--show-options")))

(provide 'pcomplete-extra)
;;; pcomplete-extra.el ends here
