;;: pcomplete-extra.el - -*- lexical-binding: t; -*-

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

;;;###autoload
(defun pcomplete/buck2 ()
  "Completion for `buck2'."
  (while (string-prefix-p "-" (pcomplete-arg))
    (pcomplete-here (pcomplete-from-help "buck2 --help" :argument "[[:alnum:]-]+")))
  (pcomplete-here (pcomplete-from-help "buck2 --help" :argument "[[:alnum:]-]+"))
  (let ((subcmd (pcomplete-arg 1)))
    (while (cond
	    ((string-prefix-p "-" (pcomplete-arg))
	     (pcomplete-here (pcomplete-from-help (format "buck2 %s --help" subcmd))))
	    ((member subcmd '("build" "ctargets" "install" "run" "targets" "test" "utargets"))
	     (pcomplete-here
              (completion-table-dynamic
               (lambda (string)
		 (append
		  (when (string-suffix-p "/" string) (list (concat string "...")))
		  (ignore-errors
		    (process-lines "buck2" "complete"
				   (concat "--target=" string))))))))
	    (t (pcomplete-here (pcomplete-from-help (format "buck2 %s --help" subcmd))))))))

;;;###autoload
(defalias 'pcomplete/buck 'pcomplete/buck2)

(provide 'pcomplete-extra)
;;; pcomplete-extra.el ends here
