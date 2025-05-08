;;; package --- A major mode for editing yesod persistent templates -*- lexical-binding: t; -*-

;;; Commentary:

(require 'generic-x)

;;; Code:

;;;###autoload
(define-generic-mode
    'persistent-mode ;; mode name
  '("--")            ;; comments
  '(                 ;; keywords
    ;; keywords
    "\\(sql\\)=?\\w+"
    "deriving" "default"

    ;; instances we might derive
    "Eq" "Show" "Ord"

    ;; Common haskell types
    "Bool" "Int8" "Int16" "Int32" "Int" "Int64" "Integer" "Word8" "Word16"
    "Word32" "Word64" "Double" "ByteString" "UTCTime" "Day" "Text" "TimeOfDay"
    "UUID" "Maybe"
    )
  '(("=" . 'font-lock-operator)
    ("'.*'" . font-lock-string-face)
    ("\\[\\(\s?\\w+\\)*\\]" . 'font-lock-type-face)
    ("^\s+\\(\\w+\\)" . 'font-lock-variable-name-face)
    ("default=" . 'font-lock-keyword-face)
    )                ;; faces
  '("\\.persistentmodels$")
  (list (lambda () (setq comment-start "--")))                ;; extra functions to call
  "A mode for yesod persistent schema files"
  )

(provide 'persistent-mode)
;;; persistent-mode.el ends here
