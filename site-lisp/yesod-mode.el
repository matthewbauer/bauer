;;; yesod-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Luis Borjas Reyes
;;
;; Author: Luis Borjas Reyes <luisfborjas@gmail.com>
;; Maintainer: Luis Borjas Reyes <luisfborjas@gmail.com>
;; Created: March 23, 2022
;; Modified: March 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lfborjas/yesod-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Basic syntax highlighting for Yesod routes and models
;;
;;; Code:

(defgroup yesod nil
  "A collection of modes for Yesod"
  :prefix "yesod-"
  :group 'data)

;; From
;; https://github.com/rebeccaskinner/persistent-mode-el/blob/master/persistent-mode.el
(define-generic-mode
    ;; mode name
    'yesod-persistent-mode

  ;; comment starters
  '("--")

  ;; stuff to highlight as keywords
  '(;; keywords
    "\\(sql\\)=?\\w+"
    "deriving" "default" "json"

    ;; instances we might derive
    "Eq" "Show" "Ord"

    ;; Common haskell types
    "Bool" "Int8" "Int16" "Int32" "Int" "Int64" "Integer" "Word8" "Word16"
    "Word32" "Word64" "Double" "ByteString" "UTCTime" "Day" "Text" "TimeOfDay"
    "UUID" "Maybe")

  ;; additional expressions to highlight
  '(("=" . 'font-lock-operator)
    ("'.*'" . font-lock-string-face)
    ("\\[\\(\s?\\w+\\)*\\]" . 'font-lock-type-face)
    ("^\s+\\(\\w+\\)" . 'font-lock-variable-name-face)
    ("default=" . 'font-lock-keyword-face))

  ;; filename patterns to automatically set the mode:
  '("\\.persistentmodels$")

  ;; extra functions to call
  (list (lambda () (setq comment-start "--")))

  "A mode for yesod persistent schema files")


(define-generic-mode
    'yesod-routes-mode
  ;; comment starters
  '("--")

  ;; stuff to highlight as keywords
  '("DELETE" "GET" "POST" "PUT" "PATCH")

  ;; additional expressions to highlight
  '(("--.*" . 'font-lock-comment-face)
    ("\/\\C\v[A-Za-z0-9-.*_]*" . 'font-lock-string-face)
    ("\s[A-Z][A-Za-z0-9_]*\s" . 'font-lock-type-face)
    ("\\C\v[A-Z]{1}.*P:" . 'font-lock-constant-face)
    ("#[A-Z][A-Za-z0-9_]*" . 'font-lock-variable-name-face))

  ;; filename patterns to automatically set the mode:
  '("\\.yesodroutes$")

  ;; functions to call for additional setup
  nil

  "A mode for yesod route files")

(provide 'yesod-mode)
;;; yesod-mode.el ends here
