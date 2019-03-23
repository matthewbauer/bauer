;;; nix-haskell.el -- haskell-mode integrations for Nix -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, haskell
;; Version: 0.0.1
;; Package-Requires: ((haskell-mode "16.0") (flycheck "30") (nix-mode "1.3.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-store)
(require 'haskell)
(require 'flycheck)
;; (require 'projectile)

(defgroup nix-haskell nil
  "Nix integration with haskell-mode.el"
  :group 'nix)

(defcustom nix-haskell-trust-project nil
  "Whether to allow nix-haskell to trust the projects config.
This can leads to a security vulnerability if the project you are
using provides a malicious GHC in its Nix configuration."
  :type 'boolean
  :risky t
  :group 'nix-haskell)
(make-variable-buffer-local 'nix-haskell-trust-project)

;; Expression used to build Haskell’s package db
(defvar nix-haskell-pkg-db-expr "{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages
, filename, packageName }: let
  inherit (pkgs) lib;
  getGhc = name: let compilerName = lib.replaceStrings [\".\" \"-\"] [\"\" \"\"] name;
                     getChannel = channel: import (builtins.fetchTarball \"channel:${channel}\") {};
                     findNonNull = l: r: if l != null then l
                                         else if r.haskell.compiler ? ${compilerName}
                                              then r.haskell.compiler.${compilerName}
                                         else null;
                     compiler = builtins.foldl' findNonNull null (map getChannel [\"nixos-18.09\"]);
                 in pkgs.haskell.compiler.${compilerName} or (if compiler != null then compiler
                                                              else throw \"Can’t find compiler for ${compilerName}.\");
  buildPkgDb = pkg: let
    maybeGhc = if pkg.nativeBuildInputs != [] then builtins.head pkg.nativeBuildInputs else null;
    compiler = if pkg ? compiler then getGhc pkg.compiler.name
               else if maybeGhc != null && builtins.match \"^ghc.*\" maybeGhc.name != null
                    then getGhc (if maybeGhc ? version then \"ghc-${maybeGhc.version}\" else maybeGhc.name)
               else throw \"Can’t find compiler for ${pkg.name}.\";
    package-db = pkgs.buildEnv {
      name = \"package-db-${compiler.name}\";
      paths = lib.closePropagation (pkg.getBuildInputs.haskellBuildInputs or (pkg.buildInputs ++ pkg.propagatedBuildInputs ++ pkg.nativeBuildInputs));
      pathsToLink = [ \"/lib/${compiler.name}/package.conf.d\" ];
      buildInputs = [ compiler ];
      postBuild = ''
        ghc-pkg --package-db=$out/lib/${compiler.name}/package.conf.d recache
      '';
      ignoreCollisions = true;
    };
    compilerBin = pkgs.buildEnv {
      name = \"${compiler.name}-bins-only\";
      paths = [ compiler ];
      pathsToLink = [ \"/bin\" ];
    };
  in pkgs.buildEnv {
    name = \"${compiler.name}-env\";
    paths = [ package-db pkgs.cabal-install compilerBin ];
  };
  pkg = if lib.hasSuffix \".cabal\" filename
        then haskellPackages.callCabal2nix \"auto-callcabal2nix\" (builtins.toPath filename) {}
        else if lib.hasSuffix \".nix\" filename
        then (let nixExpr = import filename;
                  nixExpr' = if builtins.isFunction nixExpr
                             then (if (builtins.functionArgs nixExpr) ? mkDerivation
                                   then haskellPackages.callPackage filename {}
                                   else nixExpr {})
                             else nixExpr;
              in (if lib.isDerivation nixExpr' then nixExpr'
                  else if builtins.isAttrs nixExpr'
                  then let nixExpr'' = if nixExpr' ? haskellPackages then nixExpr'.haskellPackages
                                       else if nixExpr' ? haskellPackageSets then nixExpr'.haskellPackageSets.ghc
                                       else nixExpr';
                       in (if nixExpr'' ? ${packageName} then nixExpr''.${packageName}
                           else throw \"Can't find target for ${packageName} in ${filename}.\")
                  else throw \"Can't import ${filename} correctly.\"))
        else throw \"Can't do anything with ${filename}.\";
in buildPkgDb pkg")

;; Store information on running" Nix evaluations.
(defvar nix-haskell--running-processes nil)

;; Cache information on past Nix evaluations.
(defvar nix-haskell--package-db-cache nil)

(defun nix-haskell-clear-cache ()
  "Clean the nix-haskell cache."
  (interactive)
  (setq nix-haskell--package-db-cache nil))

(defun nix-haskell--store-sentinel (err buf drv-file drv _ event)
  "Make a nix-haskell process.
ERR the error buffer.
BUF the main buffer.
DRV-FILE filename of derivation.
DRV parsed derivation file.
PROC the process that has been run.
EVENT the event that was fired."
  (pcase event
    ("finished\n"
     (nix-haskell--interactive buf drv-file drv)
     (kill-buffer err))
    (_ (display-buffer err))))

(defun nix-haskell--instantiate-sentinel (prop err proc event)
  "Make a nix-haskell process.
PROP the prop name of nix-haskell--running-processes.
ERR the error buffer.
PROC the process that has been run.
EVENT the event that was fired."
  (pcase event
    ("finished\n"
     (with-current-buffer (process-buffer proc)
       (unless (eq (buffer-size) 0)
	 ;; Parse nix-instantiate output
	 (let* ((drv-file (substring (buffer-string) 0 (- (buffer-size) 1)))
		(drv (nix-instantiate--parsed drv-file))

		;; Hacky way to get output path without building it.
		(out (cdadr (cadar drv))))
	   (dolist
	       (callback (lax-plist-get nix-haskell--running-processes prop))
	     (funcall callback out drv-file))
	   (setq nix-haskell--package-db-cache
		 (lax-plist-put nix-haskell--package-db-cache
				prop (list out drv-file))))))
     (setq nix-haskell--running-processes
	   (lax-plist-put nix-haskell--running-processes prop nil))
     (kill-buffer err))
    (_ (display-buffer err)))
  (unless (process-live-p proc)
    (kill-buffer (process-buffer proc))))

(defun nix-haskell--get-pkg-db (callback)
  "Get a package-db async.
CALLBACK called once the package-db is determined."
  (let ((cabal-file (haskell-cabal-find-file default-directory))
	filename package-name root)

    ;; (when (and (projectile-project-p) (not root)
    ;;	       (or (file-exists-p (expand-file-name "default.nix" (projectile-project-root)))
    ;;		   (file-exists-p (expand-file-name "shell.nix" (projectile-project-root)))))
    ;;   (setq root (projectile-project-root)))
    (unless root
      (setq root (locate-dominating-file default-directory "cabal.project")))
    (unless root
      (setq root (locate-dominating-file default-directory "default.nix")))
    (unless root
      (setq root (locate-dominating-file default-directory "shell.nix")))
    (when root
      (setq root (expand-file-name root)))

    (when cabal-file (setq cabal-file (expand-file-name cabal-file)))
    (unless cabal-file (error "Cannot find a valid .cabal file"))
    (setq package-name (replace-regexp-in-string ".cabal$" "" (file-name-nondirectory cabal-file)))

    ;; Look for shell.nix or default.nix
    (unless (and filename (file-exists-p filename))
      (setq filename (expand-file-name "default.nix" root)))
    (unless (and filename (file-exists-p filename))
      (setq filename (expand-file-name "shell.nix" root)))
    (unless (and filename (file-exists-p filename))
      (setq filename cabal-file))

    ;; TODO: update cache after certain threshold
    (let ((cache (lax-plist-get nix-haskell--package-db-cache cabal-file)))
      (if cache (apply callback cache)
	(let* ((data (lax-plist-get nix-haskell--running-processes cabal-file))
	       (stdout (generate-new-buffer
			(format "*nix-haskell-instantiate-stdout*<%s>" cabal-file)))
	       (stderr (generate-new-buffer
			(format "*nix-haskell-instantiate-stderr*<%s>" cabal-file)))
	       (command (list nix-instantiate-executable
			      "-E" nix-haskell-pkg-db-expr
			      "--argstr" "filename" filename
			      "--argstr" "packageName" package-name)))

	  ;; Pick up projects with custom package sets. Note: this
	  ;; requires trusting the project’s configuration.
	  (when nix-haskell-trust-project
	    (when (file-exists-p (expand-file-name "reflex-platform.nix" root))
	      (setq command
		    (append command
			    (list "--arg" "haskellPackages"
				  (format "(import %s {}).ghc"
					  (expand-file-name "reflex-platform.nix"
							    root))))))
	    (when (file-exists-p (expand-file-name ".obelisk/impl/default.nix" root))
	      (setq command
		    (append command
			    (list "--arg" "haskellPackages"
				  (format "(import %s {}).haskellPackageSets.ghc"
					  (expand-file-name ".obelisk/impl/default.nix"
							    root)))))))

	  (setq nix-haskell--running-processes
		(lax-plist-put nix-haskell--running-processes
			       cabal-file (cons callback data)))
	  (make-process
	   :name (format "*nix-haskell*<%s>" cabal-file)
	   :buffer stdout
	   :command command
	   :noquery t
	   :sentinel (apply-partially 'nix-haskell--instantiate-sentinel
				      cabal-file stderr)
	   :stderr stderr)))))

  ;; t is used here so the hook doesn’t wait for the process above to
  ;; finish.
  t)

(defun nix-haskell--interactive (buf out drv)
  "Setup interactive buffers for nix-haskell.

Handles flycheck and haskell-interactive modes currently.

BUF the buffer this was called from.
OUT filename of derivation.
DRV derivation file."
  (if (file-exists-p out)
      (let ((package-db out))
	(with-current-buffer buf
	  ;; Find package db directory.
	  (setq package-db (expand-file-name "lib" package-db))
	  (setq package-db (expand-file-name
			    (car (directory-files package-db nil "^ghc"))
			    package-db))
	  (setq package-db (expand-file-name "package.conf.d" package-db))

	  ;; Doesn’t seem to work?
	  (setq-local haskell-compile-cabal-build-command
		      (format "%s new-build" (expand-file-name "bin/cabal" out)))

	  ;; Setup haskell-mode args.
	  (setq-local haskell-process-type 'cabal-new-repl)
	  (setq-local haskell-process-path-cabal (expand-file-name "bin/cabal" out))
	  (make-local-variable 'haskell-process-args-cabal-new-repl)
	  (add-to-list 'haskell-process-args-cabal-new-repl
		       (format "--with-ghc-pkg=%s/bin/ghc-pkg" out) t)
	  (add-to-list 'haskell-process-args-cabal-new-repl
		       (format "--with-ghc=%s/bin/ghc" out) t)
	  (add-to-list 'haskell-process-args-cabal-new-repl
		       (format "--with-hsc2hs=%s/bin/hsc2hs" out) t)
	  (add-to-list 'haskell-process-args-cabal-new-repl
		       (format "--ghc-pkg-option=--package-db=%s" package-db) t)
	  (add-to-list 'haskell-process-args-cabal-new-repl
		       (format "--ghc-option=-package-db=%s" package-db) t)
	  (interactive-haskell-mode 1)

	  ;; Setup flycheck.
	  (setq-local flycheck-haskell-ghc-executable
		      (expand-file-name "bin/ghc" out))
	  (make-local-variable 'flycheck-ghc-package-databases)
	  (add-to-list 'flycheck-ghc-package-databases package-db)
	  (flycheck-mode 1)))
    (let ((stderr (generate-new-buffer
		   (format "*nix-haskell-store-stderr*<%s>" drv))))
      (make-process
       :name (format "*nix-haskell-store*<%s>" drv)
       :buffer nil
       :command (list nix-store-executable "-r" drv)
       :noquery t
       :sentinel (apply-partially 'nix-haskell--store-sentinel stderr buf out drv)
       :stderr stderr))))

(defun nix-haskell-setup ()
  "Hook to run to set up Haskell buffer for nix-haskell.

To use with use-package, something like this will work:

\(use-package nix-haskell
  :hook \(haskell-mode . nix-haskell-setup))"
  (interactive)

  ;; Disable flycheck and interactive-haskell-mode.
  ;; They will be reenabled later.
  (flycheck-mode -1)
  (interactive-haskell-mode -1)

  ;; Need to keep the buffer for after the process has run.
  (nix-haskell--get-pkg-db (apply-partially 'nix-haskell--interactive
					    (current-buffer))))

(provide 'nix-haskell)
;;; nix-haskell.el ends here
