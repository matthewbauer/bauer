;;; nix-haskell.el -- haskell-mode integrations for Nix -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, haskell
;; Version: 0.0.1
;; Package-Requires: ((haskell-mode "16.0") (flycheck "30") (nix-mode "1.3.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Setup for this is fairly straightforward. It aims to automatically
;; configure everything for you. It assumes that you are already using
;; haskell-mode and flycheck.

;; If you have use-package setup, this is enough to get nix-haskell
;; working,

;; (use-package nix-haskell
;;   :hook (haskell-mode . nix-haskell-setup))

;; Opening a buffer will start a nix process to get your dependencies.
;; Flycheck and interactive-haskell-mode will start running once they
;; have been downloaded. This is cached so it will only be done once
;; for each buffer.

;; Flycheck will be started automatically. To start a haskell session,
;; press C-c C-l.

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

(defcustom nix-haskell-verbose nil
  "Whether to print lots of messages when running nix-haskell.el."
  :type 'boolean
  :group 'nix-haskell)

(defcustom nix-haskell-ttl (* 60 10)
  "Time in seconds to keep the cache."
  :type 'integer
  :group 'nix-haskell)

(defcustom nix-haskell-auto-create-session nil
  "Whether to start an interactive-haskell-mode session automatically."
  :type 'boolean
  :group 'nix-haskell)

;; Expression used to build Haskell’s package db

;; We don’t want to just get ghc from the Nix file. This would leave
;; us vulnerable to malicious projects. Instead, we get the compiler
;; name and try to find it in nixpkgs.haskell.compiler. These aren’t
;; left in Nixpkgs for very long so we try to download some channels
;; to see if they are in there.
(defvar nix-haskell-pkg-db-expr "{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages
, nixFile ? null, packageName, cabalFile
, channels ? [\"nixos-18.09\" \"nixos-18.03\" \"nixos-17.09\" \"nixos-17.03\" \"nixos-16.09\"] }: let
  inherit (pkgs) lib;
  getGhc = name: let compilerName = lib.replaceStrings [\".\" \"-\"] [\"\" \"\"] name;
                     getChannel = channel: import (builtins.fetchTarball \"channel:${channel}\") {};
                     findNonNull = l: r: if l != null then l
                                         else if r.haskell.compiler ? ${compilerName}
                                              then r.haskell.compiler.${compilerName}
                                         else null;
                     compiler = builtins.foldl' findNonNull null (map getChannel channels);
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
      paths = lib.closePropagation
                (pkg.getBuildInputs.haskellBuildInputs or
                  (pkg.buildInputs ++ pkg.propagatedBuildInputs ++ pkg.nativeBuildInputs));
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
  pkg = if nixFile == null
        then haskellPackages.callCabal2nix \"auto-callcabal2nix\" (builtins.toPath cabalFile) {}
        else (let nixExpr = import nixFile;
                  nixExpr' = if builtins.isFunction nixExpr
                             then (if (builtins.functionArgs nixExpr) ? mkDerivation
                                   then haskellPackages.callPackage nixFile {}
                                   else if (builtins.functionArgs nixExpr) == {}
                                   then haskellPackages // (nixExpr haskellPackages pkgs)
                                   else nixExpr {})
                             else nixExpr;
              in (if lib.isDerivation nixExpr'
                     then (if nixExpr' ? shells
                           then nixExpr'.shells.ghc
                           else nixExpr')
                  else if builtins.isAttrs nixExpr'
                  then let nixExpr'' = if nixExpr' ? proj then nixExpr'.proj
                                       else if nixExpr' ? shells then nixExpr'
                                       else if nixExpr' ? haskellPackages then nixExpr'.haskellPackages
                                       else if nixExpr' ? haskellPackageSets then nixExpr'.haskellPackageSets.ghc
                                       else if nixExpr' ? ghc then nixExpr'.ghc
                                       else nixExpr';
                       in (if nixExpr'' ? shells then nixExpr''.shells.ghc
                           else if nixExpr'' ? ${packageName} then nixExpr''.${packageName}
                           else if nixExpr'' ? callCabal2nix
                                then nixExpr''.callCabal2nix \"auto-callcabal2nix\" (builtins.toPath cabalFile) {}
                           else haskellPackages.callCabal2nix \"auto-callcabal2nix\" (builtins.toPath cabalFile) {})
                  else throw \"Can't import ${nixFile} correctly.\"));
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
    (_
     (display-buffer err)
     (error "Running nix-haskell failed to realise the store path"))))

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
				prop (list (float-time) out drv-file))))))
     (setq nix-haskell--running-processes
	   (lax-plist-put nix-haskell--running-processes prop nil))
     (kill-buffer err))
    (_
     (display-buffer err)
     (error "Running nix-haskell failed to instantiate")))
  (unless (process-live-p proc)
    (kill-buffer (process-buffer proc))))

(defun nix-haskell--get-pkg-db (callback)
  "Get a package-db async.
CALLBACK called once the package-db is determined."
  (let ((cabal-file (haskell-cabal-find-file default-directory))
	nix-file package-name root)

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
    ;; (unless cabal-file (error "Cannot find a valid .cabal file"))
    (when cabal-file
      (setq package-name (replace-regexp-in-string ".cabal$" "" (file-name-nondirectory cabal-file)))

      (unless root
        (setq root (file-name-directory cabal-file)))

      ;; Look for shell.nix or default.nix
      (unless (and nix-file (file-exists-p nix-file))
        (setq nix-file (expand-file-name "shell.nix" root)))
      (unless (and nix-file (file-exists-p nix-file))
        (setq nix-file (expand-file-name "default.nix" root)))

      (let ((cache (lax-plist-get nix-haskell--package-db-cache cabal-file)))
        (when cache (apply callback (cdr cache)))

        (when (or (not cache)
                  (> (float-time) (+ (car cache) nix-haskell-ttl))
                  (> (time-to-seconds
                      (nth 5 (file-attributes cabal-file)))
                     (car cache)))
          (let* ((data (lax-plist-get nix-haskell--running-processes cabal-file))
	         (stdout (generate-new-buffer
		          (format "*nix-haskell-instantiate-stdout<%s>*" cabal-file)))
	         (stderr (generate-new-buffer
		          (format "*nix-haskell-instantiate-stderr<%s>*" cabal-file)))
	         (command (list nix-instantiate-executable
			        "-E" nix-haskell-pkg-db-expr
			        "--argstr" "cabalFile" cabal-file
			        "--argstr" "packageName" package-name)))

            (when nix-haskell-verbose
              (message "Running nix-instantiate for %s..." cabal-file))

            (when (and nix-file (file-exists-p nix-file))
              (when nix-haskell-verbose
                (message "Found Nix file at %s." nix-file))
              (setq command
                    (append command (list "--argstr" "nixFile" nix-file))))

	    ;; Pick up projects with custom package sets. This is
	    ;; required for some important projects like those based on
	    ;; Obelisk or reflex-platform.
            (cond
             ((file-exists-p (expand-file-name "reflex-platform.nix" root))

              (when nix-haskell-verbose
                (message "Detected reflex-platform project."))

	      (setq command
		    (append command
			    (list "--arg" "haskellPackages"
			          (format "(import %s {}).ghc"
				          (expand-file-name "reflex-platform.nix"
							    root))))))

             ((file-exists-p (expand-file-name ".obelisk/impl/default.nix" root))

              (when nix-haskell-verbose
                (message "Detected obelisk project."))

	      (setq command
		    (append command
			    (list "--arg" "haskellPackages"
			          (format "(import %s {}).haskellPackageSets.ghc"
				          (expand-file-name ".obelisk/impl/default.nix"
							    root))))))

             ;; ((and (file-exists-p (expand-file-name "default.nix" root))
             ;;       (not (string= (expand-file-name "default.nix" root)
             ;;                     nix-file)))
             ;;  (when nix-haskell-verbose
             ;;    (message "Detected default.nix."))
	     ;;  (setq command
	     ;;        (append command
	     ;;             (list "--arg" "haskellPackages"
	     ;;                   (format "(import %s {})"
	     ;;                           (expand-file-name "default.nix"
	     ;;                                             root))))))
             )

	    (setq nix-haskell--running-processes
	          (lax-plist-put nix-haskell--running-processes
			         cabal-file (cons callback data)))

            ;; (when nix-haskell-verbose
            ;;   (message "Running %s." command))

	    (make-process
	     :name (format "*nix-haskell*<%s>" cabal-file)
	     :buffer stdout
	     :command command
	     :noquery t
	     :sentinel (apply-partially 'nix-haskell--instantiate-sentinel
				        cabal-file stderr)
	     :stderr stderr))))))

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

        (when nix-haskell-verbose
          (message "nix-haskell succeeded in buffer."))

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
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path (format "%s/bin" out) t)
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
	  (flycheck-mode 1)

          (when nix-haskell-auto-create-session
            (let ((haskell-process-load-or-reload-prompt nil))
              (haskell-session-new-assume-from-cabal)))))
    (let ((stderr (generate-new-buffer
		   (format "*nix-haskell-store<%s>*" drv))))
      (make-process
       :name (format "*nix-haskell-store<%s>*" drv)
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
