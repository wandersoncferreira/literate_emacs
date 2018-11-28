;;; init.el setup
(package-initialize)

;; remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(defconst osx? (eq system-type 'darwin))

(setq setting-dir (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)
(load custom-file :noerror)

(require 'setup-appearance)
(require 'setup-defaults)
(require 'setup-package)
(require 'setup-ido)
(require 'setup-dired)
(require 'setup-eshell)
(require 'setup-git)
(require 'setup-org)

;; load all my functions
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(when osx?
  (require 'setup-mac)
  (bk/install-maybe-require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-multiple-cursors)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-prodigy)
(require 'setup-mode-mapping)
(require 'setup-keybindings)

;; programming languages
(require 'setup-snippets)
(require 'setup-lisp)
(require 'setup-paredit)

(eval-after-load 'clojure-mode '(require 'setup-clojure))
(eval-after-load 'python '(require 'setup-python))
(eval-after-load 'php-mode '(require 'setup-php))

;; emacs on server mode
(require 'server)
(unless (server-running-p)
  (server-start))

(eshell)
