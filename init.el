;;; init.el setup
(package-initialize)

;; remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(setq osx? (eq system-type 'darwin))
(setq setting-dir (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)
(load custom-file :noerror)

(require 'setup-appearance)
(require 'setup-defaults)
(require 'setup-package)

(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'eshell '(require 'setup-eshell))
(eval-after-load 'magit '(require 'setup-git))
(eval-after-load 'org '(require 'setup-org))

(when osx?
  (require 'setup-mac)
  (bk/install-maybe-require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-keybindings)
(require 'setup-functions)
(require 'setup-multiple-cursors)
(require 'setup-editing)
(require 'setup-prodigy)
(require 'setup-mode-mapping)

;; programming languages
(require 'setup-snippets)
(require 'setup-lisp)

(eval-after-load 'clojure-mode '(require 'setup-clojure))
(eval-after-load 'python '(require 'setup-python))

;; emacs on server mode
(require 'server)
(unless (server-running-p)
  (server-start))

(eshell)
(provide 'init)
