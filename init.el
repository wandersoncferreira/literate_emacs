;;; init.el --- Emacs

;;; Commentary:

;; Author: Wanderson Ferreira
;; Emacs user since 2017

;;; Code:

(package-initialize)

(defconst init-osx? (eq system-type 'darwin))
(defconst setting-dir (expand-file-name "settings" user-emacs-directory))
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)
(load custom-file :noerror)

(require 'setup-appearance)
(require 'setup-defaults)
(require 'setup-package)
(require 'setup-ido)
(require 'setup-eshell)
(require 'setup-git)
(require 'setup-dired)
(require 'setup-org)

(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(when init-osx?
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
(require 'setup-erc)
(require 'setup-snippets)
(require 'setup-lisp)
(require 'setup-paredit)
(require 'setup-flycheck)
(require 'setup-elfeed)
(require 'setup-help)
(require 'setup-grep)
(require 'setup-expand)
(require 'setup-web)
(require 'setup-clojure)
(require 'setup-python)

(require 'setup-php)

(require 'server)
(unless (server-running-p)
  (server-start))

(eshell)

(provide 'init.el)
;;; init.el ends here
