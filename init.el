;;; init.el --- Emacs

;;; Commentary:

;; Author: Wanderson Ferreira
;; Emacs user since 2017

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)

(package-initialize)

(defconst init-osx? (eq system-type 'darwin))

(defconst setting-dir (expand-file-name "settings" user-emacs-directory))
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)
(load custom-file :noerror)

(defconst defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(load-packages-from-folder "maintainer")
(load-packages-from-folder "third-party")

(require 'setup-package)
(require 'setup-appearance)
(require 'setup-maintainer)
(require 'setup-defaults)
(require 'setup-ido)
(require 'setup-org)
(require 'setup-dired)
(require 'setup-buffers)
(require 'setup-bk-mode)
(require 'setup-git)
(require 'setup-eshell)

(when init-osx?
  (require 'setup-mac)
  (bk/install-maybe 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-multiple-cursors)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-mode-mapping)
(require 'setup-keybindings)
(require 'setup-snippets)
(require 'setup-paredit)
(require 'setup-grep)
(require 'setup-advices)
(require 'setup-expand)
(require 'setup-docker)
(require 'setup-flycheck)
(require 'setup-company)

;;; programming languages
(eval-after-load "python" '(require 'setup-python))
(eval-after-load "js" '(require 'setup-javascript))
(eval-after-load "clojure-mode" '(require 'setup-clojure))
(eval-after-load 'php-mode '(require 'setup-php))
(eval-after-load 'typescript-mode '(require 'setup-typescript))
(eval-after-load "tex" '(require 'setup-latex))

(defalias 're 'restart-emacs)
(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(require 'server)
(unless (server-running-p)
  (server-start))

(load-file "~/.emacs.d/bk-after-init.el")

(provide 'init.el)
;;; init.el ends here
