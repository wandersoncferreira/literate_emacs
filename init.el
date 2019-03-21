;; -*- lexical-binding: t -*-
;;; init.el --- Emacs

;;; Commentary:

;; Author: Wanderson Ferreira
;; Emacs user since 2017

;;; Code:

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (bk--file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.6
        file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist bk--file-name-handler-alist))))

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(package-initialize)

(defconst init-osx? (eq system-type 'darwin))

(defconst setting-dir (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)

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
(require 'setup-hydras)

(when init-osx?
  (require 'setup-mac)
  (bk/install-maybe 'exec-path-from-shell)
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES"))
  (exec-path-from-shell-initialize))

(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-mode-mapping)
(require 'setup-keybindings)
(require 'setup-snippets)
(require 'setup-paredit)
(require 'setup-grep)
(require 'setup-advices)
(require 'setup-expand)
(require 'setup-modeline)
(require 'setup-docker)
(require 'setup-company)

;;; programming languages
(require 'setup-clojure)
(require 'setup-php)
(require 'setup-go)

(eval-after-load "python" '(require 'setup-python))
(eval-after-load "js" '(require 'setup-javascript))
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

;; variables configured via the interactive `customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;;; start eshell
(eshell)

(provide 'init.el)
;;; init.el ends here
