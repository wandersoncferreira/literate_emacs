;;; init.el --- Emacs settings -*- lexical-binding: t -*-

;;; Commentary:

;; Author: Wanderson Ferreira
;; Emacs user since 2017

;;; Code:

;;; 0. Faster startup
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

(setenv "BROWSER" "qutebrowser")

(setq site-run-file nil)

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst setting-dir (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path setting-dir)
(require 'setup-package)

(defconst defuns-dir (expand-file-name "defuns" user-emacs-directory))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-appearance)
(require 'setup-defaults)
(require 'setup-completion)
(require 'setup-dired)
(require 'setup-git)
(require 'setup-eshell)
(require 'setup-hydras)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES" "JAVA_HOME" "PATH"))
  (exec-path-from-shell-initialize))

(require 'setup-org)
(require 'setup-company)
(require 'setup-projectile)
(require 'setup-editing)
(require 'setup-media)
(require 'setup-misc)
(require 'setup-keybindings)
(require 'setup-snippets)
(require 'setup-paredit)
(require 'setup-grep)
(require 'setup-advices)
(require 'setup-markdown)
(require 'setup-clojure)
(require 'setup-scala)
(require 'setup-python)
(require 'setup-latex)
(require 'setup-cheatsheet)

(defalias 're 'restart-emacs)
(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(require 'server)
(unless (server-running-p) (server-start))

;; variables configured via the interactive `customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;;; conclude init by setting up specifics for the current user
(setq user-settings-dir (concat user-emacs-directory "users/"))
(add-to-list 'load-path user-settings-dir)
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;; disabled command
(put 'narrow-to-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
