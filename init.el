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

(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-git))
(eval-after-load 'eshell '(require 'setup-eshell))

(when init-osx?
  (require 'setup-mac)
  (bk/install-maybe 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'setup-company)
(require 'setup-google)
(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-multiple-cursors)
(require 'setup-editing)
(require 'setup-pdf)
(require 'setup-misc)
(require 'setup-prodigy)
(require 'setup-mode-mapping)
(require 'setup-keybindings)
(require 'setup-erc)
(require 'setup-snippets)
(require 'setup-paredit)
(require 'setup-flycheck)
(require 'setup-elfeed)
(require 'setup-help)
(require 'setup-grep)
(require 'setup-advices)
(require 'setup-expand)
(require 'setup-docker)
(require 'setup-web)

;;; programming languages
(eval-after-load "python" '(require 'setup-python))
(eval-after-load "js" '(require 'setup-javascript))
(eval-after-load "clojure-mode" '(require 'setup-clojure))
(eval-after-load 'ruby-mode '(require 'setup-ruby))
(eval-after-load "php-mode" '(require 'setup-php))
(eval-after-load 'typescript-mode '(require 'setup-typescript))
(eval-after-load 'go-mode '(require 'setup-go))
(eval-after-load 'sml-mode '(require 'setup-sml))
(eval-after-load "tex" '(require 'setup-latex))
(eval-after-load "cc-mode" '(require 'setup-c))


(defalias 're 'restart-emacs)
(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp "Query replace regexp")

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init.el)
;;; init.el ends here
