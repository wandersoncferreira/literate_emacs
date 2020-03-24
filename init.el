;;; init.el -- Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; The beginning of a Literate Emacs setup
;; Heavily inspired by the work of Protesilaos on Emacs

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

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; initialize the packages, avoiding a re-initialisation
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; make sure `use-package' is available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'dash))

;; configure `use-package' prior to loading it
(eval-and-compile
  (setq use-package-always-ensure nil
	use-package-always-defer nil
	use-package-always-demand nil
	use-package-expand-minimally nil
	use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

;;; init.el ends here
