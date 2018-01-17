;;; Here be dragons!!


;;; packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :defer t)

;;; user interface
(use-package base16-theme :ensure t)
(load-theme 'base16-google-light t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (display-graphic-p)
  (setq frame-title-format
	    '((:eval (if (buffer-file-name)
			 (abbreviate-file-name (buffer-file-name))
		       "%b")))))

(when (display-graphic-p)
  (setq-default cursor-type 'bar))

(column-number-mode +1)
(display-time-mode +1)


;;; completions
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-height 8)
  (setq ivy-use-virtual-buffers t
	    ivy-current-matching nil
	    ivy-minibuffer-faces nil
	    ivy-fixed-height-minibuffer t
	    ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :bind (:map ivy-minibuffer-map
		  ("C-j" . ivy-immediate-done)
		  ("RET" . ivy-alt-done))
  :config
  (add-hook 'after-init-hook 'ivy-mode))


(use-package counsel
  :ensure t
  :config
  (use-package smex :ensure t :config (smex-initialize))
  (add-hook 'after-init-hook 'counsel-mode)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (smex counsel ivy diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
