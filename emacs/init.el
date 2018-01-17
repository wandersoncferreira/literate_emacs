;;; Here be dragons!!


;;; packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :defer t)


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
