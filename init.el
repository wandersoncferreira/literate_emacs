(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(setq my-external-packages '(cider
			     paredit
			     clojure-mode
			     magit
			     change-inner
			     smex
			     projectile))

(dolist (pkg my-external-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'smex)
(smex-initialize)

(global-set-key "\C-x3" (lambda ()
			  (interactive)
			  (split-window-horizontally)
			  (other-window 1)))

(global-set-key "\C-x2" (lambda ()
			  (interactive)
			  (split-window-vertically)
			  (other-window 1)))

(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; disable modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; enable modes
(electric-pair-mode)
(show-paren-mode)
(projectile-mode)
(delete-selection-mode)
(pending-delete-mode)
(ido-mode)
(ido-everywhere)

(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-mode-line-prefix "Proj")

(setq tab-always-indent 'complete)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")))

(setq mode-line-cleaner-alist
      `((paredit-mode . " π ")
	(eldoc-mode . "")
	(auto-revert-mode . "")
	(clojure-mode . "λ")
	(emacs-lisp-mode . "λ")))

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
	   do (let* ((mode (car cleaner))
		     (mode-str (cdr cleaner))
		     (old-mode-str (cdr (assq mode minor-mode-alist))))
		(when old-mode-str
		  (setcar old-mode-str mode-str))
		(when (eq mode major-mode)
		  (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((cider-docker-translations
      ("/app/src" . "/home/wand/platform/banker/src")
      ("/app/test" . "/home/wand/platform/banker/test"))
     (cider-docker-translations
      ("/app/src" . "/home/wand/platform/register/src"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
