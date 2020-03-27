;;; init.el --- My configs
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defvar my-external-packages '(cider
			       paredit
			       clojure-mode
			       magit
			       change-inner
			       flycheck
			       flycheck-clj-kondo
			       smex
			       projectile
			       tomatinho
			       expand-region
			       restclient
			       json-mode
			       multiple-cursors
			       color-theme-sanityinc-tomorrow))

(dolist (pkg my-external-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'smex)
(smex-initialize)

(require 'dired-x)

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
(global-set-key (kbd "<f10>") 'tomatinho)
(global-set-key (kbd "C-c t") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/org/todo.org"))


;; `C-a' first takes you to the first non-whitespace char as
;; `back-to-indentation' on a line, and if pressed again takes you to
;; the actual beginning of the line.
(defun smarter-move-beginning-of-line (arg)
  "Move depending on ARG to beginning of visible line or not.
From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; disable modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; enable modes
(electric-pair-mode)
(show-paren-mode)
(delete-selection-mode)
(pending-delete-mode)
(ido-mode)
(ido-everywhere)

(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

(require 'projectile)
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line-prefix "Proj"))
(projectile-mode)

(setq tab-always-indent 'complete)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(require 'em-alias)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")))

(defvar mode-line-cleaner-alist
  `((paredit-mode . " π ")
    (eldoc-mode . "")
    (auto-revert-mode . "")
    (clojure-mode . "λ")
    (emacs-lisp-mode . "λ")))

(defun clean-mode-line ()
  "Clean your modeline."
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
	   do (let* ((mode (car cleaner))
		     (mode-str (cdr cleaner))
		     (old-mode-str (cdr (assq mode minor-mode-alist))))
		(when old-mode-str
		  (setcar old-mode-str mode-str))
		(when (eq mode major-mode)
		  (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook #'clean-mode-line)

(require 'org)
(require 'org-capture)
(setq org-directory "/home/wand/org")
(setq org-agenda-files (list "/home/wand/org/todo.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(D)" "|" "CANCELLED(C)")
			  (sequence "STUDY(s)" "|" "STUDIED(S)")
			  (sequence "ACT(a)" "|" "ACTED(A)")))
(setq org-capture-templates
      '(("c" "Capture some concise actionable item and exist" entry
	 (file+headline "todo.org" "Task list without a defined date")
	 "* TODO [#B] %^{Title}\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i %l" :immediate-finish t)
	("t" "Task of importance with a tag, deadline, and further editable space" entry
	 (file+headline "todo.org" "Task list with a date")
	 "* %^{Scope of task||TODO [#A]|STUDY [#A]|Act on} %^{Title} %^g\n DEADLINE: %^t\n :PROPERTIES:\n :CONTEXT: %a\n:CAPTURED: %U\n :END:\n\n %i %?")))

;;; imenu - produces menus for accessing locations in documents
;; for source-code buffer the locations to index are typically definitions
;; of functions, variables, and so on.
(require 'imenu)

(defun ido-menu--read (index-alist &optional prompt)
  "Show imenu INDEX-ALIST on ido interface as PROMPT."
  (let* ((symatpt (thing-at-point 'symbol))
         (names (mapcar 'car index-alist))
         (name (ido-completing-read (or prompt "imenu ") names
                                    nil t nil nil nil))
         (choice (assoc name index-alist)))
    (if (imenu--subalist-p choice)
        (ido-menu--read (cdr choice) prompt nil)
      choice)))

(defun bk/ido-menu ()
  "Public interface to my custom imenu through ido."
  (interactive)
  (let ((index-alist (cdr (imenu--make-index-alist))))
    (if (equal index-alist '(nil))
	(message "No imenu tags in buffer")
      (imenu (ido-menu--read index-alist nil)))))

(global-set-key (kbd "C-.") 'bk/ido-menu)

(add-to-list 'exec-path "/home/wand/scripts")

(require 'url-http)
(defvar url-http-end-of-headers)
(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let* ((endpoint "https://api.ipify.org")
	 (myip (with-current-buffer (url-retrieve-synchronously endpoint)
		 (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
    (kill-new myip)
    (message "IP: %s" myip)))


(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'sanityinc-tomorrow-night t)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


;;; after calling the `org-todo', the org mode tries to store some
;;; sort of a "note" using `org-store-log-note' function. I want that
;;; every modification done in my todo file save the file right after.
(advice-add 'org-deadline :after (lambda (&rest _rest)
				   (org-save-all-org-buffers)))
(advice-add 'org-schedule :after (lambda (&rest _rest)
				   (org-save-all-org-buffers)))
(advice-add 'org-todo :after (lambda (&rest _rest)
			       (org-save-all-org-buffers)))
(advice-add 'org-store-log-note :after (lambda (&rest _rest)
					 (org-save-all-org-buffers)))


(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-flycheck-mode)

(require 'flycheck-clj-kondo)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages
   (quote
    (flycheck-clj-kondo flycheck multiple-cursors restclient color-theme-sanityinc-tomorrow json-mode tomatinho smex projectile paredit magit cider change-inner)))
 '(safe-local-variable-values
   (quote
    ((cider-docker-translations
      ("/app/src" . "/home/wand/platform/seu-barriga/src")
      ("/app/test" . "/home/wand/platform/seu-barriga/test"))
     (cider-docker-translations
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

(provide 'init)
;;; init.el ends here
