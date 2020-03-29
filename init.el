;;; init.el --- My configs -*- lexical-binding: t -*-

;;; Commentary:

;; Do what you want, but learn your tool very very well.  This is my
;; attempt to keep my emacs-fu sharpe.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defvar my-external-packages '(cider
			       smartparens
			       clojure-mode
			       magit
			       change-inner
			       smart-shift
			       flycheck
			       flycheck-clj-kondo
			       smex
			       docker
			       docker-tramp
			       dockerfile-mode
			       docker-compose-mode
			       eshell-bookmark
			       projectile
			       tomatinho
			       expand-region
			       restclient
			       json-mode
			       multiple-cursors
			       markdown-mode
			       night-owl-theme))

(dolist (pkg my-external-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; add new paths to emacs
(setenv "PATH" (concat (getenv "PATH") ":/home/wand/scripts"))
(setq exec-path (append exec-path '("/home/wand/scripts")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(smex-initialize)

;; help to change text
(global-smart-shift-mode t)

;; docker
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("DockerfileDev\\'" . dockerfile-mode))

(defun bk/dockerfile-add-build-args ()
  "Add env variables to your docker build."
  (interactive)
  (let* ((vars (read-from-minibuffer "sequence of <envName>=<envValue>: "))
	 (split-vars (split-string vars " ")))
    (setq dockerfile-build-args nil)
    (dolist (v split-vars)
      (add-to-list 'dockerfile-build-args v))
    (setq docker-build-history-args vars)))

(require 'docker-compose-mode)
(add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.yml\\'" . docker-compose-mode))

(require 'docker)

(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
	 (idg (shell-command-to-string "id -g"))
	 (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=300 CURRENT_UID=$(id -u):$(id -g) done!")))

(global-set-key (kbd "C-c d") 'docker)

(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

(defun bk/docker-cleanup-buffers ()
  "Delete all the docker buffers created."
  (interactive)
  (kill-matching-buffers "docker" nil t))

;; git
(require 'magit)
(add-to-list 'magit-no-confirm 'stage-all-changes)

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
(set-register ?k '(file . "~/.emacs.d/docs/keys.org"))

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
(show-paren-mode)
(delete-selection-mode)
(pending-delete-mode)
(global-eldoc-mode t)

(require 'ido)
(setq ido-use-virtual-buffers t)
(setq ido-use-faces nil)

(ido-mode t)
(ido-everywhere t)
(recentf-mode t)

(add-hook 'text-mode-hook #'auto-fill-mode)

(require 'smartparens)
(setq sp-highlight-pair-overlay nil)

(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(with-eval-after-load "smartparens"
  ;; remove some pairs
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)

  ;; include new wrap of pairs
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "[" "]" :wrap "M-[")
  
  (sp-use-smartparens-bindings)		;enable default smartparens bindings

  (sp-local-tag 'markdown-mode "c" "```clojure" "```")
  (sp-local-tag 'markdown-mode "e" "```elisp" "```")
  (sp-local-tag 'markdown-mode "b" "```bash" "```")
  (sp-local-tag 'markdown-mode "p" "```python" "```")

  (define-key smartparens-mode-map (kbd "M-p") 'sp-prefix-pair-object))

(add-hook 'clojure-mode-hook 'eldoc-mode)

(require 'projectile)
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line-prefix "Proj"))
(projectile-mode)

(setq tab-always-indent 'complete)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq custom-safe-themes t)

(require 'em-alias)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")))

(defvar mode-line-cleaner-alist
  `((paredit-mode . " π ")
    (eldoc-mode . "")
    (auto-fill-mode . "")
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

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Github flavored markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


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

;; integrate emacs kill ring with the system clipboard.
(setq save-interprogram-paste-before-kill t)

;; aesthetics
(load-theme 'night-owl t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-indentation smart-shift night-owl-theme minimal-theme warm-night-theme monochrome-theme humanoid-themes nord-theme mlso-theme gotham-theme eshell-bookmark docker docker-compose-mode dockerfile-mode fantom-theme markdown-mode smartparens flycheck-clj-kondo flycheck multiple-cursors restclient color-theme-sanityinc-tomorrow json-mode tomatinho smex projectile paredit magit cider change-inner)))
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
	   (\`
	    (((\,
	       (concat "("
		       (regexp-opt
			(quote
			 ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
			t)
		       "\\_>"))
	      1
	      (quote font-lock-variable-name-face)))))
     (cider-docker-translations
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
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
