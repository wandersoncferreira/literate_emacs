;;; Here be dragons!!
;; Time-stamp: "2018-01-20 14:57:39 wanderson"

;;; packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :defer t)

;;; constants
(defconst isOSX (eq system-type 'darwin))
(defconst isUnix (eq system-type 'gnu/linux))

;;; user interface
(use-package base16-theme :ensure t)
(load-theme 'base16-google-light t)

;; removing distractive ui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; improve the frame title in Emacs to show the path to file visited
(when (display-graphic-p)
  (setq frame-title-format
	    '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;; cursor like a thin bar. Idk why I got used to it.
(when (display-graphic-p)
  (setq-default cursor-type 'bar))

;; more info into the modeline
(column-number-mode +1)
(display-time-mode +1)

;; there is a need to change some default behaviors
(setq-default tab-always-indent 'complete
              tab-width 4
              indent-tabs-mode nil
              require-final-newline t
              auto-save-default nil
              auto-save-list-file-prefix nil
              set-mark-command-repeat-pop t
              indicate-empty-lines t
              truncate-partial-width-windows nil
              kill-ring-max 100
              search-whitespace-regexp ".*?"
              x-select-enable-clipboard t
              select-active-regions t
              save-interprogram-paste-before-kill t
              yank-pop-change-selection t
              shift-select-mode nil
              delete-by-moving-to-trash t
              echo-keystrokes 0.1
              recenter-positions '(top middle bottom))

;; backup
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory)))
      backup-by-copying-when-linked t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil
      version-control t)

;; functions to find functions =)
(defun lgm/describe-func ()
  "Jump to Elisp functions."
  (interactive)
  (describe-function (function-called-at-point)))

(defun lgm/jump-to-elisp-func-def ()
  "Jump to Elisp definitions."
  (interactive)
  (find-function (function-called-at-point)))

(global-set-key (kbd "C-h C-j") 'lgm/jump-to-elisp-func-def)
(global-set-key (kbd "C-h C-f") 'lgm/describe-func)

;; smart `beginning-of-line'
(defadvice move-beginning-of-line (around smarter-bol activate)
  "Move to requested line if needed."
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))


;; expand region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; pop to mark advice
;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy -- skip past
(defadvice pop-to-mark-command (around ensure-new-poisition activate)
  "Continue popping until the cursor move."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; activate some modes
(electric-pair-mode t)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'pending-delete-mode)
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)
(show-paren-mode t)
(subword-mode t)

;; emacs export path correctly
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Users/wandersonferreira/dotfiles/scripts")

;; uniquify
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; saving all buffers from emacs when frame lose focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; better movement around buffers
(windmove-default-keybindings)

;; outside border to make it better in fullscreen mode
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; elisp be less conservative
(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-lisp-eval-depth (* 15 max-lisp-eval-depth))

;; encoding
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8) 
(set-keyboard-coding-system 'utf-8) 
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; time-stamp: " "
(require 'time-stamp)
(setq time-stamp-line-limit 20)
(add-hook 'before-save-hook #'time-stamp)

;; revert buffers
(require 'autorevert)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; saveplace
(if (version< emacs-version "25.1")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode t)
  (setq save-place-file "~/.emacs.d/saveplace.log"))

;; savehist
(setq savehist-file "~/.emacs.d/savehist.log")
(savehist-mode 1)
(setq history-length t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
	  '(kill-ring
	    search-ring
	    regexp-search-ring))

;;; git
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind
  (("C-c m s" . magit-status)))


;; flyspell
(use-package flyspell
  :diminish flyspell-mode
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list")
  (if isOSX
      (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-program-name "/usr/bin/aspell"))
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-correct
    :ensure t
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

;;; completions
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode +1))


(use-package counsel
  :ensure t
  :config
  (use-package smex :ensure t :config (smex-initialize))
  (add-hook 'after-init-hook 'counsel-mode)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)))

;;; Python mode
(use-package python
  :mode ("\\.py$\\'" . python-mode)
  :init
  (setq python-shell-completion-native-enable nil))

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :after python
  :config
  (elpy-enable))

;;; Mac OSX specific settings
(when isOSX
  (require 'ls-lisp)
  (setq ns-pop-up-frames nil
        trash-directory "~/.Trash/emacs"
        ls-lisp-use-insert-directory-program t
        mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smoth-scroll nil
        mouse-wheel-scroll-amount '(5 ((shift) . 2))
        mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        insert-directory-program "/usr/local/bin/gls"
        epg-gpg-program "gpg"
        epa-pinetry-mode 'loopback)

  (setenv "GPG_AGENT_INFO" nil)
  (add-to-list 'exec-path "/usr/local/bin")
  (setq bk/default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font bk/default-font))

;;; abbreviation mode
(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq save-abbrevs 'silently)
  
  (unless (file-exists-p abbrev-file-name)
    (with-temp-buffer (write-file abbrev-file-name)))

  (quietly-read-abbrev-file)
  
  :config
  (add-hook 'prog-mode-hook #'abbrev-mode)
  (add-hook 'text-mode-hook #'abbrev-mode))


;;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching nil
        projectile-completion-system 'ivy
        projectile-sort-order 'recently-active
        projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  :config
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (projectile-global-mode +1))


;;; ibuffer
(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-projectile-set-filter-groups)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))

;;; deft
(use-package deft
  :ensure t
  :init
  (setq deft-extensions '("org")
        deft-default-extension "org"
        deft-directory "~/Dropbox/notes"
        deft-recursive t
        deft-auto-save-interval 0)
  :bind
  ("<f6>" . deft)
  :config
  (add-hook 'deft-mode-hook (lambda () (visual-line-mode +1))))

;;; avy
(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  ("C-:" . avy-goto-char-timer))


;; shackles
(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '((help-mode :select t)
                        (compilation-mode :noselect t :align t :size 0.3)))
  :config
  (add-hook 'after-init-hook #'shackle-mode))


;; typographical editing
(use-package typo
  :ensure t
  :diminish typo-global-mode
  :config
  (add-hook 'after-init-hook 'typo-global-mode))


;;; shebang
(use-package insert-shebang
  :ensure t
  :init
  (setq insert-shebang-ignore-extensions '("txt" "org" "el")))


;;; bash completions
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)

;;; tldr - concise man pages
(use-package tldr
  :ensure t
  :init
  (setq tldr-enabled-categories '("common" "linux" "osx")))


;;; dumb-jump
(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-selector 'ivy)
  :config
  (bind-key* "M-g o" 'dumb-jump-go-other-window)
  (bind-key* "M-g j" 'dumb-jump-go)
  (bind-key* "M-g i" 'dumb-jump-go-prompt)
  (bind-key* "M-g x" 'dumb-jump-go-prefer-external)
  (bind-key* "M-g z" 'dumb-jump-go-prefer-external-other-window))

;;; ace-link
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default)
  :bind (:map org-mode-map
              ("M-o" . ace-link-org)))

;;; restclient
(use-package restclient :ensure t)


;;; company
(use-package company
  :ensure t
  :diminish " CMP"
  :init
  (setq company-transformers '(company-sort-by-occurrence)
        company-idle-delay nil
        company-minimum-prefix-length 0
        company-echo-delay 0)
  :config
  (global-company-mode +1)
  :bind
  ("M-o" . company-complete))

(use-package company-flx
  :ensure t
  :after company
  :config
  (company-flx-mode +1))

;;; dired --- really nice directory editing experience
(use-package dired
  :preface
  (defun dired-back-to-top ()
    "Function to jump dired to the right place at the top."
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))

  (defun dired-jump-to-bottom ()
    "Function to jump dired to the right place at the bottom."
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  
  :init

  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'top
        dired-dwim-target t
        dired-listing-switches "-alGhvF --group-directories-first")
  :config

  (add-hook 'dired-mode-hook #'(lambda ()
                                 (visual-line-mode 0)
                                 (linum-mode 0)
                                 (auto-revert-mode)))

  (define-key dired-mode-map (vector 'remap 'end-of-buffer)
    'dired-jump-to-bottom)

  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer)
    'dired-back-to-top))

;;; dired font lock
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode +1))

;; dired sort
(use-package dired-sort
  :ensure t)


;;; Go mode
(use-package go-mode
  :ensure t
  :preface
  
  (defun bk/set-go-compiler ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go run"))
    (local-set-key (kbd "M-p") 'compile))

  :init
  (if (not isOSX)
      (progn
        (add-to-list 'exec-path "/home/wanderson/go/bin")
        (setenv "GOPATH" "/home/wanderson/go"))
    (setenv "GOPATH" "/Users/wandersonferreira/go")
    (add-to-list 'exec-path "/Users/wandersonferreira/go/bin"))

  (setq gofmt-command "goimports")
  
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'bk/set-go-compiler)
  
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c i" . go-goto-imports)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (dired-sort diredfl company-flx restclient ace-link dumb-jump tldr insert-shebang typo shackle avy deft projectile flyspell-correct magit expand-region elpy smex counsel ivy diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
