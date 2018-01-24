;;; package --- Emacs settings
;;; Commentary:

;; Here be dragons!!
;; Time-stamp: "2018-01-24 17:01:24 wanderson"

;;; Code:


;;; packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :defer t)

;; I need this to work!
(use-package org
  :ensure org-plus-contrib)

;;; constants
(defconst isOSX (eq system-type 'darwin))
(defconst isUnix (eq system-type 'gnu/linux))
(defconst isEmacs25 (>= emacs-major-version 25))


;;; user interface
(use-package base16-theme :ensure t)
(setq custom-safe-themes t)
;; (load-theme 'base16-google-light t) ;; I also like this team, however is time to let the dark option grow a little more.

(load-theme 'base16-dracula t)

;; I don't like of too much things happening when I open Emacs
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "iagwanderson@gmail.com")

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

(setq ring-bell-function 'ignore)

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
(diminish 'subword-mode)

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
  :init
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silently)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes))
  :bind
  (("C-c m s" . magit-status)))

(defun my/magit-cursor-fix ()
  "Fix the cursor on magit buffers."
  (goto-char (point-min))
  (when (looking-at "#")
    (forward-line 2)))

(defun bk/ignore-and-pull ()
  "Function to ignore all the modified files in current repo and sync with upstream."
  (interactive)
  (magit-reset-hard "HEAD~1")
  (magit-pull "origin/master" "-v")
  (message "Your repository is synced with your master branch."))

(defun bk/commit-pull-push ()
  "Function to stage all modified fiels and sync with upstream."
  (interactive)
  (magit-stage-modified)
  (let* ((msg-string (read-string "Commit message: ")))
    (magit-commit (concat "-m " msg-string))
    (magit-pull "origin/master" "-v")
    (magit-push "master" "origin/master" "-v")
    (message "Your repository is synced with the master branch.")))


;;; gist
(use-package gist
  :ensure t
  :init
  (setq gist-list-format
        '((id "Id" 10 nil identity)
          (created "Created" 20 nil "%D %R")
          (visibility "Visibility" 10 nil
                      (lambda
                        (public)
                        (or
                         (and public "public")
                         "private")))
          (description "Description" 60 nil identity)
          (files "Files" 0 nil
                 (lambda
                   (files)
                   (mapconcat 'identity files ", "))))))


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
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

;;; completions
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-height 8
        ivy-use-virtual-buffers t
        ivy-current-matching nil
        ivy-wrap t
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-fixed-height-minibuffer t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode +1)
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done)))


(use-package counsel
  :ensure t
  :config

  (use-package smex
    :ensure t
    :init
    (setq smex-completion-system 'ivy)
    :config (smex-initialize))
  
  (add-hook 'after-init-hook 'counsel-mode)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)))

;; global visual line
(global-visual-line-mode +1)
(diminish 'visual-line-mode)

;;; Python mode
(use-package python
  :mode ("\\.py$\\'" . python-mode)
  :init
  (setq python-shell-completion-native-enable nil))

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :config
  (elpy-enable)
    (delete `elpy-module-highlight-indentation elpy-modules))

(use-package pythonic
  :ensure t
  :preface
  (defun set-right-python-environment ()
    "Activate the right python env depending where I am."
    (cond ((string= (system-name) "suse-captalys")
           (pythonic-activate "~/miniconda3/envs/captalys")
           (message "Python environment Captalys was activated!"))
          ((string= (system-name) "Home")
           (pythonic-activate "~/miniconda3")
           (message "Python environment Miniconda Default was activated!"))
          ((string= (system-name) "Wandersons-Air")
           (pythonic-activate "~/miniconda3")
           (message "Python environment Miniconda Default was activated!"))
          ))
  :config
  (set-right-python-environment))

(use-package electric-operator
  :ensure t
  :config
  (add-hook 'python-mode-hook #'electric-operator-mode))

;; nice functions from lgmoneda! I am just renaming to improve the chance I will find it later. LOL
(defun bk/python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
    (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
    (kill-process (get-buffer-process "*Python*"))
    (sleep-for 0.5))
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  (unless (get-buffer-window "*Python*" t)
    (python-shell-switch-to-shell)))

(defun bk/python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-c") 'bk/python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'bk/python-shell-run-region)
     (define-key python-mode-map (kbd "C-c C-i") 'pyimport-insert-missing)))

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
        deft-directory "~/dotfiles/notes"
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
  :diminish company-mode
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


;;; GO MODE
(use-package go-mode
  :ensure t
  :preface
  
  (defun bk/set-go-compiler ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go run"))
    (local-set-key (kbd "M-p") 'compile))

  :init
  (setq gofmt-command "goimports")
  :config
  (use-package go-guru
    :ensure t)
  
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'bk/set-go-compiler)
  
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c i" . go-goto-imports)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)))

;; activate Go on Linux
(when isUnix
  (add-to-list 'exec-path "/home/wanderson/go/bin")
  (setenv "GOPATH" "/home/wanderson/go"))

(when isOSX
  (add-to-list 'exec-path "/Users/wandersonferreira/go/bin")
  (setenv "GOPATH" "/Users/wandersonferreira/go"))

;;; company go
(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

;; go stackstracer
(use-package go-stacktracer :ensure t)

;; go add tags
(use-package go-add-tags :ensure t)

;; enable go-eldoc
(use-package go-eldoc
  :ensure t
  :diminish eldoc-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; gopath
(use-package go-gopath :ensure t)

;; direx
(use-package go-direx :ensure t)

;; go-playground
(use-package go-playground :ensure t)

;;; custom functions
(defun bk/eval-buffer ()
  "Function to evaluate the current buffer."
  (interactive)
  (eval-buffer)
  (message "Your buffer was evaluated!"))

(defun bk/duplicate-line ()
  "Function to duplicate the current line."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun bk/mac-open-files ()
  "Open files in OSX."
  (interactive)
  (shell-command (concat "open" (buffer-file-name))))

(defun bk/nicklist-toggle ()
  "Function to see the nicklist panel with all members logged in irc channel."
  (interactive)
  (let ((nicklist-buffer-name (format " *%s-nicklist*" (buffer-name))))
	(if (get-buffer nicklist-buffer-name)
	    (kill-buffer nicklist-buffer-name)
	  (erc-nicklist))))

(defun bk/indent-buffer ()
  "Function to indent the whole buffer altogether."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun bk/nuke-all-buffers ()
  "Delete all open buffers."
  (interactive)
  (mapc
   (lambda (buffer)
	 (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

(defun bk/delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	    (buffer (current-buffer))
	    (name (buffer-name)))
	(if (not (and filename (file-exists-p filename)))
	    (error "Buffer '%s' is not visiting a file!" name)
	  (when (yes-or-no-p "Are you sure you want to remove this file? ")
	    (delete-file filename)
	    (kill-buffer buffer)
	    (message "File '%s' successfully removed" filename)))))

(defun bk/untabify-buffer ()
  "Remove the tab character from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun bk/create-scratch nil
  "Create a new scratch buffer to work in."
  (interactive)
  (let ((n 0)
	    bufname)
	(while (progn
             (setq bufname (concat "*scratch*"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
	(switch-to-buffer (get-buffer-create bufname))
	(emacs-lisp-mode)))

;; extracted from Wiegley dotfiles
(defun bk/normalize-file ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (goto-char (point-max))
    (delete-blank-lines)
    (set-buffer-file-coding-system 'unix)
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match ""))
    (set-buffer-file-coding-system 'utf-8)
    (let ((require-final-newline t))
      (save-buffer))))


(defun bk/number-of-packages ()
  "Function to return the number of active packages."
  (interactive)
  (let ((size (length package-activated-list)))
    (message (concat "The number of activated packages are: " (number-to-string size)))))

;;; Tramp mode
(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist backup-directory-alist
        tramp-use-ssh-controlmaster-options "ssh"))

;;; load my secrets folder
(let ((secrets-dir (concat user-emacs-directory "secrets/")))
  (unless (file-exists-p secrets-dir)
    (make-directory secrets-dir)))

(defvar authinfo (concat user-emacs-directory "secrets/authinfo.gpg"))
(defvar auth-sources '((:source "~/.emacs.d/secrets/authinfo.gpg")))

(require 'netrc)
(defun get-authinfo (host)
  "Function to get login and password given a HOST name."
  (let* ((entry (netrc-parse authinfo))
         (hostentry (netrc-machine entry host)))
    (when hostentry
      hostentry)))

;;; recentf
(use-package recentf
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 1000
        recentf-filename-handlers '(file-truename)
        recentf-exclude '("^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                          "^/var/folders/.+$"))
  :config
  (add-hook 'after-init-hook 'recentf-mode))

;;; programming mode
;; watch out words
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\)"
                                       1 font-lock-warning-face t)))))

(use-package goto-addr
  :init
  (setq goto-address-mail-face 'link)
  :config
  (add-hook 'prog-mode-hook 'goto-address-prog-mode))

;; cleaning up the whitespaces
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'whitespace-cleanup-mode))

(use-package whitespace
  :init
  (setq whitespace-line-column 150
        whitespace-auto-cleanup t
        whitespace-style '(face trailing lines space-before-tab empty
                                indentation space-after-tab)))

;;; folding code
(use-package yafolding
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (yafolding-mode))))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("M-i" . yas-expand)))

;; json mode
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2))))

;; rst mode
(use-package rst
  :ensure t
  :config
  (add-hook 'rst-mode-hook 'auto-fill-mode))


;;; very smart way for dealing with line number
;; line number
(add-hook 'after-init-hook 'global-linum-mode)

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      profiler-report-mode
                                      ffip-diff-mode
                                      dictionary-mode
                                      erc-mode
                                      dired-mode
                                      help-mode
                                      text-mode
                                      fundamental-mode
                                      inferior-python-mode
                                      inferior-scheme-mode
                                      ivy-occur-grep-mode ; for better performance
                                      compilation-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      gnus-group-mode
                                      inf-ruby-mode
                                      org-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      term-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)

;; updated line number every second
(setq linum-delay t)
(setq linum-format "%3d ")
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.5 nil #'linum-update-current))

;;; highlights
(use-package idle-highlight-mode
  :ensure t
  :diminish hi-lock-mode
  :config
  (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode +1))))

;; highlight numbers
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; volatile highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (add-hook 'after-init-hook #'volatile-highlights-mode))

;; misc
(setq track-eol t
      line-move-visual nil)

;;; environment variable
(use-package exec-path-from-shell
  :ensure t
  :preface
  (defun set-exec-path-from-shell-PATH ()
    "Set the environment variables."
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$"
                            ""
                            (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq eshell-path-env path-from-shell) ; for eshell users
      (setq exec-path (split-string path-from-shell path-separator))))
  :config
  (exec-path-from-shell-initialize)
  (when window-system (set-exec-path-from-shell-PATH)))

;; eshell
(use-package eshell
  :init
  (setq eshell-where-to-jump 'begin
        eshell-kill-processes-on-exit t
        eshell-destroy-buffer-when-process-dies t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input t
        eshell-scroll-to-bottom-on-output t
        eshell-history-size 1024
        eshell-error-if-no-glob t
        eshell-save-history-on-exit t)
  (ignore-errors
    (dolist (i (list 'eshell-handle-ansi-color
                     'eshell-handle-control-codes
                     'eshell-watch-for-password-prompt))
      (add-to-list 'eshell-output-filter-functions i)))
  :config
  (require 'em-smart))

(defun eshell/clear ()
  "Function to clear eshell buffer."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(eval-after-load 'em-term (lambda ()
                            (setq eshell-visual-commands '("less" "more" "tmux" "htop" "top" "ranger"))
                            (setq eshell-visual-subcommands '(("git" "log" "l" "diff" "show")))))


(defun eshell-here ()
  "Open up a new shell in the directory associated with current buffer's file.
The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
	     (height (/ (window-total-height) 2))
	     (name   (car (last (split-string parent "/" t)))))
	(split-window-vertically (- height))
	(other-window 1)
	(eshell "new")
	(rename-buffer (concat "*eshell: " name "*"))
	(insert (concat "ls"))
	(eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(add-hook 'eshell-mode-hook
	      (lambda ()
            (add-to-list 'eshell-visual-commands "htop")
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")))

(defun eshell/gst (&rest args)
  "Function to use magit status (ARGS)."
  (magit-status (pop args) nil)
  (eshell/echo))

;; non-zero value for line-spacing can mess up ansi-term and so.
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))

;; finally load eshell on startup
(add-hook 'emacs-startup-hook (lambda ()
                                (let ((default-directory (getenv "HOME")))
                                  (command-execute 'eshell))))

;; some aliases
(setq eshell-command-aliases-list
      '(("q" "exit")
        ("l" "ls -1")
        ("ll" "ls -l")
        ("la" "ls -la")
        ("emacs" "find-file $1")))

;;; ERC
(defvar site-packages
  (expand-file-name "site-packages" user-emacs-directory))
(add-to-list 'load-path (concat site-packages "/erc-extras") t)

(erc-spelling-mode +1)
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
	      '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

;; stop starting all the channels
;; (when (assoc "en0" (network-interface-list))
;;   (erc :server "irc.freenode.net" :port 6667 :nick "bartuka"))

(require 'erc)
(require 'erc-track)
(erc-track-mode +1)
(setq erc-keywords '("machine" "bayesian" "cassandra" "bayes" "models" "scala"))
(setq erc-track-exclude-server-buffer t)

(defun bk/login-irc ()
  "Connecting to my IRC account."
  (let* ((hostentry (get-authinfo "irc.freenode.net"))
	     (login (netrc-get hostentry "login"))
	     (password (netrc-get hostentry "password")))
	(setq erc-nick login)
	(setq erc-password password)))

(bk/login-irc)

(setq erc-current-nick-highlight-type 'nick)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
	  '(erc-current-nick-face
	    erc-keyword-face
	    erc-direct-msg-face))
(setq erc-track-priority-faces-only 'all)

(require 'erc-services)
(erc-services-mode)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-prompt-for-password nil)


(erc-autojoin-mode +1)
(setq erc-autojoin-timing :ident)
(setq erc-join-buffer 'bury)
(setq erc-autojoin-channels-alist
	  '(("freenode.net" "#sptk" "#emacs" "#go-nuts" "#tensorflow")))

(erc-autojoin-after-ident "irc.freenode.net" "bartuka")
(add-hook 'erc-nickserv-identified-hook 'erc-autojoin-after-ident)

;; login in a buffer when people talk to me
(setq erc-log-matches-flag t)
(setq erc-log-matches-types-alist
	  '((keyword . "### Keywords Log ###")
	    (current-nick . "### Me Log ###")))

(load-file (concat site-packages "/erc-extras/erc-nicklist.el"))
(define-key erc-mode-map (kbd "<f7>") 'bk/nicklist-toggle)

;; Smarter beep
;; Remember to apt-get install mplayer!
;; todo: no beep when buffer is visible
(eval-when-compile
  '(require 'cl))

(add-hook 'erc-text-matched-hook 'erc-sound-if-not-server)
(defun erc-sound-if-not-server (match-type nickuserhost msg)
  "Beep when MATCH-TYPE NICKUSERHOST MSG were called."
  (unless (or
	       (string-match "Serv" nickuserhost)
	       (string-match nickuserhost (erc-current-nick))
	       (string-match "Server" nickuserhost))
    (when (string= match-type "current-nick")
      (start-process-shell-command "lolsound" nil "mplayer ~/.emacs.d/sounds/icq-message.wav"))
    (message
     (format "[%s|<%s:%s> %s]"
             (format-time-string "%Hh%M" (date-to-time (current-time-string)))
             (subseq nickuserhost 0 (string-match "!" nickuserhost))
             (or (erc-default-target) "")
             (subseq msg 0 (- (length msg) 1))
             )
     ;; Show msg for 20s
     (run-with-timer 20 nil
                     (lambda ()
                       (message nil)))
     )))

;; logging is activated
(require 'erc-log)
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps t)

;; Flycheck settings
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode))

;; goflycheck
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(require 'go-flycheck)

;;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; fix-words
(use-package fix-word
  :ensure t
  :bind
  (("M-u" . fix-word-upcase)
   ("M-l" . fix-word-downcase)
   ("M-c" . fix-word-capitalize)))

;;; KEYS

;; org
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n") 'org-search-view)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; fullscreen
(global-set-key (kbd "M-<f10>") 'toggle-frame-fullscreen)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


(global-set-key (kbd "C-x , b") 'bk/eval-buffer)
(global-set-key (kbd "C-c d") 'bk/duplicate-line)
(global-set-key (kbd "\C-x2") (lambda () (interactive)
                                (split-window-vertically) (other-window 1)))
(global-set-key (kbd "\C-x3")
                (lambda () (interactive)
                  (split-window-horizontally) (other-window 1)))

;; describe personal bindings
(global-set-key (kbd "C-h C-b") 'describe-personal-keybindings)

;; repeat command like Vim
(global-set-key (kbd "C-.") 'repeat)

;; kill grep
(defun bk/kill-grep ()
  (interactive)
  (kill-grep)
  (message "Grep was killed!"))

(global-set-key (kbd "C-c x") 'bk/kill-grep)

;; go to eshell faster
(defun bk/switch-to-eshell ()
  (interactive)
  (switch-to-buffer "*eshell*"))

(global-set-key (kbd "C-c e") 'bk/switch-to-eshell)


;;; SQL
(defun my-sql-connect (product connection)
  "Connect to sql using the encrypted passwords receive PRODUCT AND CONNECTION."
  
  (load-library "~/.emacs.d/secrets/databases.el.gpg")
  (load-library "~/.emacs.d/secrets/dbpass.el.gpg")
  
  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
	    (sql-password (car (last (assoc connection my-sql-password)))))
	(delete sql-password connection-info)
	(nconc connection-info `((sql-password ,sql-password)))
	(setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
	(add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))

(defun bk/mysql-AWS ()
  "Function to connect to MySQL database in AWS."
  (interactive)
  (my-sql-connect 'mysql 'mysqlAWS)
  (message "Connected to MySQL AWS database"))

(use-package sqlup-mode
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))


;; server mode in Emacs
(require 'server)
(unless (server-running-p)
  (server-start))

;; uptimes - monitor for how long Emacs has been running!
(use-package uptimes
  :ensure t)

;; helm-spotify-plus
(use-package helm-spotify-plus
  :ensure t)

;;; mail
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-auth-credentials (expand-file-name "~/.emacs.d/secrets/authinfo.gpg"))
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq starttls-extra-arguments nil
	  starttls-gnutls-program "/usr/bin/gnutls-cli"
	  starttls-use-gnutls t)
(setq message-signature "Wanderson Ferreira
    http://bartuka.com
    Sent from Emacs")

;;; grep-folder
(add-to-list 'load-path "~/.emacs.d/site-packages/grep-folder")
(require 'grep-folder)
(setq grep-folder-setup-dirs '(("~/.emacs.d" . ("var/" "etc/" ".cask/" ".git/" "site-packages" "elpa/" "themes/"))
                               ("~/myScripts")
                               ("~/go/src/github.com/Captalys")))
(setq grep-folder-setup-files '(("~/.emacs.d" . (".gitmodules"))))
(global-set-key (kbd "C-c g") 'grep-folder)

;;; Latex
(defun bk/template-latex-simple ()
  "Personal LaTeX template."
  (interactive)
  (insert "\\documentclass[11pt]{article}\n")
  (insert "\\usepackage[brazilian]{babel}\n")
  (insert "\\usepackage[utf8]{inputenc}\n")
  (insert "\\usepackage[T1]{fontenc}\n")
  (insert "\\usepackage{graphicx}\n")
  (insert "\\usepackage{cite}\n")
  (insert "\\title{INSERT YOUR TITLE HERE}\n")
  (insert "\\author{Wanderson Ferreira}\n")
  (insert "\\date{\\today}\n")
  (insert "\\begin{document}\n")
  (insert "\\maketitle\n\n\n")
  (insert "\\bibliographystyle{plain}\n")
  (insert "\\bibliography{NAME-BIB-FILE}\n")
  (insert "\\end{document}\n\n")
  (insert "This text will not show up in the output"))

(setq TeX-parse-self t
      TeX-electric-sub-and-superscript t
      TeX-electric-math '("\\(" "\\)")
      TeX-quote-after-quote t
      TeX-clean-confirm nil
      TeX-engine 'luatex
      TeX-PDF-mode t
      TeX-auto-save t
	  TeX-parse-self t
	  TeX-save-query nil)

(require 'flymake)
(defun flymake-get-tex-args (file-name)
  "Function to capture errors in flymake FILE-NAME."
  (list "pdflatex"
	    (list "-file-line-error" "-draftmode" "-interacton=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(if isOSX
    (progn
      (setq TeX-view-program-list
            (quote
             (("Skim"
               (concat "/Applications/Skim.app/"
                       "Contents/SharedSupport/displayline"
                       " %n %o %b")))))
      (setq TeX-view-program-selection
            (quote (((output-dvi style-pstricks) "dvips and gv")
                    (output-dvi "xdvi")
                    (output-pdf "Skim")
                    (output-html "xdg-open"))))))


(require 'org)
(setq-default org-confirm-babel-evaluate nil
              org-return-follows-link t
              org-log-done t
              org-edit-timestamp-down-means-later t
              org-catch-invisible-edits 'show
              org-tags-column 100
              org-startup-indented t
              org-startup-folded t
              org-cycle-separator-lines 0
              org-image-actual-width nil)

(add-to-list 'auto-mode-alist '("\\.txt$\'" . org-mode))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))

;; remove from the modeline
(require 'org-indent)

(require 'ob-scheme)
(require 'ob-python)
(require 'ob-emacs-lisp)
(require 'ob-shell)
(use-package ob-go :ensure t)

(org-babel-do-load-languages
 'org-babel-do-load-languages
 '((scheme . t)
   (python . t)
   (go . t)
   (sh . t)
   (emacs-lisp . t)))

(eval-after-load 'org
  '(add-to-list 'org-structure-template-alist
                (list "em" (concat
                            "#+BEGIN_SRC emacs-lisp\n"
                            "?\n"
                            "#+END_SRC"))))

(eval-after-load 'org
  '(add-to-list 'org-structure-template-alist
                (list "sc" (concat
                            "#+BEGIN_SRC scheme :tangle yes :noweb yes :results output\n"
                            "?\n"
                            "#+END_SRC"))))

(eval-after-load 'org
  '(add-to-list 'org-structure-template-alist
                (list "pl" (concat
                            "#+BEGIN_SRC python :noweb yes :tangle <FILENAME> :results output :exports both\n"
                            "?\n"
                            "#+END_SRC"
                            ))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "lightblue"    :weight bold)
              ("NEXT"      :foreground "red"          :weight bold)
              ("STARTED"   :foreground "red"          :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "orange"       :weight bold)
              ("TEAM"      :foreground "orange"       :weight bold)
              ("SOMEDAY"   :foreground "magenta"      :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("QUOTE"     :foreground "red"          :weight bold)
              ("QUOTED"    :foreground "magenta"      :weight bold)
              ("APPROVED"  :foreground "forest green" :weight bold)
              ("EXPIRED"   :foreground "forest green" :weight bold)
              ("REJECTED"  :foreground "forest green" :weight bold)
              ("OPEN"      :foreground "blue"         :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold))))
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/Agenda/notes.org" "Notes")
         "** Note: %?\n")
        
        ("l" "Link" entry (file+headline "~/Dropbox/Agenda/links.org" "Links")
         "** %? %^L %^g \n%T" :prepend t)

        ("t" "To Do Item" entry (file+headline "~/Dropbox/Agenda/todo.org" "To Do Items")
         "** TODO %?\n" :prepend t)))

(defun bk/org-notes ()
  "Function to open my personal org notes file."
  (interactive)
  (find-file "~/Dropbox/Agenda/notes.org"))

(defun bk/org-links ()
  "Function to open all my personal links saved."
  (interactive)
  (find-file "~/Dropbox/Agenda/links.org"))

;;;###autoload
(defun bk/org-todo ()
  "Function to open my `TODO' list."
  (interactive)
  (find-file "~/Dropbox/Agenda/todo.org"))

(setq org-agenda-files '("~/Dropbox/Agenda"))

;; org download package
(use-package org-download
  :ensure t
  :init
  (setq org-download-image-dir "~/Dropbox/ORGIMG"))

(defun bk/meeting-notes ()
  "Call this after creating an `org-mode' heading for where the notes for the meeting should be."
  (interactive)
  (outline-mark-subtree)
  (narrow-to-region (region-beginning) (region-end))
  (deactivate-mark)
  (delete-other-windows)
  (text-scale-set 1)
  (fringe-mode 0)
  (message "When finished taking your notes, run meeting-done."))

(defun bk/meeting-done ()
  "Attempt to undo the effects of taking meeting notes."
  (interactive)
  (widen)
  (text-scale-set 0)
  (fringe-mode 1)
  (winner-undo))

;; org reveal
(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t))

;;; jekyll
(use-package org2jekyll
  :ensure t
  :init
  (custom-set-variables '(org2jekyll-blog-author "Wanderson Ferreira")
                        '(org2jekyll-source-directory (expand-file-name "~/Dropbox/blogging"))
                        '(org2jekyll-jekyll-directory (expand-file-name "~/wandersoncferreira.github.io"))
                        '(org2jekyll-jekyll-drafts-dir "")
                        '(org2jekyll-jekyll-posts-dir "_posts/")
                        '(org-publish-project-alist
                          `(("default"
                             :base-directory ,(org2jekyll-input-directory)
                             :base-extension "org"
                             :publishing-directory ,(org2jekyll-output-directory)
                             :publishing-function org-html-publish-to-html
                             :headline-levels 4
                             :section-numbers nil
                             :with-toc nil
                             :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                             :html-preamble t
                             :recursive t
                             :make-index t
                             :html-extension "html"
                             :body-only t)
                            ("post"
                             :base-directory ,(org2jekyll-input-directory)
                             :base-extension "org"
                             :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
                             :publishing-function org-html-publish-to-html
                             :headline-levels 4
                             :section-numbers nil
                             :with-toc nil
                             :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                             :html-preamble t
                             :recursive t
                             :make-index t
                             :html-extension "html"
                             :body-only t)
                            ("images"
                             :base-directory ,(org2jekyll-input-directory "images")
                             :base-extension "jpg\\|gif\\|png"
                             :publishing-directory ,(org2jekyll-output-directory "images")
                             :publishing-function org-publish-attachment
                             :recursive t)
                            ("js"
                             :base-directory ,(org2jekyll-input-directory "js")
                             :base-extension "js"
                             :publishing-directory ,(org2jekyll-output-directory "js")
                             :publishing-function org-publish-attachment
                             :recursive t)
                            ("css"
                             :base-directory ,(org2jekyll-input-directory "css")
                             :base-extension "css\\|el"
                             :publishing-directory ,(org2jekyll-output-directory "css")
                             :publishing-function org-publish-attachment
                             :recursive t)
                            ("web" :components ("images" "js" "css")))))
  :config
  (add-hook 'org-mode-hook 'org2jekyll-mode))


;;; edit indirect
(use-package edit-indirect
  :ensure t)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
