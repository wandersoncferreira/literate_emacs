;;; package --- Emacs settings
;;; Commentary:

;; Here be dragons!!
;; Time-stamp: "2018-01-28 19:45:27 wandersonferreira"

;;; Code:

;;; Garbage collector:
(setq gc-cons-threshold (* 10 1024 1024))
(setq ac-redefinition-accept 'accept)

;;; Packages:
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; installing the `use-package' if not enabled!
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

(use-package diminish :ensure t :defer t)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; emacs outline mode
(use-package outline
  :diminish outline-minor-mode
  :init
  (setq-default outline-regexp "^;;; ")
  (setq-default outline-heading-end-regexp ":\n")
  :config
  (outline-minor-mode))


;; I need this to work!
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$\'" . org-mode)
  :defer t)

;;; Constants:
(defconst init-isOSX (eq system-type 'darwin))
(defconst init-isUnix (eq system-type 'gnu/linux))
(defconst init-isEmacs25 (>= emacs-major-version 25))

;; load custom themes
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; case insensitive
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;; User Interface:

(use-package base16-theme :ensure t)
(setq custom-safe-themes t)
(load-theme 'default-black t)


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

;; use dialog boxes
(setq use-dialog-box nil)
(setq use-file-dialog nil)

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


;;** time display
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-world-list '(("Etc/GMT+2" "São Paulo")
                                ("America/New_York" "New York")
                                ("Europe/Italy" "Milan")))
(display-time-mode +1)

(setq ring-bell-function 'ignore)

;; setup minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

;; there is a need to change some default behaviors
(setq-default tab-always-indent 'complete
              tab-width 4
              indent-tabs-mode nil
              require-final-newline t
              auto-save-default nil
              auto-save-list-file-prefix nil
              set-mark-command-repeat-pop t
              register-preview-delay nil
              indicate-empty-lines t
              truncate-partial-width-windows nil
              kill-ring-max 100
              global-mark-ring-max 200
              message-log-max 1000
              truncate-lines t
              search-whitespace-regexp ".*?"
              x-select-enable-clipboard t
              select-active-regions t
              save-interprogram-paste-before-kill t
              yank-pop-change-selection t
              shift-select-mode nil
              delete-by-moving-to-trash t
              echo-keystrokes 0.1
              recenter-positions '(top middle bottom))

;; killing text
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


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

(global-set-key (kbd "C-x p") 'pop-to-mark-command)

;; alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; activate some modes
(electric-pair-mode t)
(add-hook 'after-init-hook #'delete-selection-mode)
(add-hook 'after-init-hook #'pending-delete-mode)
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)
(diminish 'auto-revert-mode)
(subword-mode t)
(diminish 'subword-mode)

;; show parenthesis mode
(use-package paren
  :init
  (setq show-paren-ring-bell-on-mismatch t)
  :config
  (show-paren-mode +1))


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

;; case regions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; remove selected region if typing
(pending-delete-mode +1)

;; disable bidi
(setq-default bidi-display-reordering nil)

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
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
	  '(kill-ring
	    search-ring
	    regexp-search-ring))

;; ediff
(use-package ediff
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally)))


;; Minibuffer editing - more space
;; this binds C-M-e in the minibuffer so that you can edit the contents of the minibuffer before submitting it.
(use-package miniedit
  :ensure t
  :commands minibuffer-edit)


;;; Undo tree mode:
;; People often struggle with the Emacs undo model, where there's really no concept of "redo" - you simply undo the undo. This lets you use C-x u to visually walk through the changes you've made

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode +1))

;;; Git:

(use-package magit
  :ensure t
  :commands (magit-status)
  :init
  (setq magit-item-highlight-face 'bold)
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


;;; Gist:

(use-package gist
  :ensure t
  :commands gist)


;; flyspell
(use-package flyspell
  :diminish flyspell-mode
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list")
  (if init-isOSX
      (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-program-name "/usr/bin/aspell"))
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-.") nil))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous-word-generic)))

;;; Completions:

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

;;; Python mode:

(use-package python
  :ensure t
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

;;; Mac OSX specific settings:

;; switch the cmd and meta keys
(when init-isOSX
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil))

;; menu bar is not anoyying in OSX
(when init-isOSX
  (menu-bar-mode 1))


(defun bk/osx-default-font ()
  "Set the default font for OSX."
  (interactive)
  (setq bk/default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font bk/default-font))

(defun bk/source-code-pro ()
  "Define Source Code fonts."
  (interactive)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 140))


(when init-isOSX
  (require 'ls-lisp)
  (setq ns-pop-up-frames nil
        delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs"
        ls-lisp-use-insert-directory-program t
        mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smoth-scroll nil
        mouse-wheel-scroll-amount '(5 ((shift) . 2))
        insert-directory-program "/usr/local/bin/gls"
        epg-gpg-program "gpg"
        epa-pinetry-mode 'loopback)
  (bk/source-code-pro)
  (setenv "GPG_AGENT_INFO" nil)
  (add-to-list 'exec-path "/usr/local/bin"))


(defun bk/finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file!"))))

;;; Abbreviation mode:

(defvar abbrev-data '(("wo" . "without")
                      ("bc" . "because")
                      ("dot" . "http://github.com/wandersoncferreira/dotfiles")))
(dolist (var abbrev-data)
  (let ((base (car var))
        (expansion (cdr var)))
    (pp expansion)
    (define-global-abbrev base expansion)))
(message "Done setting your abbreviations!!")

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


;;; Projectile:

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching nil
        projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-verbose nil
        projectile-sort-order 'recently-active
        projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (setq projectile-switch-project-action
        (lambda () (dired (projectile-project-root))))
  :config
  (projectile-global-mode +1))


;;; Ibuffer:

(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-projectile-set-filter-groups)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))

;;; Deft:

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

;;; Avy:

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
  (setq shackle-rules '((help-mode :select t :align t :size 0.3)
                        (compilation-mode :noselect t :align t :size 0.3)
                        (special-mode-hook :select t :align t :size 0.3)))
  :config
  (add-hook 'after-init-hook #'shackle-mode))


;; typographical editing
(use-package typo
  :ensure t
  :diminish typo-global-mode
  :config
  (add-hook 'after-init-hook 'typo-global-mode))


;;; Shebang:

(use-package insert-shebang
  :ensure t
  :init
  (setq insert-shebang-ignore-extensions '("txt" "org" "el")))


;;; Bash Completions:
(add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)

;;; Tldr - concise man pages:

(use-package tldr
  :ensure t
  :init
  (setq tldr-enabled-categories '("common" "linux" "osx")))


;;; Dumb-jump:

(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-selector 'ivy)
  :config
  (define-key prog-mode-map (kbd "M-.") 'dumb-jump-go)
  (define-key prog-mode-map (kbd "M-,") 'dumb-jump-back))


;;; Ace-link:

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default)
  :bind (:map org-mode-map
              ("M-o" . ace-link-Org)))

;;; Restclient:
(use-package restclient :ensure t)


;;; Company mode:

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))
  :init
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-require-match 'never)
  (setq company-show-numbers t)
  :config
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  (global-company-mode +1))

;; company flx matching
(use-package company-flx
  :ensure t
  :config
  (company-flx-mode +1))

;;; Dired --- really nice directory editing experience:
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
        dired-auto-revert-buffer t
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

;; dired font lock
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode +1))

;; direx
(use-package go-direx :ensure t)


;;; GO mode:

;; packages that you need to install on your OS-box
;; go get github.com/nsf/gocode
;; go get github.com/rogpeppe/godef
;; go get golang.org/x/tools/cmd/goimports
;; go get golang.org/x/tools/cmd/godoc

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
(when init-isUnix
  (add-to-list 'exec-path "/home/wanderson/go/bin")
  (setenv "GOPATH" "/home/wanderson/go"))

(when init-isOSX
  (add-to-list 'exec-path "/Users/wandersonferreira/go/bin")
  (setenv "GOPATH" "/Users/wandersonferreira/go"))

;; company go
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

;; go-playground
(use-package go-playground :ensure t)

;;; Custom functions:

;; insert date
(defun bk/insert-date (prefix)
  "Function to insert the current date.
With PREFIX-argument, use ISO format."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%d.%m.%Y")
                 (t "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun bk/eval-buffer ()
  "Function to evaluate the current buffer."
  (interactive)
  (eval-buffer)
  (message "Your buffer was evaluated!"))

(defun bk/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

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

(defun bk/dired-open-marked-files ()
  "Open marked files."
  (interactive)
  (let ((distinguish-one-marked nil))
    (mapc 'find-file
          (dired-map-over-marks (dired-get-file-for-visit)
                                current-prefix-arg))))

(defun bk/beautiful-json (beg end)
  "Function to fix JSON objects between region BEG and END."
  (interactive "r")
  (shell-command-on-region beg end "python -mjson.tool" (current-buffer) 'replace))

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

;; write your own lookup command
(require 'browse-url)
(defun bk/lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))))

(global-set-key (kbd "<f8>") 'bk/lookup-wikipedia)

;;; Tramp mode:
(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist backup-directory-alist
        tramp-use-ssh-controlmaster-options "ssh"))

;;; Load Secrets:
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

;;; Recentf:
(use-package recentf
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 1000
        recentf-filename-handlers '(file-truename)
        recentf-exclude '("^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                          "^/var/folders/.+$"))
  :config
  (add-hook 'after-init-hook 'recentf-mode))

;;; Programming mode:
;; watch out words
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\)"
                                       1 font-lock-warning-face t)))))

;; underscore -> UPCASE -> CamelCase conversion of names
(use-package string-inflection
  :ensure t
  :config
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'string-inflection-all-cycle))


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

;; aliases
(defalias 'dtw 'delete-trailing-whitespace)

;; indent tools
(use-package indent-tools
  :ensure t
  :bind
  ("C-c >" . indent-tools-hydra/body))

(use-package whitespace
  :init
  (setq whitespace-line-column 150
        whitespace-auto-cleanup t
        whitespace-style '(face trailing lines space-before-tab empty
                                indentation space-after-tab)))

;; show trailing whitespaces
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; folding code
(use-package yafolding
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (yafolding-mode))))

;;; Yasnippet:
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-verbosity 0)
  (setq yas-triggers-in-field t)
  :config
  (yas-global-mode +1)
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("M-i" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :defer 4)

;;; Try:
(use-package try :ensure t)

;;; Json mode:
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2))))

;;; Rst mode:
(use-package rst
  :ensure t
  :config
  (add-hook 'rst-mode-hook 'auto-fill-mode))


;;; Line number:
;; very smart way for dealing with line number
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


;; misc
(setq track-eol t
      line-move-visual nil)

;;; Environment variables:
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

;;; Eshell:
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

;;; ERC mode:
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

(defun bk/ERC ()
  "Start the ERC correctly."
  (interactive)
  (bk/login-irc)
  (erc))

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

;;; Flycheck settings:

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

;;; Markdown mode:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; Fix-words:

(use-package fix-word
  :ensure t
  :bind
  (("M-u" . fix-word-upcase)
   ("M-l" . fix-word-downcase)
   ("M-c" . fix-word-capitalize)))

;;; Global keybindings:

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
(global-set-key (kbd "C-c d") 'bk/duplicate-current-line-or-region)
(global-set-key (kbd "\C-x2") (lambda () (interactive)
                                (split-window-vertically) (other-window 1)))
(global-set-key (kbd "\C-x3")
                (lambda () (interactive)
                  (split-window-horizontally) (other-window 1)))

;; describe personal bindings
(global-set-key (kbd "C-h C-b") 'describe-personal-keybindings)

;; repeat command like Vim
(global-set-key (kbd "C-.") 'repeat-complex-command)

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


;;; SQL mode:

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


;;; Server mode in Emacs:

(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (server-start)
                               (setq server-raise-frame t))))

;;; Uptimes - monitor for how long Emacs has been running!:
(use-package uptimes
  :ensure t)

;;; helm-spotify-plus:
(use-package helm-spotify-plus
  :ensure t)

;;; Mail settings:

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

;;; grep-folder:

(add-to-list 'load-path "~/.emacs.d/site-packages/grep-folder")
(require 'grep-folder)
(setq grep-folder-setup-dirs '(("~/.emacs.d" . ("var/" "etc/" ".cask/" ".git/" "site-packages" "elpa/" "themes/"))
                               ("~/myScripts")
                               ("~/go/src/github.com/Captalys")))
(setq grep-folder-setup-files '(("~/.emacs.d" . (".gitmodules"))))
(global-set-key (kbd "C-c g") 'grep-folder)

;;; Latex settings:

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

(if init-isOSX
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


;;; Org mode settings:

(require 'org)
(setq-default org-confirm-babel-evaluate nil
              org-return-follows-link t
              org-log-done t
              org-edit-timestamp-down-means-later t
              org-catch-invisible-edits 'show
              org-tags-column 100
              org-startup-indented t
              org-completion-use-ido t
              org-startup-folded t
              org-cycle-separator-lines 0
              org-image-actual-width nil)

;; interactions
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "firefox %s")
        ("\\.pdf\\'" . "evince \"%s\"")))

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

(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/dotfiles/agenda/notes.org" "Notes")
         "** Note: %?\n")

        ("l" "Link" entry (file+headline "~/dotfiles/agenda/links.org.gpg" "Links")
         "** %? %^L %^g \n%T" :prepend t)

        ("t" "To Do Item" entry (file+headline "~/dotfiles/agenda/todo.org.gpg" "To Do Items")
         "** TODO %?\n" :prepend t)))

(defun bk/org-notes ()
  "Function to open my personal org notes file."
  (interactive)
  (find-file "~/dotfiles/agenda/notes.org"))

(defun bk/org-links ()
  "Function to open all my personal links saved."
  (interactive)
  (find-file "~/dotfiles/agenda/links.org.gpg"))

;;;###autoload
(defun bk/org-todo ()
  "Function to open my `TODO' list."
  (interactive)
  (find-file "~/dotfiles/agenda/todo.org.gpg"))

(setq org-agenda-files '("~/dotfiles/agenda"))

;; org download package
(use-package org-download
  :ensure t
  :init
  (setq org-download-image-dir "~/ORGIMG"))

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

;;; Jekyll - Blogging made easy!:

(use-package org2jekyll
  :ensure t
  :init
  (custom-set-variables '(org2jekyll-blog-author "Wanderson Ferreira")
                        '(org2jekyll-source-directory (expand-file-name "~/blogging"))
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


;;; Edit indirect:

(use-package edit-indirect
  :ensure t
  :commands edit-indirect)

;;; imenu everywhere:

(use-package imenu-anywhere
  :ensure t
  :bind
  ("C-c C-j" . ivy-imenu-anywhere)
  ("C-c j" . ivy-imenu-anywhere))

;;; Ranger:
(use-package ranger
  :ensure t)

;;; emacs profiling:
(use-package esup
  :ensure t
  :commands esup)

;;; Move text:

;; In order to use the move-text, M-Up and M-Down.
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;;; Additional packages:

;; boxquote
(use-package boxquote
  :ensure t)

;; google this error, forecast, line, yank....
(use-package google-this
  :ensure t)

;; smartscan
;; this makes M-n and M-p look for the symbol at point.
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode +1))

;; artbollocks-mode
;; Emacs minor mode for avoiding cliches and bad grammar when writing
(use-package artbollocks-mode
  :ensure t
  :init
  (setq artbollocks-weasel-words-regex
        (concat "\\b" (regexp-opt
                       '("one of the"
                         "should"
                         "just"
                         "sort of"
                         "probably"
                         "maybe"
                         "perhaps"
                         "I think"
                         "really"
                         "nice"
                         "utilize"
                         "pretty"))))
  :config
  (add-hook 'text-mode-hook 'artbollocks-mode))

;; hippie expand
(use-package hippie-expand
  :init
  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-line
          try-expand-all-abbrevs
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ))
  :bind
  ("M-/" . hippie-expand))

;; restart emacs
(use-package restart-emacs :ensure t)


;; golden ratio
(use-package golden-ratio
  :diminish golden-ratio-mode
  :ensure t
  :init
  (setq golden-ratio-recenter t)
  :config
  (golden-ratio-mode +1))


;;; Custom file:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
