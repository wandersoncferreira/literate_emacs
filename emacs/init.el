;;; Here be dragons!!
;; Time-stamp: "2018-01-21 10:13:00 wandersonferreira"

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
(defconst isEmacs25 (>= emacs-major-version 25))

;;; user interface
(use-package base16-theme :ensure t)
(load-theme 'base16-google-light t)

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
  :after flyspell
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

(use-package pythonic
  :ensure t
  :after python
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
  (add-hook 'go-mode-hook 'flycheck-mode)
  
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c i" . go-goto-imports)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)))

;;; company go
(use-package company-go
  :ensure t
  :after go-mode
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
                                    '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\|THE HORROR\\)"
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

;;; let's always start on eshell?
(eshell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (go-direx go-gopath go-eldoc go-add-tags go-stacktracer company-go exec-path-from-shell gist volatile-highlights voletile-highlights highlight-numbers idle-highlight-mode json-mode yafolding whitespace-cleanup-mode electric-operator pythonic dired-sort diredfl company-flx restclient ace-link dumb-jump tldr insert-shebang typo shackle avy deft projectile flyspell-correct magit expand-region elpy smex counsel ivy diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
