(setq debug-on-error t
      debug-on-quit t)

(add-hook 'focus-out-hook #'garbage-collect)

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(setq user-full-name "Wanderson Ferreira"
      user-mail-address "wanderson.ferreira@protonmail.com")

(defconst custom-dir (expand-file-name "custom" user-emacs-directory))
(add-to-list 'load-path custom-dir)

(defalias 'cquit 'cider-quit)
(defalias 'ctest 'cider-test-run-test)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package cheatsheet 
  :ensure t
  :config
  (define-key cheatsheet-mode-map (kbd "q") 'delete-window)
  :bind
  ("C-h 0" . cheatsheet-show))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode +1))

(use-package cus-edit
  :custom
  (custom-file "~/.emacs.d/custom.el")
  :hook (after-init . (lambda ()
			(unless (file-exists-p custom-file)
			  (write-region "" nil custom-file))
			(load custom-file))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES" "JAVA_HOME" "PATH"))
  (exec-path-from-shell-initialize))

(defun bk/uptime-box ()
  "Uptime your OS."
  (interactive)
  (message
   (shell-command-to-string "uptime")))

(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let ((endpoint "https://api.ipify.org"))
    (message "IP: %s"
	     (with-current-buffer (url-retrieve-synchronously endpoint)
	       (buffer-substring (+ 1 url-http-end-of-headers) (point-max))))))

(add-hook 'after-init-hook 'delete-selection-mode)

(setq echo-keystrokes 0.02)

(fset #'yes-or-no-p #'y-or-n-p)

(setq idle-update-delay 1)

(add-hook 'after-init-hook #'global-auto-revert-mode)

(setq save-interprogram-paste-before-kill t)

(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(setq vc-make-backup-files t)

(setq create-lockfiles nil
      auto-save-default nil
      make-backup-files nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent 'complete
              fill-column 70)

(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil
              sentence-end-double-space nil
              delete-trailing-lines nil
              require-final-newline t
              tabify-regexp "^\t* [ \t]+")

(defun bk/auto-fill ()
  "My autofill setup for text buffers."
  (auto-fill-mode t)
  (diminish 'auto-fill-mode))

(add-hook 'text-mode-hook #'bk/auto-fill)

(setq blink-matching-paren nil)

(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-autosave-interval 60
      history-delete-duplicates nil
      savehist-save-minibuffer-history t
      history-length 30000
      savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode +1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-show-file-shortcuts-flag nil
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;;; rename entries in recentf when moving files in dired
(defun rjs/recentf-rename-directory (oldname newname)
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-prefix-p oldname name)
                      (concat newname (substring name (length oldname)))))
                recentf-list))
  (recentf-cleanup))

(defun rjs/recentf-rename-file (oldname newname)
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-equal name oldname)
                      newname
                    oldname))
                recentf-list))
  (recentf-cleanup))

(defun rjs/recentf-rename-notify (oldname newname &rest args)
  (if (file-directory-p newname)
      (rjs/recentf-rename-directory oldname newname)
    (rjs/recentf-rename-file oldname newname)))

(advice-add 'dired-rename-file :after #'rjs/recentf-rename-notify)


(defun contrib/recentf-add-dired-directory ()
  "Include Dired buffers in the `recentf' list."
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook 'contrib/recentf-add-dired-directory)

(global-hl-line-mode +1)

(setq shift-select-mode nil)

(setq show-trailing-whitespace t)

(setq-default ad-redefinition-action 'accept
              help-window-select t
              select-enable-clipboard t)

(setq ffap-machine-p-known 'reject)

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace"
        save-place-forget-unreadable-files t)
  (save-place-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator " * "
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t
        uniquify-ignore-buffers-re "^\\*"))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
       (quote (("Main"
                ("Directories" (mode . dired-mode))
                ("Programming" (or
                                (mode . c-mode)
                                (mode . clojure-mode)
                                (mode . emacs-lisp-mode)
                                (mode . python-mode)))
                ("Org" (mode . org-mode))
                ("Markdown" (mode . markdown-mode))
                ("Magit" (or
                          (mode . magit-blame-mode)
                          (mode . magit-cherry-mode)
                          (mode . magit-diff-mode)
                          (mode . magit-log-mode)
                          (mode . magit-process-mode)
                          (mode . magit-status-mode)))
                ("Emacs" (or
                          (name . "^\\*Help\\*$")
                          (name . "^\\*Custom.*")
                          (name . "^\\*Org Agenda\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*scratch\\*$")
                          (name . "^\\*Backtrace\\*$")
                          (name . "^\\*Messages\\*$")))))))
  :hook
  (ibuffer-mode . hl-line-mode)
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
  :bind
  (("C-x C-b" . ibuffer)))

(electric-indent-mode +1)

(add-hook 'text-mode-hook 'electric-quote-local-mode)

(electric-layout-mode +1)

(defun bk/clear-registers ()
  "Remove all saved registers."
  (interactive)
  (setq register-alist nil))

(require 'grep)

(setq grep-highlight-matches t
      grep-scroll-output t)

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "elpa")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories "node_modules")

     (define-key grep-mode-map "q" 'rgrep-quit-window)
     (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
     (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)))

(defun bk/rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows and searching for REGEXP.
in FILES and DIR without CONFIRM."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp))
            (dir (ido-read-directory-name "Base directory: "
                                          nil default-directory t))
            (confirm (equal current-prefix-arg '(4))))
       (list regexp files dir confirm))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (goto-char (point-min)))

(defun rgrep-quit-window ()
  "Simply jump to the register where all your windows are."
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  "Go to file and close rgrep window."
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

(setq auth-source-debug t)

(setq auth-sources '((:source "~/.emacs.d/secrets/.authinfo")))

(use-package emacs
  :custom
  (display-buffer-alist
   '(("\\*e?shell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.30)
      (side . bottom)
      (slot . -1))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.30)
      (side . bottom)
      (slot . 0))))
  :bind
  ("<f8>" . window-toggle-side-windows))

(use-package emacs
  :config
  (defun bk/window-dired-vc-root-left ()
    "Open root directory of current version-controlled repository
or the present working directory with `dired' and bespoke window
parametersg."
    (interactive)
    (let ((dir (if (eq (vc-root-dir) nil)
		   (dired-noselect default-directory)
		 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir `((side . left)
	     (slot . 0)
	     (window-width . 0.15)))
      (with-current-buffer dir
	(rename-buffer "*Dired-Side*"))))
  :bind
  ("C-c d" . bk/window-dired-vc-root-left))

(use-package winner
  :hook (after-init . winner-mode)
  :init
  (setq winner-boring-buffers
	'("*Completions*"
	  "*Compile-Log*"
	  "*inferior-lisp*"
	  "*Fuzzy Completions*"
	  "*Apropos*"
	  "*Help*"
	  "*cvs*"
	  "*Buffer List*"
	  "*Ibuffer*"
	  "*esh command on file*"))
  :bind (("C-x 4 u" . winner-undo)
	 ("C-x 4 U" . winner-redo)))

(global-set-key (kbd "<f9>") (lambda ()
			       (interactive)
			       (toggle-frame-fullscreen)
			       (sit-for 1)
			       (if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
				   (display-time-mode 1)
				 (display-time-mode 0))))

(setq help-window-select t)

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?h ?j ?k ?l ?y ?u ?i ?o ?p))
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?s aw-swap-window "swap window")
     (?2 aw-split-window-vert "split window vertically")
     (?3 aw-split-window-horz "split window horizontally")
     (?? aw-show-dispatch-help)))
  (aw-minibuffer-flag t)
  (aw-ignore-current nil)
  (aw-display-mode-overlay t)
  (aw-background t)
  :config
  (ace-window-display-mode -1)
  (global-set-key (kbd "M-o") 'ace-window))

(windmove-default-keybindings 'meta)

(cheatsheet-add-group 'Defaults
                      '(:key "C-x n s" :description "narrow region")
                      '(:key "M-s o" :description "fires occur")
                      '(:key "e (inside occur)" :description "turns into editable")
                      '(:key "= (inside occur)" :description "fires diff"))

(use-package paren
  :config
  (setq show-paren-delay 0.01
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

(use-package highlight-numbers
  :ensure t
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(electric-pair-mode)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq frame-title-format
      '("" invocation-name ": Bartuka - " (:eval (if (buffer-file-name)
                                                     (abbreviate-file-name (buffer-file-name))
                                                   "%b"))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))

(setq use-file-dialog nil
      use-dialog-box nil)

(require 'fira-code)
(add-hook 'org-mode-hook #'fira-code-enable)
(diminish 'ligature-font-mode)

;; (set-face-attribute 'default nil :height 100)

(use-package modus-operandi-theme
  :ensure t
  :config
  (load-theme 'modus-operandi t))

(defun bk/light-theme ()
  "Custom light theme option."
  (interactive)
  (load-theme 'modus-operandi t))

(defun bk/dark-theme ()
  "Custom dark theme option."
  (interactive)
  (load-theme 'modus-vivendi t))

(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode))

(use-package hydra
  :ensure t)

(use-package move-dup
  :ensure t
  :config
  (global-set-key [C-up] 'md/move-lines-up)
  (global-set-key [C-down] 'md/move-lines-down))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package restart-emacs :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (require 'ob-plantuml)
  (setq org-plantuml-jar-path "/home/wand/plantuml.jar"))

(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("Urban Dictionary" . [simple-query
						       "www.urbandictionary.com"
						       "http://www.urbandictionary.com/define.php?term="
						       ""])))

(global-set-key (kbd "C-c j") 'webjump)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(setq tab-always-indent 'complete)

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-x C-m" . smex)))

(use-package ido
  :init
  (setq ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1
	ido-use-filename-at-point 'guess
	ido-create-new-buffer 'always
	ido-use-virtual-buffers t
	ido-max-prospects 10
	ido-max-window-height 1
	ido-ignore-extensions t)
  (setq ido-file-extensions-order '(".clj" ".edn" ".org" ".md" ".el"))
  :config
  (ido-mode +1)
  (ido-everywhere +1)
  :bind (:map ido-common-completion-map
	      ("M-e" . ido-edit-input)
	      ("M-r" . ido-toggle-regexp)))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package crm-custom
  :ensure t
  :config
  (crm-custom-mode +1))

(use-package icomplete
  :config
  (icomplete-mode +1))

(defadvice ido-switch-buffer (around no-confirmation activate)
  (let ((confirm-nonexistent-file-or-buffer nil))
    ad-do-it))

(cheatsheet-add-group 'Ido
		      '(:key "C-b" :description "Reverts to the old 'switch-buffer' completion engine.")
		      '(:key "C-f" :description "Reverts to the old 'file-name' completion engine.")
		      '(:key "C-d" :description "Opens a dired buffer in the current directory.")
		      '(:key "C-a" :description "Toggles ignored files.")
		      '(:key "C-p" :description "Toggles prefix matching; match the beginning of a filename.")
		      '(:key "M-r" :description "Toggles matching by Emacs regexp.")
		      '(:key "C-k" :description "Kills the currently focused buffer or deletes the file")
		      '(:key "M-m" :description "Creates a new subdirectory to the directory you are in."))

(setq org-confirm-babel-evaluate nil)

(setq org-src-window-setup 'current-window)

(setq org-agenda-files (list "~/calendar-captalys.org"
                             "~/todo.org"))


(setq org-capture-templates
      '(("a" "Appointment Captalys" entry (file "~/calendar-captalys.org")
         "* %?\n\n%T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("t" "To Do Item" entry (file+headline
                                 "~/todo.org" "To Do")
         "* TODO %?\n%u" :prepend t)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-log-done 'time)

(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id (auth-source-pick-first-password
                            :host "gcal.com"
                            :user "client-id")
        org-gcal-client-secret (auth-source-pick-first-password
                                :host "gcal.com"
                                :user "client-secret")
        org-gcal-file-alist '(("wanderson.ferreira@captalys.com.br" . "~/gcal-captalys.org"))
        org-gcal-notify-p nil))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))

(add-to-list 'org-structure-template-alist 
	     (list "elisp" (concat "#+BEGIN_SRC emacs-lisp\n"
				   "?\n"
				   "#+END_SRC")))

(cheatsheet-add-group 'Org
		      '(:key "C-c C-w" :description "Move item to other location"))

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line-prefix ""
        projectile-completion-system 'ido
        projectile-globally-ignored-files '("TAGS" ".DS_Store")
        projectile-switch-project-action 'projectile-find-file
        projectile-globally-ignored-file-suffixes '(".csv" ".svg" ".pdf" ".asc" ".doc" ".docx" ".csv"))
  :config
  (projectile-mode +1))

(defhydra hydra-projectile (:color blue)
  ("q" nil "quit" :column "Projectile")

  ("b" projectile-switch-to-buffer "list" :column "Buffers")
  ("K" projectile-kill-buffers "kill all" :column "Buffers")
  ("S" projectile-save-project-buffers "save all" :column "Buffers")

  ("d" projectile-find-dir "directory" :column "Find")
  ("D" projectile-dired "root" :column "Find")
  ("f" projectile-find-file "file" :column "Find")
  ("p" projectile-switch-project "project" :column "Find")

  ("r" projectile-replace "replace" :column "Search")
  ("R" projectile-replace-regexp "regexp replace" :column "Search")
  ("g" bk/rgrep-fullscreen "grep" :column "Search"))

(define-key projectile-mode-map (kbd "C-c p") 'hydra-projectile/body)

(setq projectile-project-root-files-bottom-up
      (append '(".project" ".git"))
      projectile-project-root-files '()
      projectile-project-root-files-top-down-recurring '("Makefile"))

(defun projectile-short-mode-line ()
  "Short version of the default projectile mode line."
  (format " P[%s]" (projectile-project-name)))

(setq projectile-mode-line-function 'projectile-short-mode-line)

(setq dired-dwim-target t)

(setq dired-listing-switches "-la"
      dired-ls-F-marks-symlinks nil
      dired-auto-revert-buffer t
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (dired-sort-toggle-or-edit)))

(defun bk/dired-directories-first ()
  "Sort Dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'bk/dired-directories-first)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(cheatsheet-add-group 'Dired
		      '(:key "C-x C-q" :description "Turn Dired mode editable."))

(defun eshell-cwd ()
  "Sets the eshell directory to the current buffer."
  (interactive)
  (let ((path (file-name-directory (or (buffer-file-name) default-directory))))
    (with-current-buffer "*eshell*"
      (cd path)
      (eshell-reset))))

(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook '(lambda ()
			       (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun bk/explain-shell (cmd)
  "Open a help browser for the CMD."
  (interactive (list (read-shell-command "Command: ")))
  (browse-url (format "http://explainshell.com/explain?cmd=%s"
		      (url-encode-url cmd))))

(global-set-key (kbd "C-c e") 'eshell)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (eshell/alias "e" "find-file $1")
	    (eshell/alias "emacs" "find-file $1")
	    (eshell/alias "ee" "find-file-other-window $1")
	    (eshell/alias "d" "dired $1")))

(use-package git-timemachine
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-no-confirm '(stage-all-changes
               unstage-all-changes))
  (setq magit-completing-read-function 'magit-ido-completing-read))

(defhydra hydra-magit (:color blue)
  ("q" nil "quit" :column "Magit")
  ("b" magit-blame "blame" :column "Do")
  ("c" magit-clone "clone" :column "Do")
  ("i" magit-init "init" :column "Do")
  ("s" magit-status "status" :column "Do")
  ("t" git-timemachine "time-travel" :column "TimeMachine"))

(global-set-key (kbd "C-c g") 'hydra-magit/body)

(use-package elfeed
  :ensure t
  :init
  (setq-default elfeed-search-filter "@24-months-ago +unread")
  (setq elfeed-feeds
	'(("http://lambda-the-ultimate.org/rss.xml" functional)
	  ("https://byorgey.wordpress.com/feed/" functional)
	  ("http://gigasquidsoftware.com/atom.xml" clojure)
	  ("http://swannodette.github.com/atom.xml" clojure)
	  ("https://rigsomelight.com/feed.xml" clojure)
	  ("http://planet.emacsen.org/atom.xml" emacs)
	  ("https://gigasquidsoftware.com/atom.xml" clojure)
	  ("https://lambdaisland.com/feeds/blog.atom" clojure)
	  ("https://nullprogram.com/feed/" programming)
	  ("http://feeds.feedburner.com/cognicast" clojure)
	  ("http://feeds2.feedburner.com/StuartSierra" clojure)
	  ("http://feeds.feedburner.com/Juxt" clojure)
	  ("http://blog.cognitect.com/blog?format=rss" clojure)
	  ("https://www.reddit.com/r/emacs/.rss" emacs)
	  ("http://feeds.feedburner.com/stevelosh?format=xml" clojure)
	  ("https://existentialtype.wordpress.com/feed/" functional)
	  ("http://planet.clojure.in/atom.xml" clojure)
	  ("http://insideclojure.org/feed.xml" clojure)
	  ("https://yogthos.net/feed.xml" clojure)
	  ("http://endlessparentheses.com/atom.xml" emacs)
	  ("http://www.blackhats.es/wordpress/?feed=rss2" emacs)
	  ("http://www.howardism.org/index.xml" emacs)
	  ("http://www.masteringemacs.org/feed/" emacs)
	  ("http://tonsky.me/blog/atom.xml" clojure)
	  ("https://danlebrero.com/feed.rss" programming)
	  ("http://www.clojure.net/rss.xml" clojure)
	  ("https://www.youtube.com/feeds/videos.xml?user=techguruuk" emacs)
	  ("http://emacsrocks.com/atom.xml" emacs)
	  ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
	  ("http://yqrashawn.com/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs))))

(defun bk/elfeed-search-live-filter-space ()
  "Insert space when running elfeed filter"
  (interactive)
  (let ((elfeed-search-filter (concat elfeed-search-filter " ")))
    (elfeed-search-live-filter)))

(define-key elfeed-search-mode-map (kbd "/") 'bk/elfeed-search-live-filter-space)

(use-package slack
  :ensure t
  :init
  (setq slack-buffer-emojify t
	slack-prefer-current-team t
	slack-buffer-function #'switch-to-buffer
	slack-completing-read-function #'ido-completing-read
	slack-display-team-name nil)
  :config
  (slack-register-team
   :name "captalysdev"
   :default t
   :token (auth-source-pick-first-password
	   :host "slack.com"
	   :user "captalysdev")
   :subscribed-channels '(onboarding geral dev)
   :full-and-display-names t)

  (slack-register-team
   :name "clojurians"
   :token (auth-source-pick-first-password
	   :host "slack.com"
	   :user "clojurians")
   :subscribed-channels '(beginners reitit))

  (slack-register-team
   :name "captalys-oficial"
   :token (auth-source-pick-first-password
	   :host "slack.com"
	   :user "captalys-oficial")
   :subscribed-channels '(devops)
   :full-and-display-names t))

(define-key slack-mode-map "@"
  (defun endless/slack-message-embed-mention ()
    (interactive)
    (call-interactively #'slack-message-embed-mention)
    (insert " ")))

(define-key slack-mode-map (kbd "C-c C-d") #'slack-message-delete)
(define-key slack-mode-map (kbd "C-c C-e") #'slack-message-edit)
(define-key slack-mode-map (kbd "C-c C-k") #'slack-channel-leave)

(use-package circe :ensure t)

(use-package emojify :ensure t)

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(require 'whitespace)
(setq whitespace-style '(trailing lines space-before-tab
                  indentation space-after-tab))
(setq whitespace-line-column 100)
(whitespace-mode +1)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode +1))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(add-hook 'prog-mode-hook (defun bk--add-watchwords ()
			    (font-lock-add-keywords
			     nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
				    1 font-lock-warning-face t)))))

(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
	("FIXME"  . "#FF0000")
	("DEBUG"  . "#A020F0")
	("GOTCHA" . "#FF4500")
	("STUB"   . "#1E90FF")))
  :config
  (global-hl-todo-mode +1))

(global-set-key (kbd "C-x t p") 'hl-todo-previous)
(global-set-key (kbd "C-x t n") 'hl-todo-next)
(global-set-key (kbd "C-x t o") 'hl-todo-occur)
(global-set-key (kbd "C-x t i") 'hl-todo-insert)

(setq-default display-line-numbers-width 3)

(setq-default display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(cheatsheet-add-group 'TODO
		      '(:key "C-x t n" :description "Find next TODO/FIXME")
		      '(:key "C-x t p" :description "Find previous TODO/FIXME")
		      '(:key "C-x t o" :description "Use occur to find all TODO or similar keywords")
		      '(:key "C-x t i" :description "Insert TODO or similar keyword"))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (global-flycheck-mode))

(defun disable-flycheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)

(defun bk/eval-buffer ()
  "Eval the entire buffer and gives a message if succedded"
  (interactive)
  (eval-buffer)
  (message "Successful evaluated."))

(global-set-key (kbd "C-c C-k") 'bk/eval-buffer)

(use-package clojure-mode
  :ensure t
  :init
  (setq cljr-eagerly-build-asts-on-startup nil)
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq buffer-save-without-query t))))

(use-package cider
  :ensure t
  :init
  (setq cider-popup-stacktraces t
        cider-popup-stacktraces-in-repl t
        cider-overlays-use-font-lock t)
  :config
  (add-hook 'clojure-mode-hook 'cider-mode)
  (use-package cider-eval-sexp-fu :ensure t))

(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-warn-on-eval nil
        cljr-favor-prefix-notation nil)
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m")))
  (define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
  (define-key clojure-mode-map (kbd "C-c >") 'cljr-cycle-coll))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(cheatsheet-add-group 'Clojure
                      '(:key "C-c SPC" :description "Vertically align some forms")
                      '(:key "f (in report test buffer)" :description "Re-run the failing tests")
                      '(:key "C-u C-M-x" :description "Instrument a function to debug"))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(use-package py-autopep8
  :ensure t
  :init
  (setq py-autopep8-options '("--max-line-length=250"))
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(defun kill-cider-buffers ()
  "Kill all CIDER buffers without asking any questions.
Useful to execute when Emacs gets stuck."
  (interactive)
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (let ((kill-buffer-query-functions
	   (delq 'process-kill-buffer-query-function kill-buffer-query-functions))))
    (kill-matching-buffers "cider")))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  :config
  (yas-reload-all)
  (yas-global-mode +1))

;;; a snippet collection maintained by AndreaCrotti.
(use-package yasnippet-snippets
  :ensure t)

(global-set-key (kbd "C-c s") '(lambda ()
				    (interactive)
				    (yas/describe-tables)
				    (other-window 1)))

(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(cheatsheet-add-group 'Snippets
		      '(:key "M-x yas/describe-table" :description "Show available snippets in current mode")
		      '(:key "C-c s" :description "Show available snippets in current mode"))

(use-package tex-site
  :ensure auctex
  :config
  (require 'latex)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  ;; to refresh the buffer after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package reftex
  :ensure t
  :config
  (setq reftex-cite-prompt-optional-args t))

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'Latex-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(with-eval-after-load 'tex
  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape")))

(defun bk/spell-buffer-pt-BR ()
  "Function to spell check in Portuguese."
  (interactive)
  (ispell-change-dictionary "pt_BR")
  (flyspell-buffer))

(defun bk/spell-buffer-en ()
  "Function to spell check in English."
    (interactive)
    (ispell-change-dictionary "en_US")
    (flyspell-buffer))

(defun spell-checking/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))

(use-package auto-dictionary
  :ensure t
  :disabled t
  :defer t
  :init
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
  (defun bk/adict-set-local-dictionary ()
    "Set the local dictionary if not nil."
    (when (and (fboundp 'adict-change-dictionary)
       ispell-local-dictionary)
  (adict-change-dictionary ispell-local-dictionary)))
  (add-hook 'auto-dictionary-mode-hook 'bk/adict-set-local-dictionary 'append))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :commands (spell-checking/change-dictionary)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
	     flyspell-correct-previous-word-generic))

(require 'flyspell-correct-ido)
(setq flyspell-correct-interface #'flyspell-correct-ido)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(defun bk/kill-buffer-and-file (buffer-name)
  "Removes file connected to current buffer and kills buffer."
  (interactive "bKill buffer and its file:")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (buffer-file-name buffer)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" buffer-name)
      (delete-file filename)
      (kill-buffer buffer))))

(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(setq debug-on-error nil
      debug-on-quit nil)
