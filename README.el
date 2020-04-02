(require 'package)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  '(require 'use-package))

(defvar my-external-packages '(cider
			       smartparens
			       elfeed
			       rg
			       company
			       company-quickhelp
			       company-restclient
			       clojure-mode
			       clojure-mode-extra-font-locking
			       clj-refactor
			       which-key
			       yasnippet
			       yasnippet-snippets
			       avy
			       ace-window
			       magit
			       git-timemachine
			       change-inner
			       smart-shift
			       flycheck
			       flycheck-clj-kondo
			       smex
			       plantuml-mode
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
			       fix-word
			       modus-operandi-theme
			       modus-vivendi-theme))

(dolist (pkg my-external-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; plantuml
(require 'ob-plantuml)
(setq org-plantuml-jar-path "/home/wand/plantuml.jar")

;; add new paths to emacs
(setenv "PATH" (concat (getenv "PATH") ":/home/wand/scripts"))
(setq exec-path (append exec-path '("/home/wand/scripts")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(smex-initialize)

;; aesthetics
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t)

(defun bk/font-family-size (family size)
  "Set frame font to FAMILY at SIZE."
  (set-frame-font
   (concat family "-" (number-to-string size) ":hintstyle=hintfull") t t))

(bk/font-family-size "Source Code Pro Medium" 12)

(require 'clojure-mode-extra-font-locking)

(defun bk/load-light-theme ()
  "Load a light theme for the day."
  (interactive)
  (load-theme 'modus-operandi t))

(defun bk/load-dark-theme ()
  "Load a dark theme for the night."
  (interactive)
  (load-theme 'modus-vivendi t))

;; before 17h just load my light theme pls
(if (< (string-to-number (format-time-string "%H" (current-time))) 17)
    (bk/load-light-theme)
  (bk/load-dark-theme))

(require 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-saved-filter-groups
      '(("Main"
	 ("Directories" (mode . dired-mode))
	 ("Rest" (mode . restclient-mode))
	 ("Docker" (or
		    (mode . docker-compose-mode)
		    (mode . dockerfile-mode)))
	 ("Programming" (or
			 (mode . clojure-mode)
			 (mode . emacs-lisp-mode)
			 (mode . python-mode)))
	 ("Org" (mode . org-mode))
	 ("Markdown" (or
		      (mode . markdown-mode)
		      (mode . gfm-mode)))
	 ("Git" (or
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
		   (name . "^\\*Messages\\*$"))))))

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "Main")))

(require 'ibuffer-vc)
(define-key ibuffer-mode-map (kbd "/ V") 'ibuffer-vc-set-filter-groups-by-vc-root)

;; help to change text
;; move text using C-c up/down
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

(add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.yml\\'" . docker-compose-mode))

(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
	 (idg (shell-command-to-string "id -g"))
	 (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=3000 CURRENT_UID=$(id -u):$(id -g) done!")))

(global-set-key (kbd "C-c d") 'docker)

(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook (lambda ()
			      (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun bk/docker-cleanup-buffers ()
  "Delete all the docker buffers created."
  (interactive)
  (kill-matching-buffers "docker" nil t))

;; git
(require 'magit)
(add-to-list 'magit-no-confirm 'stage-all-changes)

(require 'dired-x)

;; useful option when I want to rename/move files in dired
(setq dired-dwim-target t)

(defun bk/dired-directories-first ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'bk/dired-directories-first)

(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(define-key dired-mode-map (kbd "O") 'bk/dired-xdg-open)

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
(set-register ?c '(file . "~/.emacs.d/docs/cheatsheet.org"))

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
(show-paren-mode t)
(delete-selection-mode t)
(pending-delete-mode t)
(global-eldoc-mode t)
(global-prettify-symbols-mode t)
(display-time-mode t)
(blink-cursor-mode 0)

(setq which-key-add-column-padding 12)
(setq which-key-allow-imprecise-window-fit t)
(setq which-key-echo-keystrokes 0.2)
(setq which-key-idle-delay 0.8)

(which-key-mode)

(line-number-mode)
(column-number-mode)
(size-indication-mode)
(global-auto-revert-mode)


(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-background nil)
(global-set-key (kbd "C-x o") 'ace-window)

;; winner
(setq winner-dont-bind-my-keys t)
(add-hook 'after-init-hook 'winner-mode)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)

;; input one char, jump to it with a tree
(global-set-key (kbd "C-c ;") 'avy-goto-char)

;; completions
(setq ido-use-virtual-buffers t)
(setq ido-use-faces t)
(ido-mode t)
(ido-everywhere t)

(recentf-mode t)

(require 'company)
(setq company-require-match 'never
      company-show-numbers t
      company-transformers '(company-sort-by-occurrence)
      company-idle-delay 0.3)

(add-to-list 'company-backends 'company-restclient)

(add-hook 'after-init-hook 'global-company-mode)

;; very interesting behavior which is to choose the candidate based on its number
;; this feature was implemented by ora in his blog post
(defun ora-company-number ()
  "Choose the candidate based on his number at candidate list."
  (interactive)
  (let* ((k (this-command-keys))
	 (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s)) company-candidates)
	(self-insert-command)
      (company-complete-number (string-to-number k)))))

(defun ora-activate-number ()
  "Activate the number-based choices in company."
  (interactive)
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(ora-activate-number)

;; provide documentation to the candidates of company mode
(eval-after-load 'company
  '(company-quickhelp-mode))

(add-hook 'text-mode-hook #'auto-fill-mode)

(setq sp-highlight-pair-overlay nil)

(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

(add-hook 'clojure-mode-hook (lambda ()
			       (clj-refactor-mode t)
			       (cljr-add-keybindings-with-prefix "C-c C-m")))

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

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

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-mode-line-prefix " Proj"))
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
  `((eldoc-mode . "")
    (abbrev-mode . "")
    (company-mode . "")
    (yas-minor-mode . "")
    (auto-fill-mode . "")
    (auto-revert-mode . "")
    (clojure-mode . "λ")
    (subword-mode . "")
    (flyspell-mode . "")
    (which-key-mode . "")
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
(setq org-confirm-babel-evaluate nil)
(setq org-agenda-files (list "/home/wand/org/todo.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "|" "DOING(d)" "|" "DONE(D)" "|" "CANCELLED(C)")
			  (sequence "STUDY(s)" "|" "STUDIED(S)")
			  (sequence "ACT(a)" "|" "ACTED(A)")))
(setq org-capture-templates
      '(("c" "Capture some concise actionable item and exist" entry
	 (file+headline "todo.org" "Task list without a defined date")
	 "* TODO [#B] %^{Title}\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i %l" :immediate-finish t)
	("t" "Task of importance with a tag, deadline, and further editable space" entry
	 (file+headline "todo.org" "Task list with a date")
	 "* %^{Scope of task||TODO [#A]|STUDY [#A]|Act on} %^{Title} %^g\n DEADLINE: %^t\n :PROPERTIES:\n :CONTEXT: %a\n:CAPTURED: %U\n :END:\n\n %i %?")))

(setq org-agenda-window-setup 'only-window)

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

(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-flycheck-mode)

(require 'flycheck-clj-kondo)

;; spelling
(defun bk/spell-buffer-pt-BR ()
  "Spell check in portuguese."
  (interactive)
  (ispell-change-dictionary "pt_BR")
  (flyspell-buffer))

(defun bk/spell-buffer-en ()
  "Spell check in english."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; integrate emacs kill ring with the system clipboard.
(setq save-interprogram-paste-before-kill t)


;; built in htop
(setq proced-auto-update-flag t
      proced-auto-update-interval 1
      proced-descend t)

;; words
(global-set-key (kbd "M-u") #'fix-word-upcase)
(global-set-key (kbd "M-l") #'fix-word-downcase)
(global-set-key (kbd "M-c") #'fix-word-capitalize)

;; feed
(require 'elfeed)
(setq-default elfeed-search-filter "@24-months-ago +unread")
(setq elfeed-feeds
      '(("http://lambda-the-ultimate.org/rss.xml" functional)
	("https://byorgey.wordpress.com/feed/" functional)
	("http://gigasquidsoftware.com/atom.xml" clojure)
	("http://swannodette.github.com/atom.xml" clojure)
	("https://rigsomelight.com/feed.xml" clojure)
	("https://lambdaisland.com/feeds/blog.atom" clojure)
	("https://nullprogram.com/feed/" programming)
	("http://feeds.feedburner.com/cognicast" clojure)
	("http://feeds2.feedburner.com/StuartSierra" clojure)
	("http://feeds.feedburner.com/Juxt" clojure)
	("http://blog.cognitect.com/blog?format=rss" clojure)
	("https://existentialtype.wordpress.com/feed/" functional)
	("http://insideclojure.org/feed.xml" clojure)
	("https://yogthos.net/feed.xml" clojure)
	("http://endlessparentheses.com/atom.xml" emacs)
	("http://www.blackhats.es/wordpress/?feed=rss2" emacs)
	("http://www.howardism.org/index.xml" emacs)
	("http://www.masteringemacs.org/feed/" emacs)
	("http://tonsky.me/blog/atom.xml" clojure)
	("http://www.clojure.net/rss.xml" clojure)
	("https://www.youtube.com/feeds/videos.xml?user=techguruuk" emacs)
	("http://emacsrocks.com/atom.xml" emacs)
	("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
	("http://yqrashawn.com/feeds/lazyblorg-all.atom_1.0.links-only.xml" emacs)))

(setq-default abbrev-mode t)

(defun bk/add-region-local-abbrev (start end)
  "Go from START to END and add the selected text to a local abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
	(add-mode-abbrev num-words)
	(deactivate-mark))
    (message "No selected region!")))

(global-set-key (kbd "C-x a l") 'bk/add-region-local-abbrev)

(defun bk/add-region-global-abbrev (start end)
  "Go from START to END and add the selected text to global abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
	(add-abbrev global-abbrev-table "Global" num-words)
	(deactivate-mark))
    (message "No selected region!")))

(global-set-key (kbd "C-x a g") 'bk/add-region-global-abbrev)

;; grep

(require 'rg)
(rg-define-search bk/search-git-root-or-dir
  :query ask
  :format regexp
  :files "everything"
  :dir (let ((vc (vc-root-dir)))
	 (if vc
	     vc
	   default-directory))
  :confirm prefix
  :flags ("--hidden -g !.git"))

(global-set-key (kbd "M-s g") 'bk/search-git-root-or-dir)

;; snippets
(yas-global-mode +1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
