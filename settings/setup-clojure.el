;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


(use-package clojure-mode
  :ensure t
  :config
  (define-key clojure-mode-map [remap paredit-forward] 'clojure-forward-logical-sexp)
  (define-key clojure-mode-map [remap paredit-backward] 'clojure-backward-logical-sexp))

(use-package clojure-mode-extra-font-locking :ensure t)

(use-package ivy-clojuredocs
  :ensure t
  :bind (:map clojure-mode-map
              (("C-c d" . ivy-clojuredocs-at-point))))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-use-pretty-printing t
        cider-repl-use-clojure-font-lock t
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t
        cider-prompt-for-symbol nil
        cider-repl-history-size 3000
        nrepl-hide-special-buffers nil)
  (setq cider-print-options
        '(("length" 80)
          ("level" 20)
          ("right-margin" 80)))
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package emidje
  :ensure t
  :after cider
  :config
  (emidje-setup))

(defalias 'cider-default-connection 'cider-current-connection)

;;; cider config to use Clojure inside Docker and have nagivation
(eval-after-load "cider"
  '(progn
     (defcustom cider-docker-translations nil
       "Translate docker endpoints to home dirs."
       :type '(alist :key-type string :value-type string)
       :group 'cider)

     (defun cider--translate-docker (path)
       "Attempt to translate the PATH.
Looks at `cider-docker-translations' for (docker . host) alist of path
prefixes.  TRANSLATIONS is an alist of docker prefix to host prefix."
       (hack-dir-local-variables)
       (seq-some (lambda (translation)
                   (when (string-prefix-p (car translation) path)
                     (replace-regexp-in-string (format "^%s" (file-name-as-directory (car translation)))
                                               (file-name-as-directory (cdr translation))
                                               path)))
                 cider-docker-translations))

     (defun cider--file-path (path)
       "Return PATH's local or tramp path using `cider-prefer-local-resources'.
If no local or remote file exists, return nil."
       (let* ((local-path (funcall cider-from-nrepl-filename-function path))
              (tramp-path (and local-path (cider--client-tramp-filename local-path)))
              (reverse-docker-path (cider--translate-docker local-path)))
         (cond ((equal local-path "") "")
               ((and reverse-docker-path (file-exists-p reverse-docker-path)) reverse-docker-path)
               ((and cider-prefer-local-resources (file-exists-p local-path))
                local-path)
               ((and tramp-path (file-exists-p tramp-path))
                tramp-path)
               ((and local-path (file-exists-p local-path))
                local-path))))))


(use-package clj-refactor
  :ensure t
  :init
  (setq cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")

  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("set" . "clojure.set")
          ("s" . "clojure.string")
          ("walk" . "clojure.walk")
          ("time" . "clj-time.core")
          ("log" . "clojure.tools.logging")
          ("json" . "cheshire.core")))
  :config
  (defun my-clojure-mode-hook ()
    "Activate the refactor library."
    (clj-refactor-mode +1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;; allow single semicolon comments on a line, only on emacs 26
(if (version<= "26" emacs-version)
    (progn
      (defun clojure-indent-line ()
        "Indent current line as Clojure code."
        (interactive)
        (if (clojure-in-docstring-p)
            (save-excursion
              (beginning-of-line)
              (when (and (looking-at "^\\s-*")
                         (<= (string-width (match-string-no-properties 0))
                             (string-width (clojure-docstring-fill-prefix))))
                (replace-match (clojure-docstring-fill-prefix))))
          ;; `lisp-indent-line', but without special handling of comments
          (let ((pos (- (point-max) (point)))
                (indent (progn (beginning-of-line)
                               (calculate-lisp-indent (lisp-ppss)))))
            (when indent (indent-line-to indent))
            ;; If initial point was within line's indentation,
            ;; position after the indentation. Else stay at same point in text.
            (if (> (- (point-max) pos) (point))
                (goto-char (- (point-max) pos))))))
      (add-hook 'clojure-mode-hook
                (lambda ()
                  (setq comment-indent-function 'comment-indent-default)
                  (setq comment-add 0)
                  (comment-normalize-vars))))
  "running emacs 25 or lower")

(provide 'setup-clojure)
;;; setup-clojure.el ends here
