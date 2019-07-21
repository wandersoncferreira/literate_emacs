;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


(use-package clojure-mode
  :load-path "contrib/clojure-mode/"
  :config
  (define-key clojure-mode-map [remap paredit-forward] 'clojure-forward-logical-sexp)
  (define-key clojure-mode-map [remap paredit-backward] 'clojure-backward-logical-sexp))

(use-package clojure-mode-extra-font-locking :ensure t)

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

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(save))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(defalias 'cider-default-connection 'cider-current-connection)

(use-package flycheck-clojure
  :ensure t
  :after flycheck
  :config
  (use-package let-alist :ensure t)
  (flycheck-clojure-setup))

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

;;; allow single semicolon comments on a line, only on emacs 26
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
