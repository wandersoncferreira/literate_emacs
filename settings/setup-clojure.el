;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


(use-package clojure-mode
  :ensure t
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
        cljr-clojure-test-declaration
        "[clojure.test :refer [deftest is testing]]")
  
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

(defun fabrik-reset ()
  "Reset `lacinia' GraphQL server."
  (interactive)
  (cider-insert-in-repl "(reset)" 't))

(defun fabrik-refresh ()
  "Refresh all namespaces of my project."
  (interactive)
  (cider-insert-in-repl "(refresh-all)" 't))


(provide 'setup-clojure)
;;; setup-clojure.el ends here
