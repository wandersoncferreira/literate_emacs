;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


;; clojure mode
(bk/install-maybe 'clojure-mode)
(bk/install-maybe 'clojure-mode-extra-font-locking)
(bk/install-maybe 'clj-refactor)

(require 'clojure-mode)
(require 'clj-refactor)

(use-package cider
  :ensure t
  :init
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-use-pretty-printing t
        cider-repl-use-clojure-font-lock t
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t
        cider-repl-history-size 3000)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

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

(defun my-clojure-mode-hook ()
  "Activate the refactor library."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))


(defun cider-figwheel-repl ()
  "Evaluate the figwheel code inside a regular clojure repl."
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
	     (figwheel-sidecar.repl-api/start-figwheel!)
	     (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(define-key clojure-mode-map [remap paredit-forward] 'clojure-forward-logical-sexp)
(define-key clojure-mode-map [remap paredit-backward] 'clojure-backward-logical-sexp)

(setq cljr-favor-prefix-notation nil
      cljr-favor-private-functions nil)

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojure-mode-hook 'cider-mode)

(setq cider-repl-result-prefix ";; => "
      cider-prompt-for-symbol nil
      nrepl-hide-special-buffers nil)

(provide 'setup-clojure)
;;; setup-clojure.el ends here
