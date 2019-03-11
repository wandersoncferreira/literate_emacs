;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


;; clojure mode
(bk/install-maybe 'clojure-mode)
(bk/install-maybe 'clojure-mode-extra-font-locking)
(bk/install-maybe 'clj-refactor)
(bk/install-maybe 'cider)
(bk/install-maybe 'kibit-helper)

(require 'clojure-mode)
(require 'clj-refactor)
(require 'cider)
(require 'kibit-helper)

(defun my-clojure-mode-hook ()
  "Activate the refactor library."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun cider-jack-in-test-profile ()
  "Function to start clojure with test profile."
  (interactive)
  (let ((cider-lein-parameters (concat "with-profile +test"
				       cider-lein-parameters)))
    (cider-jack-in)))

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
