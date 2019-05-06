;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


;; clojure mode
(bk/install-maybe 'clojure-mode)
(bk/install-maybe 'clojure-mode-extra-font-locking)
(bk/install-maybe 'clj-refactor)
(bk/install-maybe 'cider)

(require 'clojure-mode)
(require 'clj-refactor)
(require 'cider)

(defun my-clojure-mode-hook ()
  "Activate the refactor library."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun start-cider-repl-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Enter profile name: "))
           (lein-parms (concat "with-profile +" profile " repl :headless")))
    (message "lein-params set to: %s" lein-parms)
    (set-variable 'cider-lein-parameters lein-parms)
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
