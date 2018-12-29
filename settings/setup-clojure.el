;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:


;; clojure mode
(bk/install-maybe 'clojure-mode)
(bk/install-maybe 'clojure-mode-extra-font-locking)
(bk/install-maybe 'clj-refactor)
(bk/install-maybe 'cider)
(bk/install-maybe 'elein)

(require 'cider)
(require 'clj-refactor)

(add-to-list 'exec-path "/Users/wandersonferreira/dotfiles/scripts")
(add-to-list 'exec-path "/usr/local/bin")
(add-hook 'clojure-mode-hook 'cider-mode)

(define-key clojure-mode-map [remap paredit-forward] 'clojure-forward-logical-sexp)
(define-key clojure-mode-map [remap paredit-backward] 'clojure-backward-logical-sexp)

;; cider
(setq cider-repl-result-prefix ";; => ")

;; refactor
(setq cljr-favor-prefix-notation nil
      cljr-favor-private-functions nil)

(defun my-clojure-mode-hook ()
  "Activate the refactor library."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(provide 'setup-clojure)
;;; setup-clojure.el ends here
