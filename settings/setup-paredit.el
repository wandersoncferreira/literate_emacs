;;; setup-paredit --- Paredit
;;; Commentary:

;; Take care of your parenthesis

;;; Code:

(bk/install-maybe 'paredit)
(require 'paredit)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(diminish 'paredit-mode)

;;; makes paredit-mode work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;;; changing nasty paredit keybindings
(defvar nasty-paredit-bindings
  '(("M-s" "s-s" paredit-splice-sexp)
    ("M-<up>" "s-<up>" paredit-splice-sexp-killing-backward)
    ("M-<down>" "s-<down>" paredit-splice-sexp-killing-forward)
    ("C-<right>" "s-<right>" paredit-forward-slurp-sexp)
    ("C-<left>" "s-<left>" paredit-forward-barf-sexp)
    ("C-M-<left>" "s-S-<left>" paredit-backward-slurp-sexp)
    ("C-M-<right>" "s-S-<right>" paredit-backward-barf-sexp)))

(dolist (it nasty-paredit-bindings)
  (let ((original (car it))
	(replacement (cadr it))
	(command (car (last it))))
    (define-key paredit-mode-map (read-kbd-macro original) nil)
    (define-key paredit-mode-map (read-kbd-macro replacement) command)))

(define-key paredit-mode-map (kbd "s-r") 'paredit-raise-sexp)
(define-key paredit-mode-map (kbd "\\") nil)


(provide 'setup-paredit)
;;; setup-paredit.el ends here
