;;; setup-completion.el --- completions
;;; Commentary:
;;; Code:

(use-package ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-virtual-buffers t
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-all-frames nil
        ido-confirm-unique-completion nil
        ido-completion-buffer-all-completions nil
        ido-default-buffer-method 'selected-window
        ido-use-url-at-point nil
        ido-use-faces t
        ido-max-window-height 1
        ido-decorations
        '(""
          ""
          "  |  "
          "  | ..."
          "["
          "]"
          " [No match] "
          " [Matched] "
          " [Not readable] "
          " [Too big] "
          " [Confirm] "
          ))
  :hook
  (minibuffer-setup . (lambda ()
                        (visual-line-mode 1)
                        (setq-local truncate-lines nil)
                        (setq-local resize-mini-windows nil)
                        (setq-local max-mini-window-height 1)))
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

(use-package amx
  :ensure t
  :after (ido ido-completing-read+)
  :init
  (setq amx-backend 'ido
        amx-save-file (no-littering-expand-var-file-name "amx-items")
        amx-history-length 10
        amx-show-key-bindings nil)
  :config
  (amx-mode 1))

;;; isearch enhancements
;;; C-h k C-s to get an awesome help menu with all the extra keys you can use with `isearch'

;; | keychord | description                  |
;; |----------+------------------------------|
;; | C-s C-w  | search char or word at point |
;; | M-s .    | similar, but broader match   |
;; | M-s o    | run `occur' on regexp        |
;; | M-s h r  | highlight regexp             |
;; | M-s h u  | undo the highlight           |
;; | C-s M-r  | toggle regexp search         |
;; | M-%      | run `query-replace'          |
;; | C-M-%    | `query-replace-regexp'       |

(use-package isearch
  :config
  (setq search-highlight t
        search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        isearch-lazy-highlight t
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s)"
        lazy-count-suffix-format nil
        isearch-yank-on-move 'shift
        isearch-allow-scroll 'unlimited)

  (defun bk/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  :bind (("M-s %" . bk/isearch-query-replace-symbol-at-point)))


(use-package re-builder
  :config
  (setq reb-re-syntax 'read))

(use-package visual-regexp
  :ensure t
  :config
  (setq vr/default-replace-preview nil
        vr/default-separator-use-custom-face t))

(provide 'setup-completion)
;;; setup-completion.el ends here
