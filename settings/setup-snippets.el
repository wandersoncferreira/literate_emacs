;;; setup-snippets --- Snippets
;;; Commentary:

;; ease your life with snippets

;;; Code:

(use-package yasnippet
  :ensure t
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-verbosity 1
        yas-wrap-around-region t)
  :config
  (yas-global-mode 1)
  :bind (
         :map yas-keymap
         ("C-e" . yas/goto-end-of-active-field)
         ("C-a" . yas/goto-start-of-active-field)
         ("<return>" . yas-exit-all-snippets)
         :map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil)
         ("C-M-<return>" . yas-expand)))

(use-package yasnippet-snippets :ensure t)
(use-package clojure-snippets :ensure t)


(defun yas/goto-end-of-active-field ()
  "Go to the end of the active field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  "Go to the beginning of the active field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))


(provide 'setup-snippets)
;;; setup-snippets.el ends here
