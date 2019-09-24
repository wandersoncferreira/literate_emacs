;;; setup-dired.el --- Dired
;;; Commentary:
;;; Code:

(require 'dired)
(require 'dired-x)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-listing-switches "-alh"
      dired-ls-F-marks-symlinks nil
      dired-auto-revert-buffer t
      dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(defun bk/dired-directories-first ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2)
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'bk/dired-directories-first)

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;;; enable 'a'-keybinding in dired - which opens file and closes dired
(put 'dired-find-alternate-file 'disabled nil)

;;; delete with c-x c-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(use-package dired-narrow
  :ensure t
  :config
  (bind-key "C-c C-n" #'dired-narrow)
  (bind-key "C-c C-f" #'dired-narrow-fuzzy))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))


(provide 'setup-dired)
;;; setup-dired.el ends here
