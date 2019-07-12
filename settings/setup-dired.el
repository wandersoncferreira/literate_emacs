;;; setup-dired.el --- Dired
;;; Commentary:
;;; Code:

(require 'dired)

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

;;; delete with c-x c-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(provide 'setup-dired)
;;; setup-dired.el ends here
