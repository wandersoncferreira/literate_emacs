;;; setup-dired.el --- Dired
;;; Commentary:
;;; Code:

(bk/install-maybe 'dired-collapse)

(require 'dired-x)
(require 'dired)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(defun ensure-buffer-name-ends-in-slash ()
  "Change buffer name to end with slash."
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
	(rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
	  (lambda () (setq truncate-lines 1)))

(add-hook 'dired-mode-hook 'dired-collapse-mode)

(provide 'setup-dired)
;;; setup-dired.el ends here
