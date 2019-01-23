;;; setup-dired.el --- Dired
;;; Commentary:
;;; Code:

(require 'dired-x)
(require 'dired)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'setup-dired)
;;; setup-dired.el ends here
