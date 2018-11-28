(require 'dired-x)
(require 'dired)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)

;; enale `a'-keybinding in dired - which opens the file
;; and closes the dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; make dired less verbose
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'setup-dired)
