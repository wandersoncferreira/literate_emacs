;;; setup-hydras --- Let's try to tame those heads again!
;;; Commentary:

;;

;;; Code:

(use-package hydra :ensure t)
(defconst hydras-dir (expand-file-name "hydras" user-emacs-directory))
(dolist (file (directory-files hydras-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'dired)
(require 'ibuffer)

(define-key dired-mode-map "." 'hydra-dired/body)
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)

(provide 'setup-hydras)
;;; setup-hydras.el ends here
