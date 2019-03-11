;;; setup-hydras --- Let's try to tame those heads again!
;;; Commentary:

;;

;;; Code:

(bk/install-maybe 'hydra)

(defconst hydras-dir (expand-file-name "hydras" user-emacs-directory))
(dolist (file (directory-files hydras-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'dired)
(require 'ibuffer)

(define-key dired-mode-map "." 'hydra-dired/body)
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)

(global-set-key (kbd "C-c f") 'hydra-flycheck/body)
(global-set-key (kbd "C-c g") 'hydra-magit/body)
(global-set-key (kbd "C-c s") 'hydra-system/body)
(global-set-key (kbd "C-c p") 'hydra-projectile/body)

(provide 'setup-hydras)
;;; setup-hydras.el ends here
