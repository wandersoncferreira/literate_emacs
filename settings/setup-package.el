;;; setup-package --- Packages in Emacs
;;; Commentary:

;; Packages

;;; Code:

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))

(setq package-archive-priorities
      '(("org" . 10)
        ("melpa" . 5)))

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; do not litter config
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq mc/list-file (no-littering-expand-etc-file-name "mc-list.el"))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq abbrev-file-name (no-littering-expand-etc-file-name "abbrev.el")))

(use-package dash :ensure t)
(use-package auto-package-update
  :ensure t
  :init
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(provide 'setup-package)
;;; setup-package.el ends here
