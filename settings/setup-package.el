;;; setup-package --- Packages in Emacs
;;; Commentary:

;; Packages

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

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

(use-package quelpa :ensure t)

(provide 'setup-package)
;;; setup-package.el ends here
