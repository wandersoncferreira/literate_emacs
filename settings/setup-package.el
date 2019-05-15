;;; setup-package --- Packages in Emacs
;;; Commentary:

;; Packages

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(setq package-archive-priorities
      '(("melpa-stable" . 15)
        ("org" . 10)
        ("melpa" . 5)))

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun bk/install-maybe (package &optional min-version no-refresh)
  "Function to install a PACKAGE if not already present."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (bk/install-maybe package min-version t)))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))


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
