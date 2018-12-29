;;; setup-package --- Packages in Emacs
;;; Commentary:

;; Packages

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(setq package-archive-priorities
      '(("melpa-stable" . 15)
	("org" . 10)
	("melpa" . 5)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t)

(defun bk/install-maybe (package)
  "Function to install a PACKAGE if not already present."
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'setup-package)
;;; setup-package.el ends here
