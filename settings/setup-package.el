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

(when (not package-archive-contents)
  (package-refresh-contents))

(defun bk/install-maybe (package)
  "Function to install a PACKAGE if not already present."
  (when (not (package-installed-p package))
    (package-install package)))

(bk/install-maybe 'use-package)

(provide 'setup-package)
;;; setup-package.el ends here
