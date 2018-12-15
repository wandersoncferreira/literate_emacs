;;; setup-package --- Packages in Emacs
;;; Commentary:

;; Packages

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun bk/install-maybe-require (package)
  "Function to install a PACKAGE if not already present."
  (when (not (package-installed-p package))
    (package-install package))
  (require package))

(provide 'setup-package)
;;; setup-package.el ends here
