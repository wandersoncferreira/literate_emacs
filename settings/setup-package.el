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

(bk/install-maybe 'use-package)

(provide 'setup-package)
;;; setup-package.el ends here
