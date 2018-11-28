(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-pinned-packages nil)

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun bk/install-maybe-require (package)
  (when (not (package-installed-p package))
    (package-install package))
  (require package))

;;; remove text from modeline
(bk/install-maybe-require 'diminish)

(provide 'setup-package)
