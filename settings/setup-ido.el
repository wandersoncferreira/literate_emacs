;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'ido-completing-read+)
(ido-ubiquitous-mode +1)

(require 'ido)
(ido-mode +1)
(ido-everywhere +1)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)

(setq ido-file-extensions-order
      '(".clj" ".py" ".org" ".php" ".rest"))

(bk/install-maybe-require 'ido-vertical-mode)
(ido-vertical-mode +1)
(defvar ido-vertical-define-keys nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(bk/install-maybe-require 'ido-at-point)
(ido-at-point-mode +1)

(require 'icomplete)
(icomplete-mode +1)

(bk/install-maybe-require 'company)
(global-company-mode +1)

(provide 'setup-ido)
;;; setup-ido.el ends here
