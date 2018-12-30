;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(bk/install-maybe 'ido-completing-read+)
(bk/install-maybe 'ido-vertical-mode)
(bk/install-maybe 'ido-at-point)

(require 'ido)
(require 'icomplete)
(require 'ido-at-point)

(ido-ubiquitous-mode +1)
(ido-mode +1)
(ido-everywhere +1)
(ido-at-point-mode +1)
(icomplete-mode +1)
(ido-vertical-mode +1)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-max-prospects 10
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)

(setq ido-file-extensions-order
      '(".clj" ".py" ".org" ".php" ".rest"))

(defvar ido-vertical-define-keys nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'setup-ido)
;;; setup-ido.el ends here
