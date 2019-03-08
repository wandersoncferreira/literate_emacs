;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(bk/install-maybe 'ido-completing-read+)
(bk/install-maybe 'ido-at-point)
(bk/install-maybe 'ido-vertical-mode)

(require 'ido)
(require 'ido-at-point)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)
(require 'icomplete)

(icomplete-mode)
(ido-ubiquitous-mode)
(ido-mode)
(ido-everywhere)
(ido-at-point-mode)
(ido-vertical-mode)

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-auto-merge-work-directories-length -1
      ido-case-fold nil
      ido-max-prospects 10
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)

(setq ido-file-extensions-order '(".clj" ".py" ".org" ".php" ".rest"))
(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(setq magit-completing-read-function 'magit-ido-completing-read)

(provide 'setup-ido)
;;; setup-ido.el ends here
