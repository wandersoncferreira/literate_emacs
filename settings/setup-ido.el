(require 'ido)
(ido-mode +1)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-case-fold nil
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t)

(bk/install-maybe-require 'flx-ido)
(flx-ido-mode +1)
(setq ido-use-faces nil)

(bk/install-maybe-require 'ido-vertical-mode)
(ido-vertical-mode +1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; always rescan buffer for imenu
(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")

;; ignore .ds_store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

(bk/install-maybe-require 'ido-at-point)
(ido-at-point-mode +1)

;; use ido everywhere
(ido-mode +1)
(ido-everywhere +1)

(bk/install-maybe-require 'ido-completing-read+)
(ido-ubiquitous-mode +1)

(require 'icomplete)
(icomplete-mode +1)

(provide 'setup-ido)
