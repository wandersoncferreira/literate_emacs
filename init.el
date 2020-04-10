;;; init.el --- My configs -*- lexical-binding: t -*-

;;; Commentary:

;; Do what you want, but learn your tool very very well.  This is my
;; attempt to keep my emacs-fu sharpe.

;;; Code:

;; startup time matters. This bit will improve garbage collector of Emacs during its loading process.
(let ((init-gc-cons-threshold (* 128 1024 1024))
      (bk--file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.6
        file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 800000
                    gc-cons-percentage 0.1
                    file-name-handler-alist bk--file-name-handler-alist))))
(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
