;;; init.el --- My configs -*- lexical-binding: t -*-

;;; Commentary:

;; Do what you want, but learn your tool very very well.  This is my
;; attempt to keep my emacs-fu sharpe.

;;; Code:

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
