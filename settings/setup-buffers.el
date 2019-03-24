;; -*- coding: utf-8; lexical-binding: t; -*-

;;; setup-buffers --- Specific settings for buffers only
;;; Commentary:

;; Maybe it's time to split my settings to be more specific about its behavior

;;; Code:

(require 'resurrect)
(resurrect-mode +1)

(require 'winner)
(winner-mode)

(require 'ibuffer)
(setq ibuffer-formats
      '((mark modified read-only vs-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
(setq ibuffer-filter-group-name-face 'font-lock-doc-face)
(setq ibuffer-default-sorting-mode 'major-mode)
(add-hook 'ibuffer-mode-hook (lambda nil (visual-line-mode -1)))

(provide 'setup-buffers)
;;; setup-buffers.el ends Here
