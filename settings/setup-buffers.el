;; -*- coding: utf-8; lexical-binding: t; -*-

;;; setup-buffers --- Specific settings for buffers only
;;; Commentary:

;; Maybe it's time to split my settings to be more specific about its behavior

;;; Code:

(require 'resurrect)
(resurrect-mode +1)

(require 'winner)
(setq winner-boring-buffers
      '("*Completions*"
        "*Compile-Log*"
        "*inferior-lisp*"
        "*Fuzzy Completions*"
        "*Apropos*"
        "*Help*"
        "*cvs*"
        "*Buffer List*"
        "*Ibuffer*"
        "*esh command on file*"))
(winner-mode 1)

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

(eval-after-load 'ibuffer
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))


;;; popup-kill-ring
(bk/install-maybe 'popup-kill-ring)

;; scroll
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      hscroll-step 1
      hscroll-margin 1)

(provide 'setup-buffers)
;;; setup-buffers.el ends Here
