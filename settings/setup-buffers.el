;; -*- coding: utf-8; lexical-binding: t; -*-

;;; setup-buffers --- Specific settings for buffers only
;;; Commentary:

;; Maybe it's time to split my settings to be more specific about its behavior

;;; Code:

(use-package winner
  :init
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
  :config
  (winner-mode +1))

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
