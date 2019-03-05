;;; setup-mac --- Mac settings
;;; Commentary:

;; Macbook specifics

;;; Code:

;;; https://github.com/benmaughan/spotlight.el
(bk/install-maybe 'spotlight)

(require 'ls-lisp)
(require 'ispell)
(require 'dired)
(require 'flyspell)

(setq mac-command-modifier 'meta
      mac-right-option-modifier 'none
      ns-function-modifier 'hyper
      mac-option-modifier 'super)

(setq ls-lisp-use-insert-directory-program nil
      dired-use-ls-dired nil)

;; use aspell for spell checking
(setq ispell-program-name "/usr/local/bin/aspell"
      ispell-extra-args '("--sug-mode=ultra")
      flyspell-issue-message-flag nil)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; the C-M-d is a global keybinding in OSX which prevents me
;; to use the `down-list' sexp motion.
;; please run the following command at your macOS
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

(provide 'setup-mac)
;;; setup-mac.el ends here
