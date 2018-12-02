;;; erc --- IRC from your buffer
;;; Commentary:
;;; Code:

(require 'erc)
(require 'erc-join)
(require 'erc-spelling)
(require 'erc-services)
(require 'erc-truncate)

(setq erc-port 6667
      erc-accidental-paste-threshold-seconds nil
      erc-prompt-for-nickserv-password nil
      erc-fill-column 75
      erc-nick "bartuka"
      erc-hide-list '("JOIN" "PART" "QUIT"))

(erc-autojoin-enable)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#emacs")))

(setq erc-join-buffer 'bury
      erc-autojoin-timing :indent)

(erc-services-mode +1)
(erc-truncate-mode +1)
(add-to-list 'erc-modules 'spelling)

(set-face-foreground 'erc-input-face "dim gray")
(set-face-foreground 'erc-my-nick-face "blue")


(provide 'setup-erc)
;;; setup-erc.el ends here
