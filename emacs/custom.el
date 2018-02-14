;;; package --- custom.el
;;; Commentary:
;;; Code:

(custom-set-variables
 '(auth-source-save-behavior nil)
 '(default-input-method (quote latin-postfix))
 '(delete-selection-mode nil)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track truncate)))
 '(fci-rule-color "#3f1a1a")
 '(org-agenda-files (quote ("~/dotfiles/agenda/todo.org.gpg")))
 '(org-fontify-whole-heading-line t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(custom-set-faces
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c")))))


(provide 'custom)
;;; custom.el ends here
