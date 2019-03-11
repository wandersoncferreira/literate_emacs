;;; hydra-system --- General stuff
;;; Commentary:

;;

;;; Code:

(defhydra hydra-system (:color blue)

  ("q" nil "quit" :column "System")
  ("u" emacs-uptime "uptime" :column "System")

  ("l" paradox-list-packages "list" :column "Packages")
  ("P" paradox-upgrade-packages "upgrade" :column "Packages")

  ("e" (eshell t) "eshell" :column "Shell")
  ("t" term "term" :column "Shell")
  ("T" ansi-term "ansi-term" :column "Shell")
  )

(provide 'hydra-system)
;;; hydra-system.el ends here
