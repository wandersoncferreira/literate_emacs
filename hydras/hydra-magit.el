;;; hydra-magit --- more git facilitites
;;; Commentary:

;;

;;; Code:

(defhydra hydra-magit (:color blue)
  ("q" nil "quit" :column "Magit")
  ("b" magit-blame "blame" :column "Do")
  ("c" magit-clone "clone" :column "Do")
  ("i" magit-init "init" :column "Do")
  ("s" magit-status "status" :column "Do")
  ("t" git-timemachine "time-travel" :column "TimeMachine")
  )

(provide 'hydra-magit)
;;; hydra-magit.el ends here
