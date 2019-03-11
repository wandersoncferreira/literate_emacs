;;; hydra-date --- Manage data insertion like a PRO
;;; Commentary:

;;

;;; Code:

(defhydra hydra-date (:color blue)
  ("q" nil "quit" :column "Dates")
  ("d" me/date-short "short" :column "Insert")
  ("D" me/date-short-with-time "short" :column "Insert with Time")
  ("i" me/date-iso "iso" :column "Insert")
  ("I" me/date-iso-with-time "iso" :column "Insert with Time")
  ("l" me/date-long "long" :column "Insert")
  ("L" me/date-long-with-time "long" :column "Insert with Time")
  )

(provide 'hydra-date)
;;; hydra-date.el ends here
