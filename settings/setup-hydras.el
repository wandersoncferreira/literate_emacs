;;; setup-hydras --- Let's try to tame those heads again!
;;; Commentary:

;;

;;; Code:

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-magit (:color blue)
    ("q" nil "quit" :column "Magit")
    ("b" magit-blame "blame" :column "Do")
    ("c" magit-clone "clone" :column "Do")
    ("i" magit-init "init" :column "Do")
    ("s" magit-status "status" :column "Do")
    ("t" git-timemachine "time-travel" :column "TimeMachine"))

  (defhydra hydra-projectile (:color blue)
  ("q" nil "quit" :column "Projectile")

  ("b" projectile-switch-to-buffer "list" :column "Buffers")
  ("K" projectile-kill-buffers "kill all" :column "Buffers")
  ("S" projectile-save-project-buffers "save all" :column "Buffers")

  ("d" projectile-find-dir "directory" :column "Find")
  ("D" projectile-dired "root" :column "Find")
  ("f" projectile-find-file "file" :column "Find")
  ("p" projectile-switch-project "project" :column "Find")

  ("r" projectile-replace "replace" :column "Search")
  ("R" projectile-replace-regexp "regexp replace" :column "Search")
  ("g" bk/rgrep-fullscreen "grep" :column "Search")))

(provide 'setup-hydras)
;;; setup-hydras.el ends here
