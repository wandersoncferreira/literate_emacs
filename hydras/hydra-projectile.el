;;; hydra-projectile --- Projectile goodies
;;; Commentary:

;;

;;; Code:

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
  ("g" bk/rgrep-fullscreen "grep" :column "Search")
  )

(provide 'hydra-projectile)
;;; hydra-projectile.el ends here
