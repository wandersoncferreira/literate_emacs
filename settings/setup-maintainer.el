;;; setup-maintainer --- All my custom packages
;;; Commentary:

;; Custom packages

;;; Code:

(bk/install-maybe 'helm)
(bk/install-maybe 'multi)


(defun load-packages-from-folder (folder-name)
  "Function to load a package from a specific FOLDER-NAME."
  (dolist (l (directory-files (concat user-emacs-directory folder-name) nil "^[^\.]"))
    (add-to-list 'load-path (concat user-emacs-directory folder-name "/" l))
    (autoload (intern l) (concat l ".el"))))

(load-packages-from-folder "maintainer")


(require 'helm-spotify-plus)
(global-set-key (kbd "C-c s s") 'helm-spotify-plus)
(global-set-key (kbd "C-c s f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c s b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c s p") 'helm-spotify-plus-play)
(global-set-key (kbd "C-c s g") 'helm-spotify-plus-pause)

(provide 'setup-maintainer)
;;; setup-maintainer.el ends here
