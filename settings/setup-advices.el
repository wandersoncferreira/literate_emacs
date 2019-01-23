;;; setup-advices --- Advices to improve Emacs default functionalities
;;; Commentary:

;;

;;; Code:

(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current THEME before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'load-theme--disable-old-theme)

(defadvice magit-status (around magit-fullscreen activate)
  "Advice for magit status to show in fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice he-substitute-string (after he-paredit-fix)
  "Remove extra paren when expanding line in paredit."
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn
	(backward-delete-char 1)
	(forward-char))))

(provide 'setup-advices)
;;; setup-advices.el ends here
