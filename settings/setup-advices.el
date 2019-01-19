;;; setup-advices --- Advices to improve Emacs default functionalities
;;; Commentary:

;;

;;; Code:

(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current THEME before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'load-theme--disable-old-theme)


(provide 'setup-advices)
;;; setup-advices.el ends here
