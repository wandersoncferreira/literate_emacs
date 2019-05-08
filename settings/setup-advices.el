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

;; also, is the last command was a copy - skip past all the expand-region cruft
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually move."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

(provide 'setup-advices)
;;; setup-advices.el ends here
