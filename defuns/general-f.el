;;; general-f --- General functions in Elisp
;;; Commentary:

;;

;;; Code:

(defun bk/uptime-osx ()
  "Uptime for your OS."
  (interactive)
  (message
   (shell-command-to-string "uptime")))

(defun bk/restclient ()
  "Open the restclient buffer."
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

(defun mu-face-at-point (pos)
  "Show face at POS (point)."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
		  (get-char-property pos 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))


(provide 'general-f)
;;; general-f.el ends here

