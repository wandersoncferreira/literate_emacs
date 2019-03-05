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


(provide 'general-f)
;;; general-f.el ends here

