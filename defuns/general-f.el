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
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

(defun explain-shell (cmd)
  (interactive (list (read-shell-command "Command: ")))
  (browse-url (format "http://explainshell.com/explain?cmd=%s"
                      (url-encode-url cmd))))


(provide 'general-f)
;;; general-f.el ends here

