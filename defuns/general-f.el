;;; general-f --- General functions in Elisp
;;; Commentary:

;;

;;; Code:

(defun bk/uptime-box ()
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

(defun bk/explain-shell (cmd)
  "Function to open a help at the CMD provided."
  (interactive (list (read-shell-command "Command: ")))
  (browse-url (format "http://explainshell.com/explain?cmd=%s"
                      (url-encode-url cmd))))

(defun bk/eval-buffer ()
  "Evaluate the current buffer with feedback."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))


(provide 'general-f)
;;; general-f.el ends here

