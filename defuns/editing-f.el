;;; editing-f.el --- functions to deal with buffers
;;; Commentary:
;;; Code:

(defun open-line-below ()
  "Create a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Create a new line above the cursor."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun bk/insert-date ()
  "Function to insert date into buffer."
  (interactive)
  (insert (format-time-string
	   "%Y-%m-%d" (current-time))))

(defun bk/back-to-indentation-or-beginning ()
  "Function to dwm for line movements."
  (interactive)
  (if (= (point)
         (progn
           (back-to-indentation)
           (point)))
      (beginning-of-line)))

(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(provide 'editing-f)
;;; editing-f.el ends here
