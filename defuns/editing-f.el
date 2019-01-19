;;; editing-f.el --- functions to deal with buffers
;;; Commentary:
;;; Code:

(defun open-line-below ()
  "Create a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun comment-kill-all ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
		    (goto-char (point-max))
		    (line-number-at-pos)))))

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
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun bk/remove-python-print-statements ()
  "Function to remove all print statements from my python code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^print(" nil t)
      (kill-whole-line)
      (bk/remove-all-statements)))
  (message "All occurrences of the print statement were removed!"))


(provide 'editing-f)
;;; editing-f.el ends here
