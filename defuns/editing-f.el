;;; editing-f.el --- functions to deal with buffers
;;; Commentary:
;;; Code:

(defun open-line-below ()
  "Create a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun goto-line-with-feedback ()
  "Show line numbers when `goto-line' is pressed."
  (interactive)
  (unwind-protect
      (progn
	(linum-mode +1)
	(goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

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


(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
	    (files (grep-read-files regexp))
	    (dir (ido-read-directory-name "Base directory: "
					  nil default-directory t))
	    (confirm (equal current-prefix-arg '(4))))
       (list regexp files dir confirm))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (goto-char (point-min)))

(defun rgrep-quit-window ()
  "Simply jump to the register where all your windows are."
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  "Go to file and close rgrep window."
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))


(provide 'editing-f)
;;; editing-f.el ends here
