(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)


(defun bk/insert-date ()
  "Function to insert date into buffer."
  (interactive)
  (insert (format-time-string
	   "%Y-%m-%d" (current-time))))

(provide 'setup-functions)
