(defun bk/scratch-buffer ()
  "Function to change buffer to scratch buffer."
  (interactive)
  (let ((buf (get-buffer "*scratch*")))
    (if buf
	(switch-to-buffer buf)
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (lisp-interaction-mode))))

(defun bk/normalize-buffer ()
  "Function to organize the buffer."
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))
