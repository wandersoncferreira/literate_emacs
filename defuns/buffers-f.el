;;; buffers-f --- functions to deal with buffers
;;; Commentary:
;;; Code:

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

(defun bk/new-scratch-buffer ()
  "Create a new empty buffer.
Removed from xah."
  (interactive)
  (let ((buff (generate-new-buffer "*scratch*")))
    (switch-to-buffer buff)
    (setq buffer-offer-save t)
    (lisp-interaction-mode)
    buff))


(provide 'buffers-f)
;;; buffers-f.el ends here
