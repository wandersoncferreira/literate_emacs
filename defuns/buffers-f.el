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

(defun bk/eshell-full-or-restore ()
  "Bring up a full-screen eshell or restore previous config."
  (interactive)
  (if (string= "eshell-mode" major-mode)
      (jump-to-register :eshell-fullscreen)
    (progn
      (window-configuration-to-register :eshell-fullscreen)
      (eshell)
      (delete-other-windows))))


(provide 'buffers-f)
;;; buffers-f.el ends here
