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

(defun touch-buffer-file ()
  "Create file from current buffer."
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

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

(defun bk/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (new-name (read-file-name "New name: " filename)))
    (if (get-buffer new-name)
	(error "A buffer named '%s' already exists!" new-name)
      (rename-file filename new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "File '%s' sucessfully renamed to '%s'"
	       name (file-name-nondirectory new-name)))))

(defun bk/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' sucessfully removed" filename)))))


(provide 'buffers-f)
;;; buffers-f.el ends here
