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

(defun bk/spell-buffer-pt-BR ()
  "Function to spell check inside the buffer."
  (interactive)
  (ispell-change-dictionary "pt_BR")
  (flyspell-buffer))

(defun bk/spell-buffer-en ()
  "Function to spell check inside the buffer."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

(defun bk/add-region-global-abbrev (start end)
  "Adicionar a região selecionada como uma abreviação global."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-abbrev global-abbrev-table "Global" num-words)
        (deactivate-mark))
    (message "No selected region!")))

(defun bk/add-region-local-abbrev (start end)
  "Adicionar a região selecionada como abreviação especifica do mode."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-mode-abbrev num-words)
        (deactivate-mark))
    (message "No selected region!")))

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun where-am-i ()
  "Displays `buffer-file-name' when visiging a file.
Otherwise the function display `buffer-name'"
  (interactive)
  (let ((dir-file (buffer-file-name)))
    (if dir-file
        (message dir-file)
      (message (buffer-name)))))


(provide 'buffers-f)
;;; buffers-f.el ends here
