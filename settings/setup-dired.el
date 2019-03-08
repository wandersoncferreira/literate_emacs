;;; setup-dired.el --- Dired
;;; Commentary:
;;; Code:

(bk/install-maybe 'dired-collapse)

(require 'dired-x)
(require 'dired)
(require 'wdired)

;;; reload dired after making changes
(--each '(dired-do-rename
	  dired-do-copy
	  dired-do-delete
	  dired-create-directory
	  wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
	   (revert-buffer))))

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(defun ensure-buffer-name-ends-in-slash ()
  "Change buffer name to end with slash."
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
	(rename-buffer (concat name "/") t))))

(defun dired-back-to-start-of-files ()
  "Make dired move back to start of file with [`beginning-of-line']."
  (interactive)
  (backward-char (- (current-column) 2)))

(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines 1)))
(add-hook 'dired-mode-hook 'dired-collapse-mode)

;;; delete with c-x c-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
     ))

(provide 'setup-dired)
;;; setup-dired.el ends here
