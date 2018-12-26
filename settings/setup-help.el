;;; setup-help --- Help
;;; Commentary:

;; I'm reading the Emacs Info continuously, but I really like the change
;; in font-lock when I visit some nodes at *Info*.  But they are gone
;; when you restart an Emacs session.  Let's make them persistent.

;;; Code:

(defvar bk--name-of-file (expand-file-name "bk-info.history"
					   user-emacs-directory))
(defun bk/save-info-history-visited-nodes ()
  "Save visited nodes."
  (interactive)
  (with-temp-file bk--name-of-file
    (prin1 Info-history-list (current-buffer))))

(add-hook 'kill-emacs-hook #'bk/save-info-history-visited-nodes)

(defun bk/load-info-history-visited-nodes ()
  "Load visited nodes."
  (interactive)
  (with-temp-buffer
    (insert-file-contents bk--name-of-file)
    (goto-char (point-min))
    (setq Info-history-list (read (current-buffer)))))

(add-hook 'after-init-hook 'bk/load-info-history-visited-nodes)

(provide 'setup-help)
;;; setup-help.el ends here
