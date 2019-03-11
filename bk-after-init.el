;;; bk-after-init --- After init hook (?)
;;; Commentary:

;;

;;; Code:

(defvar mode-line-cleaner-alist
  `((yas/minor-mode . "")
    (paredit-mode . "")
    (eldoc-mode . "")
    (hs-minor-mode . "")
    (subword-mode . "")
    (abbrev-mode . "")
    (company-mode . "")
    (flyspell-mode . "")
    (whitespace-cleanup-mode . "")
    (global-whitespace-mode . "")
    (yas-minor-mode . "")
    (google-this-mode . "")
    (paredit-everywhere-mode . "")
    (flymake-mode . "")
    (whitespace-mode . ""))
  "Small doc.")

(defun clean-mode-line ()
  "Function for cleaning up the modeline."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
	do (let* ((mode (car cleaner))
		  (mode-str (cdr cleaner))
		  (old-mode-str (cdr (assq mode minor-mode-alist))))
	     (when old-mode-str
	       (setcar old-mode-str mode-str))
	     (when (eq mode major-mode)
	       (setq mode-name mode-str)))))

(add-hook 'after-init-hook 'clean-mode-line)

(provide 'bk-after-init)
;;; bk-after-init.el ends here
