;;; setup-expand --- Expand commands settings
;;; Commentary:

;;

;;; Code:

(require 'hippie-exp)

(setq hippie-expand-verbose t)

(defun hippie-expand-lines ()
  "Function to expand lines explicitly."
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
					    try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

(provide 'setup-expand)
;;; setup-expand.el ends here
