;;; setup-expand --- Expand commands settings
;;; Commentary:

;;

;;; Code:

;; (require 'hippie-exp)

;; (setq hippie-expand-verbose t)
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-expand-dabbrev-from-kill
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))

;; (defun hippie-expand-lines ()
;;   "Function to expand lines explicitly."
;;   (interactive)
;;   (let ((hippie-expand-try-functions-list '(try-expand-line
;;                                             try-expand-line-all-buffers)))
;;     (end-of-line)
;;     (hippie-expand nil)))

(provide 'setup-expand)
;;; setup-expand.el ends here
