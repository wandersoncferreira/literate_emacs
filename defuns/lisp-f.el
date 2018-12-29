;;; lisp-f --- Functions specifics for Lisp mode
;;; Commentary:

;; Elisp is my major source of lisp-like code

;;; Code:

(defun bk/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))

(provide 'lisp-f)
;;; lisp-f.el ends here
