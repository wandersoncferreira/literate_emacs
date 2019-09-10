;;; lisp-f --- Functions specifics for Lisp mode
;;; Commentary:

;; Elisp is my major source of lisp-like code

;;; Code:

(cl-defmacro or-protected (&body body)
  "Return first successfully computed result."
  `(or ,@(mapcar
	  (lambda (x) `(ignore-errors ,x))
	  body)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(provide 'lisp-f)
;;; lisp-f.el ends here
