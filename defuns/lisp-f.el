;;; lisp-f --- Functions specifics for Lisp mode
;;; Commentary:

;; Elisp is my major source of lisp-like code

;;; Code:

(cl-defmacro or-protected (&body body)
  "Return first successfully computed result."
  `(or ,@(mapcar
	  (lambda (x) `(ignore-errors ,x))
	  body)))

(provide 'lisp-f)
;;; lisp-f.el ends here
