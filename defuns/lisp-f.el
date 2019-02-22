;;; lisp-f --- Functions specifics for Lisp mode
;;; Commentary:

;; Elisp is my major source of lisp-like code

;;; Code:

(defmacro bk/after-load (mode &rest code)
  "Custom `eval-after-load' to a MODE and CODE functionality."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@code)))

(cl-defmacro or-protected (&body body)
  "Return first successfully computed result."
  `(or ,@(mapcar
	  (lambda (x) `(ignore-errors ,x))
	  body)))

(defun bk/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))

(defun load-packages-from-folder (folder-name)
  "Function to load a package from a specific FOLDER-NAME."
  (dolist (l (directory-files (concat user-emacs-directory folder-name) nil "^[^\.]"))
    (add-to-list 'load-path (concat user-emacs-directory folder-name "/" l))
    (autoload (intern l) (concat l ".el"))))


(provide 'lisp-f)
;;; lisp-f.el ends here
