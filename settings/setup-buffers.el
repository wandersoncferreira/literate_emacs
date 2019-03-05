;;; setup-buffers --- Specific settings for buffers only
;;; Commentary:

;; Maybe it's time to split my settings to be more specific about its behavior

;;; Code:

(require 'resurrect)
(resurrect-mode +1)

;;; interactively mark "things at point"
(defmacro gk-make-thing-marker (thing)
  "Macro to make THING markable."
  (let ((thingname (symbol-name thing)))
    `(defun ,(intern (concat "gk-mark-" thingname)) ()
       ,(concat "Mark the " thingname " under cursor.")
       (interactive)
       (let ((b (bounds-of-thing-at-point (quote ,thing))))
	 (set-mark (point))
	 (goto-char (car b))
	 (push-mark (cdr b) t t)))))

(defvar gk-things '(list sexp defun filename url email
			 word paragraph sentence whitespace
			 line page symbol)
  "A list of known things.")

(dolist (thing gk-things)
  (eval `(gk-make-thing-marker ,thing)))

(defun gk-mark-thing ()
  "Interactively find some THING to mark."
  (interactive)
  (funcall
   (intern
    (concat "gk-mark-" (completing-read
			"What to mark (hit TAB to complete): "
			(mapcar #'symbol-name gk-things)
			nil t)))))


(provide 'setup-buffers)
;;; setup-buffers.el ends here
