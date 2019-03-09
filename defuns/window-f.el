;;; window-f --- Functions to deal with Windows in Emacs
;;; Commentary:

;; 

;;; Code:

(defun split-window-func-with-other-buffer (split-function)
  "Split window using SPLIT-FUNCTION and switch to the new window."
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provied."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
	(select-window target-window)))))


(provide 'window-f)
;;; window-f.el ends here
