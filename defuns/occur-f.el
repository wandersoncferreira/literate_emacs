;;; occur-f.el --- Occur
;;; Commentary:
;;; Code:

;; (defun bk/improve-occur ()
;;   "Function to improve `occur' with the default value."
;;   (interactive)
;;   (push (let ((syb (thing-at-point 'symbol)))
;;           (when (stringp syb)
;;             (regexp-quote syb)))
;;         regexp-history)
;;   (call-interactively 'occur))

;; (add-hook 'occur-hook
;; 	  (lambda nil
;; 	    (switch-to-buffer-other-window "*Occur*")
;; 	    (delete-other-windows)))

;; (defadvice occur-mode-goto-occurrence (after occur-goto-after activate)
;;   "Advice to delete other windows after RET in occurence."
;;   (delete-other-windows))

(provide 'occur-f.el)
;;; occur-f.el ends here
