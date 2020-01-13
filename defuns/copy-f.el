;;; Let's try to make usage of the advices put in the EmacsWiki website
;;; I agree that selecting what you want accurately is the most timing-consuming thing


(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
  "copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  (message "copied WORD"))

(global-set-key (kbd "C-c w") 'copy-word)

(defun copy-backward-word ()
  "copy word before point"
  (interactive "")
  (save-excursion
    (let ((end (point))
          (beg (get-point 'backward-word 1)))
      (copy-region-as-kill beg end))
    (message "copied backward WORD")))

(global-set-key (kbd "C-c W") 'copy-backward-word)

(defun copy-line (&optional arg)
  "Save current line into kill-ring without mark the line."
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (message "Copied LINE."))

(global-set-key (kbd "C-c l") 'copy-line)

(defun copy-paragraph (&optional arg)
  "Copy paragraphs at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (message "Copied PARAGRAPH"))

(global-set-key (kbd "C-c P") 'copy-paragraph)
