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

(defun beginning-of-string (&optional arg)
  (when (re-search-backward "[ \t]" (line-beginning-position) :noerror 1)
    (forward-char 1)))

(defun end-of-string (&optional arg)
  (when (re-search-forward "[ \t]" (line-end-position) :noerror arg)
    (backward-char 1)))


(defun thing-copy-string-to-mark (&optional arg)
  "Try to copy a string and paste it to the mark
When used in shell-mode, it will paste string on shell prompt by default."
  (interactive "P")
  (copy-thing 'beginning-of-string 'end-of-string)
  (message "Copied STRING"))

(global-set-key (kbd "C-c s") 'thing-copy-string-to-mark)
