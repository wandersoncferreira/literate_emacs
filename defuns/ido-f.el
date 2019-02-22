;;; ido-f --- functions to deal with ido
;;; Commentary:
;;; Code:

;;;###autoload
(defun cleaning-recent-file-paths (file-path)
  "Helper function to clean strings FILE-PATH from recent file list."
  (let ((clean-str (s-chop-prefix (concat (expand-file-name "~") "/") file-path)))
    (cons clean-str file-path)))

(defvar recentf-list nil)
;;;###autoload
(defun ido-recentf-open ()
  "Open recent files."
  (interactive)
  (let* ((recent-files (mapcar 'cleaning-recent-file-paths recentf-list))
	 (desired-file (ido-completing-read+ "Find recentf file: " (mapcar 'car recent-files))))
    (if desired-file
	(find-file (cdr (assoc desired-file recent-files)))
      (message "Aborting"))))


(require 'ido)
(require 'imenu)
;;; I was curious about the function that reads a choice
;;; from Imenu alist through ido. I removed the portion of
;;; the code that tried to guess a good default value for
;;; `ido-completing-read'
;;;###autoload
(defun idomenu--read (index-alist &optional prompt guess)
  "Function to read the options and a guess from a INDEX-ALIST.
PROMPT GUESS"
  (let* ((symatpt (thing-at-point 'symbol))
	 (names (mapcar 'car index-alist))
	 (name (ido-completing-read (or prompt "imenu ") names
				    nil t nil nil nil))
	 (choice (assoc name index-alist)))
    (if (imenu--subalist-p choice)
	(idomenu--read (cdr choice) prompt nil)
      choice)))

;;;###autoload
(defun bk/ido-menu ()
  "Use ido to read Imenu options."
  (interactive)
  (let ((index-alist (cdr (imenu--make-index-alist))))
    (if (equal index-alist '(nil))
	(message "No imenu tags in buffer")
      (imenu (idomenu--read index-alist nil t)))))

(provide 'ido-f.el)
;;; ido-f.el ends here
