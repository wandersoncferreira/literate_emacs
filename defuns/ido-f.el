(defun cleaning-recent-file-paths (file-path)
  (let ((clean-str (s-chop-prefix (concat (expand-file-name "~") "/") file-path)))
    (cons clean-str file-path)))

(defun ido-recentf-open ()
  (interactive)
  (let* ((recent-files (mapcar 'cleaning-recent-file-paths recentf-list))
	 (desired-file (ido-completing-read+ "Find recentf file: " (mapcar 'car recent-files))))
    (if desired-file
	(find-file (cdr (assoc desired-file recent-files)))
      (message "Aborting"))))
