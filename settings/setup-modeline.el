;;; setup-modeline --- Settings for the modeline
;;; Commentary:

;; Let's just try it

;;; Code:


(unless (fboundp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE."
    (or (file-remote-p file 'localname) file)))

(setq mode-line-position
      '((line-number-mode ("(%l" (column-number-mode ",%c")))
        (-4 ":%p" ) (")")))

(defun modeline-project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (file-local-name
   (or
    (when (featurep 'projectile)
      (ignore-errors (projectile-project-root)))
    default-directory)))

(defun truncate-relative-path (path)
  "Return the truncate of relative PATH."
  (save-match-data
    (let ((pos 0) matches)
      (setq path (concat "/" path))
      (while (string-match "\\(\/\\.?.\\)" path pos)
        (setq matches (concat matches (match-string 0 path)))
        (setq pos (match-end 0)))
      (concat matches "/"))))

(defun modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name'."
  (let* ((buffer-file-truename (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
         (project-root (modeline-project-root)))
    (concat
     ;; project
     (propertize
      (concat (file-name-nondirectory (directory-file-name project-root)) "/")
      'face '(:inherit font-lock-string-face :weight bold))
     ;; relative path
     (propertize
      (when-let (relative-path (file-relative-name
                                (or (file-name-directory buffer-file-truename) "./")
                                project-root))
        (if (string= relative-path "./") ""
          (substring (truncate-relative-path relative-path) 1)))
      'face 'font-lock-comment-face)
     ;; file name
     (propertize (file-name-nondirectory buffer-file-truename)
                 'face 'font-lock-warning-face))))

(defvar-local modeline-buffer-info nil)
(defvar mode-line-buffer-info
  '(:propertize
    (:eval (or modeline-buffer-info
               (setq modeline-buffer-info
                     (if buffer-file-name
                         (modeline-buffer-file-name)
                       (propertize "%b" 'face '(:weight bold))))))))
(put 'mode-line-buffer-info 'risky-local-variable t)

(defsubst modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))
(defun selection-info()
  "Information about the current selection."
  (when mark-active
    (cl-destructuring-bind (beg . end)
        (cons (region-beginning) (region-end))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((bound-and-true-p rectangle-mark-mode)
                        (let ((cols (abs (- (modeline-column end)
                                            (modeline-column beg)))))
                          (format "(%dx%d)" lines cols)))
                       ((> lines 1)
                        (format "(%d,%d)" lines (- end beg)))
                       ((format "(%d,%d)" 0 (- end beg))))))
       'face 'font-lock-warning-face))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-info
                " "
                mode-line-position
                (:eval (selection-info))
                (vc-mode vc-mode)
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
