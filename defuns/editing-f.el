;;; editing-f.el --- functions to deal with buffers
;;; Commentary:
;;; Code:

;;;###autoload
(defun open-line-below ()
  "Create a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun goto-line-with-feedback ()
  "Show line numbers when `goto-line' is pressed."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode +1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;;;###autoload
(defun comment-kill-all ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-white space character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;;;###autoload
(defun open-line-above ()
  "Create a new line above the cursor."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34."
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))


;;;###autoload
(defun bk/back-to-indentation-or-beginning ()
  "Function to dwm for line movements."
  (interactive)
  (if (= (point)
         (progn
           (back-to-indentation)
           (point)))
      (beginning-of-line)))

;;;###autoload
(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun bk/remove-python-print-statements ()
  "Function to remove all print statements from my python code."
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (when (re-search-forward "^\s+print")
      (kill-whole-line)
      (bk/remove-python-print-statements)))
  (message "All occurrences of the print statement were removed!"))


;;;###autoload
(defun bk/rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows and searching for REGEXP.
in FILES and DIR without CONFIRM."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp))
            (dir (ido-read-directory-name "Base directory: "
                                          nil default-directory t))
            (confirm (equal current-prefix-arg '(4))))
       (list regexp files dir confirm))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (delete-other-windows)
  (goto-char (point-min)))

;;;###autoload
(defun rgrep-quit-window ()
  "Simply jump to the register where all your windows are."
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

;;;###autoload
(defun rgrep-goto-file-and-close-rgrep ()
  "Go to file and close rgrep window."
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))


(defun bk/diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; implementation depends on `lexical-binding' being t, otherwise
  ;; #'clean-up will not be saved as closure to `ediff-cleanup-hook'
  (let ((a (generate-new-buffer "*diff-yank*"))
        (b (generate-new-buffer "*diff-yank*")))
    (cl-labels ((clean-up ()
                          (kill-buffer a)
                          (kill-buffer b)
                          (remove-hook 'ediff-cleanup-hook #'clean-up)
                          (winner-undo)))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

(defun eshell/kg (&rest args)
  "Find status of pods in ARGS."
  (let* ((env (car args)))
    (with-current-buffer "*eshell*"
      (insert "kubectl get pods -n " env)
      (eshell-send-input))))

(defun get-pod-name (pod env)
  "Get POD name from correct ENV."
  (let ((res (eshell-command-result (concat "kubectl get pods -n " env))))
    (string-match (concat pod ".*") res 0)
    (car (split-string (match-string 0 res) " "))))

(defun eshell/kl (&rest args)
  "Get logs from PODS and ENVS in ARGS."
  (let* ((pod (car args))
         (env (car (cdr args)))
         (pod-name (get-pod-name pod env)))
    (with-current-buffer "*eshell*"
      (insert "kubectl logs -n " env " " pod-name " " pod "-" env " -f")
      (eshell-send-input))))

(provide 'editing-f)
;;; editing-f.el ends here

