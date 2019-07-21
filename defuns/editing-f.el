;;; editing-f.el --- functions to deal with buffers
;;; Commentary:
;;; Code:

(defun goto-line-with-feedback ()
  "Show line numbers when `goto-line' is pressed."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode +1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

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

(defun rgrep-quit-window ()
  "Simply jump to the register where all your windows are."
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  "Go to file and close rgrep window."
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

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
