;;; setup-company --- Completion framework
;;; Commentary:

;; Let's give it another try!

;;; Code:


(bk/install-maybe 'company)

(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (dolist (backend '(company-eclim company-semantic))
       (delq backend company-backends))
     (define-key company-active-map (kbd "M-n") nil)
     (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (setq-default company-dabbrev-other-buffers 'all
		   company-tooltip-align-annotations t)))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq company-global-modes '(not eshell-mode
				 shell-mode
				 org-mode
				 term-mode)
      company-idle-delay (/ 60.0)
      company-minimum-prefix-length 3
      company-begin-commands (list 'self-insert-command)
      company-transformers '(company-sort-by-occurrence)
      company-require-match 'never
      company-show-numbers t
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-dabbrev-time-limit 0.001
      company-dabbrev-code-time-limit 0.001
      company-dabbrev-downcase nil)

(diminish 'company-mode)

(defun ora-company-number ()
  "Choose the candidate based on his number at candidate list."
  (interactive)
  (let* ((k (this-command-keys))
	 (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
		    company-candidates)
	(self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(defun ora-activate-number ()
  "Activate the number-based choice in company."
  (interactive)
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(ora-activate-number)

(provide 'setup-company)
;;; setup-company.el ends here
