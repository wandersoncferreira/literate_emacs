;;; setup-company --- Completion framework
;;; Commentary:

;; Let's give it another try!

;;; Code:

(use-package company
  :ensure t
  :init
  (setq company-transformers '(company-sort-by-occurrence)
      company-global-modes '(not eshell-mode shell-mode
                                 org-mode term-mode)
      company-require-match 'never
      company-idle-delay 0.2
      company-show-numbers t
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2)
  :config
  (global-company-mode +1))

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
