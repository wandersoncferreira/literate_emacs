;;; setup-company --- Completion framework
;;; Commentary:

;; Let's give it another try!

;;; Code:


(bk/install-maybe 'company)
(require 'company)
(require 'company-dabbrev)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-global-modes '(not eshell-mode shell-mode
                                 org-mode term-mode))

(setq-default company-transformers '(company-sort-by-occurrence)
              company-begin-commands '(self-insert-command)
              company-require-match 'never
              company-show-numbers t
              company-idle-delay 0.1
              company-tooltip-align-annotations 't
              company-minimum-prefix-length 1)

;; redefine keys
(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

;; stop lower-casing all the completions
(setq company-dabbrev-downcase nil)

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
