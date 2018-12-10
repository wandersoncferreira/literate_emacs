;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'ido-completing-read+)
(ido-ubiquitous-mode +1)

(require 'ido)
(ido-mode +1)
(ido-everywhere +1)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)

(setq ido-file-extensions-order
      '(".clj" ".py" ".org" ".php" ".rest"))

(bk/install-maybe-require 'ido-vertical-mode)
(ido-vertical-mode +1)
(defvar ido-vertical-define-keys nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(bk/install-maybe-require 'ido-at-point)
(ido-at-point-mode +1)

(require 'icomplete)
(icomplete-mode +1)


(bk/install-maybe-require 'company)

(require 'company)
(setq company-global-modes '(not eshell-mode shell-mode
                                 org-mode term-mode)
      company-idle-delay 0.1
      company-transformers '(company-sort-by-occurrence)
      company-require-match 'never
      company-show-numbers t)
(global-company-mode +1)

(defun ora-company-number ()
  "Helper function to choose company candidate by number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(defun ora-activate-number ()
  "Choose company candidates by number."
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

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-previous)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(ora-activate-number)
(provide 'setup-ido)
;;; setup-ido.el ends here
