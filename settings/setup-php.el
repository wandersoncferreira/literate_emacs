;;; setup-php --- PHP
;;; Commentary:

;; PHP programming language

;;; Code:

(bk/install-maybe 'php-mode)
(bk/install-maybe 'company-php)

(with-eval-after-load 'company
  (push 'company-ac-php-backend company-backends))

(provide 'setup-php)
;;; setup-php.el ends here
