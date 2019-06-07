;;; setup-sql --- SQL configurations
;;; Commentary:

;; 

;;; Code:


(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter))

(provide 'setup-sql)
;;; setup-sql.el ends here

