;;; setup-grep --- Grep
;;; Commentary:

;; Search for your patterns with ease

;;; Code:

(require 'grep)

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "elpa")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories "node_modules")))

(provide 'setup-grep)
;;; setup-grep.el ends here
