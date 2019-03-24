;;; setup-markdown --- Markdorn setup
;;; Commentary:

;; 

;;; Code:

(bk/install-maybe 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.\\(m[k]d\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(defun markdown-mode-hook-setup ()
  ;; Stolen from http://stackoverflow.com/a/26297700
  ;; makes markdown tables saner via orgtbl-mode
  ;; Insert org table and it will be automatically converted
  ;; to markdown table
  (unless (featurep 'org-table) (require 'org-table))
  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))
  (add-hook 'after-save-hook 'cleanup-org-tables nil 'make-it-local)
  (orgtbl-mode 1) ; enable key bindings
  ;; don't wrap lines because there is table in `markdown-mode'
  (setq truncate-lines t)
  (setq imenu-create-index-function 'markdown-imenu-index))
(add-hook 'markdown-mode-hook 'markdown-mode-hook-setup)

(eval-after-load 'markdown-mode
  '(progn
     ;; `pandoc' is better than obsolete `markdown'
     (when (executable-find "pandoc")
       (setq markdown-command "pandoc -f markdown"))))


(provide 'setup-markdown)
;;; setup-markdown.el ends here
