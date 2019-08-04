;;; org-mode --- Org settings
;;; Commentary:
;;; Code:

(if (file-directory-p "~/notes")
    (setq-default org-directory "~/notes"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.org" "Tasks")
         "* TODO %?\n %i\n %a"))
      )

(provide 'setup-org)
;;; setup-org.el ends here
