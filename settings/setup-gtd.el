;;; setup-gtd --- Getting Things Done
;;; Commentary:

;; Org settings to provide a better GTD environment

;;; Code:

(require 'org)

(setq org-agenda-files'("~/gtd/inbox.org"
			"~/gtd/gtd.org"
			"~/gtd/tickler.org"))

;;; capture data into the inbox and tickler files.
(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headlne "~/gtd/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))

(provide 'setup-gtd)
;;; setup-gtd.el ends here
