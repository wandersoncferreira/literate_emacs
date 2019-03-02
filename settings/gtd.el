(setq org-agenda-files'("~/gtd/inbox.org"
			"~/gtd/gtd.org"
			"~/gtd/tickler.org"
			"~/platform/inbox.org"
			))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Tasks")
			       "* TODO %i%? %^G \n %U" :empty-lines 1)
			      ("p" "Platform TODO [inbox]" entry
			       (file+headline "~/platform/inbox.org" "Platform Tasks")
			       "* TODO %i%? %^G \n %U" :empty-lines 1)
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U" :empty-lines 1)))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
			   ("~/gtd/someday.org" :maxlevel . 1)
			   ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "OPEN(o!)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
	("ga" "All TODO" alltodo nil
	 ((org-agenda-sorting-strategy '(tag-up priority-up))))

	("gc" "Clojure" tags-todo "clojure"
	 ((org-agenda-sorting-strategy '(priority-up))
	  (org-agenda-prefix-format "[ ] %T: ")
	  (org-agenda-compact-blocks t)
	  (org-agenda-remove-tags t)))

	("gp" "Captalys Platform" tags-todo "@platform"
	 ((org-agenda-sorting-strategy '(priority-up))
	  (org-agenda-prefix-format "[ ] %T: ")
	  (org-agenda-compact-blocks t)))

	("gs" "Study Time" tags-todo "@study"
	 ((org-agenda-sorting-strategy '(priority-up))))

	("ge" "Emacs Time" tags-todo "@emacs")))

(set-register ?i '(file . "~/gtd/inbox.org"))
(set-register ?p '(file . "~/gtd/gtd.org"))
