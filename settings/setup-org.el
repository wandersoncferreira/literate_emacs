;;; org-mode --- Org settings
;;; Commentary:
;;; Code:

(setq-default org-confirm-babel-evaluate nil
              org-confirm-elisp-link-function nil
              org-use-speed-commands t
              org-return-follows-link t)

(unless (file-directory-p "~/notes/")
  (make-directory "~/notes")
  (setq-default org-directory "~/notes"))

(unless (file-exists-p "~/notes/gtd/")
  (make-directory "~/notes/gtd"))

(setq org-agenda-files '("~/notes/gtd/gtd.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/notes/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/notes/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/notes/gtd/gtd.org" :maxlevel . 3)
                           ("~/notes/gtd/someday.org" :maxlevel . 1)
                           ("~/notes/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "STARTED(s)"
                                    "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("ga" "All TODO" alltodo nil
         ((org-agenda-sorting-strategy '(tag-up priority-up))))

        ("gc" "Clojure" tags-todo "clojure"
         ((org-agenda-sorting-strategy '(priority-up))
          (org-agenda-prefix-format "[ ] %T: ")
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)))

        ("gw" "Captalys" tags-todo "captalys"
         ((org-agenda-sorting-strategy '(priority-up))
          (org-agenda-prefix-format "[ ] %T: ")
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)))

        ("gs" "Study Time" tags-todo "study"
         ((org-agenda-sorting-strategy '(priority-up))))

        ("ge" "Emacs Time" tags-todo "emacs")))

;; The inbox should be processed and emptied daily. When
;; processing the inbox, I’d refile each entry that is
;; actionable and belongs to a project using C-c C-w, moving
;; the entry to the appropriate place.  The main file is the
;; gtd.org. That’s where all the active projects are
;; kept. Each project contains actions to be performed. The
;; first action of each project is called its “next action”,
;; and that’s always the one I will do when working on a
;; project.

;; Tags —- Tagging is done using C-c C-c on a headline,
;; whether it’s a project or action. Purposes of he tags:

;; regular categories, like :emacs: tags that link to
;; people, like :veridiana: GTD contexts GTD contexts are
;; just regular tags, starting with @. Some useful ones,
;; @home, @office, @travelling, @email..


;;; make the windmove.el workds in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'org-shiftdown-final-hook 'windmove-down)

;; after changing the state of any TODO item, save the current buffer.
(add-hook 'org-trigger-hook 'save-buffer)


;;; reveal.js
(use-package ox-reveal
  :ensure t
  :config
  (require 'ox-reveal)
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax-url t))

(use-package htmlize :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path (expand-file-name "/home/wand/plantuml.jar"))
(setq org-startup-with-inline-images t)

(provide 'setup-org)
;;; setup-org.el ends here
