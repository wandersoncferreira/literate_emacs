;;; misc --- my miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(bk/install-maybe 'htmlize)
(bk/install-maybe 'restclient)
(bk/install-maybe 'yaml-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'whitespace-cleanup-mode)
(bk/install-maybe 'graphviz-dot-mode)
(bk/install-maybe 'quickrun)
(bk/install-maybe 'poporg)
(bk/install-maybe 'windresize)
(bk/install-maybe 'minions)
(bk/install-maybe 'request)
(bk/install-maybe 'restart-emacs)
(bk/install-maybe 'rotate)
;; rotate-window: change the windows inside the frame
;; rotate-layout: change the layout inside the frame. -- to |


(require 'whitespace)
(require 'quickrun)
(require 'poporg)

(require 'minions)
(setq minions-mode-line-lighter "☰")
(minions-mode +1)

(add-hook 'prog-mode-hook
	  (defun bk--add-watchwords ()
	    (font-lock-add-keywords
	     nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
		    1 font-lock-warning-face t)))))

(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab)
      whitespace-line-column 100)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package hideshow
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(global-whitespace-cleanup-mode +1)

(provide 'setup-misc)
;;; setup-misc.el ends here
