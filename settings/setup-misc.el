;;; misc --- my miscellaneous
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook
          (defun bk--add-watchwords ()
            (font-lock-add-keywords
             nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
                    1 font-lock-warning-face t)))))

(require 'whitespace)
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab))
(setq whitespace-line-column 100)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(diminish 'whitespace-mode)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(diminish 'hs-minor-mode)

(bk/install-maybe-require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode +1)
(diminish 'whitespace-cleanup-mode)


(bk/install-maybe-require 'keyfreq)
(keyfreq-mode +1)
(keyfreq-autosave-mode +1)

;;; install only packages
(bk/install-maybe-require 'htmlize)
(bk/install-maybe-require 'restclient)
(bk/install-maybe-require 'yaml-mode)
(bk/install-maybe-require 'restart-emacs)
(bk/install-maybe-require 'dockerfile-mode)
(bk/install-maybe-require 'fix-word)

(provide 'setup-misc)
;;; setup-misc.el ends here
