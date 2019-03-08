;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe 'expand-region)
(bk/install-maybe 'jump-char)
(bk/install-maybe 'ace-jump-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'ws-butler)
(bk/install-maybe 'browse-kill-ring)

(require 'expand-region)
(require 'jump-char)
(require 'ace-jump-mode)
(require 'whitespace)

;; don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; auto fill
(auto-fill-mode +1)
(setq-default fill-column 100)

;;; programming mode settings
(add-hook 'prog-mode-hook
	  (defun bk--add-watchwords ()
	    (font-lock-add-keywords
	     nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
		    1 font-lock-warning-face t)))))

(setq whitespace-style '(trailing lines space-before-tab
				  indentation space-after-tab)
      whitespace-line-column 100)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)


;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(provide 'setup-editing)
;;; setup-editing.el ends here
