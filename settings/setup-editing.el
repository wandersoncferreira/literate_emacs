;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe 'ace-jump-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'jump-char)

(use-package expand-region
  :ensure t
  :init
  (setq expand-region-fast-keys-enabled nil))

(use-package multiple-cursors :ensure t)

;; auto fill
(auto-fill-mode +1)
(setq-default fill-column 100)

;;; programming mode settings
(add-hook 'prog-mode-hook
          (defun bk--add-watchwords ()
            (font-lock-add-keywords
             nil `(("\\<\\(FIX\\(ME\\))?\\|TODO\\)"
                    1 font-lock-warning-face t)))))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; DEL during isearch should edit the search string, not jump back to the previous result.
(eval-after-load 'isearch
  '(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))


(provide 'setup-editing)
;;; setup-editing.el ends here
