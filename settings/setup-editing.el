;;; editing.el --- help me improve
;;; Commentary:
;;; Code:

;;; expand region
(bk/install-maybe 'expand-region)
(bk/install-maybe 'ace-jump-mode)
(bk/install-maybe 'fix-word)
(bk/install-maybe 'multiple-cursors)
(bk/install-maybe 'jump-char)

(require 'expand-region)
(require 'ace-jump-mode)
(require 'multiple-cursors)

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

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; DEL during isearch should edit the search string, not jump back to the previous result.
(eval-after-load 'isearch
  '(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

;;; pretty symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols. See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805)
          ("!=" . 8800)
          ("map" . 8614)
          )))
(add-hook 'prog-mode-hook 'add-pretty-lambda)


;;; grammatical parenthesis completion
(require 'awesome-pair)
(add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1)))

(provide 'setup-editing)
;;; setup-editing.el ends here
