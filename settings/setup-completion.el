;;; setup-completion.el --- completions
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t
        ivy-extra-directories nil)
  (setq ivy-initial-inputs-alist nil)
  :bind (:map ivy-mode-map
              ("C-c w" . ivy-push-view)
              ("C-c W" . ivy-pop-view))
  :config
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (ivy-mode +1))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (setq counsel-find-file-at-point t)
  :config
  (counsel-mode +1)
  :bind (:map counsel-mode-map
              ([remap execute-extended-command] . counsel-M-x)
              ([remap find-file] . counsel-find-file)
              ([remap bookmark-jump] . counsel-bookmark)
              ([remap bookmark-set] . counsel-bookmark)
              ([remap describe-bindings] . counsel-descbinds)
              ([remap describe-variable] . counsel-describe-variable)
              ([remap describe-function] . counsel-describe-function)
              ([remap finder-by-keyword] . counsel-package)))

(use-package smex
  :ensure t
  :init
  (setq smex-prompt-string "Here be dragons => ")
  :config
  (smex-initialize))

(provide 'setup-completion)
;;; setup-completion.el ends here
