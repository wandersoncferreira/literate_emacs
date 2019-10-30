;;; setup-completion.el --- completions
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format ""
        enable-recursive-minibuffers t
        projectile-completion-system 'ivy
        ivy-magic-tilde nil
        ivy-dynamic-exhibit-delay-ms 150
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create
        ivy-use-selectable-prompt t
        ivy-height 12
        ivy-extra-directories nil
        ivy-on-del-error-function nil)
  :bind (:map ivy-mode-map
              ("C-c e" . ivy-switch-buffer-eshell)
              ("C-c v" . ivy-push-view)
              ("C-c V" . ivy-pop-view))
  :config
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  (defun ivy-ignore-non-shell-buffers (str)
    (let ((buf (get-buffer str)))
      (if buf
          (with-current-buffer buf
            (not (eq major-mode 'eshell-mode)))
        t)))
  (defun ivy-switch-buffer-eshell ()
    "Like ivy-switch-buffer but only shows eshell buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers
                                      '(ivy-ignore-non-shell-buffers))))
      (ivy-switch-buffer)))
  (ivy-mode +1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (setq ivy-rich-parse-remote-buffer nil
        ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-set-display-transformer))

(use-package ido-completing-read+ :ensure t)

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
  :config
  (smex-initialize))

(use-package flx
  :ensure t)

(provide 'setup-completion)
;;; setup-completion.el ends here
