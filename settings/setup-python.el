;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

;; elpy is a framework to handle python buffers
(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable)
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(use-package electric-operator
  :ensure t
  :after python
  :hook (python-mode . electric-operator))

(use-package py-autopep8
  :ensure t
  :after python
  :init
  (setq py-autopep8-options '("--max-line-length=150"))
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(provide 'setup-python)
;;; setup-python.el ends here
