;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable)
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules)
  (pyvenv-activate "~/miniconda3"))

(use-package pip-requirements
  :ensure t
  :after python)

(provide 'setup-python)
;;; setup-python.el ends here
