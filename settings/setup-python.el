;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'elpy)
(bk/install-maybe-require 'electric-operator)

(elpy-enable)
(pyvenv-activate "~/miniconda3")

(delete `elpy-module-highlight-indentation elpy-modules)
(delete `elpy-module-django elpy-modules)

(bk/install-maybe-require 'pip-requirements)
(add-hook 'python-mode-hook 'electric-operator-mode)

(provide 'setup-python)
;;; setup-python.el ends here
