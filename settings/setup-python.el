;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

(bk/install-maybe-require 'elpy)
(bk/install-maybe-require 'pip-requirements)

(elpy-enable)
(pyvenv-activate "~/miniconda3")

(delete `elpy-module-highlight-indentation elpy-modules)
(delete `elpy-module-django elpy-modules)

(provide 'setup-python)
;;; setup-python.el ends here
