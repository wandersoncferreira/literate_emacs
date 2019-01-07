;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)

(require 'elpy)
(require 'pip-requirements)

(elpy-enable)
(pyvenv-activate "~/miniconda3")

(delete `elpy-module-django elpy-modules)
(delete `elpy-module-highlight-indentation elpy-modules)

(provide 'setup-python)
;;; setup-python.el ends here
