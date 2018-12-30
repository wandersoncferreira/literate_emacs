;;; setup-python.el --- Python
;;; Commentary:
;;; Code:

(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)

(require 'elpy)
(require 'pip-requirements)

(bk/after-load 'python-mode
  (elpy-enable)
  (pyvenv-activate "~/miniconda3"))

(bk/after-load 'elpy
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(provide 'setup-python)
;;; setup-python.el ends here
