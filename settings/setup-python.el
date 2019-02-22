;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)
(bk/install-maybe 'electric-operator)
(bk/install-maybe 'python-pytest)

(elpy-enable)

(delete `elpy-module-django elpy-modules)
(delete `elpy-module-highlight-indentation elpy-modules)

(pyvenv-activate "~/miniconda3")

(electric-operator-mode +1)

(provide 'setup-python)
;;; setup-python.el ends here
