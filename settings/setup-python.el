(bk/install-maybe-require 'elpy)

(elpy-enable)
(pyvenv-activate "~/miniconda3")

(delete `elpy-module-highlight-indentation elpy-modules)
(delete `elpy-module-django elpy-modules)

(provide 'setup-python)
