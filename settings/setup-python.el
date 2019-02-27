;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)
(bk/install-maybe 'electric-operator)
(bk/install-maybe 'python-pytest)
(bk/install-maybe 'pydoc-info)

(elpy-enable)

(delete `elpy-module-django elpy-modules)
(delete `elpy-module-highlight-indentation elpy-modules)

(pyvenv-activate "~/miniconda3")

(add-hook 'python-mode-hook 'electric-operator-mode)

(require 'python)
(setq tab-width 4
	  python-indent-offset 4
	  python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i")

(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(pythonIndex" nil "")))

(provide 'setup-python)
;;; setup-python.el ends here
