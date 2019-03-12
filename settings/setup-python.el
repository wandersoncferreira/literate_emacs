;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)
(bk/install-maybe 'electric-operator)

;; elpy is a framework to handle python buffers
(require 'elpy)
(elpy-enable)
(delete `elpy-module-django elpy-modules)
(delete `elpy-module-highlight-indentation elpy-modules)

;;; pip requirements
(defun bk/pip-requirements-ignore-case ()
  "Function to ignore case in completions on requirements files."
  (setq-local completion-ignore-case t))
(add-hook 'pip-requirements-mode-hook 'bk/pip-requirements-ignore-case)

;; correct spaces between elements in python buffers.
(require 'electric-operator)
(add-hook 'python-mode-hook 'electric-operator-mode)

(pyvenv-activate "~/miniconda3")

;; python default variables
(require 'python)
(setq tab-width 4
	  python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt")

(provide 'setup-python)
;;; setup-python.el ends here
