;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)
(bk/install-maybe 'electric-operator)
(bk/install-maybe 'smart-dash)
(bk/install-maybe 'py-autopep8)

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

(require 'smart-dash)
(add-hook 'python-mode-hook 'smart-dash-mode)

(pyvenv-activate "~/miniconda3")

;;; pep8 compliance
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=150"))

;; python default variables
(require 'python)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt")

(provide 'setup-python)
;;; setup-python.el ends here
