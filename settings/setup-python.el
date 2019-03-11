;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'pip-requirements)
(bk/install-maybe 'electric-operator)
(bk/install-maybe 'auto-virtualenvwrapper)
(bk/install-maybe 'py-isort)
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

;; python default variables
(require 'python)
(setq tab-width 4
	  python-indent-offset 4
	  python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt")

;; virtualenv
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate)
(add-hook 'window-configuration-change-hook 'auto-virtualenvwrapper-activate)
(add-hook 'focus-in-hook 'auto-virtualenvwrapper-activate)

;; sorting imports
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)
(setq py-isort-options '("--lines=150"))


;; pep8
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=150"))

(provide 'setup-python)
;;; setup-python.el ends here
