;;; setup-python.el --- Python
;;; Commentary:
;;; Code:


(bk/install-maybe 'elpy)
(bk/install-maybe 'electric-operator)
(bk/install-maybe 'py-autopep8)

;; elpy is a framework to handle python buffers
(require 'elpy)
(elpy-enable)
(delete `elpy-module-django elpy-modules)
(delete `elpy-module-highlight-indentation elpy-modules)

;; correct spaces between elements in python buffers.
(require 'electric-operator)
(add-hook 'python-mode-hook 'electric-operator-mode)

(pyvenv-activate "~/miniconda3")

;;; pep8 compliance
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=150"))


(provide 'setup-python)
;;; setup-python.el ends here
