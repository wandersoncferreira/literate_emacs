;;; setup-javascript --- Javascript
;;; Commentary:

;;

;;; Code:

(bk/install-maybe 'json-mode)
(bk/install-maybe 'js2-mode)
(bk/install-maybe 'coffee-mode)
(bk/install-maybe 'typescript-mode)
(bk/install-maybe 'prettier-js)
(bk/install-maybe 'xref-js2)
(bk/install-maybe 'js-comint)
(bk/install-maybe 'skewer-mode)
(bk/install-maybe 'add-node-modules-path)

(require 'json-mode)
(require 'js2-mode)
(require 'coffee-mode)
(require 'typescript-mode)
(require 'prettier-js)
(require 'xref-js2)
(require 'js-comint)
(require 'skewer-mode)
(require 'add-node-modules-path)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)
(eval-when-compile (require 'cl))
(setq auto-mode-alist
      (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
	    (loop for entry in auto-mode-alist
		  unless (eq preferred-javascript-mode (cdr entry))
		  collect entry)))

;; js2-mode
(setq-default js2-bounce-indent-p nil)
(eval-after-load 'js2-mode
  '(progn
     (setq-default js2-mode-show-parse-errors nil
		   js2-mode-show-strict-warnings nil)
     (autoload 'flycheck-get-checker-for-buffer "flycheck")
     (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
       (unless (flycheck-get-checker-for-buffer)
	 (set (make-local-variable 'js2-mode-show-parse-errors) t)
	 (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
     (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
     (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
     (js2-imenu-extras-setup)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)
(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

(when (executable-find "ag")
  (eval-after-load 'js2-mode
    '(progn
       (define-key js2-mode-map (kbd "M-.") nil)
       (add-hook 'js2-mode-hook
		 (lambda ()
		   (add-hook 'xref-backend-functions
			     #'xref-js2-xref-backend nil t))))))


;;; coffescript
(eval-after-load 'coffee-mode
  '(progn
     (setq coffee-js-mode preferred-javascript-mode
	   coffee-tab-width preferred-javascript-indent-level)))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

(setq js-comint-program-command "node")
(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

(defun inferior-js-mode-hook-setup ()
  "Better output when using nodejs."
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))


;;; alternatively, use skewer-mode
(eval-after-load 'skewer-mode
  '(add-hook 'skewer-mode-hook
	     (lambda () (inferior-js-keys-mode -1))))

(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-hook 'add-node-modules-path))

(eval-after-load 'js2-mode-mode
  '(add-hook 'js2-mode-mode-hook 'add-node-modules-path))


;;; completion using tern
;;; run npm install -g tern
(bk/install-maybe 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)))
(eval-after-load 'tern-mode
  '(progn
     (define-key tern-mode-keymap (kbd "M-.") nil)
     (define-key tern-mode-keymap (kbd "M-,") nil)))

(provide 'setup-javascript)
;;; setup-javascript.el ends here
