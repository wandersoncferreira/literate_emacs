;;; setup-go --- Golang settings
;;; Commentary:

;; Golang configurations

;;; Code:

(bk/install-maybe 'go-mode)

(require 'go-mode)

(defun bk/set-go-compiler ()
  "Function to configure the GO compiler."
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go run"))
  (local-set-key (kbd "M-p") 'compile))

(when (string-equal  system-type "darwin")
  (add-to-list 'exec-path "/usr/local/go/bin/go")
  (setenv "GOPATH" "/usr/local/go/bin/go"))

(setq gofmt-command "goimports")

(define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
(define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-*") 'pop-tag-mark)

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'bk/set-go-compiler)

(provide 'setup-go)
;;; setup-go.el ends here
