;;; setup-go --- Golang
;;; Commentary:

;; Settings for the Golang programming language

;;; Code:

;; install additional go tools: godoc
;; go get golang.org/x/tools/cms/...

;; godef: it lets you quickly jump around the code
;; go get github.com/rogpeppe/godef

(bk/install-maybe 'go-mode)
(bk/install-maybe 'company-go)
(require 'go-mode)
(require 'company-go)

(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))))

(defun bk/set-go-compiler ()
  "Function to change the go compiler at cc."
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go run"))
  (local-set-key (kbd "M-p") 'compile))

(when (string-equal system-type "darwin")
  (let ((go-path "/usr/local/go/bin/go"))
    (add-to-list 'exec-path go-path)
    (setenv "GOPATH" "/Users/wandersoncferreira/go")))

(setq gofmt-command "goimports")
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-*") 'pop-tag-mark)
  )

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'bk/set-go-compiler)

(provide 'setup-go)
;;; setup-go.el ends here
