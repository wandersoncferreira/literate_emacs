;;; mode-mapping --- several mode mappings
;;; Commentary:
;;; Code:


(defvar bk/auto-install-alist
  '(("\\Jenkinsfile\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("yasnippet/snippets" snippet-mode snippet-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\Dockerfile\\'" dockerfile-mode dockerfile-mode)
    ("\\.phtml\\'" web-mode web-mode)
    ("\\.tpl\\.php\\'" web-mode web-mode)
    ("\\.html\\.twig\\'" web-mode web-mode)
    ("\\.fish\\'" fish-mode fish-mode)
    ("\\.dot\\'" graphviz-dot-mode)
    ("\\.html?\\'" web-mode web-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.scala\\'" scala-mode scala-mode)))

(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; macro from prelude!
(defmacro bk/auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (bk/auto-install extension package mode))))
 bk/auto-install-alist)


(provide 'setup-mode-mapping)
;;; setup-mode-mapping.el ends here
