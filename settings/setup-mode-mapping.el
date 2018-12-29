;;; mode-mapping --- several mode mappings
;;; Commentary:
;;; Code:

;; yaml
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; restclient
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

;; snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; php
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; clojure
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

;; web mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; dockerfile
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(provide 'setup-mode-mapping)
;;; setup-mode-mapping.el ends here
