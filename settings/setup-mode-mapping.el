;; restclient
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

(provide 'setup-mode-mapping)
