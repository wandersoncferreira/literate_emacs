;;; setup-web --- WEB
;;; Commentary:

;; mode to deal with WEB file formatting

;;; Code:

(bk/install-maybe-require 'web-mode)
(bk/install-maybe-require 'emmet-mode)
(bk/install-maybe-require 'rjsx-mode)

(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)


(provide 'setup-web)
;;; setup-web.el ends here
