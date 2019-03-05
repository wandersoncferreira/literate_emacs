;;; bk-after-init --- After init hook (?)
;;; Commentary:

;;

;;; Code:

(bk/install-maybe 'diminish)
(require 'diminish)
(diminish 'paredit-mode)
(diminish 'google-this-mode)
(diminish 'flyspell-mode)
(diminish 'abbrev-mode)
(diminish 'whitespace-mode)
(diminish 'hs-minor-mode)
(diminish 'whitespace-cleanup-mode)
(diminish 'subword-mode)
(diminish 'yas-minor-mode)
(diminish 'company-mode)
(diminish 'eldoc-mode)


(provide 'bk-after-init)
;;; bk-after-init.el ends here
