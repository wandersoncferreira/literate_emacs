;;; setup-google --- Google package for Emacs
;;; Commentary:

;;

;;; Code:

(bk/install-maybe 'google-this)
(require 'google-this)

(google-this-mode 1)
(diminish 'google-this-mode)


(provide 'setup-google)
;;; setup-google.el ends here
