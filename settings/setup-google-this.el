;; Setup Google-this
;; first contribution to configuration

(bk/install-maybe 'google-this)

(google-this-mode 1)

(require 'google-this)

(global-set-key (kbd "C-c w") 'google-this-search)

(provide 'setup-google-this)
;;; setup-google-this ends here =) ggizi



