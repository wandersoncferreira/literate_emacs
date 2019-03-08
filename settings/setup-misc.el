;;; misc --- my miscellaneous
;;; Commentary:
;;; Code:

;;; install only packages
(bk/install-maybe 'htmlize)
(bk/install-maybe 'restclient)
(bk/install-maybe 'yaml-mode)
(bk/install-maybe 'graphviz-dot-mode)
(bk/install-maybe 'quickrun)
(bk/install-maybe 'windresize)
(bk/install-maybe 'vlf)
(bk/install-maybe 'rotate)
(bk/install-maybe 'webpaste)
(bk/install-maybe 'google-this)


(require 'google-this)
(google-this-mode 1)


;; rotate-window: change the windows inside the frame
;; rotate-layout: change the layout inside the frame. -- to |

(require 'vlf-setup)


(provide 'setup-misc)
;;; setup-misc.el ends here
