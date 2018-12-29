;;; setup-docker --- Docker
;;; Commentary:

;; Containers eh?

;;; Code:

(bk/install-maybe 'dockerfile-mode)
(bk/install-maybe 'docker)

(require 'docker)

(provide 'setup-docker)
;;; setup-docker.el ends here
