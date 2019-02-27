;;; setup-docker --- Docker
;;; Commentary:

;; Containers eh?

;;; Code:

(bk/install-maybe 'dockerfile-mode)
(bk/install-maybe 'docker)
(bk/install-maybe 'docker-compose-mode)

(require 'docker)
(require 'docker-tramp-compat)

;; open files in Docker containers
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(provide 'setup-docker)
;;; setup-docker.el ends here
