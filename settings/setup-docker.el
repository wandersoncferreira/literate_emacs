;;; setup-docker --- Docker
;;; Commentary:

;; Containers eh?

;;; Code:


(use-package dockerfile-mode :ensure t)
(use-package docker :ensure t)
(use-package docker-compose-mode :ensure t)

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
