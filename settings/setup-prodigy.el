;;; setup-prodigy.el --- Services
;;; Commentary:
;;; Code:

(bk/install-maybe 'prodigy)
(require 'prodigy)

;; define some useful tags and proper ready messages
(prodigy-define-tag
  :name 'clj-ring
  :ready-message "Started server on port [0-9]+")

;; register a service that you want to run
(prodigy-define-service
  :name "Clojure Eval Data"
  :command "lein"
  :args '("ring" "server")
  :cwd "/Users/wandersonferreira/eval-data"
  :stop-signal 'sigkill
  :tags '(clj-ring))

(prodigy-define-service
  :name "Pricing"
  :command "python"
  :args '("app.py")
  :cwd "/Users/wandersonferreira/captalys/api-pricing/pricing"
  :stop-signal 'sigkill
  :tags '(captalys))

(prodigy-define-service
  :name "Robot"
  :command "python"
  :args '("app.py")
  :cwd "/Users/wandersonferreira/captalys/credito-digital-robot/cdRobot"
  :stop-signal 'sigkill
  :tags '(captalys))

(prodigy-define-service
  :name "Serviços"
  :command "python"
  :args '("app.py")
  :cwd "/Users/wandersonferreira/captalys/credito-digital-servicos/capservice"
  :stop-signal 'sigkill
  :tags '(captalys))

(provide 'setup-prodigy)
;;; setup-prodigy.el ends here
