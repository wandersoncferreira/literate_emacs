(bk/install-maybe-require 'prodigy)

;; define some useful tags and proper ready messages
(prodigy-define-tag
  :name 'clojure-ring
  :ready-message "Started server on port [0-9]+")

;; register a service that you want to run
(prodigy-define-service
  :name "Clojure Eval Data"
  :command "lein"
  :args '("ring" "server")
  :cwd "/Users/wandersonferreira/eval-data"
  :stop-signal 'sigkill
  :tags '(clojure-ring company))


(provide 'setup-prodigy)
