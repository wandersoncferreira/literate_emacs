;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:

(use-package cheatsheet
  :ensure t)

;;; common
(cheatsheet-add :group 'Common
                :key "C-c r"
                :description "Restart Emacs")

(cheatsheet-add :group 'Common
                :key "C-x p"
                :description "Jump to mark, and pop a new position for mark off the ring")

(cheatsheet-add :group 'Common
                :key "C-M-<down>"
                :description "Duplicate line or region down")

(cheatsheet-add :group 'Common
                :key "C-M-<up>"
                :description "Duplicate line or region up")

;;; window
(cheatsheet-add :group 'Window
                :key "C-x 4 u"
                :description "Undo the last window configuration setup")


;;; emacs lisp
(cheatsheet-add :group 'Emacs-Lisp
                :key "C-c x"
                :description "Evaluate buffer")

;;; code
(cheatsheet-add :group 'Code
                :key "C-c g"
                :description "Prefix to access MAGIT functionalities")

(provide 'setup-cheatsheet)
;;;  setup-cheatsheet.el ends here
