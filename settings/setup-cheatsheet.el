;;; clojure-mode --- Clojure
;;; Commentary:
;;; Code:

(use-package cheatsheet :ensure t)

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

(cheatsheet-add :group 'Ido
                :key "C-SPC"
                :description "Confirm search in Ido and restart the search from there")

(cheatsheet-add :group 'Ido
                :key "M-e"
                :description "Edit a search in ido minibuffer")

(cheatsheet-add :group 'Ido
                :key "M-r"
                :description "Toggle the search to regexp mode.")

(cheatsheet-add :group 'Window
                :key "C-x 4 u"
                :description "Undo the last window configuration setup")

(cheatsheet-add :group 'Window
                :key "<f9>"
                :description "Change Emacs to Full Screen mode.")

(cheatsheet-add :group 'Window
                :key "C-x 4 d"
                :description "Ido Dired other window")

(cheatsheet-add :group 'Window
                :key "M-x writeroom"
                :description "Focus mode to deep work!")


(cheatsheet-add :group 'Emacs-Lisp
                :key "C-c x"
                :description "Evaluate buffer")

(cheatsheet-add :group 'Code
                :key "C-c g"
                :description "Prefix to access MAGIT functionalities")

(provide 'setup-cheatsheet)
;;;  setup-cheatsheet.el ends here
