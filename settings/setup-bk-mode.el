;;; setup-bk-mode --- Bartuka's minor mode to handle special keys
;;; Commentary:

;; The idea is to produce context specific bindings

;;; Code:


;;; special binding mode
(defvar bartuka-mode-map (make-keymap)
  "Special bindings active in specific contexts.")

(define-minor-mode bartuka-mode
  "A minor mode for activating context specific bindings."
  :global t
  :lighter " Bartuka"
  :init-value t)

(add-to-list 'emulation-mode-map-alists
             `((bartuka-mode . ,bartuka-mode-map)))

(define-key bartuka-mode-map
  (kbd "C-x o")
  '(menu-item "" nil :filter
              (lambda
                (&optional _)
                (when (one-window-p)
                  'other-frame))))

(bartuka-mode +1)

(provide 'setup-bk-mode)
;;; setup-bk-mode.el ends here
