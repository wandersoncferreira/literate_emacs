;;; Disable dynamic scopes ---  -*- lexical-binding: t -*- 
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
(bk/install-maybe 'keycast)
(bk/install-maybe 'change-inner)
(bk/install-maybe 'wgrep)
(bk/install-maybe 'dotenv-mode)
(bk/install-maybe 'pomodoro)
(bk/install-maybe 'discover-my-major)

;;; pomodoro
(require 'pomodoro)
(setq pomodoro-break-time 2
      pomodoro-long-break-time 5
      pomodoro-work-time 15
      pomodoro-desktop-notification t
      pomodoro-show-number t)

(setq-default mode-line-format
              (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
                    mode-line-format))

(unless (featurep 'pomodoro)
  (require 'pomodoro)
  (pomodoro-add-to-mode-line))

;;; windows
(windmove-default-keybindings)

(require 'google-this)
(google-this-mode 1)

;; rotate-window: change the windows inside the frame
;; rotate-layout: change the layout inside the frame. -- to |

(require 'vlf-setup)
(require 'webpaste)
(setq webpaste-provider-priority '("dpaste.de"))

;; change inner as in vi
(require 'change-inner)

;;; edit several files in egrep buffer
(require 'wgrep)

;;; neotree .... because.. why not!
(bk/install-maybe 'neotree)

;; mcfly
(setq mcfly-commands
      '(query-replace-regexp
        query-replace
        flush-lines
        keep-lines))

(provide 'setup-misc)
;;; setup-misc.el ends here
