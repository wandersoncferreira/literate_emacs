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
(bk/install-maybe 'alert)
(bk/install-maybe 'beginend)
(bk/install-maybe 'change-inner)
(bk/install-maybe 'symon)
(bk/install-maybe 'keyfreq)
(bk/install-maybe 'wgrep)
(bk/install-maybe 'dotenv-mode)

;;; windows
(windmove-default-keybindings)

;;; track emacs commands frequency
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;; tiny graphical system monitor
(require 'symon)
(symon-mode)

(require 'alert)
(when (eq system-type 'darwin)
  (setq-default alert-default-style 'osx-notifier))

(require 'google-this)
(google-this-mode 1)

;; rotate-window: change the windows inside the frame
;; rotate-layout: change the layout inside the frame. -- to |

(require 'vlf-setup)
(require 'webpaste)
(setq webpaste-provider-priority '("dpaste.de"))

;; change inner as in vi
(require 'change-inner)

;; redefine M-< and M-> for some modes
(require 'beginend)
(beginend-global-mode)

;;; edit several files in egrep buffer
(require 'wgrep)

(provide 'setup-misc)
;;; setup-misc.el ends here
