;;; exwm-monitors --- Handle multi-monitor gracefully in EXWM
;;; Commentary:

;; This started as a stolen peace of code from Reddit and I am trying
;; to pull something out of it. Currently, beta stage. WIP.

;;; Code:

(require 'cl)
(require 'subr-x)
(require 'dash)
(require 'exwm-randr)

(defvar exwm-monitors--last-screen-info nil
  "Most recent screen info.")

(defvar exwm-monitors--screen-infos nil
  "List of attributes about your screen.

The list is a property list with the following properties:

`name'
  Name of your screen.")

(defvar exwm-monitors--screen-specs nil
  "List of attributes about your specs.

The list is a property list with the following properties:

`name'
  Name of your screen.")

(defun exwm-monitors-define-screen-info (&rest args)
  "Define a new screen info from ARGS."
  (let* ((screen-name (plist-get args :name))
         (fn (lambda (screen) (string= (plist-get screen :name) screen-name)))
         (screen (-first fn exwm-monitors--screen-infos)))
    (when screen
      (setq exwm-monitors--screen-infos (-reject fn exwm-monitors--screen-infos)))
    (push args exwm-monitors--screen-infos)))

(defun exwm-monitors-default-screen-info ()
  "Get default screen info from xrandr parsed output."
  (let* ((default-directory "/")
         (lines (split-string
                 (shell-command-to-string
                  ;; fetch lines of "name id wxh"
                  ;; brittle(?): assumes single preferred mode.
                  (concat "xrandr --verbose | "
                          "awk '"
                          "/( connected|00ffffffffffff)/ "
                          "{ f=1; print $1 } "
                          "f{ if(/HSync/) { f=0; print $1 } }"
                          "' "
                          "| xargs -n 3"))
                 "\n" t)))
    (setq exwm-monitors--screen-infos
          (mapcar (lambda (info)
                    (plist-put info :connected nil))
                  exwm-monitors--screen-infos))
    (setq exwm-monitors--screen-infos
          (mapcar (lambda (line)
                    (let* ((items (split-string line))
                           (dimensions (mapcar 'string-to-number
                                               (split-string (caddr items) "x")))
                           (fn (lambda (screen) (string= (plist-get screen :name) (car items))))
                           (cfg (-first fn exwm-monitors--screen-infos)))
                      (if cfg
                          (plist-put cfg :connected t)
                        (list :name (car items)
                              :nickname (car items)
                              :connected t
                              :width (car dimensions)
                              :height (cadr dimensions)))))
                  lines))))

(defun exwm-monitors-xrandr-default ()
  "Load all active monitors in horizontal position at default width and height."
  (let* ((screen-info (exwm-monitors-default-screen-info))
         (fn (lambda (screen)
               (when (plist-get screen :connected)
                 (format "--output %s --mode %sx%s "
                         (plist-get screen :name)
                         (plist-get screen :width)
                         (plist-get screen :height))))))
    (shell-command-to-string "xrandr -s 0") ;reset xrandr to guarantee a clean initial state
    (shell-command-to-string
     (format "xrandr %s"
             (string-join (mapcar fn screen-info))))))

(defun exwm-monitors-define-spec (&rest args)
  "Define a new specification for how your screens will be handled from ARGS."
  (let* ((spec-name (plist-get args :name))
         (fn (lambda (spec) (string= (plist-get spec :name) spec-name)))
         (spec (-first fn exwm-monitors--screen-specs)))
    (when spec
      (setq exwm-monitors--screen-specs (-reject fn exwm-monitors--screen-specs)))
    (push args exwm-monitors--screen-specs)))

(defun exwm-monitors-resolve-predicates (spec infos)
  "There is a little DSL between SPEC and INFOS that need to be decoded."
  (let* ((predicate (plist-get spec :pred))
         (operator (car predicate)))
    (cond
     ((equalp operator :only)
      (equalp (mapcar (lambda (s) (plist-get s :nickname)) infos)
              (cadr predicate))))))

(defun exwm-monitors-xrandr (screen-info)
  "Configure screens in preferred layouts described by SCREEN-INFO."
  (let* ((screen-count (length screen-info))
         (screen-specs exwm-monitors--screen-specs)
         (valid-spec (-first (lambda (spec)
                               (exwm-monitors-resolve-predicates spec screen-info))
                             screen-specs)))
    (if (not valid-spec)
        (exwm-monitors-xrandr-default)

      (shell-command
       (format "xrandr %s"
               (string-join
                (mapcar (lambda (action)
                          (let* ((choose-info (lambda (info) (string= (plist-get info :nickname) (car action))))
                                 (info (-first choose-info screen-info)))
                            (message "INFO")
                            (prin1 info)
                            ;; it needs to be a better loop with recursion.. because
                            ;; if this is the first guy to be assigned and has a right-of property, need to check if there is anyone esle...
                            ;; learn how to position the windows correctly to do proper computation
                            (cond
                             ((equalp (cadr action) :off) (format "--output %s --off " (plist-get info :name)))
                             ((equalp (cadr action) :auto) (format "--output %s --mode %sx%s --pos 0x0 "
                                                                   (plist-get info :name)
                                                                   (plist-get info :width)
                                                                   (plist-get info :height)))
                             ((equalp (cadr action) :right) (format "--output %s --mode %sx%s --right-of eDP1"
                                                                    (plist-get info :name)
                                                                    (plist-get info :width)
                                                                    (plist-get info :height)))
                             (t (format "--output %s --off " (plist-get info :name))))))
                        (plist-get valid-spec :action))))))
    (setq exwm-monitors--last-screen-info screen-info)))

(defun exwm-monitors-x-center (monitor-attributes)
  "The center of the monitor in x, from MONITOR-ATTRIBUTES."
  (+ (nth 0 (alist-get 'geometry monitor-attributes))
     (/ (nth 2 (alist-get 'geometry monitor-attributes)) 2)))

(defun exwm-monitors-y-center (monitor-attributes)
  "The center of the monitor in y, from MONITOR-ATTRIBUTES."
  (+ (nth 1 (alist-get 'geometry monitor-attributes))
     (/ (nth 3 (alist-get 'geometry monitor-attributes)) 2)))

(defun exwm-monitors-set-workspace-plist ()
  "Set exwm-randr-workspace-output-plist for the current displays.
Lists two sets of workspaces enumerated from zero across the displays."
  (let ((display-names
         (mapcar (lambda (d) (alist-get 'name d))
                 (sort (display-monitor-attributes-list) ;; sort bottom-top left-right
                       (lambda (left right)
                         (let ((right (- (exwm-monitors-x-center left)
                                         (exwm-monitors-x-center right)))
                               (below (- (exwm-monitors-y-center left)
                                         (exwm-monitors-y-center right))))
                           (or (> below 0) (and (= 0 below) (< right 0)))))))))
    (message "Setting up for display(s) %s" display-names)
    (setq display-names (apply 'append
                               (make-list (/ exwm-workspace-number
                                             (length display-names))
                                          display-names)))
    (setq exwm-randr-workspace-output-plist
          (apply 'append
                 (mapcar* 'list
                          (number-sequence 0 (1- (length display-names)))
                          display-names)))))

(defun exwm-monitors-handle-screen-changes ()
  "React to screen change - configure displays.

If the configured screens from my/last-screen-info haven't
changed, then do nothing, unless prefix arg FORCE is set."
  (interactive "P")
  (let* ((default-directory "/"))
    (when (not (equal exwm-monitors--screen-infos
                      exwm-monitors--last-screen-info))
      (message "Screens changed - reconfiguring")
      (exwm-monitors-xrandr exwm-monitors--screen-infos)
      (exwm-monitors-set-workspace-plist))))

(defun exwm-monitors-initial-setup ()
  "Call this function to handle screen layout at startup time."
  (exwm-monitors-handle-screen-changes))

(add-hook 'exwm-randr-screen-change-hook #'exwm-monitors-handle-screen-changes)
(exwm-randr-enable)

(provide 'exwm-monitors)
;;; exwm-monitors.el ends here
