;;; ipinfo.el --- Get IP info from ipinfo.io    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Daniel Kraus

;; Author: Daniel Kraus <daniel@kraus.my>
;; Version: 0.1
;; Package-Requires: ((request "0.3.0") (emacs "24.4"))
;; Keywords: utilities convenience
;; URL: https://github.com/dakra/ipinfo.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Get IP info from ipinfo.io

;;; Code:

(eval-when-compile (require 'cl))
(require 'request)

;;;###autoload
(defun ipinfo (ip)
  "Return ip info from ipinfo.io for IP."
  (interactive "sEnter IP to query (blank for own IP): ")
  (request
   (concat "https://ipinfo.io/" ip)
   :headers '(("User-Agent" . "Emacs ipinfo.io Client")
              ("Accept" . "application/json")
              ("Content-Type" . "application/json;charset=utf-8"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message
                (mapconcat
                 (lambda (e)
                   (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
                 data "\n"))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Can't receive ipinfo. Error %S " error-thrown)))))
(provide 'ipinfo)
;;; ipinfo.el ends here
