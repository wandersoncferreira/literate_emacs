;;; setup-org.el --- Org
;;; Commentary:
;;; Code:

(require 'org)
(require 'ox-publish)

(setq org-use-speed-commands t
      org-return-follows-link t
      org-log-done 'time
      org-confirm-elisp-link-function t
      org-confirm-babel-evaluate nil)

;;; Getting Things Done
(org-babel-load-file "~/.emacs.d/settings/gtd.org")


(eval-after-load 'org
  '(setq-default fill-column 60))

;; fix the behavior of ace-jump in org-mode buffers
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-'") 'ace-jump-mode)))

(defun bk/my-blog-footer (arg)
  "Function to insert the footer of my blog passing an ARG."
  (with-temp-buffer
    (insert-file-contents "~/Documents/bkblog/org/footer.html")
    (buffer-string)))

(defun bk/my-blog-header (arg)
  "Function to insert the header of my blog passing an ARG."
  (with-temp-buffer
    (insert-file-contents "~/Documents/bkblog/org/header.html")
    (buffer-string)))

(setq org-publish-project-alist
      '(("blog-notes"
	 :base-directory "~/Documents/bkblog/org"
	 :base-extension "org"
	 :publishing-directory "~/Documents/bkblog/public"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-label 4
	 :section-numbers nil
	 :html-head nil
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :html-postamble bk/my-blog-footer
	 :html-preamble bk/my-blog-header)

	("blog-static"
	 :base-directory "~/Documents/bkblog/org"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
	 :publishing-directory "~/Documents/bkblog/public"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("blog"
	 :components ("blog-notes" "blog-static"))))


(bk/install-maybe-require 'org-present)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
	       (lambda ()
		 (org-present-big)
		 (org-display-inline-images)
		 (org-present-hide-cursor)
		 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
	       (lambda ()
		 (org-present-small)
		 (org-remove-inline-images)
		 (org-present-show-cursor)
		 (org-present-read-write)))))


(provide 'setup-org)
;;; setup-org.el ends here
