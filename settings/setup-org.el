;; defaults
(require 'org)
(setq org-use-speed-commands t
      org-return-follows-link t
      org-confirm-elisp-link-function t
      org-confirm-babel-evaluate nil)


;; fix the behavior of ace-jump in org-mode buffers
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-'") 'ace-jump-mode)))

;;; blogging
(require 'ox-publish)

(defun bk/my-blog-footer (arg)
  (with-temp-buffer
    (insert-file-contents "~/Documents/bkblog/org/footer.html")
    (buffer-string)))

(defun bk/my-blog-header (arg)
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


(provide 'setup-org)
