;;; setup-ido.el --- completions
;;; Commentary:
;;; Code:

(bk/install-maybe 'ido-completing-read+)
(bk/install-maybe 'ido-at-point)

(require 'ido)
(require 'ido-at-point)

(ido-ubiquitous-mode +1)
(ido-mode +1)
(ido-everywhere +1)
(ido-at-point-mode +1)

(setq ido-use-virtual-buffers t
      ido-enable-prefix nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)

(setq ido-decorations
      '("\n-> "                             ; left bracket around prospect list
	""                                  ; right bracket around prospect list
	"\n   "                             ; separator between prospects, depends on `ido-separator`
	"\n   ..."                          ; inserted at the end of a truncated list of prospects
	"["                                 ; left bracket around common match string
	"]"                                 ; right bracket around common match string
	" [No match]"
	" [Matched]"
	" [Not readable]"
	" [Too big]"
	" [Confirm]"
	"\n-> "                             ; left bracket around the sole remaining completion
	""                                  ; right bracket around the sole remaining completion
	))

(setq ido-file-extensions-order '(".clj" ".py" ".org" ".php" ".rest"))

(setq-default imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")
(add-to-list 'ido-ignore-directories "elpa")
(add-to-list 'ido-ignore-directories "vendor")
(add-to-list 'ido-ignore-files "\\.DS_Store")

(provide 'setup-ido)
;;; setup-ido.el ends here
