;;************************************************************************
;;for js2, for customization, you should first in a js2 mode file. then
;; M-x customize-group RET js2-mode
;;************************************************************************
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; get nodejs-mode from github https://github.com/emacsmirror/nodejs-mode

;;(require 'nodejs-mode)
;; for the garbage prompt issue. run command "run-js" to revoke the node shell
;; because of the usage of nodejs-mode, the below setup is not needed any more
(require 'js-comint)
(setq inferior-js-program-command "nodejs")
(setq inferior-js-mode-hook
      (lambda ()
	;; We like nice colors
	(ansi-color-for-comint-mode-on)
	;; Deal with some prompt nonsense
	(add-to-list
	 'comint-preoutput-filter-functions
	 (lambda (output)
	   (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))
;;(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;;;;add jquery-doc.el and jquery-doc-data.el to load-path ....enable autocomplete
(add-to-list 'load-path (concat my-emacs-path "/jquery-doc.el-master"))
(require 'jquery-doc)
;; adds ac-source-jquery to the ac-sources list
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(add-to-list 'load-path (concat my-emacs-path "/tern-master/emacs/"))
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
