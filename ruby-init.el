;;(setq ruby-indent-level 4)
;;(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; completion for ruby
(require 'rsense)
(add-hook 'ruby-mode-hook
      (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))
;; below is not needed any, because the enhenced-ruby inclue this plugin.
;; (add-to-list 'load-path "~/emacs/emacs_mode/ruby-electric-master")
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;;for ruby on rails
(require 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)
;; rhtml-mode for rails
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))
;; scss mode for .scss file
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
