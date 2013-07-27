(require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'shell-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4))) ;;tab = 2 is not conform with Pep8

;; jedi for python auto-completion
(require 'epc)
(require 'deferred)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
;;(autoload 'jedi:setup "jedi" nil t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
;;**********************************************************************
;; django 
;;**********************************************************************
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
