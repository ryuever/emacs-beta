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

(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; I want to use flymake only for .py files. and disable it for the rest. 
;; but It is always enabled. now disable flymake to html file.
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
;; flymake to be activated automatically
(add-hook 'find-file-hook 'flymake-find-file-hook)
;;**********************************************************************
;; django 
;;**********************************************************************
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
