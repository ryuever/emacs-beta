(load "packages-init")  ;ELPA setup
(load "variables-init")
(load "keybinding-init") 
(load "abbrev-init")
(load "autocomplete-init")
(load "auto-pair-init")
(load "ecb-init")
(load "face-init")
(load "functions-init")
(load "html-init")
(load "c-init.el")

;; If you run emacs in shell, ibus no need to setup.
(load "ibus-init")

(load "js2-init")
(load "lookup-init")
(load "org-init")
(load "python-init")
(load "ruby-init")
(load "yasnippet-init")

;; after the upgrade to emacs 24.3.1, speck will cause "invalid timer" error
;;(load "speck-init")

(load "aspell-init")
(load "latex-init")
(load "flymake-init")
;;************************************************************************
;; add my own alias for productivity.
;;************************************************************************
(load "my_alias")
;;**********************************************************************
;; setup for emacs font 
;;**********************************************************************
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
(require 'calendar)
