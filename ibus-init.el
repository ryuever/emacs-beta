;;**********************************************************************************************************
;; Ibus : The first statement is used to resolve issue "("process: ibus-agent status: exit") "
;; and even if you add this statement but still can't start ibus, then check *Message*, if
;; there is a prompt "python xlib is required", then "sudo apt-get install python-xlib"
;;**********************************************************************************************************
(add-to-list 'load-path (concat my-emacs-path "/ibus-el-0.3.2"))
(setq ibus-python-shell-command-name "python2.7")
(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)
(setq ibus-cursor-color '("red" "blue" "limegreen"))
