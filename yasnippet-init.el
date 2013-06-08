(add-to-list 'load-path (concat my-emacs-path "/yasnippet-master"))
(require 'yasnippet)
(setq yas/root-directory (concat my-emacs-path "/yasnippet-master/snippets"))
(yas-global-mode 1)
