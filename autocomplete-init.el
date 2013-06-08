(require 'auto-complete-config)
(ac-config-default)
;; ;; auto load minor mode
;; (add-hook 'org-mode-hook (lambda()(auto-complete-mode 1)))
(add-hook 'py-shell-hook (lambda()(auto-complete-mode 1)))
(add-hook 'inferior-js-mode-hook (lambda()(auto-complete-mode 1)))
(add-hook 'org-mode-hook (lambda()(auto-complete-mode 1)))
(add-hook 'shell-mode-hook (lambda()(auto-complete-mode 1)))
