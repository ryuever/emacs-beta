;;************************************************************************
;;color theme
;;************************************************************************
(require 'color-theme)
 (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-oswald)))
;;(color-theme-hober)
;;(color-theme-oswald)
;;(color-theme-comidia)
;;(color-theme-gnome2)
;;(color-theme-classic)

(add-to-list 'custom-theme-load-path (concat home-path "/.emacs.d/theme/"))
;;(load-theme 'solarized-dark t)
;;************************************************************************
;;rainbow-mode minor    have color values colored by themselves
;;************************************************************************
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook
              html-mode-hook
              python-mode-hook
              c++-mode-hook
              lisp-mode-hook))
   (add-hook hook (lambda () (rainbow-mode 1))))
(setq rainbow-mode t)
;;************************************************************************
;;rainbow-mode minor    have color values colored by themselves
;;************************************************************************
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
