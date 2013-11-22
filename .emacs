;;(warn "Loading .emacs")
(setq debug-on-error t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode t)
 '(ecb-auto-activate t)
 '(ecb-layout-window-sizes (quote (("left15" (ecb-directories-buffer-name 0.20253164556962025 . 0.6190476190476191) (ecb-methods-buffer-name 0.20253164556962025 . 0.35714285714285715)) ("left1" (ecb-directories-buffer-name 0.21518987341772153 . 0.2857142857142857) (ecb-sources-buffer-name 0.14556962025316456 . 0.3333333333333333) (ecb-history-buffer-name 0.06962025316455696 . 0.3333333333333333) (ecb-methods-buffer-name 0.21518987341772153 . 0.35714285714285715)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("/home/ryu")))
 '(indent-tabs-mode nil)
 '(ispell-personal-dictionary "aspell.en.pws")
 '(org-agenda-files (quote ("~/org/keep-maintain-plan/refile.org" "~/org/keep-maintain-plan/diary.org" "~/org/keep-maintain-plan/fee.org" "~/org/keep-maintain-plan/home.org")))
 '(org-list-indent-offset 0)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))) t)
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))) t)
 '(js2-warning ((t nil)) t)
 '(js2warning ((t nil)) t)
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t)
 '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FF33FF"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FF6600"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#00CCCC"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))

(setq home-path (expand-file-name "~"))
(defvar my-emacs-path (concat home-path "/git/emacs.d/")
  "Change this path to wherever you want emacs.d folder to be.
    ~ is your home directory (C:/Users/username/ [or]
    /Users/username/")
(defvar my-share-emacs-path (concat (expand-file-name "~") "/Ubuntu One/"))

;; Load init.el
(add-to-list 'load-path my-emacs-path)
(load "init")

;; For gabage code when you input Japanese chars in Latex.
(prefer-coding-system 'euc-jp-unix)

;; It will not interpret Japanese correctly in latex, if gb18030 is placed after euc-jp-unix
(prefer-coding-system 'gb18030)

(prefer-coding-system 'utf-8)
