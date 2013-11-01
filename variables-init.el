;;(warn "Loading .emacs")
(setq debug-on-error t)
;;************************************************************************
;; 显示括号匹配
;;************************************************************************
(show-paren-mode t)
(setq show-paren-style 'expression) ; highlight entire bracket expression
;;(setq show-paren-style 'parentheses)
;;************************************************************************
;;显示行数
;;************************************************************************
(global-linum-mode t)
(setq column-number-mode t)
(setq line-number-mode t)
;;************************************************************************
;; 在标题栏提示你目前在什么位置
;;************************************************************************
(setq frame-title-format "ryu@%b")
;;************************************************************************
;; 默认显示 80列就换行
;;************************************************************************
(setq default-fill-column 80)
;;************************************************************************
;; Remove splash screen
;;************************************************************************
(setq inhibit-splash-screen t)
;; setup splash screen img
;;'(fancy-splash-image "/home/ryu/shufei2.png")
;;**********************************************************************
;; Use ibuffer to List Buffers
;;**********************************************************************
(defalias 'list-buffers 'ibuffer)
;;**********************************************************************
;; make buffer switch command show suggestions on minibuffer 
;;**********************************************************************
(ido-mode 1)
;;************************************************************************
;; 设置窗口的显示
;;************************************************************************
(setq initial-frame-alist '((width . 82) (height . 40)))
(setq default-frame-alist '((width . 80) (height . 45)))
;;**********************************************************************
;; make backup to a designated dir, mirroring the full path
;;**********************************************************************
(setq temporary-file-directory "~/.emacs.d/emacs-backup-autosave/")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
;;********************************************************************************
;; show file path
;;********************************************************************************
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;;**********************************************************************
;; have lines soft wrapped at word boundary?
;;**********************************************************************
;;(global-visual-line-mode 1) ; 1 for on, 0 for off.
;;**********************************************************************
;; have cursor line always highlighted
;;**********************************************************************
;; (global-hl-line-mode 1) ; turn on highlighting current line
;;**********************************************************************
;;be enabled for all windows at startup by putting in the InitFile the following line
;;**********************************************************************
;;(add-hook 'window-configuration-change-hook (lambda () (ruler-mode 1)))

;;*************************************************************************************
;; open a windows layout when you input command "M-x gdb"
;;*************************************************************************************
(setq gdb-many-windows t) 

;;*************************************************************************************
;; solve the issue "Command attempted to use minibuffer while in minibuffer"
;; when you type command M-x query-replace-regexp RET C-x 8 RET 000a RET f 
;;*************************************************************************************
(setq enable-recursive-minibuffers t)
