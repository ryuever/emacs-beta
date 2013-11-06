(global-set-key [C-f1] 'show-file-name) ; Or any other key you want
;;**********************************************************************
;; Switching Next/Previous User Buffers
;;**********************************************************************
(global-set-key (kbd "<f5>") 'find-file) ; Open file or dir
(global-set-key (kbd "<f6>") 'kill-this-buffer) ; Close file

(global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
(global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
(global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown

;;***************************************************************************
;;key binding learning from http://doc.norang.ca/org-mode.html#GettingStarted
;;***************************************************************************
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'previous-buffer)

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> g") 'gnus)

(global-set-key (kbd "<f12>") 'org-agenda)
;;**********************************************************************
;;Open File Fast: recentf-mode
;;**********************************************************************
(recentf-mode 1) ; keep a list of recently opened files
;; set F7 to open a list of recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;;**********************************************************************
;; a copy line
;;**********************************************************************
(global-set-key (kbd "C-c C-k") 'copy-line-or-region) ; copy
(global-set-key (kbd "<f2>") 'cut-line-or-region) ; cut

;; (global-set-key (kbd "C-c C-y") 'yank) ; paste.
;; (global-set-key (kbd "<f2>") 'cut-line-or-region) ; cut.
;; (global-set-key (kbd "<f3>") 'copy-line-or-region) ; copy.
;; (global-set-key (kbd "<f4>") 'yank) ; paste.
;;************************************************************************
;; Shell, shortcut for key stroke C-l
;;************************************************************************
(add-hook 'shell-mode-hook (lambda()
  		    (local-set-key (kbd "<f5>") 'recenter-top-bottom)))
;;************************************************************************
;; speck,  aspell and ispell is not a prerequisite
;;************************************************************************
(global-set-key (kbd "<f8>") 'speck-popup-menu-at-point)
(global-set-key (kbd "C-S-+") 'speck-add-word)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
;;**********************************************************************
;; outline-minor mode, http://www.emacswiki.org/emacs/EasyCodeOutline
;;**********************************************************************
(defun outline-binding()
  (interactive)
  (local-set-key [M-up] 'hide-body)
  (local-set-key [M-down] 'show-all)
  (local-set-key [C-down] 'show-subtree)
  (local-set-key [C-up] 'hide-subtree)
  )
(add-hook 'outline-minor-mode-hook 'outline-binding)

;;*************************************************************************************
;; bond the function fullscreen to f11
;;*************************************************************************************
;;(add-hook 'org-mode-hook (lambda()
;;  		    (local-set-key (kbd "<f11>") 'fullscreen)))
(global-set-key [f11] 'fullscreen)

;;*************************************************************************************
;; temp fix the issue when you use emacs in shell "M-RET" will be scanned as M-j/C-M-j
;;*************************************************************************************
(add-hook 'org-mode-hook (lambda()
  		    (local-set-key (kbd "M-j") 'org-insert-heading)))

(add-hook 'org-mode-hook (lambda()
  		    (local-set-key (kbd "C-M-j") 'org-insert-heading)))
