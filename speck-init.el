;;************************************************************************
;; speck,  aspell and ispell is not a prerequisite
;;************************************************************************
(require 'speck)
(defun turn-spell-checking-on ()
  "Turn speck-mode or flyspell-mode on."
  (speck-mode 1)
  ;;(flyspell-mode 1)
  )
;; what exectly does the "text-mode-hook" point to. it not just only the text-mode
;; but also have some other related mode.. so what's the relate modes?????
(add-hook 'text-mode-hook 'turn-spell-checking-on)


;;(setq ispell-personal-dictionary "~/Ubuntu One/aspell.en.pws")
;;(setq speck-aspell-home-dir "~/emacs/")
;;(setq speck-personal-dictionary-file "aspell.en.pws")
;;(setq speck-processs-dictionary-alist "~/Ubuntu One/")
; change Aspell process directory (aspell.en.prepl path)
;;(setq ispell-process-directory "~/Ubuntu One/")

;;************************************************************************
;; speck,  aspell and ispell is not a prerequisite
;;************************************************************************
(global-set-key (kbd "<f8>") 'speck-popup-menu-at-point)
(global-set-key (kbd "C-S-+") 'speck-add-word)
