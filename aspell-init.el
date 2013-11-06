(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/git/emacs.d/aspell.en.pws")

(setq text-mode-hook '(lambda()
                        (flymode t)))
;;(setq ispell-process-directory (expand-file-name "~/git/"))

;;(setq ispell-personal-dictionary "~/Ubuntu One/aspell.en.pws")
;;(setq speck-aspell-home-dir "~/emacs/")
;;(setq speck-personal-dictionary-file "aspell.en.pws")
;;(setq speck-processs-dictionary-alist "~/Ubuntu One/")
; change Aspell process directory (aspell.en.prepl path)
;;(setq ispell-process-directory "~/Ubuntu One/")

;;*************************************************************************************
;; easy spell check
;;*************************************************************************************
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
