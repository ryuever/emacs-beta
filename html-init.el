;;**********************************************************************
;; select a region, then call “replace-html-chars-region”, and have all & < > 
;; replaced by their encoded entity. 
;;*******************************************************************************
(defun replace-html-chars-region (start end)
  "Replace “<” to “&lt;” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction 
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" nil t))
    )
  )
;;**********************************************************************
;; nxhtml
;;**********************************************************************
(load "~/.emacs.d/nxhtml-master/autostart.el")
;; disable background color
(setq mumamo-background-colors nil)
