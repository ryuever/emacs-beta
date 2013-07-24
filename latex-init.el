;;**********************************************************************
;; disable flymake for latex
;;**********************************************************************
(delete '("\\.tex\\'" flymake-simple-tex-init) flymake-allowed-file-name-masks)
