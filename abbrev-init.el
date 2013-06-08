(setq abbrev-file-name (concat my-emacs-path "abbrev_defs"))
;; reads the abbreviations file on startup,like M-x read-abbrev-file
(quietly-read-abbrev-file)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq save-abbrevs t)              ;; save abbrevs when files are saved
