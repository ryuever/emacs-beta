;;************************************************************************
;; cedet  & ecb, cedet is shipped with emacs
;;************************************************************************
(semantic-mode)
(require 'semantic/analyze)
(provide 'semantic-analyze)
(provide 'semantic-ctxt)
(provide 'semanticdb)
(provide 'semanticdb-find)
(provide 'semanticdb-mode)
(provide 'semantic-load)
(setq stack-trace-on-error t)
;;ecb
(require 'ecb-autoloads)

(setq ecb-layout-name "left15")
