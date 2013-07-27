;;**********************************************************************
;; associated packages
;;**********************************************************************
;; flymake-cursor.el : Display flymake messages in the minibuffer. 
;;                     install from lisp package no need other setup
;;**********************************************************************
;; flymake to be activated automatically
;;**********************************************************************
;; (require 'flymake)  not needed any more
(add-hook 'find-file-hook 'flymake-find-file-hook)
;;**********************************************************************
;; disable flymake for latex
;;**********************************************************************
(delete '("\\.tex\\'" flymake-simple-tex-init) flymake-allowed-file-name-masks)
;;**********************************************************************
;; I want to use flymake only for .py files. and disable it for the rest. 
;; but It is always enabled. now disable flymake to html file.
;;**********************************************************************
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
;;**********************************************************************
;; add syntax check for python code
;;**********************************************************************
(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
;;**********************************************************************
;; syntax check for c++
;;**********************************************************************
;; Even though I could got a flymake mode when start a .cc file. but it 
;; not display the syntax error message ????
;; "Makefile" should be in the same dir with the cpp source file. 
(defvar flymake-additional-compilation-flags nil)
(put 'flymake-additional-compilation-flags 'safe-local-variable 'listp)
;; no need to arrange Makefile
(defun flymake-cc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (common-args (append (list "-Wall" "-W" "-fsyntax-only" local-file)
                              flymake-additional-compilation-flags)))
    (if (eq major-mode 'c++-mode)
        (list "g++" common-args)
      (list "gcc" common-args))))
 
(loop for ext in '("\\.c$" "\\.h$" "\\.cc$" "\\.cpp$" "\\.hh$")
      do
      (push `(,ext flymake-cc-init) flymake-allowed-file-name-masks))
 
(add-hook 'c-mode-hook (lambda () (flymake-mode t)))
(add-hook 'c++-mode-hook (lambda () (flymake-mode t)))
