;;************************************************************************
;; This is for the package you install through 'M-x list-package':Initialize & Install Package,
;; By default, Emacs loads packages after it loads your .emacs init file. if you don't execute this first. then
;; (require 'org-publish) will abend. because we install org-mode in elpa
;;************************************************************************
(require 'package)
(add-to-list
 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list
 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

