;;*************************************************************************************
;; org mode export
;;*************************************************************************************
(setq org-startup-with-inline-images 1)     ;; will inlined display image when you open a file
;;*************************************************************************************
;; use Org-mode as the default mode for all README files
;;*************************************************************************************
(add-to-list 'auto-mode-alist '("README$" . org-mode))
(setq org-src-fontify-natively t)
;; below two variables is used to solve the issue when export to a html file
;; with python code, the number and variable will got a background color.
(setq py-number-face  nil)
(setq py-variable-name-face nil)

;; ;;below key binding is suggested by manual
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; ;;keep track of when a TODO item is finished
(setq org-log-done 'time)
;; ;;record a note along with the timestamp.
(setq org-log-done 'note)
;; ;;TODO entry to automatically change to DONE when all children are done.
(defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states)   ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;;(setq org-export-with-sub-superscripts nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For org appointment reminders
;; Get appointments for today
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
         (org-agenda-to-appt))

;; Run once, activate and schedule refresh
(my-org-agenda-to-appt)
(appt-activate t)
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

; 5 minute warnings
(setq appt-message-warning-time 15)
(setq appt-display-interval 5)

; Update appt each time agenda opened.
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

; Setup zenify, we tell appt to use window, and replace default function
(setq appt-display-format 'window)
;;(setq appt-disp-window-function (function my-appt-disp-window))

;;*************************************************************************************
;; resolve idle time,  “resolve” the time you were away 
;;*************************************************************************************
;; Emacs will alert a window to query what to do after 15 minutes of no operation on 
;; computer which means you are away.
(setq org-clock-idle-time 15)
;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)
;; (setq org-clock-persist t)
;;************************************************************************
;;org-keywords
;;************************************************************************
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)")
;;         (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;         (sequence "|" "CANCELED(c)")))

;; (setq org-todo-keywords
;;       '((sequence "TODO" "|" "DONE")
;;         (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
;;         (sequence "|" "CANCELED")
;;         (type "Fred" "Sara" "Lucy" "|" "DONE")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/keep-maintain-plan/refile.org")

(setq org-agenda-files (quote ("~/org/keep-maintain-plan/reading.org"
                               "~/org/keep-maintain-plan/home.org"
                               "~/org/keep-maintain-plan/refile.org")))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/keep-maintain-plan/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/org/keep-maintain-plan/refile.org")
               "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/keep-maintain-plan/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/keep-maintain-plan/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/keep-maintain-plan/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("p" "Phone call" entry (file "~/org/keep-maintain-plan/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/keep-maintain-plan/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
;;************************************************************************
;;org-mode babel
;;************************************************************************
;; Some initial langauges we want org-babel to support.
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (clojure . t)
   ))
;;************************************************************************
;;org-mode HTML publish
;;************************************************************************
;;when you type C-c C-e b command, it will add this css to current page.
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"file:///home/ryu/org/static/css/stylesheet-old.css\" type=\"text/css\" />
;; <link rel=\"stylesheet\" href=\"file:///home/ryu/org/static/styles/default.css\">
;; <script src=\"file:///home/ryu/org/static/highlight.pack.js\"></script>
;; <script>hljs.initHighlightingOnLoad();</script>")
(setq css-path (concat my-emacs-path "org/static/css/stylesheet.css"))
(setq org-html-head-extra (concat "<link rel=\"stylesheet\" href=" css-path " type=\"text/css\" />"))
;;(setq org-export-publishing-directory "~/org/exported_html")

(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("PythonCookbook-inherit"
         :base-directory "~/org/"
         :recursive t
         :base-extension "css\\|js"
         :publishing-directory "~/public_html/PythonCookbook/"
         :publishing-function org-publish-attachment
         )

        ("PythonCookbook-notes"
         :base-directory "~/org/PythonCookbook/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/PythonCookbook/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("PythonCookbook-static"
         :base-directory "~/org/PythonCookbook/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/PythonCookbook/"
         :publishing-function org-publish-attachment)
        
        ("English-notes"
         :base-directory "~/org/english/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/english/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("English-static"
         :base-directory "~/org/english/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/english/"
         :publishing-function org-publish-attachment)
        
        ("8000-oral-english-notes"
         :base-directory "~/org/8000-oral-english/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/8000-oral-english/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("English-static"
         :base-directory "~/org/8000-oral-english/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/8000-oral-english/"
         :publishing-function org-publish-attachment)

        ("Japanese-notes"
         :base-directory "~/org/japanese/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/japanese/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("Japanese-static"
         :base-directory "~/org/japanese/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/japanese/"
         :publishing-function org-publish-attachment)

        ("Compiler-notes"
         :base-directory "~/org/compiler/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/compiler/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("Compiler-static"
         :base-directory "~/org/compiler/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/compiler/"
         :publishing-function org-publish-attachment)

        ("Linux-notes"
         :base-directory "~/org/linux/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/linux/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("Linux-static"
         :base-directory "~/org/linux/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/linux/"
         :publishing-function org-publish-attachment)

        ("Emacs-notes"
         :base-directory "~/org/emacs/"
         :auto-sitemap t
         :index-filename "sitemap.org"
         :index-title "Sitemap"
         :recursive t
         :base-extension "org"
         :publishing-directory "~/public_html/emacs/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("Emacs-static"
         :base-directory "~/org/emacs/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/emacs/"
         :publishing-function org-publish-attachment)
        
        ("PythonCookbook" :components ("PythonCookbook-inherit" "PythonCookbook-notes" "PythonCookbook-static"))
        ("Japanese" :components ("Japanese-notes" "Japanese-static"))
        ("English" :components ( "English-notes" "English-static"))
        ("Compiler" :components ( "Compiler-notes" "Compiler-static"))
        ("Linux" :components ( "Linux-notes" "Linux-static"))
        ("Emacs" :components ( "Emacs-notes" "Emacs-static"))
        ))
