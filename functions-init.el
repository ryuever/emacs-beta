;;**********************************************************************
;; Switching Next/Previous User Buffers
;;**********************************************************************
(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))
;;**********************************************************************
;; a copy line
;;**********************************************************************
;; below function is copied from http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html
(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )
;;**********************************************************************
;; customization created by ryuyutyo
;;**********************************************************************
(defun org-flush ()
  "Remove section header marks created in outline-mode."
  (interactive)
  (beginning-of-buffer)
  (delete-matching-lines "#\\+begin_html")
  (delete-matching-lines "#\\+BEGIN_HTML")
  (delete-matching-lines "#\\+end_html")
  (delete-matching-lines "#\\+END_HTML")
  (delete-matching-lines "</*ul>")
  (replace-string "<li>" "- ")
  (beginning-of-buffer)
  (replace-string "</li>" "")
)

(defun add-an-asterisk-line()
  "add an asterisk line as delimiter"
  (interactive)
  (insert ";;")
  (let ((i 0))
	(while (< i 85)
	  (insert "*")
	  (setq i (+ i 1))))
  )
;;**********************************************************************
;; How to adjust margin?
;;**********************************************************************
(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )
;;**********************************************************************
;;makes shell command always start a new shell.
;;**********************************************************************
(defadvice shell (around always-new-shell)
  "Always start a new shell."
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))
(ad-activate 'shell)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))
;;**********************************************************************
;;Open File in External App, It's useful for image files, PDF file, video, audio, ….
;;**********************************************************************
(defun ergoemacs-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil))
                                (start-process "" nil "xdg-open" fPath))) myFileList))))))
;; The following shows the file in desktop.
(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. with nautilus
    ) ))

(defun th-find-file-sudo (file)
"Opens FILE with root privileges."
(interactive "F")
(set-buffer (find-file (concat "/sudo::" file))))
(put 'dired-find-alternate-file 'disabled nil)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;;*************************************************************************************
;; full screen
;;*************************************************************************************
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;;*************************************************************************************
;; select text in quote which is from http://ergoemacs.org/emacs/modernization_mark-word.html
;;*************************************************************************************
(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “”
 For practical purposes, also: \"\", but not single quotes."
 (interactive)
 (let (p1)
   (skip-chars-backward "^<>([{“\"")
   (setq p1 (point))
   (skip-chars-forward "^<>)]}”\"")
   (set-mark p1)
   )
 )

(global-set-key (kbd "M-*") 'select-text-in-quote)
;;*************************************************************************************
;; Used for converting your chinese birthday date to the date in Gregorian form or 
;; reminding you 7 or 14 days before the given chinese birthday. Part is based on the codes 
;; in cal-china-x.el and calendar.el
;;*************************************************************************************
(defun cal-china-x-birthday-from-chinese (lunar-month lunar-day &optional interval)
  "Return birthday date this year in Gregorian form.
   In addition. it could be used to convert Chinese date to Gregorian format.

LUNAR-MONTH and LUNAR-DAY are date number used in Chinese lunar calendar.
INTERVAL is a identifier, 0 as default for the current year, 1 for the calculation of
the next year, and so on."

  (interactive "nlunar month: \nnlunar day: \nninterval: ")
  (let* ((birthday-chinese (list lunar-month lunar-day))
         (interval (if (not interval) 0 interval))
         (current-chinese-date (calendar-chinese-from-absolute
                                (calendar-absolute-from-gregorian
                                 (calendar-current-date))))
         (cycle (car current-chinese-date))
         (year (+ interval (cadr current-chinese-date)))
         (birthday-chinese-full `(,cycle ,year ,@birthday-chinese))
         (birthday-gregorian-full (calendar-gregorian-from-absolute
                                   (calendar-chinese-to-absolute
                                    birthday-chinese-full))))
    (message "Your next birthday in Gregorian is on %s"
             (calendar-date-string birthday-gregorian-full))))

(defun cal-date-from-lunar-date (lunar-month lunar-day &optional year)
  "Return birthday date this year in Gregorian form.
   In addition. it could be used to calculate Chinese date to Gregorian format.

LUNAR-MONTH and LUNAR-DAY are date number used in Chinese lunar calendar.
year is a Gregorian format, the current year as default."

  (interactive (list
                (string-to-number (read-string (format "Lunar month: ") nil nil nil))
                (string-to-number (read-string (format "Lunar day: ") nil nil nil))
                (string-to-number (read-string (format "Gregorian year: ") 
                                               nil nil (car (last (calendar-current-date)))))))

  (let* ((birthday-chinese (list lunar-month lunar-day))
         (interval (if year (- year (car (last (calendar-current-date)))) 0))
         (current-chinese-date (calendar-chinese-from-absolute
                                (calendar-absolute-from-gregorian
                                 (calendar-current-date))))
         (cycle (car current-chinese-date))
         (year (+ interval (cadr current-chinese-date)))
         (birthday-chinese-full `(,cycle ,year ,@birthday-chinese))
         (birthday-gregorian-full (calendar-gregorian-from-absolute
                                   (calendar-chinese-to-absolute
                                    birthday-chinese-full))))
    (message "The corresponding Gregorian date of Lunar Date is on %s"
             (calendar-date-string birthday-gregorian-full))))

(defun remind-chinese-birthday(lunar-month lunar-day)
  "Remind a chinese birthday, if today is your birthday or 7,14 days before your chinese birthday.

LUNAR-MONTH and LUNAR-DAY are date number used in Chinese lunar
calendar."
  (interactive "nlunar month: \nnlunar day: ")
  (let* ((current-chinese-date-14 (calendar-chinese-from-absolute 
                                   (+ (calendar-absolute-from-gregorian date) 14)))
                                   ;; (+ (calendar-absolute-from-gregorian (calendar-current-date)) 14)))
         (current-chinese-date-07 (calendar-chinese-from-absolute 
                                   (+ (calendar-absolute-from-gregorian date) 07)))
                                   ;; (+ (calendar-absolute-from-gregorian (calendar-current-date)) 07)))
         (current-chinese-date (calendar-chinese-from-absolute 
                                   (calendar-absolute-from-gregorian date)))
         (current-chinese-date-exclude-cycle-14 (cddr current-chinese-date-14))
         (current-chinese-date-exclude-cycle-07 (cddr current-chinese-date-07))
         (current-chinese-date-exclude-cycle (cddr current-chinese-date)))
    (if (and (eq (car current-chinese-date-exclude-cycle-14) lunar-month)
             (eq (cadr current-chinese-date-exclude-cycle-14) lunar-day))
        (format  "Reminder: Only 2 weekes until %s" entry)
      (if (and (eq (car current-chinese-date-exclude-cycle-07) lunar-month)
             (eq (cadr current-chinese-date-exclude-cycle-07) lunar-day))
          (format  "Reminder: Only 1 week until %s" entry)
        (if (and (eq (car current-chinese-date-exclude-cycle) lunar-month)
             (eq (cadr current-chinese-date-exclude-cycle) lunar-day))
          (format  "Reminder: Today is %s" entry))))))
;; for below logic is used to verify whether two list are the same. but one issue : 
;; for (3 10 2014), the corresponding lunar date is (02 10), After the processing 
;; of set-exclusive-or (10 02) is also matched. We should make sure month-to-month
;; day-to-day are the same. 
    ;; (if(not (set-exclusive-or current-chinese-date-exclude-cycle-14
    ;;                           (list lunar-month lunar-day)))
    ;;     (format  "Reminder: Only 2 weekes until %s" entry)
    ;;   (if (not (set-exclusive-or current-chinese-date-exclude-cycle-07 
    ;;                            (list lunar-month lunar-day)))
    ;;       (format  "Reminder: Only 1 week until %s" entry)
    ;;     (if (not (set-exclusive-or current-chinese-date-exclude-cycle
    ;;                            (list lunar-month lunar-day)))
    ;;         (format  "Reminder: Today is %s" entry)))
    ;;   )))
