;;**********************************************************************
;; Command to Lookup Reference: wiki , dictionary, google
;;**********************************************************************
(defun lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches you to your browser."
 (interactive)
 (let (myWord myUrl)
   (setq myWord
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "_" myWord))
  (setq myUrl (concat "http://en.wikipedia.org/wiki/" myWord))
  (browse-url myUrl)
  ))

(defun lookup-word-definition ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
 (interactive)
 (let (myWord myUrl)
   (setq myWord
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "%20" myWord))
  (setq myUrl (concat "http://www.answers.com/main/ntquery?s=" myWord))

  (browse-url myUrl)
  ;; (w3m-browse-url myUrl) ;; if you want to browse using w3m
   ))

;;(global-set-key (kbd "<f9>") 'lookup-word-definition)
