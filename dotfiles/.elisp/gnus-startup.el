;;; Pre-declare to avoid warnings
(defvar groupface-yellow)
(defvar groupface-green1)
(defvar groupface-green2)
(defvar groupface-skyblue)
(defvar groupface-mediumturquoise)
(defvar groupface-paleturquoise)
(defvar groupface-red)
(defvar gnus-button-url)

;;
(eval-when-compile
  (require 'custom))

(require 'custom)

;;; Force our screen placement
(if window-system
    ; Hard-coded position reflects Fvwm-supplied decorations
    (set-frame-position (buffer-dedicated-frame) 6 23)
  t)

;;; Top-level settings
(setq message-default-headers
      "From: rjray@tsoft.com (Randy J. Ray)\nReply-To: rjray@tsoft.com\n")
(setq gnus-check-new-newsgroups 'ask-server)
;(setq gnus-expert-user t)
(setq gnus-save-killed-list nil)
(setq message-cite-function 'sc-cite-original)

;;; Group mode settings
(setq gnus-group-line-format "%M%S%5y:%(%g%) %O\n")
;(setq gnus-group-highlight
;      '(((and (not mailp) (zerop unread)) . groupface-skyblue)
;        ((string-match "comp\\.lang\\.\\(perl\\|tcl\\)" group) . 
;          groupface-mediumturquoise)
;        ((string-match "rec\\.models\\.scale" group) . groupface-paleturquoise)
;        ((and (not mailp) (> unread 500)) . groupface-red)
;        ((and (not mailp) (> unread 300)) . groupface-yellow)
;        ((and (not mailp) (> unread 150)) . groupface-green1)
;        ((and (not mailp) (> unread 0)) . groupface-green2)))
(setq gnus-topic-line-format "%i[ %(%{%n%}%) -- %g groups, %A msgs ]%v\n")
(setq gnus-large-newsgroup 501)
(setq gnus-subscribe-hierarchical-interactive t)

;;; Summary mode settings
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject
        gnus-thread-sort-by-score
        gnus-thread-sort-by-number))
(define-key gnus-summary-mode-map "\C-cr" 'vm-forward-message-and-complain)

;;; Article mode settings
(setq gnus-button-url 'gnus-netscape-open-url)
(setq gnus-cite-attribution-face 'bold)
;(setq gnus-header-face-alist 
;      (list (list "From" nil 
;                  (custom-face-lookup "cyan" nil nil nil t nil))
;            (list "Subject" nil 
;                  (custom-face-lookup "cyan" nil nil nil t nil))
;            (list "Newsgroups:.*," nil
;                  (custom-face-lookup "red" nil nil t nil nil))
;            (list "Newsgroups:" nil
;                  (custom-face-lookup "pink" nil nil nil t nil))
;            (list "" 
;                  (custom-face-lookup "AntiqueWhite" nil nil t nil nil)
;                  (custom-face-lookup "wheat" nil nil nil t nil))))
