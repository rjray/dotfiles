;;; These are the defun's tied to key-combos in the other config files.

(defun yank-and-indent ()
  "yank and then indent the newly formed region according to mode"
  (interactive)
  (yank)
  (indent-region (mark t) (point) nil))

(defun update-mod-time ()
  "Insert a new time string to reflect the current time, in a module header."
  (interactive)
  (kill-line)
  (insert (current-time-string)))

(defun new-interactive-lisp ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Testbed*"))
  (lisp-interaction-mode))

(defun revert-buffer-no-confirm ()
  "Revert current buffer to copy on disk without prompting user."
  (interactive)
  (revert-buffer t t))

;; Taken from crisp.el, written by Gary D. Foster <Gary.Foster@corp.sun.com>
;--------------------------------------------------------------------------
;                                                                  HOME/END
(defvar last-last-command nil
  "Internal variable.")

(defun home ()
  "Home - begin of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'home) (eq last-last-command 'home))
     (goto-char (point-min)))
    ((eq last-command 'home)
     (move-to-window-line 0))
    (t (beginning-of-line)))
  (setq last-last-command last-command))

(defun end ()
  "End - end of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'end) (eq last-last-command 'end))
     (goto-char (point-max)))
    ((eq last-command 'end)
     (move-to-window-line -1)
     (end-of-line))
    (t (end-of-line)))
  (setq last-last-command last-command))

(defun count-region (start end)
  "Count lines, words and characters in region."
  (interactive "r")
  (let ((l (count-lines start end))
        (w (count-words start end))
        (c (- end start)))
    (message "Region has %d line%s, %d word%s and %d character%s."
             l (if (= 1 l) "" "s")
             w (if (= 1 w) "" "s")
             c (if (= 1 c) "" "s"))))

(defun count-words (start end)
  "Return number of words between START and END."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (forward-word 1)
          (setq count (1+ count)))))
    count))

(defun what-cursor-position-and-line ()
  ;; So you don't need what-line any longer.
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
  (let* ((char (following-char))
         (beg (point-min))
         (end (point-max))
         (pos (point))
         (total (buffer-size))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
         (hscroll (if (= (window-hscroll) 0)
                      ""
                    (format " Hscroll=%d" (window-hscroll))))
         (col (current-column))
         (line (save-restriction
                 (widen)
                 (save-excursion
                   (beginning-of-line)
                   (1+ (count-lines 1 (point)))))))
    (if (= pos end)
        (if (or (/= beg 1) (/= end (1+ total)))
            (message "point=%d of %d(%d%%) <%d - %d>  line %d column %d %s"
                     pos total percent beg end line col hscroll)
          (message "point=%d of %d(%d%%)  line %d column %d %s"
                   pos total percent line col hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
          (message
           "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  line %d column %d %s"
           (single-key-description char) char pos total percent beg end
           line col hscroll)
        (message "Char: %s (0%o)  point=%d of %d(%d%%)  line %d column %d %s"
                 (single-key-description char) char pos total percent
                 line col hscroll)))))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[({[]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))

;;;
;;; Insert, at the point, pre-formed headers for a variety of types
;;;
(defun insert-text-hdr (language type)
  "Basic insert-header function, encapsulated by other language-specific calls."
  (interactive)
  (insert-file-contents (concat homedir "/lib/" language "/" type "_hdr")))

;;  Now, create the language- and type-specific calls
(let ((languages '("perl" "c" "c++" "java" "tcl" "lisp"))
      (types     '("file" "sub" "lib")))
  (mapcar '(lambda (language)
             (mapcar '(lambda (type)
                        (let ((function-name (format "%s-insert-%s-hdr"
                                                     language type))
                              (docu-string
                               (format "Insert ~/lib/%s/%s_hdr at current point"
                                       language type)))
                          (eval (list 'defun (intern function-name) '()
                                      docu-string
                                      (list 'interactive)
                                      (list 'insert-text-hdr language type)))))
                     types))
          languages))

(defun unix-fmt-region ()
  "Run the UNIX fmt command on the active region."
  (interactive)
  (cond ((symbol-value 'mark-active)
         (shell-command-on-region (region-beginning) (region-end)
                                  "fmt -w 79" nil t))
        (t (message "Region is not active now."))))

(defun untab-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (cond ((symbol-value 'mark-active)
         (untabify (region-beginning) (region-end)))
        (t (untabify 0 (point-max))))
  (save-buffer))

(defun fill-paragraph-or-region ()
  "If the region is active, call fill-region. Otherwise, fill-paragraph."
  (interactive)
  (cond ((symbol-value 'mark-active)
         (fill-region (region-beginning) (region-end)))
        (t (fill-paragraph nil))))

(defun run-perltidy ()
  "Run perltidy on the buffer or region"
  (interactive)
  (cond ((symbol-value 'mark-active)
         (shell-command-on-region (region-beginning) (region-end)
                                  "perltidy" nil t))
        (t (shell-command-on-region (point-min) (point-max)
                                    "perltidy" nil t))))

;;
;; Borrowed from hm--html-mode
;;
(defvar xml--just-insert-less-than nil
  "Internal variable.")

(defun xml--html-less-than ()
  "Inserts the entity '&gt;'."
  (interactive)
  (insert "&lt;"))

(defun xml--html-smart-less-than ()
  "Insert a '<' or the entity '&lt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-less-than)
           xml--just-insert-less-than)
      (progn
        (delete-char -1)
        (xml--html-less-than)
        (setq xml--just-insert-less-than nil))
    (insert ?<)
    (setq xml--just-insert-less-than t)))

(defvar xml--just-insert-greater-than nil
  "Internal variable.")

(defun xml--html-greater-than ()
  "Inserts the entity '&gt;'."
  (interactive)
  (insert "&gt;"))

(defun xml--html-smart-greater-than ()
  "Insert a '>' or the entity '&gt;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-greater-than)
           xml--just-insert-greater-than)
      (progn
        (delete-char -1)
        (xml--html-greater-than)
        (setq xml--just-insert-greater-than nil))
    (insert ?>)
    (setq xml--just-insert-greater-than t)))

(defvar xml--just-insert-ampersand nil
  "Internal variable.")

(defun xml--html-ampersand ()
  "Inserts the entity '&amp;'."
  (interactive)
  (insert "&amp;"))

(defun xml--html-smart-ampersand ()
  "Insert a '&' or the entity '&amp;' if you execute this command twice."
  (interactive)
  (if (and (eq last-command 'xml--html-smart-ampersand)
           xml--just-insert-ampersand)
      (progn
        (delete-char -1)
        (xml--html-ampersand)
        (setq xml--just-insert-ampersand nil))
    (insert ?&)
    (setq xml--just-insert-ampersand t)))

;;; Hack to swap tab-width between 4 and 8 at whim
(defun swap-tab-width ()
  "Swap the tab width between 4 and 8"
  (interactive)
  (cond ((= tab-width 4)
         (setq tab-width 8))
        (t (setq tab-width 4)))
  (redraw-display))

;;; Set up electric-return that can be used in Lisp/ParEdit modes
(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;;; Things from EmacsWiki.org
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                        recentf-list))
         (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file (cdr (assoc-string fname tocpl))))))