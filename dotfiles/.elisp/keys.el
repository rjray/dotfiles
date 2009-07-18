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

;;; These commands work for debugging programs. One command flags the
;;; current line as a debugging statement by appending the current value
;;; of debug-line-marker-string to the line. The second command removes
;;; every line from the current buffer that contains said string. If the
;;; the string is valueless, it is read from the user via minibuffer.

(defun set-line-as-debug ()
  "Append the debugging marker to the current line. If the variable
debug-line-marker-string is not set, prompts for a value to
append."
  (interactive)
  (end-of-line)
  (insert "  ")
  (if (not (boundp 'debug-line-marker-string))
      (setq debug-line-marker-string (read-string "Debugging marker: "))
    (insert debug-line-marker-string)))

(defun delete-all-debug-lines ()
  "Delete all the lines in a buffer containing the sequence outlined in
debug-line-marker-string. If that variable is not set, prompts the
user for a value. First queries the user whether or not to verify
each line before deleting it."
  (interactive)
  (defvar d--a-d-l)
  (make-variable-buffer-local 'd--a-d-l)
  (setq d--a-d-l (y-or-n-p "Query each delete? "))
  (if (not (boundp 'debug-line-marker-string))
      (setq debug-line-marker-string (read-string "Debugging marker: ")))
  (goto-char (point-min))
  (while (search-forward debug-line-marker-string
                         (point-max)
                         t)
    (if (or (not d--a-d-l)
            (dadl-y-or-n-p "Delete this line? "))
        (progn
          (beginning-of-line)
          (kill-line)
          (kill-line))))
  (message "Done."))

(defun dadl-y-or-n-p (msg)
  (interactive)
  (message msg)
  (defvar in-char)
  (make-variable-buffer-local 'in-char)
  (setq in-char (read-char))
  (while (not (or (= in-char 121)
                  (= in-char 89)
                  (= in-char 110)
                  (= in-char 78)))
    (beep)
    (message "One of YyNn only")
    (sleep-for 2)
    (message msg)
    (setq in-char (read-char)))
  (if (or (= in-char 121)
          (= in-char 89))
      t))

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
  (insert-file (concat homedir "/lib/" language "/" type "_hdr")))

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

(defun select-perl-mode ()
  "Switch to perl-mode."
  (interactive)
  (perl-mode)
  (redraw-display)
  (font-lock-fontify-buffer))

;;;
;;; This little bit of code is to enable me to switch between 2- and 4-space
;;; indention on the fly.
;;;
(defun switch-c-mode-spacing ()
  "Toggle between 2-space and 4-space indention in Emacs' C editing mode."
  (interactive)
  (cond ((eq c-basic-offset 4)
         (setq c-basic-offset 2))
        (t
         (setq c-basic-offset 4))))

(defun unix-fmt-region ()
  "Run the UNIX fmt command on the active region."
  (interactive)
  (cond ((region-active-p)
         (shell-command-on-region (region-beginning) (region-end)
                                  "fmt -w 79" nil t))
        (t (display-message 'error' "Region is not active now."))))

(defun untab-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (cond ((region-active-p)
         (untabify (region-beginning) (region-end)))
        (t (untabify 0 (point-max))))
  (save-buffer))

(defun run-perltidy ()
  "Run perltidy on the buffer or region"
  (interactive)
  (cond ((region-active-p)
         (shell-command-on-region (region-beginning) (region-end)
                                  "perltidy" nil t))
		(t (shell-command-on-region (point-min) (point-max)
									"perltidy" nil t))))

;;;
;;; Borrowed from benscott@mcs.net:
;;;
;;; The following collection of routines are used in conjunction with MH
;;; routines, as a part of using XEmacs in server mode for editing my mail
;;; messages.
;;;
(defun exmh-signclearpem nil
  (interactive)
  (exmh-exmhpgp "PGP/MIME-signclear"))

(defun exmh-encryptsignpem nil
  (interactive)
  (exmh-exmhpgp "PGP/MIME-encryptsign"))

(defun exmh-encryptsign nil
  (interactive)
  (exmh-exmhpgp "application/pgp-encryptsign"))

(defun exmh-exmhpgp (action)
  "Damned dynamic scoping!  I want lexical!"
  (interactive)
  (save-buffer)
  (call-process "~/bin/exmhpgp" nil nil nil action
                (file-name-nondirectory buffer-file-name) buffer-file-name)
  (revert-buffer t t))

(defun exmh-quote ()
  (interactive)
  (goto-char (point-max))
  (set-mark (point-max))
  (insert-file-contents "~/@")
  (goto-char (point-max))
  (exchange-point-and-mark)
  (sc-cite-original))

(defun exmh-save-and-stop ()
  (interactive)
  (mh-send-letter)
  (delete-frame))

(defun exmh-server-edit ()
  (interactive)
  (save-buffer)
  (gnuserv-edit))

(defun exmh-signature ()
  (interactive)
  (goto-char (point-max))
  (insert "\n--\n")
  (mh-insert-signature))

;;
;; Insert line numbers for each line in the region
;;
(defun insert-line-numbers ()
  "Insert line numbers (padded to 3 columns) at the start of each line in
the region. Each call resets the counter to 1."
  (interactive)
  (cond ((region-active-p)
         (let ((cur-line 0)
               (start-pt (region-beginning))
               (end-pt (region-end)))
           (save-excursion
             (goto-char start-pt)
             (while (< (point) end-pt)
               (beginning-of-line)
               (setq cur-line (+ cur-line 1))
               (insert (format "%-3d " cur-line))
               (setq end-pt (+ end-pt 4))
               (forward-line)))
           (message "%d line%s numbered"
                    cur-line
                    (if (= cur-line 1) " was" "s were"))))
        (t (display-message 'error' "Region is not active now."))))

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

(defun perl-journal-post-entry (&optional uid     &optional passwd
                                &optional comment &optional subject)
  "Post the contents of the the current region as a use.perl.org journal
entry."
  (interactive)
  (make-local-variable 'perl-journal-uid)
  (make-local-variable 'perl-journal-passwd)
  (make-local-variable 'perl-journal-command)
  (make-local-variable 'passwd-invert-frame-when-keyboard-grabbed)
  (let* ((p-command (cond (perl-journal-command)
                          (t "upj_post.pl")))
         (passwd-invert-frame-when-keyboard-grabbed nil)
         (p-uid     (cond (uid)
                          (perl-journal-uid)
                          (t (read-string "use.perl.org UID: "))))
         (p-passwd  (cond (passwd)
                          (perl-journal-passwd)
                          (t (read-passwd "use.perl.org Password: "))))
         (p-comment (cond ((and comment (= comment "no")) "no")
                          (t "yes")))
         (p-subject (cond (subject)
                          (t (read-string "Entry subject: "))))
         (command (format "%s -m %S -c %s -C %S"
                          p-command
                          p-subject
                          p-comment
                          (format "%s:%s" p-uid p-passwd))))
    (save-excursion
      (if (not (region-exists-p))
          (mark-whole-buffer))
      (shell-command-on-region (region-beginning) (region-end) command))))
