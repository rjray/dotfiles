; Here is my cntl-l-map.el file, mentioned in another message,
; "Some Emacs stuff" .  Please reply to that *other* message
; to keep this file from being copied over and over.
;
; Cheers,
;
; -Len Weisberg

; ======================================================================
;;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         cntl-l-map.el
; Description:  A new keymap based on ^L for local (personal) functions
;            and a place for convenient bindings of any function
; Author:       Len Weisberg
; Created:      1991 Feb (expanded by merging with other files: 2003 Nov)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;  Place this file in a directory available in the load-path,
;;;  and load it from your .emacs .  The following sample code may be helpful:
; (setq load-path
;       (append (list
;            "/my/lisp/directory"
;  ;;;;  other lisp-library directories here
;              )
;            load-path ))
;
; (load-library "cntl-l-map")

;------------------------------------------------------------
  (global-unset-key "\C-l")
  (defvar ctl-l-map (make-keymap)
     "Keymap for local bindings and functions, prefixed by (^L)")
  (define-key global-map "\C-l" 'Control-L-prefix)
  (fset 'Control-L-prefix ctl-l-map)

;; definitions for the new keymap:
  (define-key ctl-l-map "\C-l" 'recenter)
  (define-key ctl-l-map "l"  'recenter)

  (define-key ctl-l-map "<"  'cursor-line-to-top-of-window)
  (define-key ctl-l-map ">"  'cursor-line-to-bottom-of-window)

  (define-key ctl-l-map "n"  'show-line-numbering)
            ; what-line or (with prefix arg) display-line-numbers
  (define-key ctl-l-map "g"  'goto-line)

  (define-key ctl-l-map "r"  'replace-string)
  (define-key ctl-l-map "R"  'replace-regexp)
  (define-key ctl-l-map "q"  'query-replace)
  (define-key ctl-l-map "Q"  'query-replace-regexp)
  (define-key ctl-l-map "c"  'toggle-case-fold-search)

  (define-key ctl-l-map "o"  'overwrite-mode)
  (define-key ctl-l-map "\C-w"  'kill-rectangle)
  (define-key ctl-l-map "\C-y"  'yank-rectangle)
  (define-key ctl-l-map "k"  'copy-line-to-killbuf)
  (define-key ctl-l-map "\C-k"  'kill-to-end-of-buffer)
             ; with prefix arg: kill to beginning of buffer

  (define-key ctl-l-map "d"  'diff-buffer)    ; compare buffer to disk copy
  (define-key ctl-l-map "s"  'shrink-window-to-point) ; prefix multiplies size
  (define-key ctl-l-map "S" 'insert-signature)
  (define-key ctl-l-map "h"  'electric-command-history)
 
  (define-key ctl-l-map "("  'blink-matching-open-int)
  (define-key ctl-l-map "p"  'blink-matching-open-int)
  (define-key ctl-l-map "%"  'go-to-matching-paren)
  (define-key ctl-l-map "P"  'go-to-matching-paren)

  (define-key ctl-l-map "w"  'write-region)
  (define-key ctl-l-map "u"  'dos2ux-region)

  (define-key ctl-l-map "\C-b"  'go-to-buffer-list)    ; improved  ^x-^b
  (define-key ctl-l-map "v"  'set-visited-file-name)
  (define-key ctl-l-map "f"  'what-file-name)

  (define-key ctl-l-map "="  'date-marker)

(defun cursor-line-to-top-of-window (line-num)
  "Scroll cursor-line to top of window;
(optional prefix-arg is line-number starting with 1)."
  (interactive "p")
  (recenter (1- line-num))
  )

(defun cursor-line-to-bottom-of-window (line-num)
  "Scroll cursor-line to bottom of window;
(optional prefix-arg is number of lines from bottom starting with 1)."
  (interactive "p")
  (recenter (- line-num))
  )

;------------------------------------------------------------
(defun show-line-numbering (display)
  "Display current line number (starting with 1) in the minibuffer.
With optional prefix arguement, displays temporary line numbers
for the window, restoring display after next key-press."
  (interactive "P")
  (if display (display-line-numbers)  (what-line))
  )

;------------------------------------------------------------
(defun go-to-buffer-list (files-only)
  "Buffer-list, placing cursor ready for action"
  (interactive "P")
  (if (not (get-buffer "*Buffer List*"))
      (list-buffers files-only))
  (bury-buffer "*Buffer List*")
  (list-buffers files-only)
  (if (not (equal major-mode 'Buffer-menu-mode))
      (switch-to-buffer-other-window "*Buffer List*"))
  (move-to-window-line 3)
  )

;------------------------------------------------------------
;;;  diff-buffer modified for Gnu-emacs v19:

(defun diff-buffer (usebackup)
"Take the diff(1) of current buffer and a file.
Default for file depends on the optional prefix arg:
  With no prefix arg, default is the file of the buffer.
  With negative prefix arg, file defaults to auto-save-file;
  With other prefix arg, default is most recent backup file;
Use narrow-to-region to diff part of the buffer."
  (interactive "P")
  (save-excursion
    (let  ((file-name buffer-file-name)
       (buffer-name (buffer-name))
       (diff-buffer (get-buffer-create "*Diff Output*"))
       )
      (if usebackup
      (if (or (equal usebackup '(4))  (>= usebackup 0))
          (setq file-name (make-backup-file-name file-name))
        (setq file-name (make-auto-save-file-name))
        ))
      (setq file-name
        (read-file-name
         (concat "file to diff with buffer<"  buffer-name ">: ")
         file-name  nil  t))

      (shell-command-on-region
       (point-min) (point-max)
       (concat "diff "  file-name " -")
       diff-buffer nil)
      (cond
       ((buffer-name diff-buffer)
    (switch-to-buffer-other-window diff-buffer)
    (goto-char (point-max))
    (insert
     (concat  "================================================\n"
          "-- diff output for  "  file-name  "   vs buffer<"
          buffer-name ">\n"
          (current-time-string)  "\n"))
    (shell-command-on-region (point) (point)
                 (concat "\ls -lq "  file-name)  t)
    )))))

;------------------------------------------------------------
(defun current-line-in-window ()
  "Return the vertical position of point in the current window.
Top line is 0.  Counts each line, even if it is a continuation."
  (save-excursion
    (let ((cntr 0)
      (win-top (window-start)))
      (beginning-of-line)
      (while (> (point) win-top)
    (setq cntr (1+ cntr))
    (vertical-motion -1))
      cntr)))

;------------------------------------------------------------
(defun shrink-window-to-point (multiplier)
  "Shrink window so the current line becomes the bottom of the window.
Optional prefix is muliplier ."
  (interactive "p")
  (let ((target-body-height
     (* multiplier
        (1+ (current-line-in-window)))    ;; (+1 for 0-orig)
        ))               
    (enlarge-window
     (- target-body-height (1- (window-height)))  ;; (-1 for mode-line)
     )))

;------------------------------------------------------------
(defun kill-to-end-of-buffer (beginning)
  "Kill from point to end of buffer;
with optional prefix arg kills to beginning of buffer."
  (interactive "P")
  (kill-region (point)
           (if beginning (point-min) (point-max)))
  )

;------------------------------------------------------------
(defvar signature-file-name "~/.nfsign")
(defvar alternative-signature-file-name "~/.nfsign.alt")

(defun insert-signature (alt)
  "Inserts a file named in the variable signature-file-name.
Optional argument gets signature from file in 
alternative-signature-file-name.

Default values for these variables are:
     signature-file-name:                ~/.nfsign
     alternative-signature-file-name:    ~/.nfsign\.alt"

  (interactive "P")
  (if alt (insert-file alternative-signature-file-name)
    (insert-file signature-file-name)))

;;;-------------------------------------------------------------------------
;; Note: The following two defuns are somewhat experimental, since they might
;;  be confusing if both defined:
;;  go-to-matching-paren looks at the character *after* (i.e. "over") point,
;;  while blink-matching-open-int looks at the character *before* point.

(defun go-to-matching-paren ()
  "Go to the matching 'parenthesis' (,),[,],{,} if on 'parenthesis'."
  (interactive)
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
    ((looking-at "[])}]") (forward-char) (backward-sexp 1))
    ))

(defun blink-matching-open-int ()
  "Put cursor momentarily under the open-paren, ( or [, matching the
character just *before* point.
If the matching character is not shown in the current window, then
its line will be displayed in the minibuffer."
  (interactive)
  (blink-matching-open))

;;;-------------------------------------------------------------------------
(defun dos2ux-region (to-dos)
  "Converts region from MS/DOS ascii-file format.
With optional prefix argument, converts *to* MS/DOS format."
  (interactive "P")
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (if to-dos (replace-regexp "\\([^\r]\\)$" "\\1\r")
        (replace-regexp "\r$" ""))
      (if (not to-dos)
          (progn
            (goto-char (point-max))
            (if (char-equal (preceding-char)  ?\^Z)
                (delete-backward-char 1))
      )))))

;;;-------------------------------------------------------------------------
;;; Len Weisberg (1993 Sep 19) based on a simple version by Andy Norman
(defun what-file-name (insert)
  "Display current buffer's filename.  Default is in minibuffer.
(Optional prefix-arg causes insertion into text; ^U^U inserts basename only.)"
  (interactive "P")
  (let ((fname (or  (buffer-file-name) default-directory)))
    (if insert
        (if buffer-read-only
            (progn
              (let ((cursor-in-echo-area t))
                (message "%s"   fname)
                (kill-line 0)
;;              (message "%s"   fname)
                ))
          (progn
            (set-mark (point))
            (insert
             (if (equal insert '(16))
                 (file-name-nondirectory fname)
               fname) )))
      (message "%s"   fname) )))

;;;----------------------------------------------------------------------
;;  (captured from kbd-macro)
(fset 'date-marker
   [?\C-u ?7 ?8 ?= return ?\C-@ ?\C-u ?\C-[ ?| ?d ?a ?t ?e
    ?  ?' ?+ ?% ?Y ?% ?/ ?% ?m ?/ ?% ?d ?  ?% ?a ?  ?% ?H ?: ?% ?M ?: ?% 
?S ?'
    return return])

;;;----------------------------------------------------------------------
