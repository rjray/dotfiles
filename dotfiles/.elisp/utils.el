;;; Basic utility functions, most of which were obtained elsewhere. None of
;;; these should be directly bound to keys (those belong in keys.el) -- these
;;; should only be things like support code for hooks, menu functionality,
;;; etc. Calls to "add-hook" or "add-submenu" are OK here (as opposed to
;;; hooks.el or e_init.el) as long as they only refer to functionality defined
;;; here.

(require 'cl)

(defun edit-string (s &optional bufname temp bindings)
  "Pops up a buffer to recursively edit STRING.  If terminated using
abort-recursive-edit, the original string is returned.  If terminated with
exit-recursive-edit, the edited string is returned.  Optional 2nd arg
BUFNAME is name of buffer to use.  Optional 3rd arg TEMP non-nil means
kill buffer when done.  Optional last arg BINDINGS is a keymap of
bindings to use in the edit buffer."
  (let ((buf (get-buffer-create (or bufname "*Edit*"))))
    (if bindings (use-local-map bindings))
    (save-window-excursion
      (pop-to-buffer buf)
      (erase-buffer)
      (insert s)
      (beginning-of-buffer)
      (prog1
          (condition-case e
              (progn
                (recursive-edit)
                (buffer-string))
            (quit s))
        (if temp (kill-buffer (current-buffer))
          (bury-buffer))))))

;; **********************************************************************
;; *
;; * Add a command history entry to the menu bar
;; *
;; **********************************************************************
(defun call-history-menu (cmd)
  (message (prin1-to-string cmd))
  (setq command-history (cons cmd (delete cmd command-history)))
  (eval cmd))

(defun compute-history-menu (max-items command-history history-menu)
  (if (and (> max-items 0)
           (not (equal command-history nil)))
      (let ((s (prin1-to-string (car command-history))))
        (if (equal nil (member s history-menu))
            (let ((v (make-vector 3 't)))
              (aset v 0 (substring s 0 (min (length s) history-menu-max-len)))
              (aset v 1 (list 'call-history-menu (list 'function
                                                       (car command-history))))
              (cons v (compute-history-menu ( - max-items 1)
                                            (cdr command-history)
                                            (cons s history-menu))))
          (compute-history-menu max-items (cdr command-history)
                                history-menu)))))

(defun create-history-menu ()
  (if (equal command-history nil)
      (add-submenu '()
                '("History"
                  ["No command history" (message "No command history") t]))
    (add-submenu '()
                 (append '("History")
                         (compute-history-menu history-menu-max-items
                                               command-history nil)))))

(add-hook 'activate-menubar-hook 'create-history-menu)
(add-submenu '()
          '("History" ["No command history" (message "No command history") t]))
(set-menubar-dirty-flag)

;;; Lifted from ctags.el to make tags work under cperl-mode:

;; Initialize the tags table in the current buffer.
;; Returns non-nil iff it is a valid tags table.  On
;; non-nil return, the tags table state variable are
;; made buffer-local and initialized to nil.
(defun initialize-new-tags-table ()
  (set (make-local-variable 'tags-table-files) nil)
  (set (make-local-variable 'tags-completion-table) nil)
  (set (make-local-variable 'tags-included-tables) nil)
  ;; Value is t if we have found a valid tags table buffer.
  (let ((hooks tags-table-format-hooks))
    (while (and hooks
                (not (funcall (car hooks))))
      (setq hooks (cdr hooks)))
    hooks))

(defun update-session ()
  (interactive)
  (save-excursion
    (defvar session-buffers-list)
    (let ((all-buffers (reverse (buffer-list))))
      (while (and all-buffers
                  (cond ((char-equal
                          ?*
                          (aref (buffer-name (car all-buffers)) 0)) t)
                        ((char-equal
                          ?\ 
                          (aref (buffer-name (car all-buffers)) 0)) t)
                        (t (setq session-buffers-list
                                 (append session-buffers-list
                                         (buffer-name (car all-buffers)))))))
        (setq all-buffers (cdr all-buffers)))
      all-buffers)
    (message "%s" session-buffers-list)))

(cond ((not (fboundp 'facep))
       (defun custom-facep (face)
         "No faces"
         nil))
      ((string-match "XEmacs" emacs-version)
       (defalias 'custom-facep 'find-face))
      (t
       (defalias 'custom-facep 'facep)))

(defun delete-trailing-whitespace (begin end)
  "Delete trailing whitespace from all lines in region BEGIN and END."
  (save-excursion
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (replace-match ""))
    (widen)))
