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

;(add-hook 'activate-menubar-hook 'create-history-menu)
;(add-submenu '()
;          '("History" ["No command history" (message "No command history") t]))

(defun my-delete-trailing-whitespace (begin end)
  "Delete trailing whitespace from all lines in region BEGIN and END."
  (save-excursion
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (replace-match ""))
    (widen)))

;; From Michał Marczyk, via
;; http://stackoverflow.com/questions/3887362/clojure-functions-for-emacs
(defmacro -> (e &rest es)
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,(caar es) ,e ,@(cdar es))
        `(,(car es) ,e))
    (if (consp es)
        `(-> (-> ,e ,(car es)) ,@(cdr es))
      e)))

(defmacro ->> (e &rest es)
  (if (and (consp es) (not (consp (cdr es))))
      (if (consp (car es))
          `(,@(car es) ,e)
        `(,(car es) ,e))
    (if (consp es)
        `(->> (->> ,e ,(car es)) ,@(cdr es))
      e)))

;; From Jake McCrary
(defun clojure-swank ()
  "Launch swank-clojure from users homedir/.lein/bin"
  (interactive)
  (let ((buffer (get-buffer-create "*clojure-swank*")))
    (flet ((display-buffer (buffer-or-name &optional not-this-window frame)
                           nil))
      (bury-buffer buffer)
      (shell-command "~/.lein/bin/swank-clojure &" buffer))
    (set-process-filter
     (get-buffer-process buffer)
     (lambda (process output)
       (with-current-buffer "*clojure-swank*"
         (insert output))
       (when (string-match "Connection opened on local port +\\([0-9]+\\)"
                           output)
         (slime-connect "localhost" (match-string 1 output))
         (set-process-filter process nil))))
    (message "Starting swank.. ")))


(defun clojure-kill-swank ()
  "Kill swank process started by lein swank."
  (interactive)
  (let ((process (get-buffer-process "*clojure-swank*")))
    (when process (ignore-errors (slime-quit-lisp))
          (let ((timeout 10))
            (while (and (> timeout 0)
                        (eql 'run (process-status process)))
              (sit-for 1)
              (decf timeout)))
          (ignore-errors (kill-buffer "*clojure-swank*")))))
