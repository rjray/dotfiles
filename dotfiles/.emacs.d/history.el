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
(add-submenu '()
             '("History"
               ["No command history" (message "No command history") t]))

(set-menubar-dirty-flag)
