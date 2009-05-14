;; journal-mode v1.0 - Easily maintain a simple text journal.
;;    M-x journal-mode places you in journal-mode.
;;    M-Return inserts a new entry.
;;    The heading format may be modified by setting journal-mode-date-format.
;;
;; This Emacs mode is released under v2.0 or greater of the GNU GPL.
;;
;; vsync@quadium.net
;; http://quadium.net/

(defvar journal-mode-date-format "\n** %a %Y-%m-%d - %H:%M"
  "Format for journal entry dates, as from `format-time-string'.")

(defun insert-journal-entry (topic)
  "Insert a new journal entry, prompting for an optional topic.
Bound to M-Return by default."
  (interactive "sTopic (optional): ")
  (end-of-buffer)
  (insert (format-time-string journal-mode-date-format))
  (end-of-line)
  (if (/= 0 (length topic))
      (insert ": " topic "\n")
    (insert "\n"))
  (end-of-buffer))

(defun journal-mode ()
  "Major mode for editing a journal."
  (interactive)
  (kill-all-local-variables)
  (use-local-map journal-mode-map)
  (setq mode-name "Journal")
  (setq major-mode 'journal-mode)
  (auto-fill-mode))

(defvar journal-mode-map nil)

(if journal-mode-map
    ()
  (setq journal-mode-map (make-sparse-keymap))
  (define-key journal-mode-map [(meta return)] 'insert-journal-entry))
