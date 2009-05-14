;;; Pre-declare to avoid warnings
(defvar gnus-select-method)
(defvar gnus-inhibit-startup-message)
(defvar gnus-use-scoring)
(defvar gnus-kill-files-directory)
(defvar gnus-message-archive-group)
(defvar mail-host-address)

(eval-when-compile
  (require 'messagexmas)
  (require 'smiley))

;;; Top-level settings
;(setq gnus-select-method '(nntp "nntp.tsoft.com"))
(setq gnus-select-method '(nntp "tremere.blackperl.com"))
(setq gnus-inhibit-startup-message t)
(setq gnus-use-scoring t)
(setq gnus-kill-files-directory "~/News/scores/")
(setq gnus-message-archive-group nil)
(setq mail-host-address "tsoft.com")
(setq gnus-use-long-file-name '('not-score))
(setq gnus-use-cache 'passive)
;; Hooks
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook
	  '(lambda ()
	     (set-buffer-menubar current-menubar)
	     ;(delete-menu-item '("Recent"))
	     (delete-menu-item '("Edit"))
	     (delete-menu-item '("Apps"))
	     (delete-menu-item '("Tools"))
	     (delete-menu-item '("History"))))
(add-hook 'gnus-summary-mode-hook
	  '(lambda ()
	     (set-buffer-menubar current-menubar)
	     ;(delete-menu-item '("Recent"))
	     (delete-menu-item '("Edit"))
	     (delete-menu-item '("Apps"))
	     (delete-menu-item '("Tools"))
	     (delete-menu-item '("Hyperbole"))
	     (delete-menu-item '("History"))))
(add-hook 'gnus-article-display-hook 'gnus-smiley-display t)
(add-hook 'gnus-article-display-hook 'gnus-article-highlight)
