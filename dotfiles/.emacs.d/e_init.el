;;; Personal initialization code, try to keep from cluttering up .emacs with
;;; anything not written by emacs' custom-mode itself. This should be loaded
;;; after keys.el, et al.

(require 'cl)

(defadvice cperl-indent-command
  (around cperl-indent-or-complete)

  "Changes \\[cperl-indent-command] so it autocompletes when at the end of a word."
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    ad-do-it))
(eval-after-load "cperl-mode"
  '(progn (require 'dabbrev) (ad-activate 'cperl-indent-command)))

(setq-default tab-stop-list
          (mapcar (lambda (x) (* 4 x))
              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(setq-default tab-width 4)

(setq default-major-mode 'text-mode)
(setq delete-auto-save-files t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key "\e*" 'new-interactive-lisp)
(global-set-key "\C-cp" 'select-perl-mode)
(global-set-key "\C-cy" 'yow)
(global-set-key "\C-c\C-i" 'swap-tab-width)

;; Function-key bindings
(global-set-key [(f1)]           'other-window)

(global-set-key [(f4)]           'delete-window)
(global-set-key [(control f4)]   'delete-other-windows)

(global-set-key [(f5)]           'search-forward-regexp)
(global-set-key [(control f5)]   'search-backward-regexp)

(global-set-key [(f6)]           'query-replace)
(global-set-key [(control f6)]   'run-perltidy)

(global-set-key [(f7)]           'fill-paragraph-or-region)
(global-set-key [(control f7)]   'untab-buffer)

(global-set-key [(f8)]           (lambda ()
                                   (interactive)
                                   (count-region (point-min) (point-max))))
(global-set-key [(control f8)]   (lambda ()
								   (interactive)
								   (cond ((symbol-value 'mark-active)
										  (count-region (region-beginning)
														(region-end)))
										 (t (message "Region not active.")))))

(global-set-key [(f9)]           'find-file)
(global-set-key [(control f9)]   'load-library)

(global-set-key [(f10)]          'execute-extended-command)
(global-set-key [(control f10)]  'compile)

;; Meta-key combinations
(global-set-key [(meta z)]       (lambda ()
                                   (interactive)
                                   (beginning-of-line)
                                   (kill-line)))
(global-set-key [(meta e)]       'find-file)
(global-set-key [(meta g)]       'goto-line)
(global-set-key [(meta h)]       'help)
(global-set-key [(meta i)]       'overwrite-mode)
(global-set-key [(meta q)]       'quote)

(global-set-key [(insert)] 'overwrite-mode)
(global-set-key [(home)] 'home)
(global-set-key [(end)] 'end)

