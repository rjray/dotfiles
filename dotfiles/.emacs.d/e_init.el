;;; Personal initialization code, try to keep from cluttering up .emacs with
;;; anything not written by emacs' custom-mode itself. This should be loaded
;;; after keys.el, et al.

(defadvice cperl-indent-command
  (around cperl-indent-or-complete)

  "Changes \\[cperl-indent-command] so it autocompletes when at the end of a word."
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    ad-do-it))
(eval-after-load "cperl-mode"
  '(progn (require 'dabbrev) (ad-activate 'cperl-indent-command)))

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "~/.schema/schemas.xml"))

(setq-default tab-stop-list
          (mapcar (lambda (x) (* 4 x))
              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(setq-default tab-width 4)

(setq delete-auto-save-files t)

(global-set-key [?\C-.]
                (lambda ()
                  (interactive)
                  (scroll-down 1)))
(global-set-key [?\C-,]
                (lambda ()
                  (interactive)
                  (scroll-up 1)))
(global-set-key "\C-c\C-i" 'swap-tab-width)
(global-set-key "\C-x\C-r" 'ido-recentf-open)
;; Browse the kill-ring with C-c k:
(global-set-key (kbd "C-c k") 'browse-kill-ring)

(global-set-key "\C-w"
                (lambda (arg)
                  (interactive "p")
                  (cond ((symbol-value 'mark-active)
                         (kill-region (region-beginning) (region-end)))
                        (t (backward-kill-word arg)))))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Function-key bindings
(global-set-key [(f5)]           'call-last-kbd-macro)
(global-set-key [(control f5)]   'edit-last-kbd-macro)

(global-set-key [(f6)]           'search-forward-regexp)
(global-set-key [(control f6)]   'search-backward-regexp)

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
