(add-hook 'font-lock-mode-hook
          '(lambda () (setq font-lock-maximum-decoration 4)))

;; (add-hook 'before-save-hook
;;           '(lambda ()
;;              (cond ((symbol-value 'mark-active)
;;                     (my-delete-trailing-whitespace (region-beginning)
;;                                                    (region-end)))
;;                    (t (delete-trailing-whitespace)))))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode t)
             (paredit-mode t)
             (define-key lisp-mode-map "%" 'match-paren)
             (if (and (featurep 'menubar)
                      current-menubar)
                 (progn
                   ;; make a local copy of the menubar, so our modes don't
                   ;; change the global menubar
                   (set-buffer-menubar current-menubar)
                   (add-submenu nil emacs-lisp-mode-menubar-menu)))))

(add-hook 'c-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (setq c-default-style "bsd")
             (define-key c-mode-map "\C-cs" 'switch-c-mode-spacing)
             (define-key c-mode-map "\C-cu" 'update-mod-time)
             (define-key c-mode-map "\C-c\C-h" 'c-toggle-hungry-state)
             (define-key c-mode-map "\C-c\C-d" 'delete-all-debug-lines)
             (setq c-basic-offset 4)
             (c-set-offset 'case-label '*)
             (c-set-offset 'statement-case-intro '*)
             (c-set-offset 'statement-case-open '*)))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (highlight-parentheses-mode t)
             (paredit-mode t)
             (slime-mode t)))

(add-hook 'csharp-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (setq c-default-style "bsd")
             (define-key c-mode-map "\C-cs" 'switch-c-mode-spacing)
             (define-key c-mode-map "\C-cu" 'update-mod-time)
             (define-key c-mode-map "\C-c\C-h" 'c-toggle-hungry-state)
             (define-key c-mode-map "\C-c\C-d" 'delete-all-debug-lines)
             (setq c-basic-offset 4)
             (c-set-offset 'case-label '*)
             (c-set-offset 'statement-case-intro '*)
             (c-set-offset 'statement-case-open '*)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (setq c-default-style "bsd")
             (define-key c++-mode-map "\C-cu" 'update-mod-time)
             (define-key c++-mode-map "\C-c\C-h" 'c-toggle-hungry-state)
             (define-key c++-mode-map "\C-c\C-d" 'delete-all-debug-lines)
             (setq c-basic-offset 4)
             (c-set-offset 'case-label '*)
             (c-set-offset 'statement-case-intro '*)))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (require 'prove)
             (turn-on-font-lock)
             (define-key cperl-mode-map "\C-cf" 'perl-insert-file-hdr)
             (define-key cperl-mode-map "\C-cl" 'perl-insert-lib-hdr)
             (define-key cperl-mode-map "\C-cs" 'perl-insert-sub-hdr)
             (define-key cperl-mode-map "\C-c%" 'match-paren)
             ;(define-key cperl-mode-map (kbd "TAB") 'tab-to-tab-stop)
             (local-set-key "%" 'self-insert-command)
             (c-set-offset 'inline-open 0)
             (setq tab-width 4)
             ;(setq cperl-tab-always-indent t)
             (setq cperl-indent-parens-as-block t)
             (setq cperl-tab-to-comment t)
             (setq cperl-indent-level 4)
             (setq cperl-continued-statement-offset 4)
             (setq cperl-continued-brace-offset 0)
             (setq cperl-brace-offset -4)
             (setq cperl-brace-imaginary-offset 0)
             (setq cperl-label-offset -2)))

(add-hook 'ediff-cleanup-hook
          '(lambda ()
             (ediff-janitor t)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode t)
             (paredit-mode t)))

(add-hook 'makefile-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (setq makefile-target-colon "::")
             (setq makefile-macro-assign " = ")
             (setq makefile-tab-after-target-colon t)
             (setq makefile-browser-auto-advance-after-selection-p t)
             (setq makefile-electric-keys t)
             (setq makefile-use-curly-braces-for-macros-p t)))

(add-hook 'mouse-track-click-hook 'id-select-double-click-hook)

(add-hook 'text-mode-hook
          '(lambda ()
             (linum-mode 1)
             (turn-on-auto-fill)))
