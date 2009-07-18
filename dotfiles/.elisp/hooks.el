(add-hook 'font-lock-mode-hook
          '(lambda () (setq font-lock-maximum-decoration 4)))

(add-hook 'before-save-hook
      '(lambda ()
         (cond ((region-active-p)
            (delete-trailing-whitespace (region-beginning)
                        (region-end)))
           (t (delete-trailing-whitespace 0 (point-max))))))

(add-hook 'dired-load-hook
          '(lambda () (define-key dired-mode-map "I"
                        'rpm-dired-install)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "%" 'match-paren)
             (if (and (featurep 'menubar)
                      current-menubar)
                 (progn
                   ;; make a local copy of the menubar, so our modes don't
                   ;; change the global menubar
                   (set-buffer-menubar current-menubar)
                   (add-submenu nil emacs-lisp-mode-menubar-menu)))))
(add-hook 'lisp-mode-hook 'cltl2-lisp-mode-install)
(add-hook 'ilisp-mode-hook 'cltl2-lisp-mode-install)

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
             (c-set-offset 'statement-case-open '*)
             (setq debug-line-marker-string "/* DEBUG */")))

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
             (c-set-offset 'statement-case-intro '*)
             (setq debug-line-marker-string "// DEBUG")))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (define-key cperl-mode-map "\C-cf" 'perl-insert-file-hdr)
             (define-key cperl-mode-map "\C-cl" 'perl-insert-lib-hdr)
             (define-key cperl-mode-map "\C-cs" 'perl-insert-sub-hdr)
             (define-key cperl-mode-map "\C-c%" 'match-paren)
             (local-set-key "%" 'self-insert-command)
             (setq cperl-tab-always-indent t)
             (setq cperl-tab-to-comment t)
             (setq cperl-indent-level 4)
             (setq cperl-continued-statement-offset 4)
             (setq cperl-continued-brace-offset 0)
             (setq cperl-brace-offset -4)
             (setq cperl-brace-imaginary-offset 0)
             (setq cperl-label-offset -2)
             (setq debug-line-marker-string "#// DEBUG")))

(add-hook 'makefile-mode-hook
          '(lambda ()
             (turn-on-font-lock)
             (setq makefile-target-colon "::")
             (setq makefile-macro-assign " = ")
             (setq makefile-tab-after-target-colon t)
             (setq makefile-browser-auto-advance-after-selection-p t)
             (setq makefile-electric-keys t)
             (setq makefile-use-curly-braces-for-macros-p t)
             (setq makefile-special-targets-list
                   (append makefile-special-targets-list
                           '(("hdrs") ("libs") ("images") ("local")
                             ("install"))))))

(add-hook 'ediff-cleanup-hook
          '(lambda ()
             (ediff-janitor t)))

(add-hook 'hm--html-mode-hook
          '(lambda ()
             (setq hm--html-signature-file nil)
             (setq hm--html-automatic-new-date nil)
             (setq hm--html-username "Randy J. Ray")
             (setq html-view-display "tzimisce:0.0")
             (setq hm--html-expert t)
             (setq hm--html-delete-wrong-path-prefix "/tmp")
             (setq w3-default-homepage nil)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'mouse-track-click-hook 'id-select-double-click-hook)

(add-hook 'sgml-mode-hook
          '(lambda () "Defaults for SGML mode."
             (auto-fill-mode t)
             (abbrev-mode 1)
             (setq fill-column 79)
             (setq sgml-indent-data t
                   sgml-auto-activate-dtd t
                   sgml-insert-missing-element-comment nil
                   sgml-auto-insert-required-elements t
                   sgml-set-face t
                   sgml-live-element-indicator t)
             (setq sgml-custom-dtd
                   '(("HTML 4.0"
                      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">"
                      sgml-default-dtd-file "~/sgml/html40.ced")
                     ("HTML 4.0+frames"
                      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Framset//EN\">"
                      sgml-default-dtd-file "~/sgml/html40+fr.ced")
                     ("DOCBOOK-article"
                      "<!DOCTYPE ARTICLE PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\" [\n]>"
                      sgml-default-dtd-file "~/sgml/docbook.ced")
                     ("DOCBOOK-book"
                      "<!DOCTYPE BOOK PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\" [\n]>"
                      sgml-default-dtd-file "~/sgml/docbook.ced")))
             (define-key sgml-mode-map "&" 'xml--html-smart-ampersand)
             (define-key sgml-mode-map "<" 'xml--html-smart-less-than)
             (define-key sgml-mode-map ">" 'xml--html-smart-greater-than)
             (define-key sgml-mode-map [mouse-3] 'sgml-attrib-menu)
             (local-set-key [(alt i)]
                            '(lambda ()
                               (interactive)
                               (sgml-indent-line)
                               (sgml-insert-element 'item)
                               (sgml-indent-line)))
             (local-set-key [(alt l)]
                            '(lambda ()
                               (interactive)
                               (sgml-insert-element 'list)
                               (sgml-insert-element 'item)
                               (sgml-indent-line)))
             (local-set-key [(alt p)]
                            '(lambda ()
                               (interactive)
                               (sgml-indent-line)
                               (sgml-insert-element 'para)
                               (sgml-indent-line)))
             (local-set-key [(alt -)]
                            '(lambda ()
                               (interactive)
                               (insert "&mdash;")))
             (local-set-key [(alt o)]
                            '(lambda ()
                               (interactive)
                               (insert "<!--  -->")
                               (backward-char 4)))))

(add-hook 'ruby-mode-hook (lambda ()
                (local-set-key 'f1 'ri)
                (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                (local-set-key 'f4 'ri-ruby-show-args)))
