;;; Basically a junk-drawer, a place to put the stuff that doesn't really
;;; fit elsewhere.

(require 'cl)
(require 'ack)
(require 'prove)

(turn-off-pending-delete)

(setq-default fill-column 78)
(setq-default case-fold-search nil)
(setq-default auto-save-default t)
(setq-default make-backup-files nil)
(setq-default ps-print-color-p nil)
(setq-default tab-stop-list
          (mapcar (lambda (x) (* 4 x))
              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
(setq-default tab-width 4)

(setq default-major-mode 'text-mode)
(setq delete-auto-save-files t)
(setq browse-url-netscape-arguments nil)

(global-set-key "\e*" 'new-interactive-lisp)
;(global-set-key "\C-cd" 'set-line-as-debug)
;(global-set-key "\C-c\C-d" 'delete-all-debug-lines)
(global-set-key "\C-cp" 'select-perl-mode)
(global-set-key "\C-cu" 'update-mod-time)
(global-set-key "\C-cy" 'yow)
(global-set-key "\C-c'" 'iso-accents-mode)
(global-set-key "\C-c;" (lambda () (iso-accents-mode -1)))

;; Function-key bindings
(global-set-key [(f1)]           'other-window)

(global-set-key [(f2) (down)]    'enlarge-window)
(global-set-key [(f2) (left)]    'shrink-window-horizontally)
(global-set-key [(f2) (right)]   'enlarge-window-horizontally)
(global-set-key [(f2) (up)]      'shrink-window)

(global-set-key [(f3) (down)]    'split-window-vertically)
(global-set-key [(f3) (right)]   'split-window-horizontally)

(global-set-key [(f4)]           'delete-window)
(global-set-key [(control f4)]   'delete-other-windows)

(global-set-key [(f5)]           'search-forward-regexp)
(global-set-key [(control f5)]   'search-backward-regexp)

(global-set-key [(f6)]           'query-replace)

(global-set-key [(f7)]           'fill-paragraph-or-region)
(global-set-key [(control f7)]   'untab-buffer)

(global-set-key [(f8)]           (lambda ()
                                   (interactive)
                                   (count-words-buffer (current-buffer))))
(global-set-key [(control f8)]   'count-words-region)

(global-set-key [(f9)]           'find-file)
(global-set-key [(control f9)]   'load-library)

(global-set-key [(f10)]          'execute-extended-command)
(global-set-key [(control f10)]  'compile)

(global-set-key [(f11)]          'insert-line-numbers)

(global-set-key [(f12)]          'htmlize-buffer)

(global-set-key [(f14)]          'advertised-undo)

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
(global-set-key [(hpInsertLine)] 'home)
(global-set-key [(hpDeleteLine)] 'end)
(global-set-key [(hpDeleteChar)] 'delete-char)
(global-set-key [(hpInsertChar)] (lambda ()
                                   (interactive)
                                   (insert " ")
                                   (backward-char)))
(global-set-key [(button4)] (lambda ()
                  (interactive)
                  (scroll-down 3)))
(global-set-key [(button5)] (lambda ()
                  (interactive)
                  (scroll-up 3)))

(define-key esc-map "=" 'count-region)
(define-key esc-map "\C-y" 'yank-and-indent)
(define-key ctl-x-map "=" 'what-cursor-position-and-line)
(define-key global-map [(control button3)] 'imenu)

;; Support for and inclusion of the recent-files package
(require 'recent-files)
(setq recent-files-add-menu-before "Edit")
(setq recent-files-menu-title "Recent")
(setq recent-files-number-of-entries 25)
(setq recent-files-dont-include
      '("/\\.newsrc" "~$" "^/tmp/." "\\.SCORE" "\(eval " "/Mail/" "/foo"
        "\\.al$" ".xemacs-options" ".recent-files.el" "/drafts/" ".emacs"
        "/\\.shadow"))
(setq recent-files-filename-replacements
        (list (cons (concat homedir "/.elisp") "$LISP")
              (cons "/home/httpd/svsm" "$SVSM")
              (cons "/var/www/svsm" "$SVSM")
              (cons "/var/www/blackperl.com" "$BLACKPERL")
              (cons (concat homedir "/work/perl5/Devel-Coverage") "$DEVELCOV")
              (cons (concat homedir "/work/perl5/Devel-Metrics") "$DEVELMET")
              (cons (concat homedir "/work/perl5/Image-Size") "$IMAGESIZE")
              (cons (concat homedir "/work/perl5/Devel-Modlist") "$DEVELMOD")
              (cons (concat homedir "/work/perl5/RPM") "$RPM")
              (cons (concat homedir "/work/perl5/RPC-XML") "$RPCXML")
              (cons (concat homedir "/work/perl5/OpenRPC") "$OpenRPC")
              (cons (concat homedir "/work/perl5/X11-Fvwm") "$X11FVWM")
              (cons homedir "~")))
(recent-files-initialize)

;; Other packages that we know we want
(require 'imenu)
(require 'iswitch-buffer)

;; For the command-history menu defined in utils.el:
(setq history-menu-max-items 45)
(setq history-menu-max-len 50)

;; Add a Transform menu to the Edit Menu
;; Populate it with items that transform buffers and loads a new buffer.
(defconst transform-menu
  '("Transform"
    ;; HTMLize the buffer.
    ["HTMLize" htmlize-buffer t]))

;;Add the Transform menu.
(add-submenu '("Edit") transform-menu "Clear")

;; Various PSGML-related stuff
(setq sgml-catalog-files
      (list "catalog"
            "CATALOG"
            "/etc/sgml/catalog"
            "/usr/lib/sgml/CATALOG"
            "/usr/lib/xemacs/xemacs-packages/etc/psgml-dtds/CATALOG"
            "/usr/lib/xemacs/xemacs-packages/etc/psgml/CATALOG"))

;; PSGML mode support
(make-face 'sgml-comment-face)
(make-face 'sgml-doctype-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-ignored-face)
(make-face 'sgml-ms-end-face)
(make-face 'sgml-ms-start-face)
(make-face 'sgml-pi-face)
(make-face 'sgml-sgml-face)
(make-face 'sgml-short-ref-face)
(make-face 'sgml-start-tag-face)

(set-face-foreground 'sgml-comment-face "AntiqueWhite")
(set-face-foreground 'sgml-doctype-face "lightgreen")
(set-face-foreground 'sgml-end-tag-face "lightblue")
(set-face-foreground 'sgml-entity-face "red2")
(set-face-background 'sgml-ignored-face "gray90")
(set-face-foreground 'sgml-ms-end-face "pink")
(set-face-foreground 'sgml-ms-start-face "pink")
(set-face-foreground 'sgml-pi-face "pink")
(set-face-foreground 'sgml-sgml-face "wheat")
(set-face-foreground 'sgml-short-ref-face "LightGreen")
(set-face-foreground 'sgml-start-tag-face "lightblue")

(setq-default sgml-markup-faces
              '((comment . sgml-comment-face)
                (doctype . sgml-doctype-face)
                (end-tag . sgml-end-tag-face)
                (entity . sgml-entity-face)
                (ignored . sgml-ignored-face)
                (ms-end . sgml-ms-end-face)
                (ms-start . sgml-ms-start-face)
                (pi . sgml-pi-face)
                (sgml . sgml-sgml-face)
                (short-ref . sgml-short-ref-face)
                (start-tag . sgml-start-tag-face)))

(defun docbook-mode ()
  (sgml-mode))
(defun html-mode ()
  (sgml-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ri-ruby-script (concat (getenv "HOME") "/.elisp/ri-emacs.rb"))

