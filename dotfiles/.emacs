;;; .emacs

(defconst homedir (getenv "HOME") "My home dir, regardless of host.")
(add-to-list 'load-path (concat homedir "/.emacs.d"))
(add-to-list 'load-path (concat homedir "/.emacs.d/slime"))
(add-to-list 'load-path (concat homedir "/.emacs.d/slime/contrib"))

;; Libs I want visible at all levels:
(require 'ack)
(require 'imenu)
(require 'iswitch-buffer)
(require 'linum)
(require 'perlcritic)
(require 'browse-kill-ring)
(require 'lacarte)
(require 'gist)
(require 'highlight-parentheses)
(require 'paredit)

;; Fire up recent-file minor-mode
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-max-menu-items 40)
(recentf-mode 1)

;; Set up and enable IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".org" ".txt" ".pm" ".pl" ".clj" ".emacs" ".xml" ".el"))
(setq ido-ignore-extensions t)
(ido-mode 1)

;; Run as a server
(server-mode 1)

;; Line numbers everywhere!
(global-linum-mode)

;; Disable auto-saving
(setq auto-save-default nil)

;; No case-folding on searches, please.
(setq default-case-fold-search nil)

;; Tab-related twiddling
(setq-default indent-tabs-mode t)
;(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; Load my personalized code
(load "e_init")
(load "keys")
(load "utils")
(load "hooks")
(load "mode-list")
(load "autoloads")

;;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook 'perlcritic-mode))

;; SLIME
(require 'slime)
(slime-setup '(slime-fancy slime-asdf))
(unload-feature 'slime-autodoc t)
(setq slime-multiprocessing t)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(cond ((string-match "Aquamacs" emacs-build-system)
       (setq slime-lisp-implementations
             '((clisp   ("/usr/local/bin/clisp" "-K full"))
               (sbcl    ("/usr/local/bin/sbcl")))))
      (t
       (setq slime-lisp-implementations
             '((clisp   ("/usr/bin/clisp" "-K full"))
               (sbcl    ("/usr/bin/sbcl"))))))
(setf slime-default-lisp 'sbcl)

;; Load Perforce mode when at work:
(when (string= system-name "rjray.hq.netapp.com")
  (load "p4"))

;;enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; Added/updated by emacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(color-theme-selection "Vim Colors" nil (color-theme))
 '(column-number-mode t)
 '(fill-column 79)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message "rjray")
 '(inhibit-startup-screen t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "dejavu-dejavu sans mono"))))
 '(develock-whitespace-3 ((t nil)))
 '(trailing-whitespace ((t (:underline t))))
 '(whitespace-highlight ((((class color) (background light)) (:background "green1" :underline t)))))
