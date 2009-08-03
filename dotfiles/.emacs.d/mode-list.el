;;; Building up of/alteration of the auto-mode-alist is limited to here. Other
;;; variables similar in nature (interpreter-mode-alist) are covered, and the
;;; special auto-loads are here, as well.

(defun extensions-list (mode extlist)
  (mapcar '(lambda (x) (cons x mode)) extlist))

(setq auto-mode-alist
      (append
       ;; Perl mode
       (extensions-list 'cperl-mode
                        '("\\.pm" "\\.PM" "\\.PL" "\\.pl" "\\.al"))

       ;; Makefiles
       (list (cons "I?[Mm]akefile$" 'makefile-mode))

       ;; The PSGML mode is applied to several
       (extensions-list 'sgml-mode
                        '("\\.html?" "\\.HTML?" "\\.sgml?"))

       ;; XML
       (extensions-list 'xml-mode
                        '("\\.rdf" "\\.xsd" "\\.xslt?"))

       ;; Tcl/Tk
       (extensions-list 'tcl-mode
                        '("\\.tcl" "\\.tk"))

       ;; Extras for C mode and related
       (list (cons "\\.xs$" 'c-mode))
       (list (cons "\\.y$" 'bison-mode))

       ;; Various DTD-related types
       (extensions-list 'dtd-mode
                        '("\\.dcl$" "\\.dec$" "\\.dtd$"
                          "\\.ele$" "\\.ent$" "\\.mod$"))

       ;; TT mode
       (list (cons "\\.tt$" 'tt-mode))

       ;; C# Files
       (extensions-list 'csharp-mode
                        '("\\.cs$" "\\.hs"))

       auto-mode-alist))

;;; Set it so any file edited UNDER ~/work/perl5 is in perl mode. MUST BE LAST
(nconc auto-mode-alist '(("/rjray/work/perl5" . cperl-mode)))

(setq interpreter-mode-alist (append interpreter-mode-alist
				     '(("miniperl" . perl-mode))))
