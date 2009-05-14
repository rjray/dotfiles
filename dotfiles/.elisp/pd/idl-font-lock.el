;;; idl-font-lock.el --- Font Lock configuration for CORBA IDL files

;; Copyright (C) 1998, 1999 The University of Utah and
;;   the Computer Systems Laboratory at the University of Utah (CSL).
;; Copyright (C) 1992-1995, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Ben Wing.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place #330, Boston, MA 02111, USA

;; idl-font-lock.el,v 1.3 1999/08/30 17:35:27 eeide Exp
;;
;; Author:		Eric Eide <eeide@cs.utah.edu>
;; Maintainer:		Eric Eide <eeide@cs.utah.edu>
;; Created:		1998/02/03
;; Keywords:		idl, languages, faces

;; LCD Archive Entry:
;; idl-font-lock|Eric Eide|eeide@cs.utah.edu|
;; Font Lock configuration for CORBA IDL files|
;; 1999/08/30 17:35:27|1.3|~/misc/idl-font-lock.el.gz|

;;; Commentary:

;; This file provides the data that Font Lock Mode needs in order to decorate
;; CORBA IDL (`.idl') files.  The Font Lock patterns will match and highlight
;; all of the keywords and types defined in the CORBA 2.3 specification.  There
;; are also patterns for highlighting operation names, attribute names, case
;; clauses, and preprocessor directives.
;;
;; If you do not know what Font Lock Mode is, this file will not help you ---
;; you will have to look elsewhere if you want to learn about Font Lock itself.
;; Similarly, this file does not define a major mode for editing CORBA IDL
;; files.  If you need IDL Mode, get the CC Mode package, version 5, from the
;; World Wide Web at <http://www.python.org/ftp/emacs/>.
;;
;; Once you have configured Font Lock Mode and installed CC Mode version 5, you
;; can load this file in order to get highlighting in IDL Mode buffers.  Put
;; this file where Emacs can find it (i.e., somewhere in your `load-path') and
;; then add the following lines to your `.emacs' file:
;;
;;   (setq auto-mode-alist (cons '("\\.idl\\'" . idl-mode) auto-mode-alist))
;;   (require 'idl-font-lock)
;;
;; The patterns for highlighting C preprocessor directives were adapted from
;; those in the `font-lock.el' file distributed with XEmacs 20.3.  Hence the
;; long copyright notice at the top of this file.
;;
;; This package has been tested with XEmacs 20.4 and CC Mode 5.25.  Note that
;; CC Mode 5.25 has problems indenting some of the newer CORBA 2.3 syntax.
;; This will presumably be fixed in a future version of the CC Mode package.

;;; Code:

;; Load `font-lock' so that we can figure out how to configure and install
;; ourselves.
(require 'font-lock)

(defvar idl-font-lock-preprocessor-face-name
  ;; If this Emacs doesn't provide `font-lock-preprocessor-face', we use
  ;; `font-lock-reference-face' instead to highlight preprocessor directives
  ;; (because that's what Emacsen without `font-lock-preprocessor-face' use).
  ;; We assume the `font-lock-reference-face' exists.
  (let ((has-preprocessor-face-p
	 (if (fboundp 'find-face)
	     (find-face 'font-lock-preprocessor-face)
	   (facep 'font-lock-preprocessor-face))
	 ))
    (if has-preprocessor-face-p
	'font-lock-preprocessor-face
      'font-lock-reference-face))
  "Name of the face used to highlight preprocessor directives in IDL Mode."
  )

(defvar idl-font-lock-keywords
  (let* (;; `idl-token' matches a CORBA IDL scoped name.  Note that `_' is
	 ;; treated as a word constituent by Font Lock Mode; see the `put' form
	 ;; at the end of this file.
	 ;;
	 (idl-token "\\(\\sw\\|::\\)+")
	 
	 ;; `int-literal' matches an integer literal.
	 (int-literal "\\([0-9]+\\|0[xX][0-9a-fA-F]+\\)")
	 
	 ;; `const-expr-and-ws' matches a constant expression and any
	 ;; surrounding whitespace.
	 ;;
	 (const-expr-and-ws "\\(\\sw\\|::\\|\\s-\\|[-|^&<>+*/%~()]\\)+")
	 
	 ;; `op-type-spec' matches an operation/attribute type specification as
	 ;; defined by the CORBA IDL grammar.
	 ;;
	 ;; The expression "\\(\\sw\\|:\\)+\\(\\s-*<[0-9]+\\s-*>\\)?" would be
	 ;; simpler but is wrong; it doesn't match `long long'.
	 ;;
	 (op-type-spec
	  (concat
	   "\\("
	   (mapconcat
	    'identity
	    (list
	     ;; Multi-token types.
	     "unsigned\\s-+\\(short\\|\\(long\\s-+\\)?long\\)"
	     "long\\s-+\\(long\\|double\\)"
	     ;; Strings.
	     (concat "w?string\\s-*\\(<\\s-*" int-literal "\\s-*>\\)?")
	     ;; Fixed-point numbers.
	     (concat "fixed\\s-*\\(<"
		     const-expr-and-ws ",\\s-*" int-literal
		     "\\s-*>\\)")
	     ;; A scoped name; catches single-token built-in types, too.
	     idl-token
	     )
	    "\\|")
	   "\\)"))
	 )
    
    (list
     ;; The forms for highlighting preprocessor directives were adapted from
     ;; `c-font-lock-keywords-1' in XEmacs 20.3's version of `font-lock.el'.
     ;; They are matched first so that subsequent rules don't interfere, e.g.,
     ;; highlight `defined' in `#if defined(foo)' as an operation name.
     
     ;; Filenames in `#include <...>' preprocessor directives.
     '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
     
     ;; `#define'd macro names.
     '("^[ \t]*#[ \t]*\\(define\\|undef\\)[ \t]+\\(\\sw+\\)"
       2 font-lock-function-name-face)
     
     ;; Symbol names in `#if/elif ... defined' preprocessor directives.
     (list "^[ \t]*#[ \t]*\\(el\\)?if\\>"
	   (list "\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?"
		 nil nil
		 (list 1 idl-font-lock-preprocessor-face-name)
		 '(2 font-lock-variable-name-face nil t)
		 ))
     
     ;; `#pragma's defined by CORBA 2.3; see Section 10.6.5 of the spec.
     (list "^[ \t]*#[ \t]*pragma[ \t]+\\(ID\\|prefix\\|version\\)\\>"
	   1 idl-font-lock-preprocessor-face-name)
     
     ;; Other preprocessor directives.
     (list "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(\\sw+\\)?"
	   (list 1 idl-font-lock-preprocessor-face-name)
	   '(2 font-lock-variable-name-face nil t)
	   )
     
     ;; Now come the forms for highlighting IDL syntax.
     
     ;; Built-in types.
     (list (concat
	    "\\<\\("
	    (mapconcat
	     'identity
	     '(
	       ;; The integral types.
	       "\\(unsigned\\s-+\\)?\\(short\\|\\(long\\s-+\\)?long\\)"
	       ;; The floating-point types.
	       "float" "\\(long\\s-+\\)?double"
	       ;; The remaining primitive types.
	       "w?char" "boolean" "octet" "any" "void"
	       ;; The standard `Object' and `ValueBase' types.
	       "Object" "ValueBase"
	       ;; Template types.
	       "sequence" "w?string" "fixed"
	       ;; Constructed types and things that are like constructed types.
	       "enum" "struct" "union" "exception" "interface" "valuetype"
	       "native" "typedef"
	       )
	     "\\|")
	    "\\)\\>")
	   1
	   'font-lock-type-face)
     
     ;; Keywords that are like type modifiers.
     (list (concat
	    "\\<\\("
	    (mapconcat
	     'identity
	     '(
	       ;; `abstract' interfaces and valuetypes.
	       "abstract"
	       ;; `custom' marshaling of valuetypes.
	       "custom"
	       
	       ;; I believe that `const', `private', and `public' should be
	       ;; viewed as language-construct-introducing keywords and not as
	       ;; type modifiers.  `const' introduces declarations of constant
	       ;; values; `private' and `public' introduce valuetype state
	       ;; members.
	       ;;
	       ;; The IDL type of an entity should be viewed as separate from
	       ;; the language class of the entity itself.  For example, the
	       ;; declaration `public long foo;' should be read as: `foo is a
	       ;; public member of type long'; not as: `foo is a member of type
	       ;; public long'.
	       ;;
	       ;; If you disagree with this view, then you should put some or
	       ;; all of the following keywords here:
	       ;;
	       ;; "attribute" "const" "factory" "in" "inout" "oneway" "out"
	       ;; "private" "public" "readonly"
	       ;;
	       )
	     "\\|")
	    "\\)\\>")
	   1
	   'font-lock-type-face)
     
     ;; Keywords, except for built-in types and type modifiers.
     (list (concat
	    "\\<\\("
	    (mapconcat
	     'identity
	     '(
	       "attribute" "case" "const" "context" "default" "factory" "FALSE"
	       "in" "inout" "module" "oneway" "out" "private" "public" "raises"
	       "readonly" "supports" "switch" "TRUE" "truncatable"
	       )
	     "\\|")
	    "\\)\\>")
	   1
	   'font-lock-keyword-face)
     
     ;; Attribute names.
     (list (concat
	    ;; The regexp below was an attempt to weed out false matches in
	    ;; ill-formed IDL.  But it doesn't really help much --- given that
	    ;; the keyword `attribute' is already required --- and including
	    ;; the extra anchor causes some valid cases not to highlight, e.g.,
	    ;; `/* readonly */ attribute long val;'.  So I took it out.  My
	    ;; goal after all is to highlight correct IDL correctly, not to
	    ;; avoid highlighting incorrect IDL.
	    ;;
	    ;; "\\(^\\s-*\\|[{\;]\\s-*\\|readonly\\s-+\\)"
	    ;;
	    "attribute\\s-+"
	    op-type-spec
	    )
	   ;; Use an anchored match: there may be several attribute names.
	   (list (concat "\\(" idl-token "\\)\\s-*[,;]")
		 nil nil
		 '(1 font-lock-function-name-face)
		 )
	   )
     
     ;; Value initializer (`factory') names.
     (list (concat
	    "factory"
	    )
	   ;; Use an anchored match: this helps us highlight the name even if
	   ;; there is intervening gunk, as in `factory /* foo */ init();'.
	   ;;
	   (list (concat "\\(" idl-token "\\)\\s-*(")
		 nil nil
		 '(1 font-lock-function-name-face)
		 )
	   )
     
     ;; Operation names.
     (list (concat
	    ;; Again, the regexp below was an attempt to weed out false matches
	    ;; in ill-formed IDL.  Weeding is more important here than in the
	    ;; attribute case because an `op-type-spec' can match any single
	    ;; token.  But again: (1) it doesn't help enough to be worthwhile;
	    ;; (2) it prevents certain valid syntax from highlighting; and (3)
	    ;; my goal is to highlight correct IDL, not to avoid highlighting
	    ;; incorrect IDL.
	    ;;
	    ;; "\\(^\\s-*\\|[{\;]\\s-*\\|oneway\\s-+\\)"
	    ;;
	    op-type-spec
	    )
	   ;; Use an anchored match: we don't know how many groups were used to
	   ;; match the `op-type-spec'.
	   ;;
	   ;; A side effect of using an anchored match is that we may highlight
	   ;; what appear to be multiple operation names, although IDL allows
	   ;; only one.  Also, we can highlight operation names even if there
	   ;; is intervening gunk, as in `string<gunk> opname();'.
	   ;;
	   (list (concat "\\(" idl-token "\\)\\s-*(")
		 nil nil
		 '(1 font-lock-function-name-face)
		 )
	   )
     
     ;; XXX --- Future enhancement: highlight the names of modules, interfaces,
     ;; structs, unions, enums, exceptions, and typedefs?
     
     ;; Case clause values.
     ;;
     ;; Note that the value may be an arbitrary constant expression; hence the
     ;; complicated regexp.  Also note that `case' and `default' are already
     ;; highlighted because they are keywords.
     ;;
     (list (concat "\\<case\\(" const-expr-and-ws "\\):\\($\\|[^:]\\)")
	   1
	   'font-lock-keyword-face)
     ))
  "Expressions to highlight in IDL Mode."
  )

;; Tell Font Lock Mode about `idl-font-lock-keywords' and IDL highlighting.
(put 'idl-mode 'font-lock-defaults
     '(idl-font-lock-keywords
       nil			;; Not keywords-only.
       nil			;; Don't fold case when matching keywords.
       ((?_ . "w"))		;; `_' is a word constituent when highlighting.
       beginning-of-defun
       ))

;; If this Emacs uses `font-lock-defaults-alist' instead of symbol properties
;; to find Font Lock data, insinuate ourselves into that alist.
(if (boundp 'font-lock-defaults-alist)
    (setq font-lock-defaults-alist
	  (cons (cons 'idl-mode (get 'idl-mode 'font-lock-defaults))
		font-lock-defaults-alist))
  )

;; We're through!
(provide 'idl-font-lock)

;; End of file.

