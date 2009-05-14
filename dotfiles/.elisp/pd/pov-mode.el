;; Major mode for editing PoVray scene Files
;; 
;; Only tested on Emacs 19 and XEmacs 19.  Let me know about
;;  any compatibility problems.
;; 
;; Modified by: Peter Boettcher <pwb@andrew.cmu.edu>
;;  5/8/97:
;;    Added font-lock support for Emacs/XEmacs 19
;;    Indent under `#declare Object=' lines
;;    Corrected comment syntax
;;    Got rid of more remnants from postscript mode
;;    General cleanup
;;    Arbitrarily chose version 1.2
;; 5/8/97:  1.21
;;    fontify-insanely was ignored.  fixed.
;;
;; Original Author:	Kevin O. Grover <grover@isri.unlv.edu>
;; 	  Cre Date:	04 March 1994
;; This file derived from postscript mode by Chris Maio
;;
;;
;;  Please send bug reports/comments/suggestions to me at
;;        pwb@andrew.cmu.edu
;;
;; The following two statements, placed in your .emacs file or site-init.el,
;; will cause this file to be autoloaded, and pov-mode invoked, when
;; visiting .pov files:
;;
;;	(autoload 'pov-mode "pov-mode.el" "PoVray scene file mode" t)
;;	(setq auto-mode-alist
;;	      (cons '("\\.pov$".pov-mode) auto-mode-alist))
;; 
;; Use this to turn on font-lock:
;;	(add-hook 'pov-mode-hook 'turn-on-font-lock)
;; 
;; These variables can be set in your .emacs (defaults given):
;;    pov-indent-level 2
;;    pov-indent-under-declare 2   Try it!  Tell me if you like it...
;;    pov-fontify-insanely t       When it's non-nil, we fontify *every*
;;                                     Povray keyword.  Careful!

(provide 'pov-mode)

(defconst pov-mode-version '1.21)

(defvar pov-indent-level 2
  "*Indentation to be used inside of PoVray blocks or arrays")

(defvar pov-indent-under-declare 2
  "*Indentation under a `#declare Object=' line")

(defvar pov-fontify-insanely t
  "*Non-nil means colorize every povray keyword.  This may take a while on lare files.  Maybe disable this on slow systems.")

(defconst pov-tab-width 8
  "*Tab stop width for PoV mode")

(defun pov-make-tabs (stop)
  (and (< stop 132) (cons stop (pov-make-tabs (+ stop pov-tab-width)))))

(defconst pov-tab-stop-list (pov-make-tabs pov-tab-width)
  "Tab stop list for PoV mode")

(defconst pov-command "povray -"
  "Command used to invoke the povray program.")

(defvar pov-mode-map nil
  "Keymap used in PoV mode buffers")

(defvar pov-mode-syntax-table nil
  "PoV mode syntax table")

(defconst pov-comment-start-regexp "//\\|/\\*"
  "Dual comment value for `comment-start-regexp'.")

(defvar pov-comment-syntax-string ". 124b"
  "PoV hack to handle Emacs/XEmacs foo")

(defun pov-setup-syntax-table nil
  (if (or (string-match "Lucid" emacs-version)
	  (string-match "XEmacs" emacs-version))
      (setq pov-comment-syntax-string ". 1456"))
  (if pov-mode-syntax-table
      ()
    (setq pov-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_ "w" pov-mode-syntax-table)
    (modify-syntax-entry ?# "w" pov-mode-syntax-table)
    (modify-syntax-entry ?/ pov-comment-syntax-string pov-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" pov-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" pov-mode-syntax-table)
    (set-syntax-table pov-mode-syntax-table)))

(defconst pov-all-keyword-matcher
  '("\\<\\(a\\(a_\\(level\\|threshold\\)\\|bs\\|cosh?\\|d\\(aptive\\|c_bailout\\)\\|gate\\(_turb\\)?\\|ll\\|lpha\\|mbient\\(_light\\)?\\|ngle\\|perture\\|r\\(c_angle\\|ea_light\\)\\|sc\\|sinh?\\|ssumed_gamma\\|t\\(an[2h]?\\|mospher\\(e\\|ic_attenuation\\)\\|tenuating\\)\\|verage\\)\\|b\\(ackground\\|l\\(ack_hole\\|ue\\|ur_samples\\)\\|o\\(unded_by\\|x_mapping\\|zo\\)\\|r\\(eak\\|ick\\(_size\\)?\\|ightness\\|illiance\\)\\|ump\\(s\\|y[123]?\\|_map\\|_size\\)\\)\\|c\\(a\\(mera\\|se\\|ustics\\)\\|eil\\|h\\(ecker\\|r\\)\\|l\\(ipped_by\\|ock\\)\\|o\\(lou?r\\(_map\\)?\\|mpo\\(nent\\|site\\)\\|n\\(cat\\|fidence\\|ic_sweep\\|stant\\|trol[01]\\)\\|sh?\\|unt\\)\\|ra\\(ckle\\|nd\\)\\|ylindrical_mapping\\)\\|d\\(e\\(grees\\|nts\\)\\|i\\(ff\\(erence\\|use\\)\\|rection\\|s\\(c\\|tance\\(_maximum\\)?\\)\\|v\\)\\|ust\\(_type\\)?\\)\\|e\\(ccentricity\\|lse\\|mitting\\|nd\\|rror_bound\\|xp\\(onent\\)?\\)\\|f\\(a\\(de_\\(distance\\|power\\)\\|l\\(loff\\(_angle\\)?\\|se\\)\\)\\|i\\(l\\(e_exists\\|ter\\)\\|nish\\|sheye\\)\\|l\\(atness\\|ip\\|oor\\)\\|o\\(cal_point\\|g\\(_alt\\|_offset\\|_type\\)?\\)\\|requency\\)\\|g\\(if\\|lo\\(bal_settings\\|wing\\)\\|r\\(adient\\|anite\\|ay_threshold\\|een\\)\\)\\|h\\(alo\\|exagon\\|f_gray_16\\|ierarchy\\|ollow\\|ypercomplex\\)\\|i\\(ff\\|mage_map\\|n\\(cidence\\|t\\(erpolate\\|ersection\\)?\\|verse\\)\\|or\\|rid\\(_wavelength\\)?\\)\\|jitter\\|l\\(ambda\\|eopard\\|i\\(ght_source\\|near\\(_spline\\|_sweep\\)?\\)\\|o\\(cation\\|g\\|oks_like\\|ok_at\\|w_error_factor\\)\\)\\|m\\(a\\(ndel\\|p_type\\|rble\\|terial_map\\|trix\\|x\\(_intersections\\|_iteration\\|_trace_level\\|_value\\)?\\)\\|e\\(rge\\|tallic\\)\\|in\\(imum_reuse\\)?\\|od\\|ortar\\)\\|n\\(earest_count\\|o\\(rmal\\(_map\\)?\\|_shadow\\)?\\|umber_of_waves\\)\\|o\\(bject\\|ctaves\\|ff\\(set\\)?\\|mega\\|mnimax\\|n\\(ce\\|ion\\)?\\|pen\\|rthographic\\)\\|p\\(a\\(noramic\\|ttern[123]\\)\\|erspective\\|gm\\|h\\(ase\\|ong\\(_size\\)?\\)\\|i\\(gment\\(_map\\)?\\)?\\|lanar_mapping\\|ng\\|o\\(int_at\\|[tw]\\)\\|pm\\|recision\\|wr\\)\\|qu\\(adratic_spline\\|aternion\\|ick_colou?r\\|ilted\\)\\|r\\(a\\(di\\(al\\|ans\\|osity\\|us\\)\\|inbow\\|mp_wave\\|nd\\|nge\\)\\|e\\(ciprocal\\|cursion_limit\\|d\\|flection\\|fraction\\|peat\\)\\|gbf?t?\\|ight\\|ipples\\|otate\\|oughness\\)\\|s\\(amples\\|ca\\(le\\|llop_wave\\|ttering\\)\\|eed\\|hadowless\\|in\\(e_wave\\|h\\)?\\|ky\\(_sphere\\)?\\|lice\\|lope_map\\|mooth\\|or\\|p\\(ecular\\|herical_mapping\\|iral[12]?\\|otlight\\|otted\\)\\|qrt?\\|t\\(atistics\\|r\\(cmp\\|ength\\|len\\|lwr\\|upr\\)?\\|urm\\)\\|ubstr\\|witch\\|ys\\)\\|t\\(anh?\\|est_camera_[1234]\\|exture\\(_map\\)?\\|ga\\|hickness\\|hreshold\\|ightness\\|ile[2s]\\|r\\(ack\\|ans\\(form\\|late\\|mit\\)\\|iangle_wave\\|ue\\)\\|tf\\|urb\\(ulence\\|_depth\\)\\|ype\\)?\\|u\\(ltra_wide_angle\\|nion\\|p\\|se_\\(colou?r\\|index\\)\\|_steps\\)?\\|v\\(a\\(l\\|riance\\|xis_rotate\\)\\|cross\\|dot\\|ersion\\|length\\|normalize\\|olume_\\(object\\|rendered\\)\\|ol_with_light\\|rotate\\|_steps\\)?\\|w\\(a\\(rp\\|ter_level\\|ves\\)\\|hile\\|idth\\|ood\\|rinkles\\)\\|x\\|y\\(es\\)?\\|z\\)\\>" . font-lock-keyword-face))

(defvar pov-font-lock-keywords
  '(
    ("^[ \t]*\\(#declare\\)\\>[ \t]*\\(\\sw+\\)"  2 font-lock-variable-name-face nil t)
    ("\\<#\\(break\\|case\\|de\\(bug\\|clare\\|fault\\)\\|e\\(lse\\|nd\\|rror\\)\\|if\\(n?def\\)?\\|include\\|r\\(ange\\|ender\\)\\|s\\(tatistics\\|witch\\)\\|version\\|w\\(arning\\|hile\\)\\)\\>" . font-lock-function-name-face)
    ("\\<\\(b\\(icubic_patch\\|lob\\|ox\\)\\|c\\(one\\|ub\\(e\\|ic\\(_spline\\)?\\)\\|ylinder\\)\\|disc\\|height_field\\|julia_fractal\\|lathe\\|mesh\\|p\\(lane\\|oly\\(gon\\)?\\|rism\\)\\|qua\\(rt\\|dr\\)ic\\|s\\(mooth_triangle\\|sor\\|phere\\|uperellipsoid\\)\\|t\\(ext\\|orus\\|riangle\\)\\|Rounded\\(Box\\|Cylinder\\)\\)\\>" . font-lock-type-face))
  "Expressions to highlight in PoV mode.")

(defun pov-setup-font-lock nil
  "Find out if we're fontifying insanely"
  (if pov-fontify-insanely
      (setq pov-font-lock-keywords (append (list pov-all-keyword-matcher) pov-font-lock-keywords))))

(defun pov-mode nil
  "Major mode for editing PoV files.

   In this mode, TAB and \\[indent-region] attempt to indent code
based on the position of {} pairs.  The variable pov-indent-level
controls the amount of indentation used inside arrays and begin/end
pairs.  This mode also provides PoVray keyword fontification.

\\{pov-mode-map}

\\[pov-mode] calls the value of the variable pov-mode-hook 
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pov-mode-map)
  (pov-setup-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'tab-stop-list)
  (set-syntax-table pov-mode-syntax-table)
  (setq comment-start "// "
	comment-start-skip "/\\*+ *\\|// *"
	comment-end ""
	comment-multi-line nil
	comment-column 60
	indent-line-function 'pov-indent-line
	tab-stop-list pov-tab-stop-list)
  (setq mode-name "PoV")
  (setq major-mode 'pov-mode)
  (pov-setup-font-lock)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords pov-font-lock-keywords)
  (run-hooks 'pov-mode-hook))

(defun pov-tab ()
  "Command assigned to the TAB key in PoV mode."
  (interactive)
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (pov-indent-line)
    (save-excursion
      (pov-indent-line))))

(defun pov-indent-line nil
  "Indents a line of PoV code."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (if (pov-top-level-p)
      (pov-indent-top-level)
    (if (not (or 
	      (looking-at "//")	; "//" comments stay at left margin
	      (looking-at "/\\*")   ; so do "/*" comments
	      (looking-at "\\*/")   ; so do the endcomments
	      (pov-top-level-p)))
	(if (pov-in-star-comment-p)
	    (indent-to '2)
	  (if (and (< (point) (point-max))
		   (eq ?\) (char-syntax (char-after (point)))))
	      (pov-indent-close)		; indent close-delimiter
	    (pov-indent-in-block))))))	; indent line after open delimiter
  
(defun pov-newline nil
  "Terminate line and indent next line."
  (interactive)
  (newline)
  (pov-indent-line))

(defun pov-in-star-comment-p nil
  "Return true if in a star comment"
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (nth 4 state)))

(defun pov-open nil
  (interactive)
  (insert last-command-char))

(defun pov-close nil
  "Inserts and indents a close delimiter."
  (interactive)
  (insert last-command-char)
  (backward-char 1)
  (pov-indent-close)
  (forward-char 1)
  (blink-matching-open))

(defun pov-indent-close nil
  "Internal function to indent a line containing a close delimiter."
  (if (save-excursion (skip-chars-backward " \t") (bolp))
      (let (x (oldpoint (point)))
	(forward-char) (backward-sexp)	;XXX
	(if (and (eq 1 (count-lines (point) oldpoint))
		 (> 1 (- oldpoint (point))))
	    (goto-char oldpoint)
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (setq x (current-column))
	  (goto-char oldpoint)
	  (delete-horizontal-space)
	  (indent-to x)))))

(defun pov-indent-in-block nil
  "Indent a line which does not open or close a block."
  (let ((goal (pov-block-start)))
    (setq goal (save-excursion
		 (goto-char goal)
		 (back-to-indentation)
		 (if (bolp)
		     pov-indent-level
		   (back-to-indentation)
		   (+ (current-column) pov-indent-level))))
    (indent-to goal)))

(defun pov-indent-top-level nil
  (if (save-excursion 
	(forward-line -1)
	(looking-at "\\<#declare[ \t]+[0-9a-zA-Z_]+[ \t]*=[ \t]*$"))
	(indent-to pov-indent-under-declare)))

;;; returns nil if at top-level, or char pos of beginning of current block

(defun pov-block-start nil
  "Returns the character position of the character following the nearest
enclosing `{' or `begin' keyword."
  (save-excursion
    (let (open (skip 0))
      (setq open (condition-case nil
		     (save-excursion
		       (backward-up-list 1)
		       (1+ (point)))
		   (error nil)))
      (pov-begin-end-hack open))))

(defun pov-begin-end-hack (start)
  "Search backwards from point to START for enclosing `begin' and returns the
character number of the character following `begin' or START if not found."
  (save-excursion
    (let ((depth 1) match)
      (while (and (> depth 0)
		  (or (re-search-backward
 "^[ \t]*\\(dict\\|class\\)?\\(end\\|grestore\\)\\|\\(begin\\|gsave\\)[ \t]*\\(%.*\\)*$" start t)
		      (re-search-backward "^[ \t]*cdef.*$" start t)))
 	(setq depth (if (looking-at "[ \t]*\\(dict\\|class\\)?\\(end\\|grestore\\)")
			(1+ depth) (1- depth))))
      (if (not (eq 0 depth))
	  start
	(forward-word 1)
	(point)))))

(defun pov-top-level-p nil
  "Awful test to see whether we are inside some sort of PoVray block."
  (and (condition-case nil
	   (not (scan-lists (point) -1 1))
	 (error t))
       (not (pov-begin-end-hack nil))))

;;; initialize the keymap if it doesn't already exist

(if (null pov-mode-map)
    (progn
      (setq pov-mode-map (make-sparse-keymap))
      (define-key pov-mode-map "{" 'pov-open)
      (define-key pov-mode-map "}" 'pov-close)
      (define-key pov-mode-map "\t" 'pov-tab)
      (define-key pov-mode-map "\r" 'pov-newline)))
