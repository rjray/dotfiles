;; VXML mode.
;;;
;;; based heavily on psgml.

(provide 'vxmlmode)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files." t)

(setq auto-mode-alist
      (append '(("\\.xml$"    . xml-mode)
                ("\\.sgml$"   . sgml-mode)
                ("\\.vxml$"   . vxml-mode)
                ("\\.txml$"   . vxml-mode)
                ) auto-mode-alist))

(setq sgml-always-quote-attributes           t
      sgml-auto-insert-required-elements     t
      sgml-suppress-warning                  t
      sgml-recompile-out-of-date-cdtd        nil
      sgml-minimize-attributes               nil
      sgml-auto-activate-dtd                 t
      sgml-system-identifiers-are-preferred  nil
      sgml-indent-step                       4
      sgml-indent-data                       t
      sgml-set-face                          t
      sgml-markup-faces '((comment . font-lock-comment-face)
                          (doctype . font-lock-builtin-face)
                          (start-tag . bold)
                          (end-tag . bold)
                          (entity . font-lock-constant-face)))

(defvar vxml-attributes nil
  "alist of which attributes are legal in which element types.")

; Additional expressions to highlight in SGML mode.

(setq vxml-font-lock-keywords
  '(; Highlight the text between these tags in VXML mode.
;    ("<!\[CDATA\[(.|\\n)*\]\]>" . font-lock-string-face) 
    ("<[^ <>!]+[ >]" . font-lock-keyword-face) 
    (">" . font-lock-keyword-face) 
    ("=\\(\"[^\"]*\"\\)" 1 font-lock-variable-name-face)
    (vxml-fontify-singlequoted-string 0 font-lock-string-face t)
    ))

(defun vxml-looking-before (regexp)
  "True if the line before the cursor matches the given regexp.  $ in the
regexp is where the current point is at."
  (let ((cursor (point)))
    (save-excursion
      (save-restriction
        (beginning-of-line)
        (narrow-to-region (point) cursor)
        (looking-at regexp)))))

(defun vxml-fontify-singlequoted-string (limit)
  (let (result sqstart sqend indq)
    (setq result nil)
    (setq sqstart 1)
    (setq sqend 1)
    (while (and sqstart sqend (< (point) limit))
      (setq sqstart (search-forward "'" limit t))
      (setq sqend (and (< (point) limit) (search-forward "'" limit t)))
      (if (and sqstart sqend)
          (progn
            (save-excursion
              (beginning-of-line)
              (setq indq nil)
              (while (and (< (point) sqstart) (search-forward "\"" sqstart t))
                (setq indq (not indq))))
            (if indq
                (progn
                  (set-match-data (list (- sqstart 1) sqend))
                  (setq result t)
                  (setq sqstart nil))))))
    result))
        
; (add-hook 'sgml-mode-hook 
;         (function (lambda()
;                     (defun sgml-lt ()
;                       (interactive)
;                       (insert "&lt;"))                      
;                     (define-key sgml-mode-map "\^C<" 'sgml-lt)

;                     (defun sgml-gt ()
;                       (interactive)
;                       (insert "&gt;"))                      
;                     (define-key sgml-mode-map "\^C>" 'sgml-gt)

;                     (defun sgml-emdash ()
;                       (interactive)
;                       (insert "&mdash;"))
;                     (define-key sgml-mode-map "\^C_" 'sgml-emdash)

;                     (defun sgml-amp ()
;                       (interactive)
;                       (insert "&amp;"))
;                     (define-key sgml-mode-map "\^C&" 'sgml-amp)
                      
;                     (defun sgml-comment ()
;                       (interactive)
;                       (insert "<!--  -->")
;                       (backward-char 4))
;                     (define-key sgml-mode-map "\^Co" 'sgml-comment)

;                     )))

; (require 'mmm-mode)
; (mmm-add-group
;  'vxml-js
;  '((js-tag
;     :submode javascript
;     :face mmm-code-submode-face
;     :front "<script\[^>/\]*>\\s-*\n"
;     :back"\\s-*</script>"
;     :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
;                  @ "\n" _ "\n" @ "</script>" @))
;     )
; ;    (js-inline
; ;     :submode javascript
; ;     :face mmm-code-submode-face
; ;     :front "\\(cond\\|expr\\)=\""
; ;     :back "\"")
; ))

(defun vxml-mode ()
  (interactive)
  (xml-mode)
  (require 'psgml-parse)                ;Works around a bug in the pgsml that
                                        ;doesn't seem to properly auto-load
                                        ;this module.

  (let ((p load-path)
        (d))
    (while p
      (setq d (concat (car p) "/parsedDTD.ced"))
      (setq p (cdr p))
      (if (file-readable-p d)
          (progn
            (setq sgml-default-dtd-file d)
            (setq p nil)))))

  (make-local-variable 'sgml-default-doctype-name)
  (setq sgml-default-doctype-name      "vxml"
        major-mode                     'vxml-mode
        mode-name                      "VoiceXML"
        indent-line-function            'vxml-indent-line-function
        indent-tabs-mode                'nil)

  (global-set-key "\^Cj" 'vxml-narrow-to-javascript)
  (global-set-key "\^Cr" 'vxml-release-javascript)
  (define-key sgml-mode-map "\t" 'vxml-expand-and-indent)
  (define-key sgml-mode-map "<" 'vxml-electric-open-angle-bracket)
  (define-key sgml-mode-map ">" 'vxml-electric-close-angle-bracket)
  (define-key sgml-mode-map "&" 'vxml-electric-ampersand)
  (define-key sgml-mode-map "\r" 'newline-and-indent)

  (modify-syntax-entry ?' "\"")
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(vxml-font-lock-keywords t))

  (turn-on-font-lock)
  (auto-fill-mode 0)

  (easy-menu-define
   vxml-menu sgml-mode-map "VXML menu"
   (list "VXML"
         ["Element documentation"       vxml-manual     t]
         ["Indent current buffer"       vxml-indent-buffer t]
         (nconc
          (list "Insert Code Template")
          (vxml-get-templates)
          (list 
           "--"
           ["Reload Templates"         vxml-update-templates-menu t]))))
  
  (easy-menu-add vxml-menu)
  
;  (setq mmm-classes 'vxml-js)
;  (mmm-mode 1)
)

(require 'psgml)
(autoload 'sgml-map-element-types "psgml-info")

(setq vxml-field-definitions
      '(("assign" . "<assign name=\"\" expr=\"\" />")
        ("audio" . ("<audio expr=\"audioPath + ''\">"
                    "<audio expr=\"\">"
                    "<audio src=\"\">"
                    "<audio>"))
        ("break" . ("<break time=\"\" />"
                    "<break timeexpr=\"\" />"))
        ("catch" . "<catch event=\"\">")
        ("clear" . "<clear namelist=\"\" />")
        ("data" . ("<data name=\"\" src=\"\" />"
                   "<data name=\"\" expr=\"\" />"))
        ("disconnect" . "<disconnect />")
        ("else" . "<else />")
        ("elseif" . "<elseif cond=\"\" />")
        ("exit" . "<exit />")
        ("goto" . ("<goto next=\"\" />"
                   "<goto expr=\"\" />"
                   "<goto nextitem=\"\" />"
                   "<goto expritem=\"\" />"))
        ("grammar" . ("<grammar src=\"\" type=\"application/x-gsl\"/>"
                     "<grammar type=\"application/x-gsl\">"
                     "<grammar expr=\"\" type=\"application/x-gsl\"/>"))
        ("if" . "<if cond=\"\">")
        ("field" . "<field name=\"\">")
        ("foreach" . "<foreach item=\"\" array=\"\">")
        ("form" . "<form id=\"\">")
        ("link" . ("<link next=\"\">"
                   "<link event=\"\">"))
        ("meta" . "<meta name=\"\" content=\"\" />")
        ("noinput" . "<noinput count=\"\">")
        ("nomatch" . "<nomatch count=\"\">")
        ("param" . "<param name=\"\" expr=\"\" />")
        ("property" . "<property name=\"\" value=\"\" />")
        ("record" . "<record name=\"\" maxtime=\"\">")
        ("reprompt" . "<reprompt />")
        ("return" . ("<return namelist=\"\" />"
                     "<return event=\"\" />"
                     "<return eventexpr=\"\" />"))
        ("script" . ("<script src=\"\" />"
                     "<script>"))
        ("subdialog" . ("<subdialog name=\"\" src=\"\">"
                        "<subdialog name=\"\" srcexpr=\"\">"))
        ("submit" . ("<submit next=\"\" namelist=\"\" />"
                     "<submit expr=\"\" namelist=\"\" />"))
        ("throw" . ("<throw event=\"\" />"
                    "<throw eventexpr=\"\" />"))
        ("transfer" . ("<transfer name=\"\" dest=\"\">"
                       "<transfer name=\"\" destexpr=\"\">"))
        ("value" . "<value expr=\"\" />")
        ("var" . ("<var name=\"\" expr=\"\" />"
                  "<var name=\"\" />"))
        ("vxml" . "<vxml application=\"\" version=\"2.0\">")))

;; These are the various initial values to show for different elements.
; (defun vxml-preformatted-line (x)
;   (cond ((equal x "form")
;        "<form id=\"\">")
;       ((equal x "assign")
;        "<assign name=\"\" expr=\"\" />")
;       (t (concat "<" x ">"))))

(defun vxml-preformatted-line (xx)
  (let ((alist vxml-field-definitions)
        (result nil))
    (while (and alist (not result))
      (if (equal (car (car alist)) xx)
          (setq result (cdr (car alist))))
      (setq alist (cdr alist)))
    (if (null result)
        (concat "<" xx ">")
      result)))

(defvar vxml-expansion-list ()
  "List of possible expansions from last time someone hit <tab>.  First element of the list is what was used last time.")

(defvar vxml-last-insertion-position ()
  "Where the beginning of the last inserted tag is.")

(defvar vxml-audio-catalog-attributes ()
  "Attributes of the audio catalog file.")
(make-variable-buffer-local 'vxml-audio-catalog-attributes)

(defvar vxml-audio-catalog ()
  "The audio catalog to use for the current buffer.  This is stored as a list
of 'triples'.  Each triple is a list of three items: the pathname to the wav
file, the TTS equivilant, and the number of milliseconds to pause after
playing the file.")
(make-variable-buffer-local 'vxml-audio-catalog)

(defun vxml-load-audio-catalog ()
  (let* ((filename (concat default-directory "AUDIOCATALOG"))
         (attr (file-attributes filename))
         (newcatalog vxml-audio-catalog))
    (if (null attr)
        (setq newcatalog ())
      (if (or (null vxml-audio-catalog-attributes)
              (not (= (nth 7 attr) (nth 7 vxml-audio-catalog-attributes)))
              (not (equal (nth 5 attr) (nth 5 vxml-audio-catalog-attributes))))
          (with-temp-buffer
            (setq vxml-audio-catalog-attributes attr)
            (insert-file-contents filename)
            (beginning-of-buffer)
            (setq newcatalog nil)
            (while (< (point) (point-max))
              (if (looking-at "^\\(.+\\)|\\(.+\\)|\\(.+\\)$")
                  (progn
                    (setq newcatalog
                          (cons
                           (list (match-string-no-properties 1)
                                 (match-string-no-properties 2)
                                 (match-string-no-properties 3))
                           newcatalog))))
              (next-line 1)))))
    (setq vxml-audio-catalog newcatalog)))

(defun vxml-update-auto-audio ()
  (interactive)
  (vxml-load-audio-catalog)
  (let (wav tts break tmp audiostart audioend endeffectedarea oldprefix)
    (save-excursion
      (search-backward "<audio")
      (if (looking-at "<audio +expr=\"\ *audioPath *\\+ *'\\(.*\\)'\">")
          (progn
            (setq wav (match-string-no-properties 1))
            (setq tmp vxml-audio-catalog)
            (while tmp
              (if (equal (car (car tmp)) wav)
                  (progn
                    (setq tts (cadr (car tmp)))
                    (setq break (caddr (car tmp)))
                    (setq tmp nil))
                (setq tmp (cdr tmp))))
            (if (or (null tts) (null break))
                (error "Not found in audio catalog: %s" wav))
            (setq audiostart (point))
            (search-forward "</audio>")
            (setq audioend (point))
            (next-line 2)
            (beginning-of-line)
            (setq endeffectedarea (point))
            (goto-char audiostart)
            (if (search-forward "autoAudioTTS" endeffectedarea t)
                (progn
                  (goto-char audiostart)
                  (search-forward ">")
                  (delete-region (point) audioend)
                  (insert "\n")
                  (sgml-indent-or-tab)
                  (setq oldprefix fill-prefix)
                  (set-fill-prefix)
                  (insert tts)
                  (insert "\n</audio>")
                  (sgml-indent-or-tab)
                  (next-line -1)
                  (fill-paragraph nil)
                  (setq fill-prefix oldprefix)))
            (search-forward "</audio>")
            (setq audioend (point))
            (next-line 2)
            (beginning-of-line)
            (setq endeffectedarea (point))
            (goto-char audiostart)
            (if (search-forward "autoAudioBreak" endeffectedarea t)
                (progn
                  (goto-char audiostart)
                  (search-forward "<break")
                  (search-backward "<")
                  (setq breakstart (point))
                  (sgml-forward-element)
                  (delete-region breakstart (point))
                  (insert "<break time=\"" break "ms\" />"))))))))

(defun vxml-load-attribute-list ()
  "Loads up the vxml-attributes variable."
  (if (null vxml-attributes)
      (progn
        (sgml-map-element-types
         (function
          (lambda (eltype)
            (let ((list nil))
              (loop for a in (sgml-eltype-attlist eltype) do
                    (setq list (cons (list (sgml-attdecl-name a)) list)))
              (setq vxml-attributes
                    (cons (cons (sgml-eltype-name eltype) list)
                          vxml-attributes))))))))
  vxml-attributes)

;vxml-insert-expansion inserts the given string, inserts matching closing
;tags if necessary, and moves the point to the right place.

(defun vxml-insert-expansion (line)
  (let ((str))
    (setq vxml-last-insertion-position (point))
    (insert line)
    (save-excursion
      (back-to-indentation)
      (forward-char)
      (let ((wordstart (point)))
        (forward-word 1)
        (setq str (buffer-substring-no-properties wordstart (point)))))
    (forward-char -2)
    (if (not (looking-at "\\/>"))
        (progn
          (end-of-line)
          (insert "\n</" str ">")
          (sgml-indent-or-tab)
          (forward-line -1)))
    (back-to-indentation)
    (if (looking-at "<audio expr=\"audioPath \\+ '")
        (goto-char (match-end 0))
      (if (looking-at "<[a-z ]*=\"")
          (goto-char (match-end 0))
        (progn
          (end-of-line)
          (sgml-indent-or-tab)
          (insert "\n"))))
    (cond ((equal str "filled") 
           (insert "<if")
           (vxml-autoexpand)
           (if
               (save-excursion
                 (search-backward-regexp
                  "field[^<>]* name=\"\\([^\"]*\\)\""
                  (point-min)
                  t))
               (progn
                 (insert
                  (match-string-no-properties 1)
                  " == ''")
                 (forward-char -1))))
          ((or (equal line "<script>")
               (equal line "<grammar type=\"application/x-gsl\">"))
           (if (looking-at "[a-z]")
               (progn
                 (sgml-indent-or-tab)
                 (end-of-line)
                 (insert "\n")
                 (sgml-indent-or-tab)))
           (insert "<![CDATA[\n\n]]>")
           (sgml-indent-or-tab)
           (forward-line -2)
           (sgml-indent-or-tab)
           (forward-line 1)
           (sgml-indent-or-tab)
;          (save-excursion
;            (search-backward line)
;            (let ((tagstart (point)))
;              (sgml-forward-element)
;              (mmm-parse-region tagstart (point))))
           )
          ((or (equal str "noinput") (equal str "nomatch"))
           (let ((parentstart)
                 (count 1))
             (save-excursion
               (beginning-of-line)
               (ignore-errors (sgml-up-element))
               (ignore-errors (sgml-backward-element))
               (setq parentstart (point)))
             (save-excursion
               (if (search-backward-regexp
                    (concat "<" str " *count=\"\\([0-9]+\\)\"")
                    parentstart t)
                   (ignore-errors
                     (progn
                       (setq count (+ (string-to-int
                                       (match-string-no-properties 1))
                                      1))))))
             (insert (int-to-string count)))))))

(defun vxml-autoexpand ()
  (interactive)
  (cond ((and (eq last-command this-command)
              (not (null vxml-expansion-list)))
         (progn
           (goto-char vxml-last-insertion-position)
           (sgml-forward-element)
           (delete-region vxml-last-insertion-position (point))
           (let ((tmp (car vxml-expansion-list)))
             (setq vxml-expansion-list
                   (append (cdr vxml-expansion-list) (list tmp))))
           (vxml-insert-expansion (car vxml-expansion-list))))
        
        ((= (buffer-size) 0)
         (progn (insert "<?xml version=\"1.0\"?>\n\n<!--\n    Tellme Source Code. Tellme Confidential. Do Not Distribute.\n    Copyright (C) 1999-2001 Tellme Networks, Inc. All Rights Reserved.\n-->\n\n<vxml")
                (vxml-autoexpand)))

        ((vxml-looking-before "^\\s-*<\\([a-z]+\\)[^<>]* \\([a-z:]*\\)$")
         (let ((elementname (match-string-no-properties 1))
               (tagname (match-string-no-properties 2))
               (origtagname)
               (alist (vxml-load-attribute-list))
               (list nil)
               (match nil))
           (setq vxml-expansion-list ())
           (if (looking-at "\\([a-z:]*\\)'")
               (if (= (point) (match-beginning 0))
                   (setq tagname (concat tagname
                                         (match-string-no-properties 1)))))
           (setq origtagname tagname)
           (while alist
             (if (equal (car (car alist)) elementname)
                 (progn
                   (setq list (cdr (car alist)))
                   (setq alist nil))
               (setq alist (cdr alist))))
           (setq match (try-completion tagname list))
           (if (null match)
               (message "No tagnames match '" tagname "'")
             (progn
               (if (not (equal match t))
                   (setq tagname match))
               (setq match nil)
               (mapc
                (function (lambda (x)
                            (if (equal (car x) origtagname)
                                (setq match t))))
                list)
               (if match
                   (setq list (list (list tagname))))
               (setq list (all-completions tagname list))
               (if (> (length list) 1)
                   (message "Ambiguous; any of %s"
                            (mapconcat 'prin1-to-string list ", ")))
               (search-backward " ")
               (forward-char 1)
               (delete-char (length origtagname))
               (insert tagname)
               (if (= (length list) 1)
                   (if (or (not (looking-at "="))
                           (not (= (point) (match-beginning 0))))
                       (progn
                         (insert "=\"\"")
                         (delete-horizontal-space)
                         (if (or (not (looking-at ">"))
                                 (not (= (point) (match-beginning 0))))
                             (progn
                               (insert " ")
                               (backward-char 1)))
                         (backward-char 1))))
               (if (not (looking-at ".*>"))
                   (save-excursion
                     (end-of-line)
                     (delete-horizontal-space)
                     (insert " />")))))))

        ((looking-at "\"[a-z ]+=\"")
         (progn
           (setq vxml-expansion-list ())
           (goto-char (match-end 0))))

        ((looking-at "\"[^\"]+$")
         (setq vxml-expansion-list ())
         (end-of-line)
         (insert "\n"))

        ((vxml-looking-before "^\\s-*<audio[^<>]*expr=\" *audioPath *\\+ *'\\([^<>'\"]*\\)")
         (setq vxml-expansion-list ())
         (let ((wav (match-string-no-properties 1))
               (match nil)
               (origwav))
           (vxml-load-audio-catalog)
           (if vxml-audio-catalog
               (progn
                 (if (looking-at "\\([^<>'\"]*\\)'")
                     (if (= (point) (match-beginning 0))
                         (setq wav (concat wav
                                           (match-string-no-properties 1)))))
                 (setq origwav wav)
                 (setq match (try-completion wav vxml-audio-catalog))
                 (if match
                     (progn
                       (setq wav match)
                       (setq match (try-completion wav vxml-audio-catalog))))
                 (if (not (equal match 't))
                     (progn
                       (setq wav
                             (completing-read
                              "Wav file to use (Ctrl-G to abort, ? for choices, tab to complete): "
                              vxml-audio-catalog nil nil wav))
                       (if (null wav)
                           (setq wav ""))))
                 (if (equal t (try-completion wav vxml-audio-catalog))
                     (progn
                       (beginning-of-line)
                       (setq startpoint (point))
                       (sgml-forward-element)
                       (end-of-line)
                       (forward-char 1)
                       (if (looking-at "^\\s-*<break.*<!-- *autoAudioBreak")
                           (next-line 1))
                       (delete-region startpoint (point))
                       (sgml-indent-or-tab)
                       (insert "<audio expr=\"audioPath + '" wav "'\">\n")
                       (sgml-indent-or-tab)
                       (insert "</audio><!-- autoAudioTTS -->\n")
                       (sgml-indent-or-tab)
                       (insert "<break time=\"0ms\" /><!-- autoAudioBreak -->\n")
                       (previous-line 1)
                       (vxml-update-auto-audio)
                       (end-of-line)
                       (forward-char 1)
                       (sgml-indent-or-tab))
                   (progn
                     (search-backward "'")
                     (forward-char 1)
                     (delete-char (length origwav))
                     (insert wav)
                     (end-of-line)
                     (insert "\n")
                     (sgml-indent-or-tab)))))))

        (t
         (let ((start-pos (point))
               (leadingchar nil))
           (setq vxml-expansion-list ())
           (save-excursion
             (back-to-indentation)
             (if (looking-at "<?[a-z]*$")
                 (progn
                   (if (looking-at "<")
                       (progn
                         (forward-char 1)
                         (setq leadingchar "<")))
                   (let ((fpos (point))
                         epos
                         str            ; The string the user typed that we
                                        ; want to expand.
                         list
                         line
                         )
                     (end-of-line)
                     (setq epos (point))
                     (setq str (buffer-substring-no-properties fpos epos))
                     (back-to-indentation)
                     (if leadingchar (delete-char 1))
                     (delete-char (length str))
                     (sgml-parse-to-here)
                     (setq list (mapcar
                                 (function (lambda (x)
                                             (let ((s (prin1-to-string x)))
                                               (if (and
                                                    (>= (length s) (length str))
                                                    (equal (substring s
                                                                      0 (length str))
                                                           str))
                                                   x
                                                 nil))))
                                 (sgml-current-list-of-valid-eltypes)))
                     (setq list (remove-if 'null list))
                     (if (< 1 (length list))
                                        ; If we have an exact match, use it.
                         (let ((l list))
                           (while l
                             (if (equal str (prin1-to-string (car l)))
                                 (setq list (list (car l))))
                             (setq l (cdr l)))))
                     (if (= 1 (length list))
                         (progn
                           (setq str (prin1-to-string (car list)))
                           (setq line (vxml-preformatted-line str))
                           (if (listp line)
                               (progn
                                 (setq vxml-expansion-list line)
                                 (setq line (car line))))
                           (vxml-insert-expansion line)
                           (setq start-pos (point)))
                       (progn
                         (if (= 0 (length list))
                             (message "No strings match %s" str)
                           (message "Ambiguous; any of %s"
                                    (mapconcat 'prin1-to-string list ", ")))
                         (if leadingchar (insert leadingchar))
                         (insert str)))))
               (ignore-errors (vxml-update-auto-audio))))
           (goto-char start-pos)))))

(defun vxml-indent-line-function ()
  (sgml-indent-line)
  (save-excursion
    (back-to-indentation)
    (let ((col (current-column))
          (correction sgml-indent-step))
      (if (looking-at "<else")
          (setq correction (+ correction sgml-indent-step)))
      (if (> correction col)
          (setq correction col))
      (backward-delete-char-untabify correction))))

(defun vxml-expand-and-indent ()
  (interactive)
  (vxml-autoexpand)
  (sgml-indent-or-tab))

;; Determine if we are inside double quotes.  Actually, more accurately,
;; determine if we are in a place where we need to escape special characters
;; like "&", "<", and ">".

(defun vxml-inside-quotes ()
  (let ((start (point))
        (result nil)
        (cdatastart))
    (save-excursion
      (beginning-of-line)
      (while (< (point) start)
        (if (looking-at "\"")
            (setq result (not result)))
        (forward-char 1)))
    ;; at this point, result should be t iff we are in an odd number of quotes.
    ;; If we are, then we want to see whether we are inside of a CDATA thingy,
    ;; because we don't want to escape things there.
    (if result
        (save-excursion
          (setq cdatastart (search-backward "[CDATA[" (point-min) t))
          (if cdatastart
              (progn
                (goto-char cdatastart)
                (ignore-errors (forward-sexp))
                (if (or (> (point) start)
                        (= (point) cdatastart))
                    (setq result nil))))))
    result))

(defun vxml-electric-open-angle-bracket ()
  (interactive)
  (if (vxml-inside-quotes)
      (insert "&lt;")
    (insert "<"))
  (sgml-indent-or-tab))

(defun vxml-electric-close-angle-bracket ()
  (interactive)
  (if (vxml-inside-quotes)
      (insert "&gt;")
    (insert ">"))
  (sgml-indent-or-tab))

(defun vxml-electric-ampersand ()
  (interactive)
  (if (vxml-inside-quotes)
      (insert "&amp;")
    (insert "&")))

  ;; These two functions narrow down (and release) to a JavaScript segment
  ;; of a VXML file.
(defun vxml-narrow-to-javascript ()
  (interactive)
  (let (beg end)
    (save-excursion
      (search-backward-regexp "<script[^<>]*>" )
      (search-forward-regexp "<!\\[CDATA\\s-*\\[")
      (setq beg (point))
      (search-forward "</script>" nil t)
      (search-backward-regexp "\\]\\s-*\\]\\s-*>" nil t)
      (setq end (point)))
    (narrow-to-region beg end)
    (java-mode)))
  
(defun vxml-release-javascript()
  (interactive)
  (widen)
  (vxml-mode))

(defun vxml-manual ()
  (interactive)
  (let ((tag ""))
    (save-excursion
      (beginning-of-line)
      (if (looking-at ".*<\\([a-z]+\\) ")
          (progn
            (setq tag (match-string-no-properties 1)))))
    (setq tag (completing-read "Display VoiceXML manual for: "
                               vxml-field-definitions nil nil tag))
    (browse-url (format "http://studio.tellme.com//vxml2/ref/elements/%s.html"
                        tag))))

(defun vxml-log (a)
  (save-window-excursion
    (find-file "/tmp/debugvxmllog")
    (end-of-buffer)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S "))
    (insert a)
    (insert "\n")
    (save-buffer)
    (bury-buffer)))

(defun vxml-indent-buffer () 
  (interactive)
  (save-excursion
    (while (< (point) (point-max))
      (sgml-indent-or-tab)
      (next-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEMPLATE CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vxml-template-buffer "*vxml-template*"
  "Buffer used temporatily to manage templates")

; Search all the load-path for files ending with .template
; to use as vxml generation templates
;

(defun vxml-update-templates-menu () 
  (interactive)
  (easy-menu-change '("VXML") "Insert Code Template"
                      (nconc
                       (vxml-get-templates)
                       (list 
                        "--"
                        ["Reload Templates"          vxml-update-templates-menu t]))))

; Search all the folders in load-path looking for files
; with extension ".template", 
; return a list with a menu vector for each template found

(defun vxml-get-templates ()
  (let ((directory load-path)
        (files-in-directory)
        (file)
        (template-file-list))
    
    (while directory

      (setq files-in-directory
            (ignore-errors
              (directory-files 
               (car directory)
               t "\\.template$")))
      
      (setq directory  (cdr directory))
      
      (while files-in-directory 
        (setq file (car files-in-directory))
        (setq files-in-directory (cdr files-in-directory))
        (if (and
             (> (length file) 9)
             (equal
              (substring file (- (length file) 9) nil)
              ".template"))
            (setq template-file-list 
                  (cons (vector file 
                                (` (vxml-insert-template (, file)))
                                t)
                        template-file-list)))))
    template-file-list))

; Given a template file name, process the template looking for
; all variables of the form %varname%, prompt the caller
; for values for the variables, perform replacement,
; then insert the template at the current position and indent 
; each line properly

(defun vxml-insert-template (template)
  (interactive)
  (let 
      ((variable-list)
       (first-tag-in-template nil)
       (template-line-count)
       (insert-template))
    
    (get-buffer-create vxml-template-buffer)
    (save-excursion
      (set-buffer vxml-template-buffer)
      (erase-buffer)
      (insert-file-contents template)
      (beginning-of-buffer)

      (while (< (point) (point-max))
        (if (and
             (not first-tag-in-template)
             (looking-at ".*<\\([a-z]+\\) "))
            (setq first-tag-in-template 
                  (match-string-no-properties 1)))
        
        (if (looking-at ".*\%\\([A-Za-z0-9_\-]+\\)\%")
            (progn
              (setq variable (match-string-no-properties 1))
              (if (not (member variable variable-list))
                  (setq variable-list
                        (cons variable variable-list)))))
        
        (next-line 1))

      (if first-tag-in-template
          (progn
            (setq insert-template nil)

            (mapc
             (function (lambda (x)
                         (let ((s (prin1-to-string x)))
                           (if  (equal s first-tag-in-template)
                               (setq insert-template t)))))
             (sgml-current-list-of-valid-eltypes))

            (if (not insert-template)
                (setq insert-template
                      (completing-read (concat "tag <" 
                                               first-tag-in-template
                                               "> is not valid at the current context.  Insert anyway ? ")
                                       '(("yes" t) ("no" nil))
                                       nil)))

            
            (if (equal insert-template "no")
                (error (concat first-tag-in-template
                               " is not a valid tag at this context. Template insertion aborted")))))

      (loop for myvar in variable-list do
            (setq myvalue
                  (read-string (concat "Value for " 
                                           myvar
                                           " : ")))
            ; replace all occurances
            (beginning-of-buffer)
            (while (re-search-forward (concat "%" myvar "%")
                                      nil t)
              (replace-match myvalue t nil)))
            
      
      (setq template-line-count (count-lines 1 (point-max))))
    
    (insert-buffer vxml-template-buffer)
    (kill-buffer vxml-template-buffer)
    
    (save-excursion
      (setq lines-processed 0)
      (while (<= lines-processed template-line-count)
        (setq lines-processed (1+ lines-processed))
        (sgml-indent-or-tab)
        (next-line 1)))))
