;;; Various auto-load commands to associate function names and mode names with
;;; specific files

(autoload 'sawfish-mode "sawfish")
(autoload 'tcl-mode "tcl" "Tcl/Tk Editing Mode" t)
(autoload 'xpm-mode "xpm-mode" "XPM Editing Mode" t)
(autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
(autoload 'xrdb-mode "xrdb-mode" "X Resources editing mode" t)
(autoload 'ada-mode "elec-ada" "Mode for ada programs" t)
(autoload 'cperl-mode "cperl-mode" "Perl editing mode" t)
(autoload 'perl-mode "cperl-mode" "Perl editing mode" t)
(autoload 'bison-mode "bison-mode" "Bison/yacc editing mode" t)
(autoload 'makefile-mode "make-mode" "Makefile Major Mode" t)
(autoload 'csharp-mode "csharp-mode" "C# Major Mode" t)
(autoload 'io-mode "io-mode" "Io Major Mode" t)
(autoload 'tt-mode "tt-mode" "Template Toolkit Major Mode" t)
(autoload 'yaml-mode "yaml-mode" "YAML Major Mode" t)
(autoload 'ri "ri-ruby" nil t)

(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(autoload 'rpm "rpm"
  "Shell for the rpm package management utility." t)
(autoload 'rpm-dired-install "rpm"
  "Install all marked (or next ARG) rpm-files." t)

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

(autoload 'db-find-file "database" "EDB database package" t)
(autoload 'man "manual" "Improved Manual Browsing" t)
(autoload 'prove "prove" "Perl prove" t)
(autoload 'ack "ack" "Perl ack" t)
(autoload 'sokoban "sokoban" "Sokoban" t)
(autoload 'solitaire "solitaire" "Solitaire Stone Game" t)
(autoload 'iso-accents-mode "iso-acc" "ISO Accents minor mode." t)
(autoload 'tmpl-expand-templates-in-buffer "tmpl-minor-mode"
  "Expand all templates in the current buffer." t)
(autoload 'dired-make-permissions-interactive "dired-chmod" nil t)
(autoload 'id-select-and-kill-thing    "id-select"
  "Kill syntactical region selection" t)
(autoload 'id-select-and-copy-thing    "id-select"
  "Select and copy syntactical region" t)
(autoload 'id-select-double-click-hook "id-select"
  "Double mouse click syntactical region selection" nil)
(autoload 'id-select-thing             "id-select"
  "Keyboard-driven syntactical region selection" t)
(autoload 'id-select-thing-with-mouse  "id-select"
  "Single mouse click syntactical region selection" t)

(autoload 'todo-mode "todo-mode" t)

(autoload 'w3-preview-this-buffer "w3" "WWW Previewer" t)
(autoload 'w3 "w3" "WWW Browser" t)
(autoload 'w3-open-local "w3" "Open local file for WWW browsing" t)
(autoload 'w3-fetch "w3" "Open remote file for WWW browsing" t)
(autoload 'w3-use-hotlist "w3" "Use shortcuts to view WWW docs" t)

(autoload 'cltl2-view-function-definition "browse-cltl2")
(autoload 'cltl2-view-index "browse-cltl2")
(autoload 'cltl2-lisp-mode-install "browse-cltl2")

(autoload 'htmlize-buffer "htmlize" "HTML-ify a lisp file" t)

(autoload 'sc-cite-original "supercite" "Supercite 3.1" t)

(autoload 'gdb "gud"
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'sdb "gud"
  "Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'dbx "gud"
  "Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'xdb "gud"
  "Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory." t nil)

(autoload 'perldb "gud"
  "Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
