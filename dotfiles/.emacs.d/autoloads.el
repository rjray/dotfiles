;;; Various auto-load commands to associate function names and mode names with
;;; specific files

(autoload 'prove "prove" "Perl prove" t)
(autoload 'ack "ack" "Perl ack" t)

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