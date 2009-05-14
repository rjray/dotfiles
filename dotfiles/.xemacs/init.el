(setq inhibit-startup-message t)
(setq options-save-faces t)
(setq fill-column 79)

;(setq homedir (getenv "HOME"))
(defconst homedir (getenv "HOME") "My home dir, regardless of host.")

(setq load-path (cons (concat homedir "/.elisp") load-path))
(setq load-path (cons (concat homedir "/.elisp/pd") load-path))

;; Options Menu Settings
;; =====================

;; ============================
;; End of Options Menu Settings

;(load "hsite")
(load "completer")
(load "e_init")
(load "autoloads")
(load "hooks")
(load "mode-list")
(load "utils")
(load "keys")
;(load "ctrl-l-map")

(gnuserv-start)

(put 'eval-expression 'disabled nil)


