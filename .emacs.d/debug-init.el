;; For isolating issues with my config.
;; Use via:
;;
;;   emacs -q -l ~/.emacs.d/debug-init.el ~/test.org
;;
;; or similar

(setenv "DEBUG_EMACS_INIT" "t")
(load "~/.emacs")

;; Pick one of the .emacs.d/init.d modules to debug, e.g.
(as-load-hook "as-org-mode")

;; Or just put some config here to test:
