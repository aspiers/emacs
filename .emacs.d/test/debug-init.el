;; Usage: emacs -Q -l ~/.emacs.d/test/debug-init.el
;;
;; in preference to emacs --debug-init

(setq debug-on-error t)
(setq truncate-lines nil)
(require 'edebug)
(switch-to-buffer "*Messages*")
(setq truncate-lines nil)

