;; This minimal test case shows how (package-initialize) sets up
;; autoloads for all installed packages without further effort.
;;
;; Usage:
;;   emacs -Q --debug-init -l ~/.emacs.d/test/package-autoloads.el

(package-initialize)


;; (require 'use-package)
;; (setq use-package-verbose 'debug)

(add-to-list 'load-path "/home/adam/.emacs.d/lib")
(add-to-list 'load-path "/home/adam/.emacs.d/init.d")

(setq which-key-enable-extended-define-key t)
(require 'which-key)
(setq which-key-idle-delay 0.1)
(setq which-key-idle-secondary-delay 0.1)
;; FIXME: This one doesn't work >-(
(setq which-key-lighter "")

(defvar as-jump-map (make-sparse-keymap "Jump to"))
(message "l %s" (featurep 'which-key))
(define-key as-jump-map "M"
  '("Switch to *Messages*" . switch-to-messages-buffer))
(bind-key "C-c j" as-jump-map)

(which-key-mode)
(switch-to-buffer "*Messages*")
(describe-function 'define-key)
