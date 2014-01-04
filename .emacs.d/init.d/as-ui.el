;;{{{ Time-stamp mode

(autoload 'time-stamp "time-stamp")
;;(time-stamp)
;;(setq time-stamp-format "------ %02d %03b %4y %2H%2M %2H%2M  : %u")

;;}}}
;;{{{ fill-column-indicator

(if (featurep 'fill-column-indicator)
    (dolist (hook '(c-mode-hook ruby-mode-hook shell-script-mode-hook
                    emacs-lisp-mode-hook python-mode-hook))
      (add-hook hook 'fci-mode)))

;;}}}
;;{{{ no toolbar

;; This is best done with X resources, otherwise you get funny
;; frame-resizing problems.  See ~/.Xresources/emacs.rdb.

;; (and window-system
;;      (>= emacs-major-version 21)
;;      (tool-bar-mode -1))

;; (setq default-frame-alist
;;       '((tool-bar-lines . 0)))

;;}}}
;;{{{ auto-complete-mode

(require 'auto-complete-config nil t)
(defun ac-config-default ())
(defvar ac-dictionary-directories)
(when (as-check-feature-loaded 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "/home/adam/.emacs.d/ac-dict")
  (ac-config-default))

;;}}}
;;{{{ ido - superior replacement for iswitchb

(require 'ido)
(ido-mode t)

;;}}}
;;{{{ Load paren library

(require 'paren)

;;}}}
(bind-key "C-'" 'speedbar-get-focus)
