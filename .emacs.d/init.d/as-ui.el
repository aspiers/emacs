;; see also as-smart-mode-line.el

;;{{{ fill-column-indicator

(use-package fill-column-indicator
  :config
  (dolist (hook '(prog-mode-hook indented-text-mode))
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

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(bind-key "C-'" 'speedbar-get-focus)

(provide 'as-ui)
