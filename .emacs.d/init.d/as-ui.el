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
;;{{{ ido - superior replacement for iswitchb

(require 'ido)
(ido-mode t)

;;}}}
;;{{{ Load paren library

(require 'paren)

;;}}}

(use-package color-theme)
(use-package color-theme-pastels-on-dark
  ;; FIXME: use req-package
  :if (featurep 'color-theme)
  :config
  (color-theme-pastels-on-dark))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(bind-key "C-'" 'speedbar-get-focus)

(use-package beacon
  :config (beacon-mode 1))

(provide 'as-ui)
