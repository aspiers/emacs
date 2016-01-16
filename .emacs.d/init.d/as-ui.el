;; see also as-smart-mode-line.el

;;{{{ fill-column-indicator

(req-package fill-column-indicator
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

(req-package ido
  :commands (ido-mode)
  :config
  (ido-mode t))

;;}}}
;;{{{ parentheses

(require 'paren)

(req-package smartparens)

;;}}}

(req-package color-theme)
(req-package color-theme-pastels-on-dark
  ;; FIXME: use req-package
  :if (featurep 'color-theme)
  :config
  (color-theme-pastels-on-dark))

(req-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(bind-key "C-'" 'speedbar-get-focus)

(req-package beacon
  :config (beacon-mode 1))

(req-package flycheck)
(req-package flycheck-package)
(req-package flymake-css)
(req-package flymake-ruby)
(req-package flymake-sass)
(req-package flymake-shell)

(req-package hideshow-org)

(req-package ido-completing-read+)
(req-package idomenu) ;; http://emacsrocks.com/e10.html
;; ido-hacks
;; ido-everywhere
(req-package ido-ubiquitous)
(req-package ido-vertical-mode)


(provide 'as-ui)
