;; See also as-templating

(bind-key "M-<tab>" 'hippie-expand)

(req-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(req-package flx-ido)

(req-package company-mode)

;; (req-package auto-complete-css)
;; (req-package auto-complete-emacs-lisp)
;; (req-package auto-complete-ruby)
;; (req-package auto-complete-yasnippet)

(provide 'as-completion)
