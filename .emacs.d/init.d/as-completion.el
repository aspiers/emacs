;; See also as-templating

(bind-key "M-<tab>" 'hippie-expand)

(req-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(req-package flx-ido)

(req-package company
  :config

  ;; fci-mode breaks company :-(
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/54
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    "Safely turn off Fill Column Indicator.
If `fci-mode' is enabled disable it and store its state in special variable.
Argument IGNORE is not used"
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    "Turn on Fill Column Indicator if it was enabled.
If `fci-mode' was enabled turn it on.
Argument IGNORE is not used."
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

;; (req-package auto-complete-css)
;; (req-package auto-complete-emacs-lisp)
;; (req-package auto-complete-ruby)
;; (req-package auto-complete-yasnippet)

(provide 'as-completion)
