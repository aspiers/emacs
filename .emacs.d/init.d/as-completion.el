;; See also as-templating

(bind-key "M-<tab>" 'hippie-expand)

(require 'as-mode-lighters)

(use-package company
  :diminish company-mode
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

  :hook ((company-completion-started . company-turn-off-fci)
         (company-completion-finished . company-maybe-turn-on-fci)
         (company-completion-cancelled . company-maybe-turn-on-fci)
         (prog-mode . company-mode)))

;; (use-package auto-complete-css)
;; (use-package auto-complete-emacs-lisp)
;; (use-package auto-complete-ruby)
;; (use-package auto-complete-yasnippet)

(provide 'as-completion)
