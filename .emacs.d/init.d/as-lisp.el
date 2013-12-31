(add-hook 'lisp-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

(add-hook 'lisp-mode-hook       (lambda () (setq comment-start ";; ")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; ")))

(use-package paredit
  :commands enable-paredit-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)))

(use-package macrostep
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (define-key emacs-lisp-mode-map
                (kbd "C-c e") 'macrostep-expand))))

(provide 'as-lisp)
