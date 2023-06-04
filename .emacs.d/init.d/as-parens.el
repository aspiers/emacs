(require 'paren)

(use-package smartparens
  :config
  (setq sp-ignore-modes-list
        '(minibuffer-mode
          minibuffer-inactive-mode
          emacs-lisp-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'as-parens)
