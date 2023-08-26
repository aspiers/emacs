(require 'paren)

(use-package smartparens
  :custom
  (sp-ignore-modes-list
   '(minibuffer-mode
     minibuffer-inactive-mode
     emacs-lisp-mode))

  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'as-parens)
