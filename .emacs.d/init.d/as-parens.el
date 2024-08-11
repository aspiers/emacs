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

;; No integration with treesit.el:
;; https://github.com/Fanael/rainbow-delimiters/issues/81
;;
;; so produces unreliable results.
;;
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(provide 'as-parens)
