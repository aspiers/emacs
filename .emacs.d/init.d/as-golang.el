(use-package go-mode)

(with-packages (go-mode lsp-mode)
  :hook (go-mode . lsp-deferred)
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; suggests:
;;
;;  (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-gopath)

(provide 'as-golang)
