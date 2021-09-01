;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)))

(provide 'as-lsp)
