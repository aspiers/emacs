;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; treesit.el support for ts-fold is still WIP:
;; https://github.com/emacs-tree-sitter/ts-fold/issues/48
;;
;; Experimental fork which doesn't do anything for me:
(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo
                          "abougouffa/treesit-fold"))
;; TODO: reuse old folding-mode bindings defined in as-outlining.el

;; From emacs 29, these are superseded by treesit.el, as documented
;; at the top of https://emacs-tree-sitter.github.io/
;;
;; (use-package tree-sitter
;;   :diminish
;;   :hook ((typescript-mode . tree-sitter-hl-mode)
;;          (typescript-tsx-mode . tree-sitter-hl-mode)))
;;
;; (use-package tree-sitter-langs
;;   :after tree-sitter
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;                '(typescript-tsx-mode . tsx)))
;;
;; ;; Only useful for Rust so far
;; (use-package tree-sitter-indent
;;   :after tree-sitter)

(provide 'as-treesit)
