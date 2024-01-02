;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Workaround for https://github.com/renzmann/treesit-auto/issues/76
  (setq major-mode-remap-alist (treesit-auto--build-major-mode-remap-alist))

  (global-treesit-auto-mode))

;; From https://github.com/mickeynp/combobulate
;; (use-package treesit
;;   :preface
;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (typescript-mode . tsx-ts-mode)
;;                      (json-mode . json-ts-mode)
;;                      (js-mode . js-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (yaml-mode . yaml-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping)))

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

;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
;;
;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET
;;
(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; See also https://github.com/haritkapadia/ts-movement
;; but it's extremely limited in comparison to combobulate.

(provide 'as-treesit)
