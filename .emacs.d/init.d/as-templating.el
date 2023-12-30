(require 'as-mode-lighters)

(use-package yasnippet
  :commands yas-minor-mode
  :diminish yas-minor-mode
  :config
  (defalias 'yamm 'yas-minor-mode)
  (defalias 'yagm 'yas-global-mode)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package yasnippet-treesitter-shim
  :straight (:host github :repo "fbrosda/yasnippet-treesitter-shim"
                   :files ("snippets/*"))
  :no-require t
  :config
  (add-to-list 'yas-snippet-dirs
               (straight--build-dir "yasnippet-treesitter-shim")))

;;{{{ _I_nsert auto-text (C-c i)

(use-feature as-autotext
  :bind (("C-c i d" . as-insert-date-and-time)
         ("C-c i D" . as-insert-date-interactive)
         ("C-c i e" . as-insert-email-address)
         ("C-c i m" . as-insert-local-mode)
         ("C-c i w" . as-insert-work-email-address)
         ("C-c i W" . as-insert-name-and-work-email)
         ("C-c i j" . as-insert-japh-method-chain-sig)
         ("C-c i J" . as-insert-japh-indirect-sig)
         ("C-c i l" . as-insert-log-timestamp)
         ("C-c i L" . as-insert-log-datestamp)
         ("C-c i N" . as-insert-name-and-email)
         ("C-c i n" . as-insert-name)
         ("C-c i r" . as-insert-rpm-changelog-datestamp)
         ("C-c i s" . as-insert-scissors)
         ("C-c i S" . as-snip-region)
         ("C-c i t" . as-insert-time)))

;;}}}

(provide 'as-templating)
