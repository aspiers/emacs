(use-package beads
  :straight (:type git :host github
                   :repo "ChristianTietze/beads.el"
                   :files ("lisp/*.el"))
  :bind ("C-S-b" . beads))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP
                                       ; tools

(provide 'as-ai)
