(use-package undo-tree
  :init
  (global-undo-tree-mode)

  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t))

(provide 'as-undo-tree)
