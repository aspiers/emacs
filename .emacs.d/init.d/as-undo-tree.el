(require 'as-el-get)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region t))

(provide 'as-undo-tree)
