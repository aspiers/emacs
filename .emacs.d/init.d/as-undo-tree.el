(require 'as-el-get)

(req-package undo-tree
  :init
  (global-undo-tree-mode))

(provide 'as-undo-tree)
