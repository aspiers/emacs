;; Only an ancient jb-misc-macros is available via marmalade,
;; and it's missing a dependency on anaphora
(req-package jb-misc-macros
  :loader el-get)

(req-package kmacro-decision
  :requires jb-misc-macros)

(provide 'as-kmacros)
