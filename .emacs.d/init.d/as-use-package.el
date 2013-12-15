;; Just syntactic sugar for packages which want to mark a dependency
;; on use-package without necessarily depending on an el-get package.
;; Currently we load use-package via el-get, so they necessarily come
;; hand in hand, but in the future we might want to obtain use-package
;; via a different mechanism, in which case we'd only need to change
;; this file, rather than removing (require 'as-el-get) from all packages
;; which need use-package but not el-get.

(eval-and-compile (as-loading-started))

(require 'as-el-get)

;; This is currently redundant because as-el-get loads it
;; automatically.
(require 'use-package)

(provide 'as-use-package)
(eval-and-compile (as-loading-done))
