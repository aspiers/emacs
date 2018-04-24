;; Provide a way for packages to mark a dependency on req-package and
;; its dependencies (such as use-package and its plugins) without
;; having to know how they are provided.  This lets us install them
;; from ELPA or el-get or elsewhere without having to change anything
;; other than this file.
;;
;; Since req-package and its dependencies are used so commonly, we
;; only load this file via as-pre-init-d.

(eval-and-compile (as-loading-started))

(require 'as-elpa)

;; req-package-log-level only takes effect if defined before req-package
;; is loaded.
;; https://github.com/edvorg/req-package/issues/33#issuecomment-211359690
(require 'as-custom)

(require-elpa-packages
 'req-package
 'use-package-ensure-system-package)

(provide 'as-req-package)
(eval-and-compile (as-loading-done))
