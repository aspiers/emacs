;; Provide a way for packages to mark a dependency on req-package
;; without having to know how it is provided.  This lets us install it
;; from ELPA or el-get or elsewhere without having to change anything
;; other than this file.
;;
;; Since req-package is used so commonly, we only load this file via
;; as-pre-init-d.

(eval-and-compile (as-loading-started))

(require 'as-elpa)

;; req-package-log-level only takes effect if defined before req-package
;; is loaded.
;; https://github.com/edvorg/req-package/issues/33#issuecomment-211359690
(require 'as-custom)

(require-elpa-package 'req-package)

(provide 'as-req-package)
(eval-and-compile (as-loading-done))
