;; Reasons for loading this before el-get packages:
;;
;;   * org-disputed-keys and org-replace-disputed-keys won't work
;;     unless set before org.el is loaded for the first time
(require 'as-el-get)
(require 'as-vars)

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

(as-progress (format "loading %s ..." custom-file))
;; This load is required, according to the info pages:
(load custom-file)
(as-progress (format "loaded %s" custom-file))

(provide 'as-custom)
