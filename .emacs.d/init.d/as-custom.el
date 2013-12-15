;; Reasons for loading this before el-get packages:
;;
;;   * org-disputed-keys and org-replace-disputed-keys won't work
;;     unless set before org.el is loaded for the first time
(require 'as-el-get)
(require 'as-vars)

(cond
 ((boundp 'running-xemacs)
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

(as-progress (format "loading %s ..." custom-file))
;; This load is required, according to the info pages:
(load custom-file)
(as-progress (format "loaded %s" custom-file))

(provide 'as-custom)
