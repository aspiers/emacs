;; Reasons for loading this before el-get packages:
;;
;;   * org-disputed-keys and org-replace-disputed-keys won't work
;;     unless set before org.el is loaded for the first time
(require 'as-el-get)
(require 'as-vars)
(require 'as-bufs-files nil 'noerror) ;; need as-make-backup-file-name

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

(as-progress (format "loading %s ..." custom-file))
;; This load is required, according to the info pages:
(load custom-file)
(as-progress (format "loaded %s" custom-file))

(unless (fboundp 'as-make-backup-file-name)
  (if (eq 'as-make-backup-file-name make-backup-file-name-function)
      (setq make-backup-file-name-function nil)))

(provide 'as-custom)
