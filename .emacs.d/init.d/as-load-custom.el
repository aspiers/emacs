;; Need to load this before org-mode packages:
;;
;;   * org-disputed-keys and org-replace-disputed-keys won't work
;;     unless set before org.el is loaded for the first time

(require 'as-vars)
(require 'as-make-backup-file-name nil 'noerror) ;; need as-make-backup-file-name

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

;; FIXME: this seems misplaced
(unless (fboundp 'as-make-backup-file-name)
  (if (eq 'as-make-backup-file-name make-backup-file-name-function)
      (setq make-backup-file-name-function nil)))

(as-progress "loading %s ..." custom-file)
;; This load is required, according to the info pages:
(load custom-file)
(as-progress "loaded %s" custom-file)

(provide 'as-load-custom)
