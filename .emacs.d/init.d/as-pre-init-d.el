;; Needs to be done early, because we load a crap-ton of stuff before
;; as-custom.
(setq message-log-max 1000)

(defun as-quick-startup nil
  "Non-nil if the current emacs was required to start up quickly."
  (getenv "QUICK_EMACS"))

(require 'as-load-paths)
(require 'as-progress)

(as-progress "loaded %s" load-file-name)

(provide 'as-pre-init-d)
