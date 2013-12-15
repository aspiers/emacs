;; Needs to be done early, because we load a crap-ton of stuff before
;; as-custom.
(setq message-log-max 1000)

(defun as-quick-startup nil
  "Non-nil if the current emacs was required to start up quickly."
  (getenv "QUICK_EMACS"))

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

(defvar as-init-d
  (cond (load-file-name
         (file-name-directory load-file-name))
        (as-init-d-suffix
         (concat edotdir "/" as-init-d-suffix))
        (t
         (error "Neither `load-file-name' nor `as-init-d-suffix' set")))
  "Directory containing Adam's emacs startup files.")

(add-to-list 'load-path as-init-d)

(require 'as-load-paths)
(require 'as-progress)

;; use-package is common enough that we don't want to have to require
;; it in each file which uses it.
(require 'as-use-package)

(as-loading-done)

(provide 'as-pre-init-d)
