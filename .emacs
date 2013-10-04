;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

(as-progress "loading as-init ...")

(load (concat as-version-post-lib-dir "/as-init"))

(as-progress "loaded as-init")

(defun as-find-hooks (hook-name)
  "Uses $ZDOT_FIND_HOOKS to return a list of hooks for `hook-name'."
  (let ((lines (split-string
                (shell-command-to-string (concat ". $ZDOT_FIND_HOOKS " hook-name))
                "\n"
                'omit-nulls)))
    (mapcar
     ;; trim .el from end to allow `load' to use byte-compiled form
     (lambda (file)
       (if (string-match "\\.el\\'" file)
           (replace-match "" nil t file)
         file))
     lines)))

(as-progress "running .emacs-hooks.d ...")

(mapcar (lambda (hook) (if (> (length hook) 0) (load hook)))
        ;; .emacs.d already taken
        (as-find-hooks ".emacs-hooks.d"))

(as-progress "ran .emacs-hooks.d")

;; Stop Red Hat trampling over my nice config :-(
(setq inhibit-default-init t)


(as-progress "end of ~/.emacs")
