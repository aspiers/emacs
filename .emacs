;; -*- Mode: Emacs-Lisp -*-
;;
;; GNU emacs startup file
;; Adam Spiers
;;

;; Stop Red Hat trampling over my nice config :-(
(setq inhibit-default-init t)

(defvar as-init-d-suffix ".emacs.d/init.d")
(load (concat (file-name-directory load-file-name) as-init-d-suffix "/as-pre-init-d"))

(require 'cl) ;; for remove-if-not
(defun as-find-hooks (hook-name)
  "Uses $ZDOT_FIND_HOOKS to return a list of hooks for `hook-name'."
  (let* ((lines (split-string
                 (shell-command-to-string (concat ". $ZDOT_FIND_HOOKS " hook-name))
                 "\n"
                 'omit-nulls))
         (files (remove-if-not (lambda (file) (string-match "\\.el\\'" file)) lines)))
    (mapcar
     ;; trim .el from end to allow `load' to use byte-compiled form
     (lambda (file)
       (if (string-match "\\.el\\'" file)
           (replace-match "" nil t file)
         file))
         files)))

(require 'as-progress)

(defun as-load-hooks (hook-name)
  "Load hooks found by `as-find-hooks'."
  (dolist (hook (as-find-hooks hook-name))
    (as-progress "loading %s... " (abbreviate-file-name hook))
    (load hook)
    (as-progress "loading %s... done" (abbreviate-file-name hook))))

(as-load-hooks as-init-d-suffix)

(as-progress "end of ~/.emacs")
