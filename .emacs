;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

(cond 
 ((boundp 'running-xemacs)
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

;; (when (getenv "EMACS_PROFILE_INIT")
;;   (load "elp")
;;   (elp-instrument-package "blah")

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

(as-progress (format "loading %s ..." custom-file))
;; This load is required, according to the info pages:
(load custom-file)
(as-progress (format "loaded %s" custom-file))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(when
    (or (load "package")
        (load (expand-file-name (concat edotdir "/.emacs.d/elpa/package.el"))))
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")))

(as-progress "end of ~/.emacs")
