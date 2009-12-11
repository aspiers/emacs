;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;


;; XEmacs adds crap to emacs-version
(defvar emacs-version-number
  (format "%d.%d" emacs-major-version emacs-minor-version)
  "emacs major.minor version number")

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

(defvar as-emacs-dir
  (concat edotdir "/lib/emacs")
  "Path to root of emacs libraries, regardless of emacs vendor.")

(defvar as-lib-dir
  (concat as-emacs-dir "/"
          (cond ((boundp 'running-xemacs) "XEmacs") (t "GNU_Emacs")))
  "Path to emacs libraries for a specific emacs vendor.")

(defvar as-init-dir
  (concat as-emacs-dir "/init/"
          (cond ((boundp 'running-xemacs) "XEmacs") (t "GNU_Emacs")))
  "Path to emacs init libraries for a specific emacs vendor.")

(defvar as-version-pre-lib-dir
  (format "%s/%s/%s/pre" as-lib-dir emacs-version-number system-type)
  "Path to personal emacs libraries which supplement those of a
particular system's emacs install.  They will be loaded in
preference to those from the system's emacs install, due to
appearing earlier on `load-path'.

It is recommended that only cutting edge versions of libraries
newer than those included in a distribution be placed under this
directory, and that the contents be reviewed every time the
system-wide emacs install is upgraded.")

(defvar as-version-post-lib-dir
  (format "%s/%s/%s/post" as-lib-dir emacs-version-number system-type)
  "Path to personal emacs libraries which supplement those of a
particular system's emacs install.  Libraries in the system's
emacs install will be loaded in preference to these, due to
appearing earlier on `load-path'.

Libraries which do not appear in older emacs installs can be
placed here.")

;; save original load-path - e.g. useful for finding site-lisp directory
(setq orig-load-path load-path)

(defvar find-function-source-path load-path
  "The default list of directories where `find-function' searches.

If this variable is nil then `find-function' searches `load-path' by
default.")

;; (require 'cl)
;; (setq org-source-paths
;;       (remove-if-not
;;        (lambda (dir) (file-directory-p dir))
;;        (directory-files (concat as-emacs-dir "/major-modes") 'full-paths "org[-.]")))
;; (mapc (lambda (dir) (add-to-list 'find-function-source-path dir))
;;       org-source-paths)

(mapc (lambda (x)
          (let ((path (concat as-emacs-dir "/" x)))
	    (message path)
            (and (file-directory-p path)
                 (add-to-list 'find-function-source-path path))))
	'(
	  "init/common"
	  "major-modes"
	  "major-modes/org-mode.git/lisp"
	  "major-modes/remember"
	  "major-modes/xtla"
	  "major-modes/mmm"
	  "major-modes/psgml"
	  "major-modes/muse"
	  "minor-modes"
	  "utils"
	  "fun"))

(add-to-list 'load-path as-version-pre-lib-dir)
(add-to-list 'load-path (concat as-version-post-lib-dir "/loaddefs") 'append-at-end)
(add-to-list 'load-path as-version-post-lib-dir 'append-at-end)

;; Add $ZDOTDIR/local/share/emacs/site-lisp and subdirs to load-path
(let ((dir (format "%s/local/share/emacs/site-lisp" edotdir))
      (orig-dir default-directory))
  (when (file-accessible-directory-p dir)
      (add-to-list 'load-path                 dir 'append-at-end)
      (add-to-list 'find-function-source-path dir 'append-at-end)
      (cd dir)
      (normal-top-level-add-subdirs-to-load-path)
      (cd orig-dir)))

(cond 
 ((boundp 'running-xemacs)
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))
;; This load is required, according to the info pages:
(load custom-file)

;; (when (getenv "EMACS_PROFILE_INIT")
;;   (load "elp")
;;   (elp-instrument-package "blah")
       
(load (concat as-version-post-lib-dir "/as-init"))

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

(mapcar (lambda (hook) (if (> (length hook) 0) (load hook)))
        ;; .emacs.d already taken
        (as-find-hooks ".emacs-hooks.d"))

;; Stop Red Hat trampling over my nice config :-(
(setq inhibit-default-init t)

(as-progress "end of ~/.emacs")
