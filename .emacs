;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;


;; Make sure running-xemacs exists for testing

(if (not (boundp 'running-xemacs))
    (defvar running-xemacs nil "non-nil if the current emacs is an XEmacs"))

;; N.B. Personal .elc archive gets added to start of load-path,
;; so that right version of stuff like mwheel gets loaded.

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
          (cond (running-xemacs "XEmacs") (t "GNU_Emacs")))
  "Path to emacs libraries for a specific emacs vendor.")

(defvar as-init-dir
  (concat as-emacs-dir "/init/"
          (cond (running-xemacs "XEmacs") (t "GNU_Emacs")))
  "Path to emacs init libraries for a specific emacs vendor.")

(defvar as-version-lib-dir
  (format "%s/%s/%s" as-lib-dir emacs-version-number system-type)
  "Path to emacs libraries for a particular system's emacs install.")

;; save original load-path - e.g. useful for finding site-lisp directory
(setq orig-load-path load-path)

(add-to-list 'load-path as-version-lib-dir)

(let ((dir (format "%s/local/share/emacs/site-lisp" edotdir))
      (orig-dir default-directory))
  (when (file-accessible-directory-p dir)
      (add-to-list 'load-path dir)
      (cd dir)
      (normal-top-level-add-subdirs-to-load-path)
      (cd orig-dir)))

(cond 
 (running-xemacs
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (concat as-init-dir "/as-custom.el"))
(load (concat as-init-dir "/as-custom") 'noerror)
(load (concat as-version-lib-dir "/as-init"))
(load (concat edotdir "/.emacs.local") 'noerror)
