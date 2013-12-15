(eval-and-compile (as-loading-started))

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

;; XEmacs adds crap to emacs-version
(defvar emacs-version-number
  (format "%d.%d" emacs-major-version emacs-minor-version)
  "emacs major.minor version number")

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

(provide 'as-vars)
(eval-and-compile (as-loading-done))
