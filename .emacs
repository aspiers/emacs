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
(setq emacs-version-number
      (format "%d.%d" emacs-major-version emacs-minor-version))

(setq as-lib-dir
      (concat "~/lib/emacs/"
              (cond (running-xemacs "XEmacs") (t "GNU_Emacs"))))
(setq as-init-dir
      (concat "~/lib/emacs/init/"
              (cond (running-xemacs "XEmacs") (t "GNU_Emacs"))))
(setq as-version-lib-dir (concat as-lib-dir "/" emacs-version-number))
(add-to-list 'load-path as-version-lib-dir)

(cond 
 (running-xemacs
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (concat as-init-dir "/as-custom.el"))
(load (concat as-init-dir "/as-custom") 'noerror)
(load (concat as-version-lib-dir "/as-init"))
(load "~/.emacs.local" 'noerror)
