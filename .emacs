;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;


;; Make sure running-xemacs exists for testing

(if (not (boundp 'running-xemacs))
    (defvar running-xemacs nil "non-nil if the current emacs is an XEmacs"))

(cond 
 (running-xemacs
  (add-to-list 'load-path "~/lib/emacs/XEmacs")

  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file "~/lib/emacs/init/XEmacs/as-options-init.el")
  (setq save-options-file "~/lib/emacs/init/XEmacs/as-options.el")
  (load "~/lib/emacs/init/XEmacs/as-options" 'noerror)

  (setq custom-file "~/lib/emacs/init/XEmacs/as-custom.el")
  (load "~/lib/emacs/init/XEmacs/as-custom" 'noerror)

  (load "~/lib/emacs/XEmacs/as-init")
  )
 (t
  (add-to-list 'load-path "~/lib/emacs/GNU_Emacs")

  (setq custom-file "~/lib/emacs/init/GNU_Emacs/as-custom.el")
  (load "~/lib/emacs/init/GNU_Emacs/as-custom" 'noerror)

  (load "~/lib/emacs/GNU_Emacs/as-init")
  ))

(load "~/.emacs.local" 'noerror)

