;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;


;; Make sure running-xemacs exists for testing

(if (not (boundp 'running-xemacs))
    (defvar running-xemacs nil "non-nil if the current emacs is an XEmacs"))


;; Set load-path and load in all options from relevant places

(setq load-path
      (append '("~/local/share/emacs/site-lisp"
                "~/lib/emacs"
                "~/lib/emacs/init") load-path))

(cond 
 (running-xemacs
  (setq load-path (append '("~/lib/emacs/XEmacs") load-path))

  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file "~/lib/emacs/init/XEmacs/options-init.el")
  (setq save-options-file "~/lib/emacs/init/XEmacs/options.el")
  (load save-options-file t)

  (setq custom-file "~/lib/emacs/init/XEmacs/custom.el")
  (load custom-file t)

  (load "~/lib/emacs/init/common/XEmacs" t)
  )
 (t
  (setq load-path (append '("~/lib/emacs/GNU_Emacs") load-path))

  (setq custom-file "~/lib/emacs/init/GNU_Emacs/custom.el")
  (load custom-file t)

  (load "~/lib/emacs/init/common/emacs" t)
  ))


(load "~/.emacs.local" t)
