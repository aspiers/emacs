;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;


;; Include private stuff on load path

(setq load-path (append '("~/lib/emacs" "~/lib/emacs/init") load-path))


;; Make sure running-xemacs exists for testing

(if (not (boundp 'running-xemacs))
    (setq running-xemacs nil))


;; Load in all options from relevant places

(cond 
 (running-xemacs
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file "~/lib/emacs/init/XEmacs/options-init.el")
  (setq save-options-file "~/lib/emacs/init/XEmacs/options.el")
  (load save-options-file t)

  (setq custom-file "~/lib/emacs/init/XEmacs/custom.el")
  (load custom-file t)

  (load "~/lib/emacs/init/common/XEmacs" t)
  )
 (t
  (setq custom-file "~/lib/emacs/init/GNU_Emacs/custom.el")
  (load custom-file t)

  (load "~/lib/emacs/init/common/emacs" t)
  ))


(load "~/.emacs.local" t)
