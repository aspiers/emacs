;; IntelliMouse
(cond ((and (< emacs-major-version 22) window-system)
       (load "mwheel" t)
       (mwheel-install)))

;; This is presumably only useful when my normal window manager
;; mouse bindings and/or Meta key binding are not in effect.
(when window-system
  (bind-key "M-mouse-4" 'lower-frame)
  (bind-key "M-mouse-5" 'raise-frame))

(global-set-key [(control meta ??)]       'bn-make-region-into-secondary)
(global-set-key [(control meta T)]        'bn-exchange-region-and-secondary)
