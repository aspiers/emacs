;; IntelliMouse
(cond ((and (< emacs-major-version 22) window-system)
       (load "mwheel" t)
       (mwheel-install)))

;; This is presumably only useful when my normal window manager
;; mouse bindings and/or Meta key binding are not in effect.
(when window-system
  (bind-key "M-mouse-4" 'lower-frame)
  (bind-key "M-mouse-5" 'raise-frame))

(bind-key "C-S-<mouse-1>"      'mouse-start-secondary)
(bind-key "C-S-<drag-mouse-1>" 'mouse-set-secondary)
(bind-key "C-S-<down-mouse-1>" 'mouse-drag-secondary)
(bind-key "C-S-<mouse-3>"      'mouse-secondary-save-then-kill)
(bind-key "C-S-<mouse-2>"      'mouse-yank-secondary)
(bind-key "C-M-?"              'bn-make-region-into-secondary)
(bind-key "C-M-T"              'bn-exchange-region-and-secondary)
