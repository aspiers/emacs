;; IntelliMouse
(req-package mwheel
  :config
  (mwheel-install))

;; This is presumably only useful when my normal window manager
;; mouse bindings and/or Meta key binding are not in effect.
(when window-system
  (bind-key "M-<mouse-4>" 'lower-frame)
  (bind-key "M-<mouse-5>" 'raise-frame))

(bind-key "C-S-<mouse-1>"      'mouse-start-secondary)
(bind-key "C-S-<drag-mouse-1>" 'mouse-set-secondary)
(bind-key "C-S-<down-mouse-1>" 'mouse-drag-secondary)
(bind-key "C-S-<mouse-3>"      'mouse-secondary-save-then-kill)
(bind-key "C-S-<mouse-2>"      'mouse-yank-secondary)

(req-package bn-secondary-region
  :bind (("C-M-?" . bn-make-region-into-secondary)
         ("C-M-T" . bn-exchange-region-and-secondary)
         ("C-g"   . bn-keyboard-quit)))

(provide 'as-mouse)
