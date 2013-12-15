(and window-system
     (global-set-key [(M-mouse-4)] 'raise-frame)
     (global-set-key [(M-mouse-5)] 'lower-frame))

(global-set-key [(control meta ??)]       'bn-make-region-into-secondary)
(global-set-key [(control meta T)]        'bn-exchange-region-and-secondary)
