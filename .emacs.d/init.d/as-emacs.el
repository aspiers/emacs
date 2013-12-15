;; Stuff relating to use of emacs

(global-set-key [(meta x)]   'smex)
(global-set-key [(meta X)]   'smex-major-mode-commands)
(global-set-key [(control h) a] 'apropos)            ;; was apropos-command

(autoload 'set-any-variable "set-any-var" "set-any-variable" t)
(global-set-key "\C-cv"   'set-any-variable)
(global-set-key "\C-cV"   'customize-variable)
(global-set-key [(control meta return)]   'repeat-complex-command)
(global-set-key [(control c) .]           'repeat)
(global-set-key "\C-cF"   'font-lock-fontify-buffer)
(global-set-key [(control g)]             'bn-keyboard-quit)

(provide 'as-emacs)
