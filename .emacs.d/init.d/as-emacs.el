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

(defun as-find-emacs-init-d ()
  (interactive)
  "Load file in ~/.emacs.d/init.d"
  (ido-file-internal ido-default-file-method
          nil "~/.emacs.d/init.d" "Find emacs init.d file: "))
(bind-key "C-c j e" 'as-find-emacs-init-d)

(defun as-find-el-get-package ()
  (interactive)
  "Load file in ~/.el-get"
  (ido-file-internal ido-default-file-method
          nil "~/.el-get" "Find emacs init.d file: "))
(bind-key "C-c j l" 'as-find-el-get-package)

(provide 'as-emacs)
