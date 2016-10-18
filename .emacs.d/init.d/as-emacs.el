;; Stuff relating to generic use of emacs in any context

(bind-key "C-h a" 'apropos)

(req-package set-any-var
  :commands set-any-variable
  :bind ("C-c v" . set-any-variable))

(bind-key "C-c V"    'customize-variable)

(bind-key "C-M-<return>" 'repeat-complex-command)
(bind-key "C-c ."        'repeat)

(bind-key "C-c F"        'font-lock-fontify-buffer)

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-emacs-init-d
  "~/.emacs.d/init.d" "Find emacs init.d file: ")
(bind-key "C-c j e" 'as-find-emacs-init-d)

(define-find-file-in-dir-function as-find-elpa-package
  "~/.emacs.d/elpa" "Find ELPA package: ")
(bind-key "C-c j l" 'as-find-elpa-package)

(define-find-file-in-dir-function as-find-el-get-package
  "~/.el-get" "Find el-get package: ")
(bind-key "C-c j L" 'as-find-el-get-package)

(bind-key "C-h B"   'describe-personal-keybindings)

;; I don't visit the FAQ very often; find-function way more useful.
(bind-key "C-h C-f" 'find-function)
(bind-key "C-h C-S-f" 'view-emacs-FAQ)

;; Similar pattern for variables
(bind-key "C-h C-v" 'find-variable)

(bind-key "C-h C-k" 'find-function-on-key)

(defun as-kbd-to-string ()
  "Prompt to enter a key sequence, and display the string
required to bind that sequence via `bind-key', `global-set-key'
etc."
  (interactive)
  (message (key-description (read-key-sequence "Enter key chord: "))))

(bind-key "C-h C-c" 'as-kbd-to-string)

(provide 'as-emacs)
