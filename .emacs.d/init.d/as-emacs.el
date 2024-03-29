;; Stuff relating to generic use of emacs in any context

(bind-key "C-h a" 'apropos)

(use-feature set-any-var
  :commands set-any-variable
  :bind
  ("C-c v" . set-any-variable)
  ("C-c E" . set-env-variable)
  :config
  (add-hook 'set-any-variable-minibuffer-setup-hook
            #'lispy-mode))

(bind-key "C-c V"    'customize-variable)

(bind-key "C-M-<return>" 'repeat-complex-command)
(bind-key "C-c ."        'repeat)

(bind-key "C-c F"        'font-lock-fontify-buffer)

(require 'find-file-in-dir)
(define-find-file-in-dir-function as-find-emacs-init-d
  "~/.emacs.d/init.d" "Find emacs init.d file: ")

;; See as-jump.el / as-package-loading.el for explanation of usage
(use-feature as-jump
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("e" ".emacs.d/init.d/" . as-find-emacs-init-d)))

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
  (message (key-description
            (read-key-sequence "Enter key sequence or chord: "))))

(bind-key "C-h C-c" 'as-kbd-to-string)

(provide 'as-emacs)
