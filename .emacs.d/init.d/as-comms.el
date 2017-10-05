;; Interaction / communication with other people

;;{{{ mutt

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")
(add-to-list 'auto-mode-alist '("/mutt-\\|itsalltext.*mail\\.google" . mail-mode))
(eval-when-compile (require 'sendmail))
(add-hook
 'mail-mode-hook
 (lambda ()
   (turn-on-auto-fill)
   (as-setup-mode-for-discussion)
   (as-set-local-server-edit-keys)))

;;}}}
;;{{{ crm114-mode

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

;;}}}
;;{{{ ispell

(bind-key "C-$"   'ispell-complete-word)
(bind-key "C-M-$" 'ispell-buffer)

;;}}}

(provide 'as-comms)
