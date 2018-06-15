;; See as-comms.el for setup related to general communication

;; messages-are-flowing is currently useless outside gnus:
;; https://github.com/legoscia/messages-are-flowing/issues/2#issuecomment-333989987
;;
;; But even if it was, it's really annoying to continually reprompt
;; https://github.com/legoscia/messages-are-flowing/issues/6
;; (req-package messages-are-flowing
;;   :config
;;   (add-hook 'message-mode-hook
;;             'messages-are-flowing-use-and-mark-hard-newlines))

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")
(add-to-list 'auto-mode-alist '("/mutt-\\|itsalltext.*mail\\.google" . message-mode))
(eval-when-compile (require 'sendmail))
(add-hook
 'mail-mode-hook
 (lambda ()
   (turn-on-auto-fill)
   (as-setup-mode-for-discussion)
   (as-set-local-server-edit-keys)))

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

(provide 'as-mail)
