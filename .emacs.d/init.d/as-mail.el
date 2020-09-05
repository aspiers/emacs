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


(use-feature flowed-text
  :bind ("M-s M-m" . format-as-flowed-text))

(use-package mutt-mode
  :mode ("/mutt-\\|itsalltext.*mail\\.google" . mail-mode)
  :config
  (require 'sendmail)
  :hook (mail-mode-hook
         . (lambda ()
             (turn-on-auto-fill)
             (as-setup-mode-for-discussion)
             (as-set-local-server-edit-keys))))

(provide 'as-mail)
