;; See as-comms.el for setup related to general communication

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

(provide 'as-mail)
