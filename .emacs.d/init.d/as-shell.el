;; Autoload sh-script on invocation
(autoload 'shell-script-mode "sh-script"
  "Major mode for editing shell scripts" t)

(autoload 'sh-ins-template "as-sh-mode-exts"
  "Template for new shell script texts" t)
(autoload 'make-buffer-file-executable-if-script-p "as-sh-mode-exts"
  "Make shell scripts executable" t)
(add-hook 'sh-mode-hook 'sh-ins-template)

;; This can accidentally change permissions in git repos, for instance.
;;(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(global-set-key "\C-cmx" 'make-buffer-file-executable)

(add-to-list 'auto-mode-alist
             '("/\\.zsh\\(env\\|rc\\|/functions/\\)\\|\\.stp$" . sh-mode))

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'shell-script-mode-hook (lambda () (setq comment-start "#")))

(defun sm () "Abbreviation for `sm-mode'." (interactive) (sh-mode))

(provide 'as-shell)