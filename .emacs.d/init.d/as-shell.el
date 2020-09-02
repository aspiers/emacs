(autoload 'sh-ins-template "as-sh-mode-exts"
  "Template for new shell script texts" t)
(autoload 'make-buffer-file-executable-if-script-p "as-sh-mode-exts"
  "Make shell scripts executable" t)
(add-hook 'sh-mode-hook 'sh-ins-template)

(req-package flymake-shell)

;; This can accidentally change permissions in git repos, for instance.
;;(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(bind-key "C-c m x" 'make-buffer-file-executable)

(add-to-list 'auto-mode-alist
             '("/\\.zsh\\(env\\|rc\\|/functions/\\)\\|\\.stp$" . sh-mode))

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'shell-script-mode-hook (lambda () (setq comment-start "#")))

(defun sm () "Abbreviation for `sm-mode'." (interactive) (sh-mode))

(provide 'as-shell)
