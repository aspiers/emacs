(add-hook 'lisp-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq comment-start ";; ")
            (bind-key "C-S-t" 'transpose-sexps lisp-mode-map)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq comment-start ";; ")
            (bind-key "C-S-t" 'transpose-sexps emacs-lisp-mode-map)))

(req-package paredit
  :commands enable-paredit-mode
  :init
  (dolist (mode '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  ielm-mode-hook
                  scheme-mode-hook
                  lisp-interaction-mode-hook
                  eval-expression-minibuffer-setup-hook))
    (add-hook mode 'enable-paredit-mode)))

(req-package macrostep
  :defer t
  :commands macrostep-expand
  :bind (:map emacs-lisp-mode-map
              (("C-c e" . macrostep-expand))))

;; From https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key "C-x C-S-e" 'eval-and-replace)

(provide 'as-lisp)
