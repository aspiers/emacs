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

(use-package paredit
  :commands enable-paredit-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)))

(use-package macrostep
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (define-key emacs-lisp-mode-map
                (kbd "C-c e") 'macrostep-expand))))

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

(bind-key "C-x C-E" 'eval-and-replace)

(provide 'as-lisp)
