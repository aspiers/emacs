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

(use-package navi-mode)
(use-package helm-navi
  :bind ("C-x c n" . helm-navi)
  :config
  (setf (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
        "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? "))

;; Nice idea, but causes "Buffer XXX.el was not set up for parsing".
;; Supposedly M-x semantic-mode should set it up, but
;; `semantic-new-buffer-fcn' bails due to `semantic--parse-table' not
;; being set.  alphapapa says that Semantic is known to be a somewhat
;; unruly package.
;;
;; (use-feature semantic
;;   :config
;;   :hook (emacs-lisp-mode . semantic-stickyfunc-mode))

(use-package lispy
  :diminish lispy-mode
  :hook (emacs-lisp-mode . lispy-mode)
  :bind (
         :map lispy-mode-map-base
         ;; We want M-o for the usual stuff, and anyway we have C-M-b
         ;; bound to lispy-backward.  Unfortunately this base gets
         ;; copied to some of the other maps on package load, so we
         ;; have to undefine it in those too.
         ("M-o" . nil)

         :map lispy-mode-map-lispy
         ("M-o" . nil)
         ("C-," . nil)

         :map lispy-mode-map-paredit
         ;; We want M-s for the usual stuff, and anyway we have /
         ;; bound to special-lispy-splice
         ("M-o" . nil)
         ("M-s" . nil)

         :map lispy-mode-map-c-digits
         ("C-5" . lispy-show-top-level))

  :hook (eval-expression-minibuffer-setup . lispy-mode)
  :config
  ;; See https://github.com/abo-abo/lispy/issues/557
  (lispy-set-key-theme '(special paredit lispy c-digits)))

(use-package paredit
  :commands enable-paredit-mode
  :init
  ;; (dolist (mode '(emacs-lisp-mode-hook
  ;;                 lisp-mode-hook
  ;;                 ielm-mode-hook
  ;;                 scheme-mode-hook
  ;;                 lisp-interaction-mode-hook
  ;;                 eval-expression-minibuffer-setup-hook))
  ;;   (add-hook mode 'enable-paredit-mode))

  :config
  (defun as-paredit-top ()
    "Move to top-level."
    (interactive)
    (condition-case nil
        (paredit-up/down -100 +1)
      (error nil)))

  :bind ("C-M-S-u" . as-paredit-top))

(use-package macrostep
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

;; See also lispy-eval-and-replace
(bind-key "C-x C-S-e" 'eval-and-replace)

(use-package flycheck-package)

(use-package describe-hash
  :defer 20
  :bind ("C-h H" . describe-hash))

(use-package ielm
  :bind (:map ielm-map
              ;; This is suggested in the docstring for
              ;; `ielm-prompt-read-only'.  Why on earth didn't they
              ;; just implement it by default?
              ("C-k" . comint-kill-whole-line)
              ("C-w" . comint-kill-region)))

;; For Common Lisp
;; (use-package slime)

;; FIXME: get this in MELPA
(use-package etrace
  :straight (etrace :host github :repo "aspiers/etrace"))

(use-package ert
  :config
  (defun ert-eval-run-test-at-point ()
    "Calls `eval-defun' and runs the resulting test interactively."
    (interactive)
    (let ((test (call-interactively #'eval-defun nil)))
      (ert-run-tests-interactively test)))

  :bind ("C-M-S-x" . ert-eval-run-test-at-point))

(use-package ppp
  :defer t)

(use-package elmacro)

(provide 'as-lisp)
