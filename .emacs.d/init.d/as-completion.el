;; See also as-templating

(bind-key "M-<tab>" 'hippie-expand)

(use-package ivy
  :after flx
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper)

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  )

(use-package counsel
  :after ivy
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  :bind (("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-!" . counsel-ibuffer)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h o" . counsel-describe-symbol)
         ("C-x M-f" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ;; ("<f2> u" . counsel-unicode-char)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ;; ("C-c k" . counsel-ag)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-S-o" . counsel-rhythmbox)
         ))

(use-package company
  :config

  ;; fci-mode breaks company :-(
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/54
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    "Safely turn off Fill Column Indicator.
If `fci-mode' is enabled disable it and store its state in special variable.
Argument IGNORE is not used"
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    "Turn on Fill Column Indicator if it was enabled.
If `fci-mode' was enabled turn it on.
Argument IGNORE is not used."
    (when company-fci-mode-on-p (fci-mode 1)))

  :hook ((company-completion-started . company-turn-off-fci)
         (company-completion-finished . company-maybe-turn-on-fci)
         (company-completion-cancelled . company-maybe-turn-on-fci)))

;; (req-package auto-complete-css)
;; (req-package auto-complete-emacs-lisp)
;; (req-package auto-complete-ruby)
;; (req-package auto-complete-yasnippet)

(provide 'as-completion)
