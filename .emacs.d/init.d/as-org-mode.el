(eval-when-compile
  ;; org-default-extensions defaults to (org-irc) which causes a
  ;; compile to require erc.el which is not in emacs 21.
  (if (or (not (boundp 'org-default-extensions))
          (memq 'org-irc org-default-extensions))
      (defvar org-default-extensions '(org-mouse))))

;; FIXME: use req-package
(require 'as-require)
(when (as-check-feature-loaded 'org-install)
  (as-progress "org-install loaded")
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
  ;; Let's see if this is already taken care of
  ;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  )

(defvar org-mode-map)
(use-package org
  :bind
  (("M-o a" . org-agenda)
   ("M-S-a" . as-org-switch-to-agenda-buffer) ;; X11 only
   ("M-o b" . as-org-switch-to-agenda-buffer)
   ("M-o q" . org-remember)
   ("M-o M-o" . as-org-jump-clock-or-agenda)

   ("C-c C-x C-j" . org-clock-goto)
   ("M-o r" . org-clock-in-daily-review)

   ;; Try to use C-c c but keeping this for backwards compatability with
   ;; my brain.
   ("C-c c" . org-capture)

   ("C-c C-?" . org-occur-in-agenda-files))

  :config
  (defun org-clock-in-daily-review ()
    (interactive)
    "Clocks in to the daily review task."
    (let ((todo-buffer (find-file "~/org/TODO.org")))
      (with-current-buffer todo-buffer
        (goto-char
         (org-find-exact-headline-in-buffer "daily review" todo-buffer 'pos-only))
        (org-clock-in)))
    (as-org-switch-to-agenda-buffer))

  (add-hook 'org-mode-hook
            (lambda ()
              (imenu-add-to-menubar "Imenu")
              (setq comment-start nil)))

  (require 'org-crypt)
  (add-to-list 'org-modules 'org-timer)
  (org-crypt-use-before-save-magic))

(req-package as-gtd
  :require org
  :ensure nil)

;; FIXME: use https://github.com/edvorg/req-package ?
;; https://github.com/jwiegley/use-package/issues/71
(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(unless (getenv "EMACS_BATCH")
  (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode))

(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)


(req-package org-sync
  :require org)
(req-package org-plus-contrib
  :require org)
(req-package orgit
  :require org)

(provide 'as-org-mode)
