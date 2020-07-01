(eval-when-compile
  ;; org-default-extensions defaults to (org-irc) which causes a
  ;; compile to require erc.el which is not in emacs 21.
  (if (or (not (boundp 'org-default-extensions))
          (memq 'org-irc org-default-extensions))
      (defvar org-default-extensions '(org-mouse))))

(use-package org
  :bind
  (("M-o a" . org-agenda)
   ("M-o c" . org-capture)
   ("M-S-a" . as-org-switch-to-agenda-buffer) ;; X11 only
   ("M-o b" . as-org-switch-to-agenda-buffer)
   ("M-o M-o" . as-org-jump-clock-or-agenda)

   ("C-c C-x C-j" . org-clock-goto)
   ("M-o r" . org-clock-in-daily-review)

   ;; Try to use C-c c but keeping this for backwards compatability with
   ;; my brain.
   ("C-c c" . org-capture)

   ("C-c C-?" . org-occur-in-agenda-files))

  :config
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))

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
              ;; imenu is horribly slow and outdated, and with
              ;; imenu-auto-rescan it triggers far too often, in
              ;; org-mode captures at least.
              ;;
              ;; (imenu-add-to-menubar "Imenu")
              (setq comment-start nil)))

  (add-to-list 'org-modules 'org-timer))

(use-package org
  :after counsel
  :bind (:map org-mode-map
              ("C-c C-j" . counsel-org-goto))
  :config
  (as-progress "org/counsel combined config"))

;; Enabling by default slows down file saving. In files which need it,
;; can be set through local variables, e.g.:
;;
;;   *** before-save-hook: (org-encrypt-entries) **
;;
;; See also https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-package org-crypt
  :requires org
  :ensure nil

  :defer t
  :init
  (autoload 'org-crypt-use-before-save-magic "org-crypt"))

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
(req-package org
  :ensure org-plus-contrib)
(req-package orgit
  :require org)

(req-package org-bullets
  :require org
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun as-helm-org-rifle-notes ()
  (interactive)
  (helm-org-rifle-directories '("~/org/notes")))

(use-package helm-org)

(use-package helm-org-rifle
  :bind (("M-o j a" . helm-org-rifle-agenda-files)
         ("M-o j j" . helm-org-rifle-current-buffer)
         ("M-o n" . as-helm-org-rifle-notes)))

(provide 'as-org-mode)
