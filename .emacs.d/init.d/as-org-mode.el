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

  (setcdr (assq 'agenda org-show-context-detail) 'tree)
  (add-to-list 'org-show-context-detail '(org-goto . tree))

  (add-to-list 'org-modules 'org-timer))

(use-package org-jump-olp
  :ensure nil
  :after org
  :commands org-jump-olp)

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

;; FIXME: still need this?
;;(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

(req-package org-sync
  :require org)
(req-package org
  :ensure org-plus-contrib)
(use-package orgit
  :after org)
(use-package orgit-forge
  :after org)

(req-package org-bullets
  :require org
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun as-helm-org-rifle-notes ()
  (interactive)
  (helm-org-rifle-directories '("~/org/notes")))

(use-package helm-org
  :defer t)

(use-package helm-org-rifle
  :bind (("M-o j a" . helm-org-rifle-agenda-files)
         ("M-o j j" . helm-org-rifle-current-buffer)
         ("M-o n" . as-helm-org-rifle-notes)))

(provide 'as-org-mode)
