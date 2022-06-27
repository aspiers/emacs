(use-package org)

(use-package org-contrib
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

   ("C-c C-?" . org-occur-in-agenda-files)

   :map org-mode-map
   (("C-c C-S-w" . org-refile-reverse)
    ("C-c s" . org-show-context)))

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

(use-feature org-jump-olp
  :commands org-jump-olp)

(with-packages (org counsel)
  :config
  (as-progress "org/counsel combined config")

  :bind (:map org-mode-map
              (("C-c C-j" . counsel-org-goto))))

;; Enabling by default slows down file saving. In files which need it,
;; can be set through local variables, e.g.:
;;
;;   *** before-save-hook: (org-encrypt-entries) **
;;
;; See also https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-feature org-crypt
  :commands org-crypt-use-before-save-magic)

(use-feature as-gtd
  :after org-plus-contrib)

;; FIXME: still need this?
;;(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

;; Supposed to help debug org-plus-contrib load warning but doesn't
(require 'use-package)

(use-package org-sync)

(use-package org-timeline)

(use-package org-bullets
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

(use-package epoch
  :straight (epoch :host github :repo "progfolio/epoch")
  :bind (:map org-mode-map
              (("C-c C-S-t" . epoch-todo))
              :map org-agenda-mode-map
              (("C-c C-S-t" . epoch-agenda-todo))))

(use-package org-super-agenda)

(use-package org-modern
  :after org
  ;; :straight (org-modern :host github :repo "minad/org-modern")
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-modern-indent
  :after org-modern
  :straight (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; The default fixed-pitch font (from which org-meta-line inherits)
;; has line spacing >1.0 on some systems. This will introduce gaps
;; even if your default font is changed, and line-space is nil. To
;; correct it, add:

(set-face-attribute 'fixed-pitch nil :family "Hack" :height 1.0) ; or whatever font family

;; Also optional; use org-bullets instead for nicely aligned bullet stars.

(use-package org-bullets-mode
  :ensure org-bullets
  :config
  :hook org-mode)

(provide 'as-org-mode)
