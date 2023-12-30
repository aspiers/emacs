(use-package org
  :bind
  (
   ("M-o c" . org-capture)
   ("C-c c" . org-capture)

   :map org-mode-map
   (("C-c C-S-w r" . org-refile-reverse)
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

  (setcdr (assq 'agenda org-fold-show-context-detail) 'tree)
  (add-to-list 'org-fold-show-context-detail '(org-goto . tree))

  (add-to-list 'org-modules 'org-timer))

(use-package org-contrib
  :after (org))

(use-feature org-refile-narrowed
  :after (org)
  :bind
  (("C-c C-S-w i" . org-refile-in-sibling)
   ("C-c C-S-w C-w" . org-refile-in-sibling)
   ("C-c C-S-w u" . org-refile-in-subtree)
   ("C-c C-S-w C-c" . org-refile-in-subtree)))

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
  :after (org))

;; FIXME: still need this?
;;(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

;; Supposed to help debug old org-plus-contrib load warning but doesn't
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

;; Hide annoying :PROPERTIES: drawers.
;;
;; See https://github.com/jxq0/org-tidy/issues/13 is annoying but not
;; a dealbreaker.
(use-package org-tidy
  :hook
  (org-mode . org-tidy-mode)
  :custom
  ;; Avoid breaking org-mode's speed keys:
  ;;
  ;;   https://github.com/jxq0/org-tidy/issues/11
  (org-tidy-protect-overlay nil))

(provide 'as-org-mode)
