(use-package org
  ;; We want the org-plus-contrib package rather than just org, but
  ;; there is no org-plus-contrib.el, so we have to load it a bit
  ;; differently, as per:
  ;; https://emacs.stackexchange.com/questions/41321/when-to-specify-a-package-name-in-use-packages-ensure-tag
  ;; https://github.com/jwiegley/use-package/issues/597
  ;; https://github.com/raxod502/straight.el/issues/252
  ;; :ensure org-plus-contrib
  :straight org-plus-contrib

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
  :after org)

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

(provide 'as-org-mode)
