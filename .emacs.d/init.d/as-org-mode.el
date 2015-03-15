(eval-when-compile
  ;; org-default-extensions defaults to (org-irc) which causes a
  ;; compile to require erc.el which is not in emacs 21.
  (if (or (not (boundp 'org-default-extensions))
          (memq 'org-irc org-default-extensions))
      (defvar org-default-extensions '(org-mouse))))

(as-progress "loading org-install ...")
(require 'org-install nil 'noerror)

(require 'as-require)
(when (as-check-feature-loaded 'org-install)
  (as-progress "org-install loaded")
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
  ;; Let's see if this is already taken care of
  ;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  )

(defvar org-mode-map)
(add-hook 'org-mode-hook
          (lambda ()
            (as-soft-require 'as-gtd)
            (imenu-add-to-menubar "Imenu")
            (setq comment-start nil)))

(declare-function org-crypt-use-before-save-magic "org-crypt")
(add-hook 'org-mode-hook
          (lambda ()
            (and (as-soft-require 'org-crypt)
                 (org-crypt-use-before-save-magic))
            (add-to-list 'org-modules 'org-timer)))

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

(bind-key "M-o a" 'org-agenda)
(bind-key "M-S-a"  'as-org-switch-to-agenda-buffer) ;; X11 only
(bind-key "M-o b" 'as-org-switch-to-agenda-buffer)
(bind-key "M-o q" 'org-remember)
(bind-key "M-o M-o" 'as-org-jump-clock-or-agenda)

(bind-key "C-c C-x C-j" 'org-clock-goto)

(require 'ido)

(fset 'as-find-personal-todo "\C-x\C-f~/org/TODO.org")
(bind-key "C-c j t" 'as-find-personal-todo)
(bind-key "C-\""    'as-find-personal-todo)
(fset 'as-find-personal-diary "\C-x\C-f~/org/diary.org")
(bind-key "C-c j d" 'as-find-personal-diary)
;;(fset 'as-find-personal-note "\C-x\C-f~/org/notes/")

;; Try to use C-c c but keeping this for backwards compatability with
;; my brain.
(bind-key "C-c c" 'org-capture)
(bind-key "C-c q" 'org-capture)

(autoload 'org-occur-in-agenda-files "org" nil t)
(bind-key "C-c C-?" 'org-occur-in-agenda-files)

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-personal-note
  "~/org/notes" "Find note: ")
(bind-key "C-c j n"  'as-find-personal-note)

(provide 'as-org-mode)
