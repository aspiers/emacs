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

;; FIXME: use https://github.com/edvorg/req-package ?
;; https://github.com/jwiegley/use-package/issues/71
(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

(bind-key "M-o a" 'org-agenda)
(bind-key "M-S-a" 'as-org-switch-to-agenda-buffer) ;; X11 only
(bind-key "M-o b" 'as-org-switch-to-agenda-buffer)
(bind-key "M-o q" 'org-remember)
(bind-key "M-o M-o" 'as-org-jump-clock-or-agenda)

(bind-key "C-c C-x C-j" 'org-clock-goto)

(defun org-clock-in-default-task ()
  (interactive)
  "Clocks in to the default task."
  (with-current-buffer (marker-buffer org-clock-default-task)
    (org-clock-clock-in (list org-clock-default-task))))

(bind-key "C-c C-x I" 'org-clock-in-default-task)
(bind-key "M-o r"     'org-clock-in-default-task)

;; Try to use C-c c but keeping this for backwards compatability with
;; my brain.
(bind-key "C-c c" 'org-capture)
(bind-key "C-c q" 'org-capture)

(autoload 'org-occur-in-agenda-files "org" nil t)
(bind-key "C-c C-?" 'org-occur-in-agenda-files)

(provide 'as-org-mode)
