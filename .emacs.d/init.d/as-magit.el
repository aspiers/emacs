;; See also as-vcs.el

(req-package magit
  :config
  (add-hook 'ido-setup-hook 'as-magit-ido-keys)

  (defun as-magit-ido-keys ()
    "Add Adam's keybindings for ido."
    (define-key ido-file-dir-completion-map (kbd "C-Y") 'ido-magit-status-internal)
    (define-key ido-file-dir-completion-map (kbd "S-<return>") 'ido-magit-status-internal))

  (defun ido-magit-status-internal ()
    "Run magit-status on current directory.  For use with
`ido-file-dir-completion-map'."
    (interactive)
    (progn
      ;; Based on part of `ido-magic-backward-char' which gets invoked
      ;; when C-d is hit during ido mode (see ido-common-completion-map).
      (setq fallback 'magit-status)
      (setq ido-exit 'fallback)
      (exit-minibuffer)))

  ;; Improved and updated version of this nice idea from
  ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
  (eval-after-load 'projectile
    (progn
      (require 'tramp)
      (setq magit-repository-directories
            (mapcar (lambda (dir) (directory-file-name dir))
                    ;; remove tramp and non-git projects
                    (remove-if
                     (lambda (project)
                       (or
                        (tramp-tramp-file-p project)
                        (not (file-directory-p (concat project "/.git")))))
                     (projectile-relevant-known-projects))))
      (setq magit-repo-dirs-depth 1)))

  (bind-key "C-c g b"  'magit-blame)
  (bind-key "C-c g B"  'magit-run-git-gui-blame)
  (bind-key "C-c g g"  'magit-run-git-gui)
  (bind-key "C-c g k"  'magit-run-gitk)
  (bind-key "C-c g a"  'magit-run-gitk-all)
  (bind-key "C-c g s"  'magit-status)
  (bind-key "C-S-g"    'magit-status)

  (autoload 'ido-buffer-internal "ido")
  (defvar ido-default-buffer-method)
  (defun ido-switch-magit-buffer ()
    "Switch to a magit status buffer via `ido'."
    (interactive)
    (ido-buffer-internal ido-default-buffer-method
                         nil "magit status: " nil "*magit: "))
  (bind-key "C-M-g" 'ido-switch-magit-buffer)

  ;; Stolen from https://github.com/syl20bnr/spacemacs/pull/3319/files
  ;; See also https://github.com/magit/magit/issues/1953#issuecomment-146900842
  (defun magit-display-buffer-fullscreen (buffer)
    (if (or
         ;; the original should stay alive, so we can't go fullscreen
         magit-display-buffer-noselect
         ;; don't go fullscreen for certain magit buffers if current
         ;; buffer is a magit buffer (we're conforming to
         ;; `magit-display-buffer-traditional')
         (and (derived-mode-p 'magit-mode)
              (not (memq (with-current-buffer buffer major-mode)
                         '(magit-process-mode
                           magit-revision-mode
                           magit-diff-mode
                           magit-stash-mode
                           magit-status-mode)))))
        ;; open buffer according to original magit rules
        (magit-display-buffer-traditional buffer)
      ;; open buffer in fullscreen
      (delete-other-windows)
      ;; make sure the window isn't dedicated, otherwise
      ;; `set-window-buffer' throws an error
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil buffer)
      ;; return buffer's window
      (get-buffer-window buffer))))

(req-package magit-topgit)
(req-package magit-annex)

(req-package magit-gerrit)

(req-package magithub)
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-features '((pull-request-merge . t) (pull-request-checkout . nil))))

;; support for magit: links in org buffers
(req-package org-magit
  :require org magit)

(provide 'as-magit)
