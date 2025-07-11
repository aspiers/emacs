;; See also as-vcs.el

(use-package magit
  :defer 4

  :bind (("C-c g b" . magit-blame-addition)
         ("C-c g B" . magit-run-git-gui-blame)
         ("C-c g g" . magit-run-git-gui)
         ("C-c g k" . magit-run-gitk)
         ("C-c g a" . magit-run-gitk-all)
         ("C-c g A" . magit-run-gitg)
         ("C-c g l" . magit-log-buffer-file)

         (:map magit-diff-mode-map
               ("J" . magit-jump-to-first-diff)))

  :config

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
      (get-buffer-window buffer)))

  (defun magit-jump-to-first-diff ()
    (interactive)
    (goto-char (point-min))
    (magit-jump-to-diffstat-or-diff)
    (forward-line)
    (magit-jump-to-diffstat-or-diff)
    (re-search-forward "^[+-]"))

  (defun magit-run-gitg ()
    "Run `gitg' in the current repository."
    (interactive)
    (magit-process-file "gitg" nil 0))

  :custom
  (git-commit-use-local-message-ring t))

(with-packages (magit projectile)
  ;; See https://gitlab.com/edvorg/req-package/issues/60
  ;; (needs docs for how to handle config which involves multiple packages)
  :config

  ;; Improved and updated version of this nice idea from
  ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
  (require 'tramp)
  (setq magit-repository-directories
        (mapcar (lambda (dir) (cons (directory-file-name dir) 0))
                ;; remove tramp and non-git projects
                (remove-if
                 (lambda (project)
                   (or
                    (tramp-tramp-file-p project)
                    (not (file-directory-p (concat project "/.git")))))
                 (projectile-relevant-known-projects))))

  :bind (("C-c g s" . magit-status)
         ("C-S-g"   . magit-status)))

(with-packages (magit org)
  :config
  (defun as-magit-post-display-org-buffer-hook ()
    "Hook for `magit-diff-visit-file-hook' to automatically run
`org-show-context'."
    (when (eq major-mode 'org-mode)
      (org-show-context 'org-goto)))
  ;; Custom contexts break `widget-apply'.
  ;; (add-to-list 'org-fold-show-context-detail '(magit-diff . canonical))
  (add-hook 'magit-diff-visit-file-hook
            'as-magit-post-display-org-buffer-hook))

(use-package difftastic
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

;; https://github.com/greenrd/magit-topgit/issues/10
;; (use-package magit-topgit)

(use-package magit-annex)

;; https://github.com/terranpro/magit-gerrit/issues/62
;; https://github.com/somtin/magit-gerrit/issues/1
;; (use-package magit-gerrit)

(use-package forge
  :after magit
  :config
  (defun as-forge-fork ()
    (interactive)
    "Use `forge-fork' to create a fork within github.com/aspiers"
    (forge-fork "aspiers" "github"))

  (transient-append-suffix 'forge-dispatch 'forge-fork
    '("c F" "fork to aspiers" as-forge-fork))

  :custom
  (forge-owned-accounts
   '(("aspiers" remote-name github remote-name aspiers)
     ("ToucanProtocol" remote-name ToucanProtocol))))

;; support for magit: links in org buffers
;; e.g. [[orgit:/path/to/repo/][my repo]]
;; https://github.com/magit/orgit/
(use-package orgit)
(use-package orgit-forge)

;; (use-package magit-todos
;;   :config
;;   (magit-todos-mode))

(use-package magit-pretty-graph
  :straight (:host github :repo "georgek/magit-pretty-graph")
  :bind ("C-c g p" . magit-pg-repo))

(provide 'as-magit)
