;; Scrolling
(defvar smooth-scrolling-repo-dir
  (concat edotdir "/.GIT/adamspiers.org/smooth-scrolling")
  "Directory where smooth-scrolling repository is checked out.")

(if (file-directory-p smooth-scrolling-repo-dir)
    (use-package smooth-scrolling
      :defer 3
      :load-path smooth-scrolling-repo-dir)
  (warn "Use mrco smooth-scrolling to check out the repository."))

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;; https://karthinks.com/software/batteries-included-with-emacs/#view-mode--m-x-view-mode
(setq view-read-only t)

;; Default right margin
(setq fill-column 70)

;; Stop down cursor adding newlines to end of buffer.
(setq next-line-add-newlines nil)

(bind-key "C-S-n" 'next-logical-line)
(bind-key "C-S-p" 'previous-logical-line)

;; Lack of C-w is currently a deal-breaker :-(
;; https://github.com/raxod502/ctrlf/issues/65
;; (use-package ctrlf
;;   :defer 2
;;   :config
;;   (ctrlf-mode +1)
;;   (add-to-list 'ctrlf-minibuffer-bindings '("<down>" . ctrlf-forward-literal))
;;   (add-to-list 'ctrlf-minibuffer-bindings '("<up>"   . ctrlf-backward-literal)))

;; Loaded by counsel
(use-package swiper
  :demand t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward))

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  )

(use-package avy
  ;; :config
  ;; (avy-setup-default)
  :bind (("C-0" . avy-goto-word-1)
         ("C-9" . avy-goto-char-timer)
         ("M-g" . avy-goto-line)))

(bind-key "M-G l"   'goto-line)
(bind-key "M-S-e" 'mark-end-of-sentence)

(use-feature bn-end-of-line-but-one
  :bind ("C-S-e" . bn-end-of-line-but-one))

(req-package expand-region
  :bind ("C-M-S-SPC" . er/expand-region))

;; emacs < 22 doesn't have x-clipboard-yank
(bind-key "S-<insert>"
          (if (boundp 'x-clipboard-yank)
              'x-clipboard-yank
            'clipboard-yank))

(bind-key "M-i" 'indent-relative)

(use-package goto-chg
  :bind (("C-<" . goto-last-change)
         ("C->" . goto-last-change-reverse)))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'as-point-motion)
