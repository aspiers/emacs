;; Scrolling
(defvar smooth-scrolling-repo-dir
  (concat edotdir "/.GIT/adamspiers.org/smooth-scrolling")
  "Directory where smooth-scrolling repository is checked out.")

(use-package smooth-scrolling
  :load-path smooth-scrolling-repo-dir)
(if (not (file-directory-p smooth-scrolling-repo-dir))
    (warn "Use mrco smooth-scrolling to check out the repository."))

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;; Default right margin
(setq fill-column 70)

;; Stop down cursor adding newlines to end of buffer.
(setq next-line-add-newlines nil)

(bind-key "C-S-n" 'next-logical-line)
(bind-key "C-S-p" 'previous-logical-line)

(use-package swiper
  :defer 2
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward))

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  )

(use-package avy
  ;; :config
  ;; (avy-setup-default)
  :bind (("C-0" . avy-goto-word-1)
         ("C-9" . avy-goto-char-timer)
         ("M-g" . avy-goto-line)))

(req-package idomenu
  :bind ("C-1" . idomenu))

(bind-key "M-G"   'goto-line)
(bind-key "M-S-e" 'mark-end-of-sentence)

(use-package bn-end-of-line-but-one
  :ensure nil
  :bind ("C-S-e" . bn-end-of-line-but-one))

(req-package expand-region
  :bind ("C-M-S-SPC" . er/expand-region))

;; emacs < 22 doesn't have x-clipboard-yank
(bind-key "S-<insert>"
          (if (boundp 'x-clipboard-yank)
              'x-clipboard-yank
            'clipboard-yank))

(bind-key "M-i" 'indent-relative)

(req-package goto-chg
  :bind (("C-<" . goto-last-change)
         ("C->" . goto-last-change-reverse)))

(provide 'as-point-motion)
