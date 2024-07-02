;; Scrolling
(defvar smooth-scrolling-repo-dir
  (concat edotdir "/.GIT/adamspiers.org/smooth-scrolling")
  "Directory where smooth-scrolling repository is checked out.")

;; (if (file-directory-p smooth-scrolling-repo-dir)
;;     (use-package smooth-scrolling
;;       :defer 3
;;       :load-path smooth-scrolling-repo-dir)
;;   (warn "Use mrco smooth-scrolling to check out the repository."))

(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")

  ;; :config
  ;; (good-scroll-mode 1)
  ;;
  ;; :bind (([next] . good-scroll-up-full-screen)
  ;;        ([prior] . good-scroll-down-full-screen))
  )

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

(use-package avy
  ;; :config
  ;; (avy-setup-default)
  :bind (("C-0" . avy-goto-word-1)
         ("C-9" . avy-goto-char-timer)
         ("M-g" . avy-goto-line)
         ("M-G d" . avy-copy-line)
         ("M-G D" . avy-copy-region)
         ("M-G y" . avy-move-line)
         ("M-G Y" . avy-move-region)))

(bind-key "M-G l"   'goto-line)
(bind-key "M-S-e" 'mark-end-of-sentence)

(use-feature bn-end-of-line-but-one
  :bind ("C-S-e" . bn-end-of-line-but-one))

(use-package expand-region
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

(use-package smart-jump
  :config
  (smart-jump-setup-default-registers))

;; Add some more conventional bindings for occur-mode
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)
(define-key occur-mode-map (kbd "TAB")
  'occur-mode-goto-occurrence-other-window)

(require 'as-mode-lighters)

;; C-x then
;;   SPC       back    in mark-ring
;;   <left>    back    in mark-ring
;;   <right>   forward in mark-ring
;;   C-SPC     back    in global-mark-ring
;;   C-<left>  back    in global-mark-ring
;;   C-<right> forward in global-mark-ring
(use-package back-button
  :config
  (back-button-mode 1)
  :diminish)

(use-package jump-char
  :bind
  (("M-'" . jump-char-forward)
   ("C-M-'" . jump-char-backward)))

(use-package mwim
  :custom
  (mwim-beginning-position-functions
   ;; FIXME: this still not quite right.  mwim-comment-beginning
   ;; ignores the comment prefix, so typically it would be the
   ;; rightmost position yielded by these four.  However putting
   ;; it third or last results in the mwim-line-beginning position
   ;; never being reached on a comment line.  Reproduce via the
   ;; following in a shell buffer, for instance:
   ;;
   ;; <indent># a comment
   '(mwim-line-beginning
     mwim-block-beginning
     mwim-code-beginning
     mwim-comment-beginning))
  (mwim-end-position-functions
   '(mwim-line-end mwim-block-end mwim-code-end))

  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))

;; Doesn't seem to work?
;; https://todo.sr.ht/~iank/visible-mark/1
;; (use-package visible-mark
;;   :config
;;   (global-visible-mark-mode))

(provide 'as-point-motion)
