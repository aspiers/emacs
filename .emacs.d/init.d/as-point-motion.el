;; Scrolling
(use-package smooth-scrolling
  :load-path (lambda ()
               (concat edotdir "/.GIT/adamspiers.org/smooth-scrolling")))

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;; Default right margin
(setq fill-column 70)

;; Stop down cursor adding newlines to end of buffer.
(setq next-line-add-newlines nil)

(req-package iy-go-to-char
  :require as-key-chord
  ;; Can't do this yet
  ;; https://github.com/edvorg/req-package/issues/56
  ;; :chords (("zv" . iy-go-to-char))
  )

(bind-key "C-S-n" 'next-logical-line)
(bind-key "C-S-p" 'previous-logical-line)

(use-package avy
  :ensure t
  ;; :config
  ;; (avy-setup-default)
  :bind (("C-0" . avy-goto-word-1)))

(req-package idomenu
  :bind ("C-1" . idomenu))

(bind-key "M-g"   'goto-line)
(bind-key "M-S-e" 'mark-end-of-sentence)

;;;###autoload
(defun bn-end-of-line-but-one (arg)
  "Move point to one character before the end of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error.
If the line is empty, doesn't do anything."
  (interactive "*p")
  (end-of-line arg)
  (unless (bolp)
    (backward-char)))

(bind-key "C-S-e" 'bn-end-of-line-but-one)

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
