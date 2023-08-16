;; Adam's editing utilities

(require 'as-projects) ;; C-c p overrides projectile bullshit

;;;###autoload
(defun mark-list ()
  "Put point at end of this list, mark at beginning.
The list marked is the one that contains point or follows point."
  (interactive)
  (forward-list)
  (push-mark (point) t t)
  (backward-list))

(global-unset-key "\M-o")

(use-feature as-word-motion
  :bind (("M-F"   . as-forward-word-start)
         ("M-B"   . as-backward-before-word)
         ("C-M-S-f" . as-forward-sexp-start)
         ("C-M-S-b" . as-backward-before-sexp)
         ("M-D"   . as-kill-word)
         ("C-M-S-k" . as-kill-sexp)))

;; https://karthinks.com/software/batteries-included-with-emacs/#cycle-spacing--m-spc
(bind-key "M-SPC"   'cycle-spacing)
;; (bind-key "M-\\"    'fixup-whitespace)
(bind-key "M-s M-k" 'delete-trailing-whitespace)

;; https://karthinks.com/software/batteries-included-with-emacs/#dwim-commands--upcase-downcase-and-more
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(use-feature bn-kill-stuff
  :bind (("C-M-(" . bn-strip-parentheses)
         ([remap kill-region] . bn-kill-region-or-backword-word)
         ([remap kill-ring-save] . bn-kill-line-or-region-save)
         ("M-Z" . bn-zap-nearly-to-char)))

;; Use consult-yank-pop instead
;; (use-package browse-kill-ring
;;   :bind (("M-Y" . browse-kill-ring)))

(use-feature as-line-ops
  :bind (("C-x C-t" . as-transpose-lines)
         ("C-c d"   . as-duplicate-line)
         ("<F5>"    . as-duplicate-line)
         ("<F7>"    . as-align-to-previous-line)
         ("<F8>"    . as-copy-previous-line-suffix)
         ("C-c p"   . as-copy-previous-line-suffix) ;; FIXME: conflict!
         ("C-x C-y" . vim-yy)
         ("C-M-y"   . as-join-line-with-next)))

(use-feature as-key-chord
  :config
  (key-chord-define-global "pq" 'as-copy-previous-line-suffix))

(bind-key "C-c l"   'align)
(bind-key "C-c TAB" 'indent-region)

(use-feature fill-common-prefix-region
  :bind (
         ;; This one might get overridden by per-mode bindings:
         ("C-M-q"   . fill-common-prefix-region)
         ;; but this one won't, so serves as a backup:
         ("C-c M-q" . fill-common-prefix-region)))

;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(bind-key "M-Q" 'unfill-paragraph)

(defun insert-euro-sign ()
  "Insert a EURO SIGN Unicode character."
  (interactive)
  (require 'ucs-utils)
  (insert-char (ucs-utils-char "euro sign"))))

(bind-key "C-x 8 £" 'insert-euro-sign)

(require 'as-mode-lighters)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (setq drag-stuff-modifier '(meta shift))
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (add-to-list 'drag-stuff-except-modes 'org-agenda-mode)
  (add-to-list 'drag-stuff-except-modes 'lispy-mode)
  (add-to-list 'drag-stuff-except-modes 'calendar-mode)
  (add-to-list 'drag-stuff-except-modes 'minibuffer-mode)
  (drag-stuff-global-mode t))

(provide 'as-editing)
