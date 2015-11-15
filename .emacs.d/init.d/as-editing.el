;; Adam's editing utilities

(require 'as-projects) ;; C-c p overrides projectile bullshit

;; FIXME: set up company-mode

;;;###autoload
(defun mark-list ()
  "Put point at end of this list, mark at beginning.
The list marked is the one that contains point or follows point."
  (interactive)
  (forward-list)
  (push-mark (point) t t)
  (backward-list))

(bind-key "C-c o" 'overwrite-mode)
(global-unset-key "\M-o")

(use-package as-word-motion
  :bind (("M-F"   . as-forward-word-start)
         ("M-B"   . as-backward-before-word)
         ("C-M-S-f" . as-forward-sexp-start)
         ("C-M-S-b" . as-backward-before-sexp)
         ("S-M-d"   . as-kill-word)
         ("C-M-S-k" . as-kill-sexp)))

(bind-key "C-M-k"   'kill-sexp)
(bind-key "M-\\"    'fixup-whitespace)
(bind-key "M-s M-k" 'delete-trailing-whitespace)

(use-package bn-kill-stuff
  :bind (("C-M-(" . bn-strip-parentheses)
         ("C-w"   . bn-kill-region-or-backword-word)
         ("M-w"   . bn-kill-line-or-region-save)
         ("M-Z" . bn-zap-nearly-to-char)))

(use-package as-line-ops
  :bind (("C-x C-t" . as-transpose-lines)
         ("C-c d"   . as-duplicate-line)
         ("<F5>"    . as-duplicate-line)
         ("C-c A"   . as-align-to-previous-line)
         ("<F7>"    . as-align-to-previous-line)
         ("C-c P"   . as-align-to-previous-line)
         ("<F8>"    . as-copy-previous-line-suffix)
         ("C-c p"   . as-copy-previous-line-suffix) ;; FIXME: conflict!
         ("C-x C-y" . vim-yy)
         ("C-M-y"   . as-join-line-with-next)))
(require 'as-key-chord)
(bind-key "C-c p" 'as-copy-previous-line-suffix)

(key-chord-define-global "pq" 'as-copy-previous-line-suffix)

(bind-key "C-c l"   'align)
(bind-key "C-c TAB" 'indent-region)

(use-package fill-common-prefix-region
  :bind (
         ;; This one might get overridden by per-mode bindings:
         ("C-M-q"   . fill-common-prefix-region)
         ;; but this one won't, so serves as a backup:
         ("C-c M-q" . fill-common-prefix-region)))

(bind-key "C-x 8 e"
          (lambda ()
            (interactive)
            (ucs-insert (cdr (assoc-string "EURO SIGN" (ucs-names))))))

(provide 'as-editing)
