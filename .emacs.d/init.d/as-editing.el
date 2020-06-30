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

(bind-key "C-c o" 'overwrite-mode)
(global-unset-key "\M-o")

(req-package as-word-motion
  :ensure nil
  :bind (("M-F"   . as-forward-word-start)
         ("M-B"   . as-backward-before-word)
         ("C-M-S-f" . as-forward-sexp-start)
         ("C-M-S-b" . as-backward-before-sexp)
         ("S-M-d"   . as-kill-word)
         ("C-M-S-k" . as-kill-sexp)))

(bind-key "C-M-k"   'kill-sexp)
(bind-key "M-\\"    'fixup-whitespace)
(bind-key "M-s M-k" 'delete-trailing-whitespace)

(defun as-format-as-flowed-text ()
  "Format the buffer as flowed text according to RFC 2646.
This ensures that appropriate lines should be terminated with a
single space, and that \"> \" quoting prefixes are replaced with
\">\".  Operates on the current region if active, otherwise on
the whole buffer."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      ;; Ensure appropriate lines end with a space
      (while (re-search-forward "^\\(>+ ?\\)?\\S-.\\{10,\\}\\S-$" end t)
        (replace-match "\\& " t))

      ;; Replace "> " quoting prefixes with ">"
      (goto-char start)
      (let ((eol)
            (eolm (make-marker)))
        (while (setq eol (re-search-forward "^>.*" end t))
          (set-marker eolm eol)
          (goto-char (match-beginning 0))
          (while (looking-at ">")
            (if (looking-at "> \\([^  ]\\)")
                (replace-match ">\\1")
              (forward-char)))
          (goto-char (marker-position eolm)))))))

(bind-key "M-s M-m" 'as-format-as-flowed-text)

(req-package bn-kill-stuff
  :ensure nil
  :bind (("C-M-(" . bn-strip-parentheses)
         ("C-w"   . bn-kill-region-or-backword-word)
         ("M-w"   . bn-kill-line-or-region-save)
         ("M-Z" . bn-zap-nearly-to-char)))

(req-package as-line-ops
  :ensure nil
  :bind (("C-x C-t" . as-transpose-lines)
         ("C-c d"   . as-duplicate-line)
         ("<F5>"    . as-duplicate-line)
         ("C-c A"   . as-align-to-previous-line)
         ("<F7>"    . as-align-to-previous-line)
         ("<F8>"    . as-copy-previous-line-suffix)
         ("C-c p"   . as-copy-previous-line-suffix) ;; FIXME: conflict!
         ("C-x C-y" . vim-yy)
         ("C-M-y"   . as-join-line-with-next)))

(bind-key "C-c p" 'as-copy-previous-line-suffix)
(req-package as-key-chord
  :ensure nil
  :config
  (key-chord-define-global "pq" 'as-copy-previous-line-suffix))

(bind-key "C-c l"   'align)
(bind-key "C-c TAB" 'indent-region)

(req-package fill-common-prefix-region
  :ensure nil
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

(bind-key "C-x 8 e"
          (lambda ()
            (interactive)
            (ucs-insert (cdr (assoc-string "EURO SIGN" (ucs-names))))))

(provide 'as-editing)
