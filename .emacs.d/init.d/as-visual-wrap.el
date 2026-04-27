;;; as-visual-wrap.el --- visual-wrap-prefix-mode tweaks  -*- lexical-binding: t; -*-

;; `visual-wrap-prefix-mode' (Emacs 30 builtin, formerly ELPA
;; `adaptive-wrap-prefix-mode') uses `fill-context-prefix' to compute
;; the wrap-prefix for visual line continuations.  For list items like
;; "- foo" or "1. foo" that returns the literal first-line indent
;; including the bullet, so wrapped lines display the bullet repeated
;; on every visual line instead of aligning under the content.
;;
;; Same flaw exists in upstream `adaptive-wrap'; the package adapts to
;; indentation depth and comment prefixes (where prefix repetition is
;; correct) but conflates that with bullet markers (where it isn't).
;;
;; This advice replaces the bullet with same-width whitespace so
;; wrapped lines align under the content column.  Indent and trailing
;; whitespace are preserved.  Width-preserving so multi-digit numbers
;; (e.g. "10. ") and nested bullets stay aligned.

(defun as-visual-wrap-blank-bullet (orig beg end)
  "Around-advice for `visual-wrap-fill-context-prefix'.
Replace any leading list bullet in the returned prefix with
same-width whitespace so wrapped continuation lines align under
the content column rather than re-rendering the bullet."
  (let ((s (funcall orig beg end)))
    (if (and s (string-match
                "\\`\\([ \t]*\\)\\([-*+]\\|[0-9]+[.)]\\)\\([ \t]+\\)"
                s))
        (let* ((indent (match-string 1 s))
               (bullet (match-string 2 s))
               (after  (match-string 3 s))
               (rest   (substring s (match-end 3)))
               (blank  (make-string (string-width bullet) ?\s))
               (new    (concat indent blank after rest))
               (face   (get-text-property 0 'face s)))
          (if face (propertize new 'face face) new))
      s)))

(advice-add 'visual-wrap-fill-context-prefix
            :around #'as-visual-wrap-blank-bullet)

;; The fringe bitmaps drawn for wrapped visual lines (curly arrows) by
;; default inherit the `fringe' face foreground, which is the same
;; brightness as ordinary fringe content (truncation arrows,
;; git-gutter, flycheck markers, etc.).  Wrap indicators appear on
;; every wrapped paragraph so they are noisy at full brightness.
;;
;; Use the cursor colour: it is theme-coherent (every modus and ef
;; theme defines it), visually distinct from `shadow' /
;; `font-lock-comment-face' (which markdown-mode reuses for
;; `markdown-list-face' bullets), and reads as an interface accent
;; rather than ordinary text.
;;
;; The `cursor' face stores the colour as `:background', not
;; `:foreground', so we can't simply `:inherit cursor' on a fringe
;; face.  Instead, recompute the wrap indicator face foreground
;; whenever themes change.

(defface as-fringe-wrap-indicator
  '((t :inherit fringe))
  "Face for the curly-arrow fringe bitmaps shown on wrapped visual lines.
The foreground is recomputed from the `cursor' face background by
`as-fringe-wrap-indicator-update'.")

(defun as-fringe-wrap-indicator-update (&rest _)
  "Set `as-fringe-wrap-indicator' foreground to the cursor colour."
  (let ((cursor-bg (face-background 'cursor nil t)))
    (when cursor-bg
      (set-face-attribute 'as-fringe-wrap-indicator nil
                          :foreground cursor-bg))))

(as-fringe-wrap-indicator-update)
(advice-add 'load-theme  :after #'as-fringe-wrap-indicator-update)
(advice-add 'enable-theme :after #'as-fringe-wrap-indicator-update)

(set-fringe-bitmap-face 'left-curly-arrow  'as-fringe-wrap-indicator)
(set-fringe-bitmap-face 'right-curly-arrow 'as-fringe-wrap-indicator)

;; Show the wrap indicator only in the right fringe.
;; `visual-line-fringe-indicators' covers visual-line-mode's own
;; indicators; `fringe-indicator-alist' covers Emacs's built-in
;; line-continuation indicator (which fires regardless of
;; visual-line-mode).  Both default to showing curly arrows in both
;; fringes; suppress the left side in each.
(setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
(setf (alist-get 'continuation fringe-indicator-alist)
      '(nil right-curly-arrow))

;; Enable visual-wrap-prefix-mode in modes where it's useful.
;; Requires visual-line-mode for the wrap-prefix property to take effect.
(dolist (hook '(markdown-mode-hook
                rst-mode-hook
                text-mode-hook
                outline-mode-hook))
  (add-hook hook #'visual-line-mode)
  (add-hook hook #'visual-wrap-prefix-mode))

(provide 'as-visual-wrap)
;;; as-visual-wrap.el ends here
