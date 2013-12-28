;; Adam's editing utilities

;;{{{ Word motion

;;;###autoload
(defun as-forward-word-start (&optional count)
  "As `forward-word', but lands at the start of a word not the end."
  (interactive "p")
  (forward-word (or count 1))
  (forward-word 1)
  (forward-word -1))

;;;###autoload
(defun as-backward-before-word (&optional count)
  "As `backward-word', but lands at the start of a word not the end."
  (interactive "p")
  (forward-word (- (or count 1)))
  (forward-word -1)
  (forward-word 1))

;;;###autoload
(defun as-kill-word ()
  "Kills forward to where as-forward-word-start would land."
  (interactive)
  (kill-region (point)
               (save-excursion (as-forward-word-start) (point))))

;;}}}
;;{{{ Sexp motion

;;;###autoload
(defun as-forward-sexp-start (&optional count)
  "As `forward-sexp', but lands at the start of a sexp not the end."
  (interactive "p")
  (forward-sexp (or count 1))
  (forward-sexp 1)
  (forward-sexp -1))

;;;###autoload
(defun as-backward-before-sexp (&optional count)
  "As `backward-sexp', but lands at the start of a sexp not the end."
  (interactive "p")
  (forward-sexp (- (or count 1)))
  (forward-sexp -1)
  (forward-sexp 1))

;;;###autoload
(defun as-kill-sexp ()
  "Kills forward to where `as-forward-sexp-start' would land."
  (interactive)
  (kill-region (point)
               (save-excursion (as-forward-sexp-start) (point))))

;;}}}
;;{{{ as-duplicate-line

;;;###autoload
(defun as-duplicate-line (&optional count) 
  "Duplicates the current line."
  (interactive "*p")
  (save-excursion
    (let ((i 0))
      (while (< i count)
        (beginning-of-line)
        (let ((bol (point))
              line-to-copy)
          (end-of-line)
          (setq line-to-copy (buffer-substring bol (point)))
          (insert "\n" line-to-copy))
        (setq i (1+ i)))))
  (let ((line-move-visual nil))
    ;; Deliberately not using forward-line in order to preserve
    ;; column of cursor
    (next-line count)))

;;}}}
;;{{{ as-join-line-with-next

;;;###autoload
(defun as-join-line-with-next (&optional preserve-comment)
  "Joins the current line with the next.  Removes any
continuation backslash from the end of the line, and any prefix
matching `adaptive-fill-regexp' from the beginning of the
next (unless a prefix argument is given) before joining."
  (interactive "*P")

  (save-excursion
    (end-of-line)
    (forward-char -1)
    (and (looking-at "\\\\") (delete-char 1))
    (unless (or preserve-comment (not comment-start))
        (save-excursion
          (forward-line 1)
          (let ((beg (point)))
            (if (looking-at adaptive-fill-regexp)
                (replace-match "")))))
    (join-line 1)))

;;}}}
;;{{{ as-copy-previous-line-suffix

;;;###autoload
(defun as-copy-previous-line-suffix ()
  "Copy the suffix of the line directly above the point and yank it at the point."
  (interactive "*")
  (save-excursion
    (let ((suffix (save-excursion
                    (let ((col (current-column)))
                      (forward-line -1)
                      (move-to-column col))
                    (let ((suffix-start (point)))
                      (end-of-line)
                      (buffer-substring suffix-start (point))))))
      (insert suffix))))

;;}}}
;;{{{ as-align-to-previous-line

;;;###autoload
(defun as-align-to-previous-line (&optional count)
  "Runs indent-relative on the current line and moves down to enable repeating."
  (interactive "*p")
  (let ((i 0))
    (while (< i count)
      (save-excursion
        (backward-char 1)
        (re-search-forward "[ \t]")
        (re-search-forward "[^ \t]")
        (backward-char 1)
        (indent-relative))
      (next-line 1) ;; sorry FSF
      (setq i (1+ i)))))

;;}}}
;;{{{ as-transpose-lines

;;;###autoload
(defun as-transpose-lines (arg)
  "Just like `transpose-lines', but preserves the point's position."
  (interactive "*p")
  (let ((col (current-column)))
    (transpose-lines arg)
    (forward-line (- arg))
    (move-to-column col)))

;;}}}
;;{{{ mark-list

;;;###autoload
(defun mark-list ()
  "Put point at end of this list, mark at beginning.
The list marked is the one that contains point or follows point."
  (interactive)
  (forward-list)
  (push-mark (point) t t)
  (backward-list)
)

;;}}}
;;{{{ vim-yy

;;;###autoload
(defun vim-yy (&optional lines)
  "Simulates vi's yy command."
  (interactive "p")
  (save-excursion
    (setq lines (if (or (null lines)
                        (< lines 1)) 1 lines))
    (beginning-of-line)
    (push-mark (point) nil t)
    (next-line lines)
    (kill-ring-save (mark) (point))))

;;}}}
;;{{{ bn-end-of-line-but-one

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

;;}}}
;;{{{ bn-strip-parentheses

;;;###autoload
(defun bn-strip-parentheses (arg)
  "Delete the parenthesis character at point, and its match.
If the character at point has either open-parenthesis or
close-parenthesis syntax, delete it, and also delete its match.
For instance, if point is at | in

   a = b * |(c + d);

then \\[bn-strip-parentheses] will result in

   a = b * |c + d;

With a prefix argument ARG, try to repeat the process ARG times.
Stop as soon as point is not at a parenthesis character.  With
just C-u as a prefix argument, repeat as many times as possible.

Adam added an extra DWIM feature - if the opening parenthesis is
immediately preceded by a letter, will also insert a space where the
parenthesis has just been removed."
  (interactive "*P")
  (let ((many-as-possible-p (and (not (null arg)) (listp arg)))
        (n (prefix-numeric-value arg))
        (open-remover (lambda ()
                        (delete-char 1)
                        ;; here's the DWIM bit
                        (when
                            (save-excursion
                              (forward-char -1)
                              (looking-at "[a-zA-Z]"))
                          (progn
                            (insert " ")
                            'dwim-space-inserted))))
        on-open-p
        on-close-p)
    (while (and (progn
                  (setq on-open-p (looking-at "\\s(")
                        on-close-p (looking-at "\\s)"))
                  (or on-open-p on-close-p))
                (or many-as-possible-p
                    (> n 0)))
      (when on-open-p
        (save-excursion
          (forward-sexp)
          (delete-char -1))
        (when (funcall open-remover)
          (forward-char -1)))
      (when on-close-p
        (save-excursion
          (forward-char 1)
          (forward-sexp -1)
          (funcall open-remover))
        (delete-char 1))
      (setq n (- n 1)))))

;;}}}
;;{{{ bn-kill-region-or-backword-word

;;;###autoload
(defun bn-kill-region-or-backword-word (arg)
  "If mark is active, do `kill-region'.  If not, do `backward-kill-word'.
\(This doesn't make very much sense unless you also have
transient-mark-mode turned on.\)  In the case that the mark is not
active, ARG specifies how many words backwards to kill."
  (interactive "*p")
  (if mark-active
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;;}}}
;;{{{ bn-kill-line-or-region-save

;;;###autoload
(defun bn-kill-line-or-region-save (arg)
  "If mark is active, do `kill-ring-save'; else copy current line\(s\).
ARG is ignored if mark is active.  If mark is not active,
copy-as-if-killed ARG lines forward, starting with the one point is
currently on.  ARG can be negative, in which case the current line is
not included in the text copied."
  (interactive "p")
  (if mark-active
      (kill-ring-save (region-beginning) (region-end))
    (let ((bol (save-excursion (beginning-of-line) (point)))
          (bonl (save-excursion (forward-line arg) (point))))
      (kill-ring-save bol bonl))))

;;}}}
;;{{{ bn-zap-nearly-to-char

(autoload 'signum "cl" "signum")

;;;###autoload
(defun bn-zap-nearly-to-char (arg char)
  "Kill up to but not including ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (let ((case-fold-search nil))
    (let ((beg (point))
          end)
      (setq end (save-excursion
                  ;; Want to avoid doing nothing if we're looking
                  ;; at CHAR already.
                  (forward-char (signum arg))
                  (search-forward (char-to-string char) nil nil arg)
                  (- (point) (signum arg))))
      (kill-region beg end))))

;;}}}
;;{{{ Ben's secondary selection hacks

;;;###autoload
(defun bn-make-region-into-secondary (start end)
  "Turn the region into the secondary selection.
The secondary selection is enabled if required, and set equal to
the region.  The region is deactivated.  The buffer is not
altered at all."
  (interactive "r")
  (if mouse-secondary-overlay
      (move-overlay mouse-secondary-overlay start end (current-buffer))
    (setq mouse-secondary-overlay (make-overlay start end)))
  (overlay-put mouse-secondary-overlay 'face 'secondary-selection)
  (x-set-selection
   'SECONDARY
   (buffer-substring (overlay-start mouse-secondary-overlay)
                     (overlay-end mouse-secondary-overlay)))
  (deactivate-mark))

;;;###autoload
(defun bn-exchange-region-and-secondary (start end)
  "Interchange the region and the secondary selection.
The results are not well-defined if the region and the
secondary selection overlap."
  (interactive "r")
  (or mouse-secondary-overlay
      (error "The secondary selection is not active now"))
  (let ((sec-start (overlay-start mouse-secondary-overlay))
        (sec-end (overlay-end mouse-secondary-overlay)))
    (let ((transpose-subr-start1 start)
          (transpose-subr-end1 end)
          (transpose-subr-start2 sec-start)
          (transpose-subr-end2 sec-end))
      (transpose-subr-1))
    (delete-overlay mouse-secondary-overlay)
    (setq mouse-secondary-overlay nil)))

;;;###autoload
(defun bn-keyboard-quit ()
  "Deactivate secondary region, deactivate region, or perform quit.
If the secondary region is active, then deactivate it.  If not, then if
the region is active, then deactivate it.  If not, then do
`keyboard-quit'."
  (interactive)
  (cond ((and (overlayp mouse-secondary-overlay)
              (overlay-buffer mouse-secondary-overlay))
         (delete-overlay mouse-secondary-overlay))
        ((and (boundp 'mark-active)
              mark-active)
         (deactivate-mark))
        (t
         (keyboard-quit))))

;;}}}
;;{{{ fill-common-prefix-region

(autoload 'cl-flet "cl"
  nil ;; not interactive
  'macro)

;;;###autoload
(defun fill-common-prefix-region (&optional justify nosqueeze)
  "Call `fill-region' on the largest region surrounding the
current point within which all line beginnings yield exactly the
same when matched against `adaptive-fill-regexp'.

A line containing nothing but the common prefix and possibly trailing
whitespace is treated as a region boundary.

This is particularly useful for filling subsections of
paragraphs, e.g. email with different levels of nested \"> \"
quoting, where by default emacs considers all the different
levels as part of one big paragraph."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full))))
  (save-excursion
    (beginning-of-line)
    (or (looking-at adaptive-fill-regexp)
        (error "Current line prefix does not match adaptive-fill-regexp"))
    (let* ((common-prefix (match-string 0))
           (common-prefix-nw (concat (regexp-quote common-prefix)
                                     "[^\n]*[^[:space:]\n]")))
      (cl-flet ((find-limit (result-fn buffer-limit-fn line-step)
               (save-excursion
                 (let (result)
                   ;; Keep looking while we see the same fill-prefix
                   ;; followed by some non-whitespace.
                   (while (and (setq result (funcall result-fn))
                               (not (funcall buffer-limit-fn))
                               (forward-line line-step)
                               (looking-at adaptive-fill-regexp)
                               (equal common-prefix (match-string 0))
                               (looking-at common-prefix-nw)))
                   result))))
        (let ((start (find-limit #'point #'bobp -1))
              (end (find-limit #'line-end-position #'eobp 1)))
          (fill-region start end justify nosqueeze))))))

;;}}}

;; FIXME - stuff from as-binding

(global-set-key [(delete)]     'delete-char)        ;; to make sure
(global-set-key [(insert)]     'overwrite-mode)     ;; to make sure

(global-set-key [(meta "\\")]   'fixup-whitespace)
                                ;; was delete-horizontal-space
(autoload 'bn-kill-region-or-backword-word
          "as-editing" "bn-kill-region-or-backword-word" t)
(global-set-key [(control w)]  'bn-kill-region-or-backword-word)
                               ;; was kill-region
(global-set-key [(meta w)]     'bn-kill-line-or-region-save)
                               ;; kill-ring-save
(autoload 'as-transpose-lines
          "as-editing" "as-transpose-lines" t)
(global-set-key [(control x)(control t)] 'as-transpose-lines) ;; was transpose-lines
(autoload 'as-kill-word "as-editing" "as-kill-word" t)
(global-set-key [(meta D)]                'as-kill-word)

(autoload 'bn-zap-nearly-to-char "as-editing" "bn-zap-nearly-to-char" t)
(global-set-key [(meta Z)]                'bn-zap-nearly-to-char)

(global-set-key [(control meta K)]        'as-kill-sexp)

(global-set-key [(control x) (control y)] 'vim-yy)
;;{{{ C-c [a-z][A-Z]

(global-set-key "\C-cA"   'as-align-to-previous-line)
(global-set-key "\C-cd"   'as-duplicate-line)
(global-set-key "\C-cl"   'align)                      ;; new in emacs 21
(global-set-key "\C-cp"   'as-copy-previous-line-suffix)
(global-set-key "\C-cP"   'as-align-to-previous-line)

;;}}}
;;{{{ Function keys f5--f9 (no modifiers)

(global-set-key [(f5)] 'as-duplicate-line)
(global-set-key [(f7)] 'as-align-to-previous-line)
(global-set-key [(f8)] 'as-copy-previous-line-suffix)

;;}}}

(global-set-key [(control meta y)]        'as-join-line-with-next)
(global-set-key [(control meta \()]       'bn-strip-parentheses)
(global-set-key [(control c) tab]         'indent-region)

;; This one might get overridden by per-mode bindings:
(global-set-key [(control meta q)]        'fill-common-prefix-region)
;; but this one won't, so serves as a backup:
(global-set-key [(control c)(meta q)]     'fill-common-prefix-region)

(global-set-key [(control x) ?8 ?e]
                (lambda ()
                  (interactive)
                  (ucs-insert (cdr (assoc-string "EURO SIGN" (ucs-names))))))
