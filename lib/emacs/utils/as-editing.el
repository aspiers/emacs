;; Adam's editing utilities

;; Should be autoloaded by as-init.el

;;{{{ Word motion

(defun as-forward-next-word (&optional prefix)
  "As `forward-word', but lands at the start of a word not the end."
  (interactive "p")
  (forward-word prefix)
  (forward-word 1)
  (forward-word -1))

(defun as-backward-before-word (&optional prefix)
  "As `backward-word', but lands at the start of a word not the end."
  (interactive "p")
  (forward-word (- prefix))
  (forward-word -1)
  (forward-word 1))

;;}}}
;;{{{ as-duplicate-line

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
  (next-line count) ;; sorry FSF
  )

;;}}}
;;{{{ as-join-line-with-next

(defun as-join-line-with-next (&optional preserve-comment)
  "Joins the current line with the next.  Removes any continuation
backslash from the end of the line, and any comment prefix from the
beginning of the next (unless a prefix argument is given) before
joining."
  (interactive "*P")

  (save-excursion
    (end-of-line)
    (forward-char -1)
    (and (looking-at "\\\\") (delete-char 1))
;;  (message (format "preserve-comment is %s" preserve-comment))
    (unless (or preserve-comment (not comment-start))
        (save-excursion
          (forward-line 1)
          (let ((beg (point)))
            (forward-line 1)
            (condition-case err
                (uncomment-region beg (point))
              (error nil)))))
    (join-line 1)))

;;}}}
;;{{{ as-copy-previous-line-suffix

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

(defun as-transpose-lines (arg)
  "Just like `transpose-lines', but preserves the point's position."
  (interactive "*p")
  (let ((col (current-column)))
    (transpose-lines arg)
    (forward-line (- arg))
    (move-to-column col)))

;;}}}
;;{{{ mark-list

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
;;{{{ Ben's query-replace history advice hack

;; The default behaviour of `query-replace' is slightly
;; annoying.  If we perform a query-replace with the region
;; active, to restrict activity to that region, then want to
;; perform the same query-replace on another region, we can't
;; use `repeat-complex-command' because the evaluated values of
;; `region-beginning' and `region-end' get stored in the command
;; history.  We fix that by defining some advice for the common
;; worker function for all replacement-type functions,
;; `perform-replace'.

(defvar bn-replace-functions-to-fix-up
  (list 'query-replace
        'query-replace-regexp
        'query-replace-regexp-eval
        'map-query-replace-regexp
        'replace-string
        'replace-regexp)
  "List of functions for which the `bn-fix-command-history' advice
will doctor `command-history'.")

(defun bn-safe-region-beginning ()
  "Return the beginning of region, or nil if region not active"
  (if mark-active
      (region-beginning)
    nil))

(defun bn-safe-region-end ()
  "Return the end of region, or nil if region not active"
  (if mark-active
      (region-end)
    nil))

(defadvice perform-replace (after bn-fix-command-history)
  "Fix up the most recent entry in `command-history' so that the
START and END arguments are replaced by calls to
`bn-safe-region-beginning' and `bn-safe-region-end'
respectively.  This allows `repeat-complex-command' to do the
Right Thing with `query-replace' and friends."
  (let ((ch-top (car command-history)))
    ;; This check may be unnecessary, but just in case we get
    ;; called as part of another interactive function, make sure
    ;; we're only changing history for suitable functions.
    (when (memq (car-safe ch-top) bn-replace-functions-to-fix-up)
      (setcar (cddddr ch-top) (list 'bn-safe-region-beginning))
      (setcar (cdr (cddddr ch-top)) (list 'bn-safe-region-end)))))

(ad-activate 'perform-replace)

;;}}}
;;{{{ bn-end-of-line-but-one

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
                          (insert " "))))
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
        (funcall open-remover))
      (when on-close-p
        (save-excursion
          (forward-char 1)
          (forward-sexp -1)
          (funcall open-remover))
        (delete-char 1))
      (setq n (- n 1)))))

;;}}}
;;{{{ bn-kill-region-or-backword-word

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

;;{{{ Ben's secondary selection hacks (not active yet)

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

(define-key global-map [(control hyper ?\ )] 'bn-make-region-into-secondary)
(define-key global-map [(control hyper t)] 'bn-exchange-region-and-secondary)
(define-key global-map [(control g)] 'bn-keyboard-quit)

;;}}}
