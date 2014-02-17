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

(provide 'bn-kill-stuff)
