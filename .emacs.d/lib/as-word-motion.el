;; Word motion

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

;; Sexp motion

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


(provide 'as-word-motion)
