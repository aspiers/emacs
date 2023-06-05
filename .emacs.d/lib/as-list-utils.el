(defun list-has-prefix (l p)
  "Return t if list `l' starts with the prefix list `p'."
  (cond ((null p) t)
        ((null l) nil)
        ((equal (car l) (car p)) (list-has-prefix (cdr l) (cdr p)))
        nil))

(provide 'as-list-utils)
