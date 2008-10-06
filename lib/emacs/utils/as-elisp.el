(autoload 'eldoc-function-arglist "eldoc")

;;;###autoload
(defun function-arg-types (fn)
  "Returns the arguments of FN as a 3 element (MANDATORY OPTIONAL REST).
MANDATORY and OPTIONAL are lists containing the argument symbols; REST
is the symbol which remaining arguments will be assigned to."
  (or (functionp fn)
      (error (format "%s is not a function" fn)))
  (let ((arglist (eldoc-function-arglist fn))
        (mandatory ())
        (optional ())
        rest
        (append-to 'mandatory))
    (or (sequencep arglist)
        (error (format "eldoc-function-arglist failed to retrieve arguments for %s; maybe it's autoloaded or a special form?" fn)))
    (mapc (lambda (arg)
            (cond ((eq arg '&optional)
                   (setq append-to 'optional))
                  ((eq arg '&rest)
                   (setq append-to 'rest))
                  (t
                   (if (eq append-to 'rest)
                       (setq rest arg)
                     (message (format "new arg %s in %s"
                                      arg append-to))
                     (set append-to (append (eval append-to) (list arg)))))))
          arglist)
    (list mandatory optional rest)))

;;;###autoload
(defun function-arity (fn)
  "Return minimum and maximum number of args allowed for FN.
Unlike with `subr-arity', FN does not have to be a built-in function.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a
function with `&rest' args."
  (let* ((arg-types (function-arg-types fn))
         (mandatory (length (car  arg-types)))
         (optional  (length (cadr arg-types)))
         (rest      (cadr (cdr arg-types))))
    (cons mandatory (if rest 'many
                      (+ mandatory optional)))))

;; See `org-no-properties'.
;;;###autoload
(defun remove-all-text-properties (string)
  "Return a copy of STRING with all text properties removed."
  (let ((copy string))
    (set-text-properties 0 (length copy) nil copy)
    copy))

;;;###autoload
(defun string-join (list separator)
  "Takes a list of string and joins them using delimiter."
  (mapconcat 'identity list separator))

;;;###autoload
(defun string-concat (list)
  "Takes a list of strings and joins them."
  (mapconcat 'identity list ""))

(provide 'as-elisp)
