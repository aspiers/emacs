(defmacro define-find-file-in-dir-function (name dir &optional prompt)
  "Defines function which invokes equivalent of `ido-find-file'
  from within a given directory."
  `(defun ,name ()
    (format "Uses ido to find a file within %s.

This function was defined via `define-find-file-in-dir-function',
and invokes `ido-file-internal'." ,dir)
    (interactive)
    (ido-file-internal ido-default-file-method
                       nil ,dir ,(or prompt "Find file: "))))

(provide 'as-find-file-in-dir)
