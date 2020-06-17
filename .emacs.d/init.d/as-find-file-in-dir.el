(defmacro define-find-file-in-dir-function (name dir &optional prompt)
  "Defines function which invokes equivalent of `ido-find-file'
  from within a given directory."
  `(defun ,name ()
    (format "Uses ido to find a file within %s.

This function was defined via `define-find-file-in-dir-function',
and invokes `ido-file-internal'." ,dir)
    (interactive)
    (counsel--find-file-1 ,(or prompt "Find file: ")
                          ,dir #'counsel-find-file-action
                          'counsel-find-file)))

(provide 'as-find-file-in-dir)
