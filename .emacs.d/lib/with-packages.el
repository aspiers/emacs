(defmacro with-packages (when &rest args)
  "Allow declaration of `use-package' config which is only
triggered when a required set of packages are loaded.  The
required set is defined by the `when' argument, whose value is
exactly the same format as with the `:after' argument to
`use-package'.

The `args' are passed straight to `use-package' for use as normal.

Example usage:

  (with-packages (org counsel)
    :bind (:map org-mode \"C-c C-j\" . counsel-org-goto))"
  (let ((pseudo-pkg-name
         (format
          "*with-packages/%s"
          (let ((s (prin1-to-string when)))
            (dolist (repl '(("(\\(.+\\))" . "\\1")
                            (" " . "-")
                            (":any" . "any")
                            (":all" . "all"))
                          s)
              (let ((from (car repl))
                    (to (cdr repl)))
                (setq s (replace-regexp-in-string from to s))))))))
    (message pseudo-pkg-name)
    `(use-package ,pseudo-pkg-name
       :no-require t
       :ensure nil
       :straight nil
       :after when
       ,@args)
    (message "use-package done for %s" pseudo-pkg-name)))

(put 'with-packages 'lisp-indent-function 'defun)

(provide 'with-packages)
