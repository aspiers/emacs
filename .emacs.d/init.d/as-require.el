(defun as-soft-require (feature &optional filename)
  "Same as `require', but only causes a warning message rather
than an error if the feature fails to load.  Returns the feature
if the feature loaded, otherwise nil.

The problem with this approach is that if it is used to replace a
call to `require' in the top-level of a file, the magic property
of `require', as described in (elisp) Named Features, is lost:

   When `require' is used at top level in a file, it takes effect
   when you byte-compile that file (*note Byte Compilation::) as
   well as when you load it.  This is in case the required
   package contains macros that the byte compiler must know
   about.  It also avoids byte compiler warnings for functions
   and variables defined in the file loaded with `require'.

If the `require' was already going to be called from a lower
level (e.g. if the require was conditional based on something
like as-quick-startup), then the benefits of the magic property
could not be reaped anyway.

In all other cases, the workaround is to call the require as
normal, and then call `as-check-feature-loaded' to issue a
warning if it failed."

  ;; Unfortunately this loss of the magic property of `require' cannot
  ;; be avoided via defmacro, because macro expansion passes a single
  ;; form to the compiler, so performing anything in addition to the
  ;; `require' would prohibit it from being at the top level; the
  ;; macro expansion would have to look something like this:
  ;;
  ;;    (defun as-soft-require (feature &optional; filename)
  ;;       (progn
  ;;         (require feature filename 'noerror)
  ;;         (as-check-feature-loaded feature)))
  ;;
  ;; and then the require would not be at the top-level.
  (require feature filename 'noerror)
  (as-check-feature-loaded feature))

(defun as-check-feature-loaded (feature)
  "Check whether FEATURE is loaded: if so, returns it, otherwise
issues a warning and returns nil."
  (if (featurep feature)
      feature
    (message "Warning: failed to load %s" feature)
    nil))

(provide 'as-require)
