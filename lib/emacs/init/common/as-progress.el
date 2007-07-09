(defun as-progress (message)
  "Display progress of loading of init files."
  (let*
      ;; Can't find a way of auto-detecting source file :-(
      ;; This doesn't work for byte-compiled functions:

;;       ((caller-function (cdr (backtrace-frame 3)))
;;        (caller-filename (symbol-file caller-fn)))

      ;; Maybe we could use defadvice to `put' the originating
      ;; filename of a byte-compiled function into its symbol-plist
      ;; immediately after compilation?

      ;; Anyway, for now, we cop out.
      ((caller-filename "as-init"))
    (message (concat caller-filename ": " message))))

(provide 'as-progress)
