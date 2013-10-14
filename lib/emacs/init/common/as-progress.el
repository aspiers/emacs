(defvar as-progress-clock (float-time)
  "Internal wallclock for `as-progress' so that it can function
as an extremely primitive profiler.")

(defun as-progress (msg &rest args)
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
      ((caller-filename "as-init")
       (now (float-time))
       (delta (- now as-progress-clock))
       (as-progress-clock now)
       )
    (message "[+%.3f] %s: %s" delta caller-filename
             (if args (format msg args) msg))))

(provide 'as-progress)
