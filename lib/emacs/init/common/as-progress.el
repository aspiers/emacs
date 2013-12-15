;; Utility functions to display timestamped messages as each init file
;; gets loaded, and as significant milestones within each init file
;; are reached.  The timestamp is relative to a clock which is started
;; when this file is loaded.  This makes it easy to see if any init
;; file is taking a significant amount of time to load.

(defvar as-progress-clock (float-time)
  "Internal wallclock for `as-progress' so that it can function
as an extremely primitive profiler.")

(defun as-progress-message (msg &rest args)
  "Output a message with a timestamp relative to the progress clock."
  (let* ((now (float-time))
         (delta (- now as-progress-clock))
         (as-progress-clock now))
    (message "[+%.3f] %s" delta (if args (format msg args) msg))))

(defun as-progress (msg &rest args)
  "Display progress during loading of init files."
  (let*
      ;; Can't find a way of auto-detecting source file :-(
      ;; This doesn't work for byte-compiled functions:

;;       ((caller-function (cdr (backtrace-frame 3)))
;;        (caller-filename (symbol-file caller-fn)))

      ;; Maybe we could use defadvice to `put' the originating
      ;; filename of a byte-compiled function into its symbol-plist
      ;; immediately after compilation?

      ;; Anyway, for now, we cop out.
      ((caller-filename (abbreviate-file-name load-file-name)))
    (as-progress-message
     (concat caller-filename ": " (if args (format msg args) msg)))))

(defun as-loading-progress ()
  "Display progress of loading of init files."
  (as-progress-message "loading %s ..." load-file-name))

(provide 'as-progress)
