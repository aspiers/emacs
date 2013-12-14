(defun as-quick-startup nil
  "Non-nil if the current emacs was required to start up quickly."
  (getenv "QUICK_EMACS"))
