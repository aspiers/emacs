(use-package hydra)

(defun set-temp-cursor-color (new-color)
  (when (not (boundp 'original-cursor-color))
    (setq original-cursor-color
          (face-attribute 'cursor :background)))
  (set-cursor-color new-color))

(defun restore-cursor-color ()
  (set-cursor-color original-cursor-color))

(defun hydra-profiler/profiler-start (mode)
  (interactive)
  (set-temp-cursor-color "red")
  (profiler-start mode))

(defun hydra-profiler/profiler-stop ()
  (interactive)
  (restore-cursor-color)
  (profiler-stop))

(use-feature profiler
  :after hydra

  :config
  (require 'dash)
  (require 's)

  (setq profiler-report-cpu-line-format
    '((70 left)
      (24 right ((19 right)
                 (5 right)))))

  ;; These two stolen from profiler.el.gz - they're commented out for
  ;; some reason.
  (cl-defmacro with-cpu-profiling ((&key sampling-interval) &rest body)
    `(unwind-protect
         (progn
           (ignore (profiler-cpu-log))
           (profiler-cpu-start ,sampling-interval)
           ,@body)
       (profiler-report)
       (profiler-cpu-stop)))

  (defmacro with-memory-profiling (&rest body)
    `(unwind-protect
         (progn
           (ignore (profiler-memory-log))
           (profiler-memory-start)
           ,@body)
       (profiler-report)
       (profiler-memory-stop)))

  (defun profiler-running-modes ()
    (let ((running-modes
           (-non-nil (list (if (profiler-cpu-running-p) "cpu")
                           (if (profiler-memory-running-p) "mem")))))
      (if running-modes
          (s-join "+" running-modes)
        "stopped")))

  (defhydra hydra-profiler
    (:color red :hint nil)
    "
elisp profiling (currently %s(profiler-running-modes))

^^Start / stop                          Reporting
^-^----------------------------------   ^-^----------------------------
_s_: start (prompt for mode)            _r_: show report
_c_: start CPU profiling
_m_: start memory profiling             _f_: find profile
_b_: start both CPU+memory profiling    _4_: find profile other window
_._: stop profiling                     _5_: find profile other frame
_R_: reset profiler logs

_q_: quit
_C_: customize profiler options
"
    ("s" hydra-profiler/profiler-start)
    ("c" (hydra-profiler/profiler-start 'cpu))
    ("m" (hydra-profiler/profiler-start 'mem))
    ("b" (hydra-profiler/profiler-start 'cpu+mem))
    ("." hydra-profiler/profiler-stop)
    ("R" profiler-reset)
    ("q" nil)
    ("C" (customize-group "profiler"))
    ("r" profiler-report :color blue)
    ("f" profiler-find-profile)
    ("4" profiler-find-profile-other-window)
    ("5" profiler-find-profile-other-frame))

  :bind (("C-c P" . hydra-profiler/body)
         :map profiler-report-mode-map
         ("." . profiler-stop)))

;; (when (getenv "EMACS_PROFILE_INIT")
;;   (load "elp")
;;   (elp-instrument-package "blah")

(provide 'as-profiling)
