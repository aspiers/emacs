(use-package dash)
(use-package hydra)
(use-package s)

(use-package profiler
  :ensure nil
  :after (dash hydra s)
  :config
  (defun profiler-running-modes ()
    (let ((running-modes
           (-non-nil (list (if (profiler-cpu-running-p) "cpu")
                           (if (profiler-memory-running-p) "mem")))))
      (if running-modes
          (s-join "+" running-modes)
        "stopped")))

  (defhydra hydra-profiler
    (:color red)
    "
elisp profiling (currently %s(profiler-running-modes))

"
    ("s" profiler-start "start (prompt for mode)" :column "Start / stop")
    ("c" (profiler-start 'cpu) "start CPU profiling")
    ("m" (profiler-start 'mem) "start memory profiling")
    ("b" (profiler-start 'cpu+mem) "start both CPU+memory profiling")
    ("." profiler-stop "stop")
    ("R" profiler-reset "Reset")
    ("q" nil "Cancel")
    ("r" profiler-report "report" :column "Reporting" :color blue)
    ("f" profiler-find-profile "find profile")
    ("4" profiler-find-profile-other-window "find profile other window")
    ("5" profiler-find-profile-other-frame  "find profile other frame"))

  :bind (("C-c P" . hydra-profiler/body)
         :map profiler-report-mode-map
         ("." . profiler-stop)))

(provide 'as-profiling)
