;;; Test cases for planner
;;; Sacha Chua (sacha@free.net.ph)
;; Version: 2005.08.20-17.59-stable

;; Just load this and complain loudly if there are any errors.
;; It only tests a handful of functions right now.

(require 'emacs-wiki)
(require 'planner)
(require 'planner-cyclic)
(require 'planner-test)

(planner-test
 "planner-cyclic-generate-task"
 '(("2004.03.09" "#A0 _ Study German" "#A0  _ Study German from 2004.03.09")
   ("2004.03.09" "#A0 _ Study German (GermanStudies)" "#A0  _ Study German from 2004.03.09 (GermanStudies)")
   ("2004.03.09" "#B0 _ Study German (GermanStudies)" "#B0  _ Study German from 2004.03.09 (GermanStudies)"))
 (lambda (test-name date task-string expected-output-after-format)
   (let ((output (planner-format-task (planner-cyclic-generate-task date task-string))))
     (unless (string= output expected-output-after-format)
       (error "%s: output %s expected %s"
              test-name output expected-output-after-format)))))

(provide 'planner-cyclic-test)

;;;_* Local emacs vars.
;; Local variables:
;; change-log-default-name: "../ChangeLog"
;; End:
;;; emacs-wiki-test.el ends here
