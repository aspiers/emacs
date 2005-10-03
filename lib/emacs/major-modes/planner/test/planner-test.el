;;; Test cases for planner
;;; Sacha Chua (sacha@free.net.ph)
;; Version: 2005.08.20-17.59-stable

;; Just load this and complain loudly if there are any errors.
;; It only tests a handful of functions right now.

(require 'emacs-wiki)
(require 'planner)

(defun planner-test (test-name test-cases test-function)
  (while test-cases
    (apply test-function test-name (car test-cases))
    (setq test-cases (cdr test-cases))))

(planner-test
 "planner-task-info-from-string"
 '(("2004.03.01" "#A0 _ Test this function"
    ("2004.03.01" "A" "0" "_" "Test this function" nil nil nil "2004.03.01"))
   ("2004.03.01" "#A0 _ Test this function (PlanPage)"
    ("2004.03.01" "A" "0" "_" "Test this function" "PlanPage" "PlanPage" "PlanPage" "2004.03.01"))
   ("2004.03.01" "#A0 _ Test this function ([[PlanPage]])"
    ("2004.03.01" "A" "0" "_" "Test this function" "PlanPage" "[[PlanPage]]" "PlanPage" "2004.03.01"))
   ("2004.03.01" "#A0 _ Test this function ([[SomePage][Really]])"
    ("2004.03.01" "A" "0" "_" "Test this function" "SomePage" "[[SomePage][Really]]" "SomePage" "2004.03.01"))
   ("2004.03.01" "#A0 _ Test this function (NotPlanPage) junk"
    ("2004.03.01" "A" "0" "_" "Test this function (NotPlanPage) junk" nil nil nil "2004.03.01"))
   ("2004.03.01" "#A0 _ Test this function (NotPlanPage) (PlanPage)"
    ("2004.03.01" "A" "0" "_" "Test this function (NotPlanPage)" "PlanPage" "PlanPage" "PlanPage" "2004.03.01"))
   ("PlanPage" "#A0 _ Test this function"
    ("PlanPage" "A" "0" "_" "Test this function" nil nil "PlanPage" nil))
   ("PlanPage" "#A0 _ Test this function (2004.03.01)"
    ("PlanPage" "A" "0" "_" "Test this function" "2004.03.01" "2004.03.01" "PlanPage" "2004.03.01"))
   ("PlanPage" "#A0 _ Test this function (2004.03.01) junk"
    ("PlanPage" "A" "0" "_" "Test this function (2004.03.01) junk" nil nil "PlanPage" nil))
   ("PlanPage" "#A0 _ Test this function (2004.03.02) (2004.03.01)"
    ("PlanPage" "A" "0" "_" "Test this function (2004.03.02)" "2004.03.01" "2004.03.01" "PlanPage" "2004.03.01"))
   ("PlanPage" "#B1 X Completed task"
    ("PlanPage" "B" "1" "X" "Completed task" nil nil "PlanPage" nil))
   ("PlanPage" "#B1 X Completed task   "
    ("PlanPage" "B" "1" "X" "Completed task" nil nil "PlanPage" nil))
   ("2004.03.01" "#A _ No priority"
    ("2004.03.01" "A" nil "_" "No priority" nil nil nil "2004.03.01")))
 (lambda (test-name page-name string expected-result)
   (let ((output (planner-task-info-from-string page-name string)))
     (unless (equal output expected-result)
       (error "%s: output %s expected %s"
              test-name output expected-result)))))


(planner-test
 "planner-format-task"
 '(("2004.03.01" "#A0  _ Test this function"
    ("2004.03.01" "A" "0" "_" "Test this function" nil nil nil "2004.03.01"))
   ("2004.03.01" "#A0  _ Test this function (PlanPage)"
    ("2004.03.01" "A" "0" "_" "Test this function" "PlanPage" "PlanPage" "PlanPage" "2004.03.01"))
   ("2004.03.01" "#A0  _ Test this function (PlanPage)"
    ("2004.03.01" "A" "0" "_" "Test this function" "PlanPage" "[[PlanPage]]" "PlanPage" "2004.03.01"))
   ("2004.03.01" "#A0  _ Test this function ([[SomePage][Really]])"
    ("2004.03.01" "A" "0" "_" "Test this function" "SomePage" "[[SomePage][Really]]" "SomePage" "2004.03.01"))
   ("2004.03.01" "#A0  _ Test this function (NotPlanPage) junk"
    ("2004.03.01" "A" "0" "_" "Test this function (NotPlanPage) junk" nil nil nil "2004.03.01"))
   ("2004.03.01" "#A0  _ Test this function (NotPlanPage) (PlanPage)"
    ("2004.03.01" "A" "0" "_" "Test this function (NotPlanPage)" "PlanPage" "PlanPage" "PlanPage" "2004.03.01"))
   ("PlanPage" "#A0  _ Test this function"
    ("PlanPage" "A" "0" "_" "Test this function" nil nil "PlanPage" nil))
   ("PlanPage" "#A0  _ Test this function (2004.03.01)"
    ("PlanPage" "A" "0" "_" "Test this function" "2004.03.01" "2004.03.01" "PlanPage" "2004.03.01"))
   ("PlanPage" "#A0  _ Test this function (2004.03.01) junk"
    ("PlanPage" "A" "0" "_" "Test this function (2004.03.01) junk" nil nil "PlanPage" nil))
   ("PlanPage" "#A0  _ Test this function (2004.03.02) (2004.03.01)"
    ("PlanPage" "A" "0" "_" "Test this function (2004.03.02)" "2004.03.01" "2004.03.01" "PlanPage" "2004.03.01"))
   ("PlanPage" "#B1  X Completed task"
    ("PlanPage" "B" "1" "X" "Completed task" nil nil "PlanPage" nil))
   ("2004.03.01" "#A   _ No priority"
    ("2004.03.01" "A" nil "_" "No priority" nil nil nil "2004.03.01")))
 (lambda (test-name page-name expected-result info)
   (with-emacs-wiki-project planner-project
     (let ((output (planner-format-task info)))
       (unless (equal output expected-result)
         (error "%s: output %s expected %s"
                test-name output expected-result))))))

(planner-test
 "planner-install-extra-task-keybindings"
 nil
 (lambda (test-case)
   (with-emacs-wiki-project planner-project
     (let ((temp (copy-keymap planner-mode-map)))
       (planner-install-extra-task-keybindings)
       (setq planner-mode-map temp)))))

(planner-test
 "planner-strip-whitespace"
 '(("Hello world" "Helloworld")
   ("Hello	too" "Hellotoo")
   ("Hello\nthree" "Hellothree"))
 (lambda (test-name string expected-result)
   (let ((output (planner-strip-whitespace string)))
     (unless (equal output expected-result)
       (error "%s: output %s expected %s"
              test-name output expected-result)))))

(provide 'planner-test)
;;;_* Local emacs vars.
;; Local variables:
;; change-log-default-name: "../ChangeLog"
;; End:
;;; planner-test.el ends here
