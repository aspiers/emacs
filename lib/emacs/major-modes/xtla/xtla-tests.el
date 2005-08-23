;;; xtla-tests.el --- unit tests for xtla.el

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Modified by: Mark Triggs <mst@dishevelled.net>

;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a test framework and a set of testcase for xtla.

;; Any user is welcome to run M-x tla-tests-batch RET, and send the
;; output in case a test fails. One can also run a particular testcase
;; with M-x tla-tests-run RET

;; xtla developers are strongly encourraged to write new testcases.
;; Doing so is rather simple :
;;
;; 1) write a function, and name it `tla-test-...'. The function must
;; raise an error when the test fails. A few functions are provided to
;; the test writers. Please refer to their docstrings for details:
;; `tla-tests-log' => write a message to the log buffer
;; `tla-tests-buffer-nonreg' => compares a buffer with the previous
;;                              execution of the test.
;;
;; 2) Add the parameters of the testcases to the alists
;; `tla-tests-command-alist' and `tla-tests-init-alist'. The first one
;; gives the expected list of tla commands to be ran. The second one
;; says how the testcase should be ran.


;;; History:
;;
;; Created in September 2004 after a discussion on IRC

;;; Code:

;;;###autoload
(eval-when-compile (require 'cl))
(require 'xtla)


;;
;; xtla-tests parameters
;;
(defvar tla-tests-scratch-dir (expand-file-name "~/tmp/arch-test")
  "Directory where the test can write.

WARNING: This directory will be deleted before each test.")

(defvar tla-tests-archive-location
  (concat tla-tests-scratch-dir "/archive")
  "Location of the archive used for xtla testing.

Must be a subdir of `tla-tests-scratch-dir'.")

(defvar tla-tests-wd-location
  (concat tla-tests-scratch-dir "/wd")
  "Location of a possible working directory used for xtla testing.
Must be a subdir of `tla-tests-scratch-dir'.")


(defvar tla-tests-log-buffer nil
  "Buffer where the tests will output messages.")

(defvar tla-tests-archive-name "foo@bar.com--2004"
  "The name of the test archive to use.")

(defvar tla-tests-project-name "xtla--test--1.0"
  "The name of the test project to use.")

;;
;; Testcase parameters
;;

(defconst tla-tests-command-alist
  `((tla-test-my-id "tla my-id" "tla my-id"
                    "tla my-id John\\ Smith\\ \\<john\\@smith.com\\>"
                    "tla my-id")
    (tla-test-make-archive
     ,(concat "tla make-archive foo\\@bar.com--2004 "
              tla-tests-archive-location)
     "tla archives" "tla my-default-archive")
    (tla-test-changes-what-changed-original-file)
    (tla-test-changes "tla inventory --nested --trees"
                      "tla inventory --nested --trees"
                      "tla changes --diffs" "tla changes --diffs"))
  "List of tla commands that should be executed by each test."
  )

(defconst tla-tests-init-alist
  '((tla-test-my-id noid noarch noproject)
    (tla-test-make-archive noarch noproject)
    (tla-test-changes-what-changed-original-file noid noarch noproject)
    (tla-test-changes)
    (tla-test-revision-lessp noid noarch noproject)
    )
  "Alist used by the initialization phase of each test.

Each element must be of the form (testcase list-of-features). The list
of feature can contain the symbols

 * noid: Don't fix tla my-id

 * noarch: Don't create an archive

 * noproject: Otherwise, create a project in the archive with a base-0
and a patch-1

 * get: Runs tla get on the project in the archive TODO

 * changes: do some modifications in the working directory after tla
get TODO")

;;
;; Functions to run tests
;;
;;;###autoload
(defun tla-tests-batch ()
  "Run all the available test-cases in batch mode."
  (interactive)
  (tla-tests-log "***************************")
  (tla-tests-log "* Starting new batch test *")
  (tla-tests-log "***************************")
  (let ((failed 0)
        (ok 0)
        (list-tests (apropos-internal "^tla-test-" 'fboundp)))
    (while list-tests
      (if (tla-tests-run (car list-tests))
          (setq ok (1+ ok))
        (setq failed (1+ failed)))
      (setq list-tests (cdr list-tests)))
    (tla-tests-log "**********************")
    (tla-tests-log "* Batch test report: *")
    (tla-tests-log "*  Passed: %3d       *" ok)
    (tla-tests-log "*  Failed: %3d       *" failed)
    (tla-tests-log "**********************")
    ))

(defun tla-tests-log (message &rest format-params)
  "Logs the message (format MESSAGE FORMAT-PARAMS).

Log messages are written to the tests log buffer."
  (unless (buffer-live-p tla-tests-log-buffer)
    (setq tla-tests-log-buffer (get-buffer-create "*tla-tests*")))
  (let ((message (apply 'format message format-params)))
    (with-current-buffer tla-tests-log-buffer
      (goto-char (point-max))
      (insert message)
      (newline)
      (message message))))

(defmacro tla-write-to-file (filename &rest forms)
  "In buffer visiting FILENAME, evaluate FORMS, save and kill the buffer."
  (let ((buf (gensym)))
    `(let ((,buf (find-file-noselect ,filename)))
       (unwind-protect
           (with-current-buffer ,buf
             ,@forms
             (save-buffer))
         (kill-buffer ,buf)))))
(put 'tla-write-to-file 'lisp-indent-function 1)

(defun tla-tests-make-dummy-project ()
  "Create a dummy project, import and commit it to the archive."
  (with-temp-buffer
    (cd tla-tests-scratch-dir)
    (make-directory tla-tests-project-name)
    (cd tla-tests-project-name)
    (tla--run-tla-sync (list "init-tree"
                             (format "%s/%s"
                                     tla-tests-archive-name
                                     tla-tests-project-name)))
    (tla--run-tla-sync (list "import" "--setup"))
    (tla-write-to-file "hello" (insert (concat "Current time is "
                                               (current-time-string))))
    (tla-add nil "hello")
    (tla--run-tla-sync (list "commit" "-L" "Test commit"))
    default-directory))


(defvar tla-tests-real-home (getenv "HOME"))

(defun tla-tests-initialize (tfeatures)
  "Initialization function called before launching a testcase.

FEATURES is the list of features got from `tla-tests-init-alist'."
  (tla-sethome tla-tests-scratch-dir)
  (shell-command (concat "rm -rf " tla-tests-scratch-dir))
  (shell-command (concat "mkdir -p " tla-tests-scratch-dir))
  (condition-case err
      (progn
        (unless (member 'noid tfeatures)
          (tla-my-id 1 "Xtla tester <bogus@email.org>"))
        (unless (member 'noarch tfeatures)
          (tla--make-archive tla-tests-archive-name tla-tests-archive-location)
          (tla-my-default-archive tla-tests-archive-name))
        (unless (member 'noproject tfeatures)
          (cd (tla-tests-make-dummy-project)))
        (tla-clear-log-buffer))
    (error
     (tla-tests-terminate)
     (error (cadr err)))))

(defun tla-tests-terminate ()
  "Terminates the execution of a testcase and restores HOME."
  (interactive)
  (tla-sethome tla-tests-real-home))

(defun tla-tests-wait-end-of-process ()
  "Waits for all asynchronous tla processes to terminate."
  (while tla-process-running
    (message "Processes: %s" tla-process-running)
    (sit-for 0.2)))


;;;###autoload
(defun tla-tests-run (test)
  "Run the testcase TEST.

Switch HOME to the test directory, clear the log buffer, call the
function TEST, and check that the list of tla commands ran by calling
TEST is the same as the one expected, stored in
`tla-tests-command-alist'"
  (interactive
   (list (intern (completing-read "Test to run: "
                                  (mapcar (lambda (x) (list (symbol-name x)))
                                          (apropos-internal "^tla-test-"))))))
  (let ((default-directory tla-tests-scratch-dir)
        (init-features (cdr (assoc test tla-tests-init-alist))))
    (with-temp-buffer
      (tla-tests-initialize init-features)
      (tla-tests-log "\n*** running test %s\n" (symbol-name test))
      (let ((commands-ok t)
            (errors nil))
        (unwind-protect
            (condition-case condition-error
                (progn
                  (funcall test)
                  (tla-tests-wait-end-of-process)
                  (let ((list-cmds (tla-tests-get-list-cmds)))
                    (unless (equal list-cmds
                                   (cdr (assoc test tla-tests-command-alist)))
                      (tla-tests-log "Different list of commands")
                      (tla-tests-log "Expected: %S"
                                     (cdr (assoc test
                                                 tla-tests-command-alist)))
                      (tla-tests-log "Got:      %S" list-cmds)
                      (setq commands-ok nil))))
              (error (progn (tla-tests-log "Error running tests")
                            (setq errors (or condition-error t)))))
          (tla-tests-terminate))
        (tla-switch-to-buffer tla-tests-log-buffer)
        (tla-tests-log "*** Report for test %s:" (symbol-name test))
        (tla-tests-log "Commands: %s\nErrors: %s"
                       (if commands-ok "OK" "ERROR")
                       (if errors (format "ERROR - %s" errors) "OK"))
        ;; return value
        (and commands-ok (not errors))))))

(defun tla-tests-get-list-cmds ()
  "Get the list of commands ran since the log buffer was cleared.
Returns a list of strings"
  (set-buffer (get-buffer-create tla-log-buffer))
  (goto-char (point-max))
  (let ((list-cmds '()))
    (while (re-search-backward "^Command: " nil t)
      (re-search-forward "^Command: ")
      (setq list-cmds (cons (buffer-substring-no-properties (point)
                                                            (line-end-position))
                            list-cmds))
      (previous-line 1))
    list-cmds
    ))

(defvar tla-tests-nonreg-dir
  (expand-file-name
   (concat (file-name-directory (locate-library "xtla"))
           "../tests"))
  "Directory where non-regression tests should be stored.")

(defun tla-tests-buffer-nonreg (buffer id)
  "Perform a non-regression script on BUFFER.

When called for the first time, stores the content of BUFFER in
`tla-tests-nonreg-dir'/ID.txt. Afterwards, compares the content of
BUFFER with the previously archived one. Raise an error when there is
a difference."
  (make-directory tla-tests-nonreg-dir t)
  (let ((filename (concat (file-name-as-directory
                           tla-tests-nonreg-dir)
                          id ".txt")))
    (with-current-buffer buffer
      (if (file-exists-p filename)
          (progn
            (let ((old (concat
                        (tla--strip-final-newline
                         (with-current-buffer (find-file-noselect
                                               filename)
                           (buffer-string)))
                        "\n"))
                  (new (concat
                        (tla--strip-final-newline
                         (replace-regexp-in-string
                          (regexp-quote (getenv "HOME")) "$HOME"
                          (buffer-string)))
                        "\n")))
              (if (string= old new)
                  (progn (tla-tests-log "non-reg %s OK" id))
                (tla-tests-log "Non regression failed for %s failed" id)
                (tla-tests-log "Expected:\n\"%s\"\n" old)
                (tla-tests-log "Got:\n\"%s\"\n" new)
                (error "Non regression failed"))))
        (let ((content (buffer-string)))
          (with-current-buffer (get-buffer-create " *tla-tmp*")
            (erase-buffer)
            (insert content)
            (goto-char (point-min))
            (while (search-forward (getenv "HOME") nil t)
              (replace-match "$HOME" nil t))
            (tla-tests-log "Archiving %s for non-regression." id)
            (tla-tests-log "please check %s for errors." filename)
            (write-file filename)
            (kill-buffer (current-buffer))
            t))))))


;;
;; Testcases
;;

(defun tla-test-my-id ()
  "Test that my-id works correctly."
  (ignore-errors (tla-my-id))
  (flet ((read-string (prompt x y z)
                      "John Smith <john@smith.com>"))
    (tla-my-id t))
  (unless (string= (tla-my-id)
                   "John Smith <john@smith.com>")
    (error "Wrong id"))
  )

(defun tla-test-make-archive ()
  "Test that make-archive works correctly."
  (tla--make-archive "foo@bar.com--2004" tla-tests-archive-location)
  (unless (file-directory-p tla-tests-archive-location)
    (error "Archive not created"))
  (tla-archives)
  (tla-tests-log "archive created. Testing tla-archives.")
  (tla-tests-buffer-nonreg (current-buffer) "make-archive-archives"))

(defun tla-test-changes-what-changed-original-file ()
  "Test that changes-what-changed-original-file correctly."
  (let ((what-changed
         "/home/jet/projects/pook/,,what-changed.pookx--prototype--0.1--base-0--jet@gyve.org--test/new-files-archive/./pook.h"))
    (unless (equal (expand-file-name "/home/jet/projects/pook/pook.h")
                   (expand-file-name (tla--changes-what-changed-original-file
                                      what-changed)))
      (error "Unexpected file name is returned"))))

(defun tla-test-changes ()
  "Test that tla-changes runs correctly."
  (tla-changes)
  (tla-tests-wait-end-of-process)
  (tla-tests-buffer-nonreg (current-buffer) "changes-nochange"))

(defun tla-test-name-split-construct ()
  "Check that `tla--name-split' and `tla--name-construct' works."
  (let ((name-alist
         '(("archive@name--year"
            ("archive@name--year" nil nil nil nil))
           ("archive@name--year/category"
            ("archive@name--year" "category" nil nil nil))
           ("archive@name--year/category--branch"
            ("archive@name--year" "category" "branch" nil nil))
           ("archive@name--year/category--1"
            ("archive@name--year" "category" "" "1" nil))
           ("archive@name--year/category--1.0--patch-42"
            ("archive@name--year" "category" "" "1.0" "patch-42"))
           ("archive@name--year/category--branch"
            ("archive@name--year" "category" "branch" nil nil))
           ("archive@name--year/category--branch--1.0"
            ("archive@name--year" "category" "branch" "1.0" nil))
           ("archive@name--year/category--branch--1.0--version-0"
            ("archive@name--year" "category" "branch" "1.0"
             "version-0")))))
    (dolist (pair name-alist)
      (unless (equal (car pair) (tla--name-construct (cadr pair)))
        (error "Bug in tla--name-construct"))
      (unless (equal (tla--name-split (car pair)) (cadr pair))
        (error "Bug in tla--name-construct")))))

(defun tla-test-revision-lessp ()
  "Checks that `tla-revision-lessp' works."
  (let ((rev-alist
         '(("archive@name--year/cat--br--0--patch-3"
            "archive@name--year/cat--br--0--patch-12")
           ("archive@name--year/cat--br--0--patch-3"
            "archive@name--year/cat--br--1--patch-1")
           ("base-0" "patch-1")
           ("patch-1" "version-0")
           ("patch-1" "version-1")
           ("version-1" "version-2")
           ("12" "13")
           ("12x" "12y")
           ("a1y" "a2y")
           ("a12x" "ax")
           ("aa" "aaa")
           ("babbb" "bb"))))
    (dolist (pair rev-alist)
      (unless (tla-revision-lessp (car pair) (cadr pair))
        (error "Bug in (tla-revision-lessp %S %S)" (car pair) (cadr pair)))
      (when (tla-revision-lessp (cadr pair) (car pair))
        (error "Bug in (tla-revision-lessp %S %S)" (cadr pair) (car pair))))))

(provide 'xtla-tests)
;; arch-tag: 4fce7821-79bb-449e-aeba-221f8ac65cc3
;;; xtla-tests.el ends here
