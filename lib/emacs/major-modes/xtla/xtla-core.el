;;; xtla-core.el --- Core of xtla

;; Copyright (C) 2003-2004 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

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

;; This file provides the low-level functions used by xtla.el


;;; History:

;; This file was created to split out some commonly-used functionality.

;;; Code:

(require 'xtla-defs)
(require 'ewoc)

;; ----------------------------------------------------------------------------
;; Compatibility stuff
;; ----------------------------------------------------------------------------
(eval-when-compile
  (require 'cl)
  (if (featurep 'xemacs)
      (require 'xtla-xemacs)
    (require 'xtla-emacs)))

(require 'pp)

(defvar tla--buffer-type-alist
  '((changes   "*tla-changes*"   root)
    (inventory "*tla-inventory*" path)
    (missing   "*tla-missing*"   single)
    (cat-log   "*tla-cat-log(%s)*" string)
    (file-diff "*tla-file-diff*" path)
    (changelog "*tla-changelog*" root)
    (tree-lint "*tla-tree-lint*" root)
    (logs      "*tla-logs*"      root)
    (errors    "*tla-error*"     multiple)
    (generic   "*tla-process*"   multiple)
    (browse    "*tla-browse*"    single)
    (changeset "*tla-changeset(%s)*" string))
  "List of (type name mode) used to generate a name for a buffer.

TYPE is the type of buffer to create, passed as the first argument to
`tla--get-buffer-create'.

NAME is a string, used as a name for the returned buffer.

MODE is a symbol defining the way to manage (value of
`default-directory' in the created buffer) paths for this type of
buffers. It can have the following values:

 * 'root: `default-directory' will be the tree-root of the specified
    directory.

 * 'path: `default-directory' will be the path specified.

For 'root and 'path, `tla--get-buffer-create' will return the existing
buffer for this type and this path if it exists, or create a new one
otherwise.

 * 'single: There is only one buffer of this type for each Emacs
   instance. If a path is provided, `default-directory' is set to that
   path. Otherwise, the path is left unchanged when a buffer is
   reused, and set to the current directory on buffer creation.

 * 'multiple: `default-directory' is set to the path specified. A new
   buffer is returned anyway. (No buffer reuse).

 * 'string: The path specified is actually a string. It won't be used
   to set `default-directory'. The name of the created buffer will be
   (format name string).")

(defvar tla--buffers-tree nil
  "Tree containing all xtla buffers.

Must be of the form
 ((type1 (\"path1\" buffer \"original name of buffer\")
         (\"path2\" buffer2 \"original name of buffer2\"))
  (type2 (\"path1\" buffer3 \"original name of buffer3\")
         (\"path3\" buffer4 \"original name of buffer4\")))
Used to keep track of all the tla related buffers.")

(defun tla--buffers-tree-remove (buffer)
  "Remove BUFFER from the buffers tree."
    (dolist (type-cons tla--buffers-tree)
      (dolist (path-buffer (cdr type-cons))
        (when (eq (cadr path-buffer) buffer)
          (setcdr type-cons (delete path-buffer (cdr type-cons)))))))

(defun tla--buffers-tree-add (type path buffer)
  "Add a buffer of TYPE visiting PATH to the buffers tree.
BUFFER should be the buffer to add."
  (let ((current-assoc (assoc type tla--buffers-tree)))
    (if current-assoc
        (setcdr current-assoc
                (cons (list path buffer (buffer-name buffer))
                      (cdr current-assoc)))
      (setq tla--buffers-tree
            (cons `(,type (,path ,buffer ,(buffer-name buffer))) tla--buffers-tree)))))

(defvar tla-temp-directory "/tmp" "Temporary directory for some tla operations.")
(defun tla--make-temp-name (file)
  "Generate a temporary file name based on FILE.
The path for the file name can be set via `tla-temp-directory'."
  (make-temp-name (concat (tla--uniquify-file-name tla-temp-directory) file)))

(defun tla--uniquify-file-name (path)
  "Return a unique string designating PATH.
If PATH is a directory,the returned contains one and exactly one trailing
slash.  If PATH is nil, then nil is returned."
  (and path
       (let ((expanded (expand-file-name
                        (if (file-directory-p path)
                            (file-name-as-directory path)
                          path))))
         (if (featurep 'xemacs)
             (replace-regexp-in-string "/+$" "/" expanded)
           expanded))))

(defun tla--config-file-full-path (file &optional create-config-dir)
  "Return the full path for the config file FILE.
FILE will be stored in the `tla-config-directory'.
If CREATE-CONFIG-DIR is non nil, ensure that the `tla-config-directory'
does exist."
  (let ((full-name (tla--uniquify-file-name
                    (concat tla-config-directory file))))
    (unless (file-exists-p tla-config-directory)
      (when create-config-dir
        (make-directory tla-config-directory t)
        (message "The config files of Xtla will be stored in %s!"
                 tla-config-directory)
        (sit-for 5)))
    ;; TODO remove migration code as some time in the future
    (unless (file-exists-p (expand-file-name tla-bookmarks-file-name
                                             tla-config-directory))
      (let ((old-ones (list (list (expand-file-name tla-bookmarks-file-name
                                                    tla-config-directory)
                                  "~/.tla-bookmarks.el"
                                  "~/.xtla/.xtla-bookmarks.el")))
            o olds n)
        (while old-ones
          (setq olds (car old-ones) old-ones (cdr old-ones))
          (if olds (setq n (car olds) olds (cdr olds)))
          (while olds
            (setq o (expand-file-name (car olds)) olds (cdr olds))
            (if (file-exists-p o)
                (if (yes-or-no-p (format "Migrate %s to %s? " o n))
                    (rename-file o n)
                  (if (yes-or-no-p (format "Delete %s? " o))
                      (delete-file o))))))))
    ;; return full-name
    full-name))

(defun tla--get-buffer-create (type &optional path)
  "Get a buffer of type TYPE for the path PATH.

Maybe reuse one if it exists, according to the value of
`tla--buffer-type-alist' (see its docstring), or, call
`create-file-buffer' to create the buffer.

See also `tla--get-buffer'"
  ;; Inspired from `cvs-get-buffer-create'
  (let* ((path (or path default-directory))
         (elem (assoc type tla--buffer-type-alist))
         (mode (caddr elem)))
    (or (tla--get-buffer type path mode)
        ;; Buffer couldn't be reused. Create one
        (let ((path (case mode
                      (root (tla--uniquify-file-name
                             (tla-tree-root path)))
                      (string path)
                      (t (tla--uniquify-file-name path))))
              (name (cadr (assoc type tla--buffer-type-alist))))
          (let ((buffer
                 (if (eq mode 'string)
                     (get-buffer-create (format name path))
                   (let ((default-directory (or path default-directory)))
                     (create-file-buffer (or name "*tla-buffer*"))))))
            (with-current-buffer buffer
              (if (featurep 'xemacs)
                  (tla--install-buffer-menu))
              (tla--buffers-tree-add type path buffer)
              buffer))))))

(add-hook 'kill-buffer-hook 'tla--kill-buffer-function)

(defun tla--kill-buffer-function ()
  "Function run when a buffer is killed."
  (tla--buffers-tree-remove (current-buffer))
  (tla--kill-process-maybe (current-buffer)))

(defun tla--get-buffer (type &optional path mode)
  "Get a buffer of type TYPE for the path PATH.

Maybe reuse one if it exists, depending on the value of MODE (see
`tla--buffer-type-alist' 's third element), otherwise, return nil.  See
also `tla--get-buffer-create'."
  (let ((mode (or mode (caddr (assoc type tla--buffer-type-alist))))
        (path (or path default-directory)))
    (if (eq mode 'single)
        ;; nothing to do about PATH. Reuse anyway
        (let* ((list-path (cdr (assoc type tla--buffers-tree)))
               (first-elem (car list-path)))
          (if list-path
              (if (string= (buffer-name (cadr first-elem))
                           (caddr first-elem))
                  (cadr first-elem)
                (setcdr (assoc type tla--buffers-tree) nil)
                nil)
            nil))
      (let ((path (and path
                       (case mode
                         (root (tla--uniquify-file-name
                                (tla-tree-root path)))
                         (string path)
                         (t (tla--uniquify-file-name path))))))
        (if (eq mode 'multiple)
            ;; no need to search an existing buffer
            nil
          (let* ((list-path (assoc type tla--buffers-tree))
                 (elem (assoc path (cdr list-path)))
                 (buffer (cadr elem)))
            (when buffer
              (if (and (buffer-live-p buffer)
                       ;; the buffer has not been renamed
                       (string= (buffer-name buffer)
                                (caddr elem)))
                  buffer
                ;; remove the buffer and try again
                (setcdr list-path
                        (delq (assoc path (cdr list-path))
                              (cdr list-path)))
                (tla--get-buffer type path mode)))))))))

(defun tla--add-buffer-type (type name)
  "Define a new TYPE of buffer whose buffer will be named NAME."
  (unless (assoc type tla--buffer-type-alist)
    (push (list type name) tla--buffer-type-alist)))

(defun tla--position (item seq)
  "Position of ITEM in list, or nil if not found.
Return 0 if ITEM is the first element of SEQ"
  (let ((pos 0)
        (seq-int seq))
    (while (and seq-int
                (not (eq (car seq-int) item)))
      (setq seq-int (cdr seq-int))
      (setq pos (1+ pos)))
    (when seq-int pos)))


(defun tla--last-visited-inventory-buffer ()
  "Return the last visited xtla's inventory buffer."
  (let ((inventories (remove nil (mapcar
                                  (lambda (elt)
                                    (when (buffer-live-p (cadr elt))
                                      elt))
                                  (cdr (assoc 'inventory tla--buffers-tree)))))
        (bl (buffer-list)))
    (cadr (car (sort inventories (lambda (a b)
                                   (let ((aindex (tla--position (cadr a) bl))
                                         (bindex (tla--position (cadr b) bl)))
                                     (< aindex bindex))))))))

(defun tla-show-inventory-buffer ()
  "Switch to the last visited inventory buffer."
  (interactive)
  (tla-switch-to-buffer (tla--last-visited-inventory-buffer)))

;; Lint trap
;; (eval-when-compile
;;   (unless (fboundp 'read-directory-name)
;;     (defun read-directory-name (&optional a b c d e)
;;       (error "This one should never be called"))))

(defun tla--read-directory-name (prompt &optional dir default-dirname
                                        mustmatch initial)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-DIRNAME if user exits with the same
non-empty string that was inserted by this function.
 (If DEFAULT-DIRNAME is omitted, the current buffer's directory is used,
  except that if INITIAL is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg MUSTMATCH non-nil means require existing directory's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
DIR should be an absolute directory name.  It defaults to
the value of `default-directory'."
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir default-dirname mustmatch initial)
    ;; The same as the definition of `read-directory-name'
    ;; in GNU Emacs in CVS.
    (unless dir
      (setq dir default-directory))
    (unless default-dirname
      (setq default-dirname
            (if initial (concat dir initial) dir)))
    (read-file-name prompt dir default-dirname mustmatch initial)))

(defun tla-sethome (dir)
  "Sets $HOME to DIR, safely.

`setenv' is not sufficient because `abbreviated-home-dir' would then
be incorrectly set, breaking a lot of Emacs function."
  (setenv "HOME" dir)
  (setq abbreviated-home-dir nil))

(eval-and-compile
  (unless (fboundp 'dired-delete-file)
    ;; NOTE: Cut-and-past from CVS Emacs
    ;;
    (defvar dired-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")
    (defun dired-make-relative (file &optional dir ignore)
      "Convert FILE (an absolute file name) to a name relative to DIR.
If this is impossible, return FILE unchanged.
DIR must be a directory name, not a file name."
      (or dir (setq dir default-directory))
      ;; This case comes into play if default-directory is set to
      ;; use ~.
      (if (and (> (length dir) 0) (= (aref dir 0) ?~))
          (setq dir (expand-file-name dir)))
      (if (string-match (concat "^" (regexp-quote dir)) file)
          (substring file (match-end 0))
;;;  (or no-error
;;;(error "%s: not in directory tree growing at %s" file dir))
        file))
    ;; Delete file, possibly delete a directory and all its files.
    ;; This function is useful outside of dired.  One could change it's name
    ;; to e.g. recursive-delete-file and put it somewhere else.
    (defun dired-delete-file (file &optional recursive) "\
Delete FILE or directory (possibly recursively if optional RECURSIVE is true.)
RECURSIVE determines what to do with a non-empty directory.  If RECURSIVE is:
Nil, do not delete.
`always', delete recursively without asking.
`top', ask for each directory at top level.
Anything else, ask for each sub-directory."
      (let (files)
        ;; This test is equivalent to
        ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
        ;; but more efficient
        (if (not (eq t (car (file-attributes file))))
            (delete-file file)
          (when (and recursive
                     (setq files
                           (directory-files file t dired-re-no-dot)) ; Not empty.
                     (or (eq recursive 'always)
                         (yes-or-no-p (format "Recursive delete of %s "
                                              (dired-make-relative file)))))
            (if (eq recursive 'top) (setq recursive 'always)) ; Don't ask again.
            (while files		; Recursively delete (possibly asking).
              (dired-delete-file (car files) recursive)
              (setq files (cdr files))))
          (delete-directory file))))))


(defun tla--add-to-list (list-var element &optional append)
  "Same behavior as GNU Emacs's `add-to-list', but also works on XEmacs.
LIST-VAR is a symbol representing the list to be modified.
ELEMENT is the element to be added to the list.
If APPEND is non-nil, add the item to the end of the list instead of the
front."
  (if (featurep 'xemacs)
      (if append
          (when (not (member element (eval list-var)))
	    (set list-var (append (eval list-var) (list element))))
        (add-to-list list-var element))
    (add-to-list list-var element append)))

;; ----------------------------------------------------------------------------
;; Process buffers
;; ----------------------------------------------------------------------------

;;;###autoload
(defcustom tla-process-buffer " *tla-process*"
  "*Name of the process buffer."
  :type 'string
  :group 'xtla-internal)

;;;###autoload
(defcustom tla-error-buffer " *tla-errors*"
  "*Name of the buffer to which tla's stderr is redirected."
  :type 'string
  :group 'xtla-internal)

(defcustom tla-number-of-dead-process-buffer 20
  "*Number of process buffers to keep after process termination.
When the number of process buffers exceeds this number, the most ancient
is killed.  This includes both the process buffer and the error
buffer (to which stderr is redirected).

A nil value here means \"Never kill any process buffer\". Useful for
debugging, but this will eat the memory of your computer ;-)"
  :type 'integer
  :group 'xtla-internal)

(defcustom tla-show-internal-buffers-on-menu t ; till 1.0
  "Toggle display of dead process buffers in the buffer menu."
  :type 'boolean
  :group 'xtla-internal)

(defvar tla--dead-process-buffer-queue nil
  "List of process buffers belonging to terminated processes.
When the list is greater than `tla-number-of-dead-process-buffer', the last
ones are killed.")

(defun tla--kill-process-buffer (buffer)
  "Don't actually kill BUFFER, but add it to `tla--dead-process-buffer-queue'.
It will eventually be killed when the number of buffers in
`tla--dead-process-buffer-queue'exceeds `tla-number-of-dead-process-buffer'."
  (tla--add-to-list 'tla--dead-process-buffer-queue buffer t)
  (when tla-number-of-dead-process-buffer
    (while (> (length tla--dead-process-buffer-queue)
              (max 2 tla-number-of-dead-process-buffer))
      (kill-buffer (car tla--dead-process-buffer-queue))
      (setq tla--dead-process-buffer-queue
            (cdr tla--dead-process-buffer-queue)))))

(defvar tla--last-process-buffer nil
  "The last created process buffer.")

(defvar tla--last-error-buffer nil
  "The last created process buffer.")

(defun tla--new-process-buffer (to-be-deleted)
  "Create a new process buffer.
If TO-BE-DELETED is non-nil, make this buffer a candidate for eventually
being deleted."
  (let ((buffer (create-file-buffer tla-process-buffer)))
    (setq tla--last-process-buffer buffer)
    (when to-be-deleted (tla--kill-process-buffer buffer))
    buffer))

(defun tla--new-error-buffer (to-be-deleted)
  "Create a new error buffer.
If TO-BE-DELETED is non-nil, make this buffer a candidate for eventually
being deleted."
  (let ((buffer (create-file-buffer tla-error-buffer)))
    (setq tla--last-error-buffer buffer)
    (when to-be-deleted (tla--kill-process-buffer buffer))
    buffer))

;; ----------------------------------------------------------------------------
;; Process management
;; ----------------------------------------------------------------------------

;; Candidates for process handlers
(defun tla--default-error-function (output error status arguments)
  "Default function called when a tla process ends with a non-zero status.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (if (> (with-current-buffer error (point-max)) 1)
      (tla--show-error-buffer error)
    (if (> (with-current-buffer output (point-max)) 1)
        (tla--show-error-buffer output)
      (error "`tla %s' failed with code %d and no output!"
             (mapconcat 'identity arguments " ")
             status)))
  (error "`tla %s' failed with code %d"
         (mapconcat 'identity arguments " ")
         status))

(defvar tla--default-killed-function-noerror 0
  "The number of killed processes we will ignore until throwing an error.
If the value is 0, `tla--default-killed-function' will throw an error.
See `tla--default-killed-function'.")

(defun tla--default-killed-function (output error status arguments)
  "Default function called when a tla process is killed.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (if (> tla--default-killed-function-noerror 0)
      (setq tla--default-killed-function-noerror
            (- tla--default-killed-function-noerror 1))
    (tla-switch-to-buffer error)
    (error "`tla %s' process killed !"
           (mapconcat 'identity arguments " "))))

(defun tla--null-handler (output error status arguments)
  "Handle a finished process without doing anything.
Candidate as an argument for one of the keywords :finished, :error or :killed
in `tla--run-tla-sync' or `tla--run-tla-async'.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  nil)

(defun tla--status-handler (output error status arguments)
  "Return an integer value that reflects the process status.
Candidate as an argument for one of the keywords :finished, :error or :killed
in `tla--run-tla-sync' or `tla--run-tla-async'.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (cond ((numberp status) status)
        ((string-match "^exited abnormally with code \\(.*\\)" status)
         (string-to-int (match-string 1)))
        (t (error status))))

(defun tla--output-buffer-handler (output error status arguments)
  "Return the output of a finished process, stripping any trailing newline.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (tla--buffer-content output))

(defun tla--output-buffer-split-handler (output error status arguments)
  "Return the output of a finished process as a list of lines.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (split-string (tla--buffer-content output) "\n"))

(defun tla--default-finish-function (output error status arguments)
  "Default function called when a tla process terminates.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (with-current-buffer output
    (tla-process-buffer-mode))
  (tla-switch-to-buffer output)
  (message "`tla %s' process finished !"
           (mapconcat 'identity arguments " "))
  status)

(defun tla--finish-function-without-buffer-switch (output error status arguments)
  "Similar to `tla--default-finish-function' but no buffer switch.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (message "`tla %s' process finished !"
           (mapconcat 'identity arguments " "))
  status)

(defvar tla--log-cookie nil)

(defstruct (tla--event) output-buffer error-buffer related-buffer
  command tree event time)

(defun tla--strip-final-newline (string)
  "Strip the final newline from STRING if there's one."
  (if (eq (aref string (- (length string) 1)) ?\n)
      (substring string 0 (- (length string) 1))
    string))

(defsubst tla--log-printer-print-buffer (buffer function)
  "Helper function for `tla--log-printer'.
Print a buffer filed for BUFFER during printing a log event.
The printed name of BUFFER is mouse sensitive.  If the user
clicks it, FUNCTION is invoked."
  (let ((alive-p (buffer-live-p buffer))
        map)
    (tla--face-add
     (or
      ;; pp-to-string is very costly.
      ;; Handle the typical case with hard-coding.
      (unless alive-p "#<killed buffer>")
      ;; Normal case.
      (buffer-name buffer)
      ;; Extra case.
      (pp-to-string buffer))
     'tla-buffer
     (when alive-p
       (setq map (make-sparse-keymap))
       (define-key map [mouse-2] function)
       map)
     nil
     "Show the buffer")))

(defun tla--log-recently-p (elem limit-minute)
  "Check ELEM recorded a recent event or not.
Return nil If ELEM recorded an event older than LIMIT-MINUTE.
Else return t."
  (let* ((recorded (tla--event-time elem))
         (cur      (current-time))
         (diff-minute (/ (+ (* 65536 (- (nth 0 cur)
                                      (nth 0 recorded)))
                          (- (nth 1 cur)
                             (nth 1 recorded)))
                       60)))
    (if (> limit-minute diff-minute)
        t
      nil)))

(defun tla--log-printer (elem)
  "Tla event printer which prints ELEM."
  (let ((event (tla--event-event elem))
        (p (point)))
    (insert
     "Command: " (tla--event-command elem)
     "\nDirectory: " (tla--face-add (tla--event-tree elem)
                                    'tla-local-directory)
     "\nDate: " (format-time-string "%c" (tla--event-time elem))
     "\nRelated Buffer: " (tla--log-printer-print-buffer
                           (tla--event-related-buffer elem)
                           'tla-switch-to-related-buffer-by-mouse)
     "\nOutput Buffer: "  (tla--log-printer-print-buffer
                           (tla--event-output-buffer elem)
                           'tla-switch-to-output-buffer-by-mouse)
     "\nError Buffer: "   (tla--log-printer-print-buffer
                           (tla--event-error-buffer elem)
                           'tla-switch-to-error-buffer-by-mouse)
     (if (not (string= event "started"))
         (concat "\nEvent: " event)
       "")
     "\n")
    ;; Reflect the point to `default-directory'.
    ;; NOTE: XEmacs doesn't have `point-entered' special text property.
    (put-text-property
     p (point)
     'point-entered (lambda (old new)
                      (setq default-directory
                            (tla--event-tree
                             (ewoc-data
                              (ewoc-locate tla--log-cookie))))))))

(defvar tla-process-running nil
  "List of tla processes running.
A value of nil indicates no processes are running.

The list is a list of pairs (process event) where EVENT is the event
corresponding to the beginning of the execution of process.  It can be
used to get more info about the process.")

(defmacro tla--switch-to-buffer-macro (function accessor)
  "Define a FUNCTION for switching to the buffer associated with some event.
ACCESSOR is a function for retrieving the appropriate buffer from a
`tla--event'structure."
  `(defun ,function ()
     "In a log buffer, pops to the output or error buffer corresponding to the
process at point"
     (interactive)
     (let ((buffer (,accessor
                    (ewoc-data (ewoc-locate tla--log-cookie)))))
       (cond ((buffer-live-p buffer)
              (tla-switch-to-buffer buffer)
              (unless (member buffer
                              (mapcar (lambda (p)
                                        (process-buffer (car p)))
                                      tla-process-running))
                (tla-process-buffer-mode)))
             (t (error "Buffer has been killed"))))))

(tla--switch-to-buffer-macro tla-switch-to-output-buffer
                             tla--event-output-buffer)

(tla--switch-to-buffer-macro tla-switch-to-error-buffer
                             tla--event-error-buffer)

(tla--switch-to-buffer-macro tla-switch-to-related-buffer
                             tla--event-related-buffer)

(defmacro tla--make-bymouse-function (function)
  "Create a new function by adding mouse interface to FUNCTION.
The new function is named FUNCTION-by-mouse; and takes one argument,
a mouse click event.
Thew new function moves the point to the place where mouse is clicked
then invoke FUNCTION."
  `(defun ,(intern (concat (symbol-name function) "-by-mouse")) (event)
     ,(concat "`" (symbol-name function) "'" " with mouse interface.")
     (interactive "e")
     (mouse-set-point event)
     (,function)))

(tla--make-bymouse-function tla-switch-to-output-buffer)
(tla--make-bymouse-function tla-switch-to-error-buffer)
(tla--make-bymouse-function tla-switch-to-related-buffer)

(defun tla--log-event (output error command tree event)
  "Log an event in the `tla-log-buffer' buffer.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
COMMAND is the command that was executed.
TREE is the process's working directory.
EVENT is the event that occurred.
Returns that event."
  (unless (and tla--log-cookie
               (buffer-live-p (ewoc-buffer tla--log-cookie)))
    (with-current-buffer (get-buffer-create tla-log-buffer)
      (setq tla--log-cookie
            (ewoc-create 'tla--log-printer))
      (tla-log-buffer-mode)))
  (let ((related-buffer (current-buffer)))
    (with-current-buffer (ewoc-buffer tla--log-cookie)
      (let ((elem (make-tla--event :output-buffer output
                                   :error-buffer error
                                   :related-buffer related-buffer
                                   :command command
                                   :tree tree
                                   :event event
                                   :time (current-time)))
            buffer-read-only)
        (ewoc-enter-last tla--log-cookie elem)
        ;; If an event is too old(30 minutes later since it is recorded),
        ;; throw away.
        (ewoc-filter tla--log-cookie 'tla--log-recently-p 30)
        (ewoc-refresh tla--log-cookie)
        elem))))

(defun tla-log-next ()
  "Move to the next log entry."
  (interactive)
  (let ((next (ewoc-next tla--log-cookie
                         (ewoc-locate tla--log-cookie))))
    (when next (goto-char (ewoc-location next)))))

(defun tla-log-prev ()
  "Move to the previous log entry."
  (interactive)
  (let ((prev (ewoc-prev tla--log-cookie
                         (ewoc-locate tla--log-cookie))))
    (when prev (goto-char (ewoc-location prev)))))

(define-derived-mode tla-log-buffer-mode fundamental-mode "Xtla Log"
  "Major mode for Xtla's internal log buffer. You can open this buffer
with `tla-open-internal-log-buffer'."
  (toggle-read-only 1))

(define-derived-mode tla-process-buffer-mode fundamental-mode
  "Xtla Process"
  "Major mode for process buffers. Mainly defines \\[bury-buffer]
to quit the buffer"
  (tla--install-buffer-menu)
  (toggle-read-only 1))

(defmacro tla--with-keywords (keywords plist &rest body)
  "Execute a body of code with keywords bound.
Each keyword listed in KEYWORDS is bound to its value from PLIST, then
BODY is evaluated."
  (flet ((keyword-to-symbol (keyword)
                            (intern (substring (symbol-name keyword) 1))))
    (let ((keyword (make-symbol "keyword"))
          (default (make-symbol "default")))
      `(let ,(mapcar (lambda (keyword-entry)
                       (keyword-to-symbol (if (consp keyword-entry)
                                              (car keyword-entry)
                                            keyword-entry)))
                     keywords)
         (dolist (keyword-entry ',keywords)
           (let ((,keyword (if (consp keyword-entry)
                               (car keyword-entry)
                             keyword-entry))
                 (,default (if (consp keyword-entry)
                               (cadr keyword-entry)
                             nil)))
             (set (intern (substring (symbol-name ,keyword) 1))
                  (or (cadr (member ,keyword ,plist))
                      ,default))))
         ,@body))))
(put 'tla--with-keywords 'lisp-indent-function 1)

(defun tla--build-tla-command (list-args)
  "Build a shell command to run tla with args LIST-ARGS."
  (mapconcat 'shell-quote-argument
             (cons tla-tla-executable
                   (delq nil list-args))
             " "))


(defcustom tla-password-prompt-regexp
  "[Pp]ass\\(word\\|phrase\\).*:\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process.
This is used by `eshell-watch-for-password-prompt'."
  :type 'regexp
  :group 'xtla)

(defun tla-process-filter (proc string)
  "Filter PROC's STRING.
Prompt for password with `read-passwd' if the output of PROC matches
`tla-password-prompt-regexp'."
  (with-current-buffer (process-buffer proc)
    (insert (replace-regexp-in-string "\015" "\n" string))
    (when (string-match tla-password-prompt-regexp string)
      (string-match "^\\([^\n]+\\)\n*\\'" string)
      (let ((passwd (read-passwd (match-string 1 string))))
        (process-send-string proc (concat passwd "\n"))))))

(defun tla--run-tla-async (arguments &rest keys)
  "Run tla asynchronously.
ARGUMENTS is a list of arguments.  nil values in this list are removed.
KEYS is a list of keywords and values.  Possible keywords are:

 :finished ....... Function run when the process finishes.  If none
                   specified, `tla--default-finish-function' is run.

 :killed ......... Function run when the process is killed.  If none
                   specified, `tla--default-killed-function' is run.

 :error .......... Function run when the process exits with a non 0
                   status.  If none specified,
                   `tla--default-error-function' is run.

All these functions take 4 arguments : output, error, status, and
arguments.

   - \"output\" is the output buffer
   - \"error\" is the buffer where standard error is redirected
   - \"status\" is the numeric exit-status or the signal number
   - \"arguments\" is the list of arguments, as a list of strings,
              like '(\"changes\" \"--diffs\")

   `tla--null-handler' can be used here if there's nothing to do.

 :output-buffer .. Buffer where the output of the process should be
                   redirected.  If none specified, a new one is
                   created, and will be entered in
                   `tla--dead-process-buffer-queue' to be killed
                   later.

 :error-buffer ... Buffer where the standard error of the process
                   should be redirected.

 :related-buffer . Defaults to `current-buffer'.  This is the buffer
                   where the result of the process will be used.  If
                   this buffer is killed before the end of the
                   execution, the user is prompted if he wants to kill
                   the process.

Example:
  (tla--run-tla-async `(\"changes\" ,(unless arg \"--diffs\"))
                      :finished
                      (lambda (output error status arguments)
                        (message \"No changes in this working copy\"))
                      :error
                      (lambda (output error status arguments)
                        (tla-show-changes-buffer output)))"
  (tla--with-keywords
      (:finished :killed :error :output-buffer :error-buffer :related-buffer)
    keys
    (let* ((output-buf (or (and output-buffer (get-buffer-create output-buffer))
                           (tla--new-process-buffer nil)))
           (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                           (tla--new-error-buffer nil)))
           (error-file (tla--make-temp-name "arch-errors"))
           (command (tla--build-tla-command arguments))
           ;; Make the `default-directory' unique. The trailing slash
           ;; may be necessary in some cases.
           (default-directory (tla--uniquify-file-name default-directory))
           (process (start-process
                     tla-tla-executable output-buf
                     "sh" "-c"
                     (format "%s 2> %s"
                             command error-file)))
           (process-event
            (list process
                  (tla--log-event output-buf
                                  error-buf
                                  command
                                  default-directory "started"))))
      (with-current-buffer (or related-buffer (current-buffer))
        (message "running process `%s' in `%s'" command default-directory)
        (add-to-list 'tla-process-running process-event)
        (set-process-filter process 'tla-process-filter)
        (set-process-sentinel
         process
         `(lambda (process event)
            (let ((default-directory ,default-directory))
              (tla--log-event ,output-buf ,error-buf ,command
                              ,default-directory
                              (tla--strip-final-newline event))
              (setq tla-process-running
                    (delq ',process-event tla-process-running))
              (when (file-exists-p ,error-file)
                (with-current-buffer ,error-buf
                  (insert-file-contents ,error-file))
                (delete-file ,error-file))
              (let ((state (process-status process))
                    (status (process-exit-status process)))
                (unwind-protect
                    (cond ((and (eq state 'exit) (= status 0))
                           (funcall (or (quote ,finished)
                                        'tla--default-finish-function)
                                    ,output-buf ,error-buf
                                    status (quote ,arguments)))
                          ((eq state 'signal)
                           (funcall (or (quote ,killed)
                                        'tla--default-killed-function)
                                    ,output-buf ,error-buf status
                                    (quote ,arguments)))
                          ((eq state 'exit) ;; status != 0
                           (funcall (or (quote ,error)
                                        'tla--default-error-function)
                                    ,output-buf ,error-buf status
                                    (quote ,arguments)))))
                ;; Schedule any buffers we created for killing
                (unless ,output-buffer (tla--kill-process-buffer ,output-buf))
                (unless ,error-buffer (tla--kill-process-buffer ,error-buf))))))
        process))))

(defun tla--run-tla-sync (arguments &rest keys)
  "Run tla synchronously.
See `tla--run-tla-async' for details on possible ARGUMENTS and KEYS."
  (tla--with-keywords
      (:finished :killed :error :output-buffer :error-buffer :related-buffer)
    keys
    (let ((output-buf (or (and output-buffer (get-buffer-create output-buffer))
                          (tla--new-process-buffer t)))
          (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                          (tla--new-error-buffer t)))
          (command (tla--build-tla-command arguments))
          (error-file (tla--make-temp-name "arch-errors"))
          ;; Make the `default-directory' unique. The trailing slash
          ;; may be necessary in some cases.
          (default-directory (tla--uniquify-file-name default-directory)))
      (with-current-buffer (or related-buffer (current-buffer))
        (tla--log-event output-buf error-buf command default-directory "started")
        (let ((status (call-process "sh" nil output-buf nil "-c"
                                    (format "%s 2> %s"
                                            command
                                            error-file))))
          (when (file-exists-p error-file)
            (with-current-buffer error-buf
              (insert-file-contents error-file))
            (delete-file error-file))
          (unwind-protect
              (cond ((stringp status)
                     (when (string= status "Terminated")
                       (funcall (or killed 'tla--default-killed-function)
                                output-buf error-buf status arguments)))
                    ((numberp status)
                     (if (zerop status)
                         (funcall (or finished 'tla--default-finish-function)
                                  output-buf error-buf status arguments)
                       (funcall (or error 'tla--default-error-function)
                                output-buf error-buf status arguments)))
                    (t (message "Unknown status - %s" status)))
            ;; Schedule any buffers we created for killing
            (unless output-buffer (tla--kill-process-buffer output-buf))
            (unless error-buffer (tla--kill-process-buffer error-buf))))))))

(defun tla--kill-process-maybe (buffer)
  "Prompts and possibly kill process whose related buffer is BUFFER."
  (let ((process-list nil))
    (dolist (process-buffer tla-process-running)
      (when (eq (tla--event-related-buffer (cadr process-buffer))
                buffer)
        (add-to-list 'process-list (car process-buffer))))
    (let ((l (length process-list)))
      (when (and process-list
                 (y-or-n-p (format "%s process%s running in buffer %s.  Kill %s? "
                                   l (if (> l 1) "es" "")
                                   (buffer-name buffer)
                                   (if (> l 1) "Them" "it"))))
        (dolist (process process-list)
          (setq tla--default-killed-function-noerror
                (1+ tla--default-killed-function-noerror))
          (if (eq (process-status process) 'run)
              (kill-process process)))))))

;;;###autoload
(add-to-list 'minor-mode-alist
             '(tla-process-running
               (:eval (if (equal (length tla-process-running) 1)
                          " Tla running"
                        (concat " Tla running("
                                (int-to-string (length tla-process-running))
                                ")")))))

(defun tla-open-internal-log-buffer ()
  "Switch to the Xtla's internal log buffer.
This buffer contains a list of all the tla commands previously executed.
The buffer uses the mode `tla-log-buffer-mode'"
  (interactive)
  (let ((buffer-name (buffer-name)))
    (tla-switch-to-buffer tla-log-buffer)
    (goto-char (point-max))
    (when (re-search-backward (concat " Buffer: "
                                      (regexp-quote buffer-name)
                                      "$")
                              nil t)
      (tla--flash-line))))

(defun tla-clear-log-buffer ()
  "Kill the log buffer."
  (when (bufferp (get-buffer tla-log-buffer))
    (kill-buffer tla-log-buffer)))

(defun tla--buffer-content (buffer)
  "Return the content of BUFFER as a string.
Strips the final newline if there is one."
  (with-current-buffer buffer
    (buffer-substring-no-properties
     (point-min)
     (progn (goto-char (point-max))
            (if (eq (char-before) ?\n)
                (- (point) 1)
              (point))))))

(defun tla--get-process-output ()
  "Return the content of the last process buffer.
Strips the final newline if there is one."
  (tla--buffer-content tla--last-process-buffer))

(defun tla--get-error-output ()
  "Return the content of the last error buffer.
Strips the final newline if there is one."
  (tla--buffer-content tla--last-error-buffer))

(defvar tla--switched-buffer nil)
(defvar tla--switched-from-buffer nil)

(defun tla-switch-to-buffer (buffer)
  "Switch to BUFFER using the user's preferred method.
See `tla-switch-to-buffer-mode' for possible settings."
  (setq tla--switched-from-buffer (current-buffer))
  (cond ((eq tla-switch-to-buffer-mode 'pop-to-buffer)
         (pop-to-buffer buffer))
        ((eq tla-switch-to-buffer-mode 'single-window)
         (switch-to-buffer buffer))
        ((eq tla-switch-to-buffer-mode 'show-in-other-window)
         (pop-to-buffer buffer)
         (setq tla--switched-buffer (current-buffer))
         (pop-to-buffer tla--switched-from-buffer))
        ;; TODO : dedicated frame.
        (t
         (error "Switch mode %s not implemented" tla-switch-to-buffer-mode))))

(defun tla-post-switch-to-buffer ()
  "Executed when showing a changeset.

If `tla--switched-buffer' is non-nil, show this buffer, but keep
cursor position in previous buffer."
  (when tla--switched-buffer
    (pop-to-buffer tla--switched-buffer)
    (setq tla--switched-buffer nil)
    (goto-char (point-min))
    (pop-to-buffer tla--switched-from-buffer)))


(defun tla-show-process-buffer ()
  "Show the process buffer of the last started tla command."
  (interactive)
  (tla-switch-to-buffer tla--last-process-buffer)
  (unless (member tla--last-process-buffer
                  (mapcar (lambda (p)
                            (process-buffer (car p)))
                          tla-process-running))
    (tla-process-buffer-mode)))

(defun tla--show-last-process-buffer (&optional type mode path)
  "Switch to the last used process buffer in a new buffer of TYPE.
If MODE is specified, it is a function that will be run in the
new buffer.  Otherwise, the buffer will remain in fundamental mode, in
read-only.

If PATH is specified, it will be passed to `tla--get-buffer-create'."
  (when (buffer-live-p tla--last-process-buffer)
    (let ((content (with-current-buffer tla--last-process-buffer
                     (buffer-string))))
      (tla-switch-to-buffer (tla--get-buffer-create
                             (or type 'generic) path))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (if mode
        (funcall mode)
      (tla-process-buffer-mode))))

(defun tla--show-error-buffer (buffer &optional type mode)
  "Pops up the error buffer.
Works like `tla--show-last-process-buffer', but displays BUFFER, of type
'errors if TYPE is not specified.
If MODE is specified, the buffer will use that mode."
  (when (buffer-live-p buffer)
    (let ((content (with-current-buffer buffer
                     (buffer-string))))
      (tla-switch-to-buffer (tla--get-buffer-create
                             (or type 'errors)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (if mode
        (funcall mode)
      (tla-process-buffer-mode))))

;; ----------------------------------------------------------------------------
;; Arch name manipulators
;; ======================
;;
;; Normally in xtla, a name, a revision specifier is represented as a
;; list like:
;;
;;    ("archive" "category" "branch" "version" "revision")
;;
;; Nil is permitted as the element. However the list length must be 5
;; like:
;;
;;    (nil "category" "branch" nil nil)
;;
;; In other hand, in tla command, the name must be represented as a
;; string like:
;;
;;    "archive/category--branch--version--revision"
;;
;; So we have to convert a name in different representation in many
;; cases.
;;
;; * tla--name-split-* is for converting from a string representation
;;   to a list representation. There are semi-qualified version and
;;   fully-qualified version.
;;
;;   - semi-qualified: "category--branch--version--revision".
;;     `tla--name-split-semi-qualified' expects a name string without
;;     archive component. The archive field of returned list is filled
;;     with nil.
;;
;;   - fully-qualified: "archive/category--branch--version--revision".
;;     `tla--name-split' expects a name string including archive.
;;
;; * tla--name-construct-* is for converting from a list
;;   representation to a string representation. The functions accept
;;   arguments two ways.
;;
;;   - normal passing: (tla--name-construct "archive" "category"...)
;;   - packed passing: (tla--name-construct '("archive" "category"...))
;;
;;   There are semi-qualified version and fully-qualified version.
;;   - semi-qualified: `tla--name-construct-semi-qualified' connects
;;     arguments with "--".
;;   - fully-qualified: `tla--name-construct" connects the first argument
;;     and the rest with "/". About the rest,
;;     `tla--name-construct-semi-qualified' is applied.
;;
;; * tla--name-{archive|category|branch|version|revision} is for
;;   extracting a component from a name. The both representations are
;;   acceptable.
;;
;; * tla--name-mask is for replace a component in the name list with nil.
;;
;; ----------------------------------------------------------------------------

;;
;; String representation -> List representation
;;
(defun tla--name-split-semi-qualified (name &optional archive)
  "Split \"--\" connected string NAME into 5 elements list.
The first element is always nil if ARCHIVE is not given.
If ARCHIVE is given, use it as the first.
Even if the elements in name are less than 5, the list is filled by nil
to make the length 5.

  ELISP> (tla--name-split-semi-qualified \"branch--category--version--revision\"
                                        \"archive\")
  (\"archive\" \"branch\" \"category\" \"version\" \"revision\")

  ELISP> (tla--name-split-semi-qualified
            \"branch--category--version--revision\")
  (nil \"branch\" \"category\" \"version\" \"revision\")

  ELISP> (tla--name-split-semi-qualified \"branch--category--version\")
  (nil \"branch\" \"category\" \"version\" nil)

  ELISP> (tla--name-split-semi-qualified
            \"branch--category--version\" \"archive\")
  (\"archive\" \"branch\" \"category\" \"version\" nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category\" \"archive\")
  (\"archive\" \"branch\" \"category\" nil nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category\"nil)
  (nil \"branch\" \"category\" nil nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category--\" nil)
  (nil \"branch\" \"category\" \"\" nil)"
  (let ((list (tla--name-split-semi-qualified-internal name)))
    (while (> 4 (length list))
      (setq list (cons nil list)))
    (let ((result (cons archive (nreverse list))))
      (when (tla--is-version-string (caddr result))
        (setq result (list (car result)
                           (cadr result)
                           ""
                           (caddr result)
                           (cadddr result))))
      result)))

(defun tla--is-version-string (string)
  "Non-nil if STRING is a candidate for a version name.
That is, if it contains only digits and dots.
The regexp here is less strict than the one of tla, but must verify
\(tla--is-version-string string) => string can't be a branch name."
  (and string (string-match "^[0-9\.]+$" string)))

(defun tla--name-split-semi-qualified-internal (name)
  "Helper function for `tla--name-split-semi-qualified'.
Splits a semi-qualified NAME."
  (if (string-match "^\\(.+\\)--\\(\\([^-]\\|-[^-]\\)*\\)" name)
      (cons (match-string 2 name)
            (tla--name-split-semi-qualified-internal
             (match-string 1 name)))
    (cons name nil)))

(defun tla--name-split (name)
  "Parse a fully qualified revision NAME, but possibly incomplete.
email@address.com--arch/cat--branch--ver ->
  (\"email@address.com--arch\" \"cat\" \"branch\" \"ver\" nil)
email@address.com--arch/cat ->
  (\"email@address.com--arch\" \"cat\" nil nil nil)
email@address.com--arch ->
  (\"email@address.com--arch\" nil nil nil nil)"
  (if (string-match "\\(.*\\)/\\(.*\\)" name)
      (tla--name-split-semi-qualified (match-string 2 name) (match-string 1 name))
    (if (string= name "")
        (list nil nil nil nil nil)
      (list name nil nil nil nil))))


;;
;; List representation -> string
;;
(defun tla--name-construct-semi-qualified (&rest comp)
  "Concatenate COMP with \"--\".
This function can accept strings or a list which contains strings.

    ELISP> (tla--name-construct-semi-qualified \"a\" \"b\" \"c\")
    \"a--b--c\"
    ELISP> (tla--name-construct-semi-qualified (list \"a\" \"b\" \"c\"))
    \"a--b--c\""
  (if (consp (car comp)) (setq comp (car comp)))
  (if (string= (cadr comp) "")
      ;; Unnamed branch.
      (concat (car comp) "--"
              (mapconcat 'identity (remove nil (cddr comp)) "--"))
    (mapconcat 'identity (remove nil comp) "--")))

(defun tla--name-construct (archive &optional
                                    category
                                    branch
                                    version
                                    revision)
  "Create the revision name ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION.
The arguments may be nil. If ARCHIVE is a revision name list like
 (archive category branch version revision), the list element is mapped
to arguments before creating the fully qualified revision name.

If the branch name is the empty string and the version is defined,
then, we have an unnamed branch. The full name is
archive/category--version."
  (when (consp archive)
    (setq category (tla--name-category archive)
          branch   (tla--name-branch archive)
          version  (tla--name-version archive)
          revision (tla--name-revision archive)
          ;; archive must be last
          archive  (tla--name-archive archive)))
  (let ((semi (tla--name-construct-semi-qualified
               category branch version revision)))
    (concat
     (and archive (not (string= archive ""))
          (concat archive (when category "/")))
     semi)))

;;
;; Get a component from a list or string.
;;
(defun tla--name-archive (target)
  "Get archive component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (car target))

(defun tla--name-category (target)
  "Get category component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (cadr target))

(defun tla--name-branch (target)
  "Get branch component from a TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (caddr target))

(defun tla--name-version (target)
  "Get version component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (cadddr target))

(defun tla--name-revision (target)
  "Get revision component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (cadddr (cdr target)))

;;
;; Utilities
;; Mask a specified component in the name.
;;
(defun tla--name-mask (original do-construct-p
                                &optional
                                archive-mask
                                category-mask
                                branch-mask
                                version-mask
                                revision-mask)
  "Mask ORIGINAL, a tla revision name by masks; and return the masked value.

If DO-CONSTRUCT-P is given, the result is converted to a string by
`tla--name-construct'.

ARCHIVE-MASK, CATEGORY-MASK, BRANCH-MASK, VERSION-MASK and REVISION-MASK should
be either nil or t, and indicate whether that field should be masked.

If a mask value is nil, the associated element in ORIGINAL is set to nil.
Else If a mask value is a string, the associated element in ORIGINAL is set
to the string.
Else the associated element in ORIGINAL is not changed.

Examples:
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") nil t t t t nil)
 (\"a\" \"c\" \"b\" \"v\" nil)

 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") nil t t t nil nil)
 (\"a\" \"c\" \"b\" nil nil)

 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t t t t nil nil)
 \"a/c--b\"
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t nil nil nil nil t)
 \"r\"
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t nil nil nil t t)
 \"v--r\"
 ELISP>"
  (when (consp original)
    (let ((masked (list
                   (if archive-mask
                       (if (stringp archive-mask)
                           archive-mask
                         (tla--name-archive original)))
                   (if category-mask
                       (if (stringp category-mask)
                           category-mask
                         (tla--name-category original)))
                   (if branch-mask
                       (if (stringp branch-mask)
                           branch-mask
                         (tla--name-branch original)))
                   (if version-mask
                       (if (stringp version-mask)
                           version-mask
                         (tla--name-version original)))
                   (if revision-mask
                       (if (stringp revision-mask)
                           revision-mask
                         (tla--name-revision original))))))
      (if do-construct-p
          (tla--name-construct masked)
        masked))))

(defun tla--name-match (target mask)
"Compare the fully qualified revision list TARGET with a MASK.
Each parameter is a list.  The elements of the both lists are compared
via a regexp match.  When the mask part of a component is nil, this
comparision is skipped.
Here are some examples:
\(tla--name-match
 '(\"xsteve@nit.at--public\" \"xtla\" \"main\" \"0.1\" \"patch-116\")
 '(nil \"xt.*\" \"main\" nil nil)) => t
\(tla--name-match
 '(\"xsteve@nit.at--public\" \"xtla\" \"main\" \"0.1\" \"patch-116\")
 '(nil \"xt.*\" \"devel\" nil nil)) => nil"  ;"
  (let ((tl target)
        (ml mask)
        (t-part)
        (m-part)
        (matching t))
    (while tl
      (setq t-part (car tl))
      (setq m-part (car ml))
      (when m-part
        (setq matching (string-match m-part t-part)))
      (if matching
          (progn
            (setq tl (cdr tl))
            (setq ml (cdr ml)))
        (setq tl nil)))
    (if matching t nil)))


(defun tla--name-match-from-list (target match-list)
  "Match TARGET against a list of possible matches.
Every entry of MATCH-LIST is a list that contains a
match element and a possible result.
The target is matched against the elements in the match-list.
If a match is found return the corresponding result,
otherwise return nil."
  (let ((ml match-list)
        (match)
        (data)
        (result))
    (while (and (not result) ml)
      (setq match (caar ml))
      (setq data (cadar ml))
      (message "match: %s, data: %s" match data)
      (setq result (when (tla--name-match target match) data))
      (setq ml (cdr ml)))
    result))

;; example:
;;(setq tla-apply-patch-mapping
;;      '(((nil "atla" nil  nil nil) "~/work/tlaaaa")
;;        ((nil "xtla" nil  nil nil) "~/work/tla/xtla")))
;;(tla--name-match-from-list
;; '("xsteve@nit.at--public" "xtla" "main" "0.1" "patch-116") tla-apply-patch-mapping)

;; ----------------------------------------------------------------------------
;; Buffers menu
;; ----------------------------------------------------------------------------
(defun tla--buffers-menu ()
  "Return menus for buffers managed in xtla."
  (let ((menu (make-sparse-keymap "Tla-Buffers"))
        label
        submenu
        (i tla-number-of-dead-process-buffer))
    ;; Debug QUEUE
    (setq submenu (make-sparse-keymap "Queue"))
    (mapcar
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (define-key submenu (vector (make-symbol (buffer-name buffer)))
           `(menu-item ,(format "%d: %s%s"
                                i
                                (if (zerop (buffer-size buffer)) "[empty] " "")
                                (buffer-name buffer))
                       (lambda () (interactive) (switch-to-buffer ,buffer))
                       :enable t)))
       (setq i (1- i)))
     tla--dead-process-buffer-queue)
    (define-key menu [queue]
      `(menu-item "Queue(DEBUG)"
                  ,submenu
                  :enable tla-show-internal-buffers-on-menu))
    (mapcar (lambda (item)
              (setq label (capitalize (symbol-name (car item))))
              (setq submenu (make-sparse-keymap label))
              (mapcar
               (lambda (subitem)
                 (let ((path (car subitem))
                       (buffer (cadr subitem)))
                   (when (buffer-live-p buffer)
                     (unless path
                       (setq path (buffer-name buffer)))
                     (define-key submenu (vector (make-symbol path))
                       `(menu-item ,path
                                   (lambda () (interactive) (switch-to-buffer ,buffer))
                                   :enable t)))))
               (cdr item))
              (when (cdr item)
                  (define-key menu (vector (car item))
                    `(menu-item ,label
                                ,submenu
                                :enable t))
                ))
            tla--buffers-tree)
    (define-key menu [list-separator]
      '(menu-item "--"))
    (define-key menu [process-buffer]
      '(menu-item "Show Process Bufffer" tla-show-process-buffer))
    (define-key menu [log-buffer]
      '(menu-item "Open Log Bufffer" tla-open-internal-log-buffer))
    menu))

(eval-when-compile
  (unless (functionp 'add-submenu)
    (defun add-submenu (&rest arg)
      "Avoids a byte-compiler warning for GNU Emacs")))

(defun tla--install-buffer-menu ()
  "Install the buffer menu."
  (if (featurep 'xemacs)
      ;; See tla-xemacs-buffers-menu in xtla-xemacs.el
      (tla--do-in-xemacs
	(add-submenu nil '("Tla-Buffers" :filter tla-xemacs-buffers-menu) nil))
    ;; GNU Emacs
    (tla--do-in-gnu-emacs
      (let ((xtla-menu (or (lookup-key global-map [menu-bar tools xtla])
			   (lookup-key global-map [menu-bar tools Xtla]))))
	(when (and xtla-menu (not (integerp xtla-menu)))
	  (define-key-after
	    xtla-menu
	    [tla-buffers]
	    (cons "Tla-Buffers" (tla--buffers-menu)))))
      (let ((map (and
		  (current-local-map)
		  (or (lookup-key (current-local-map) [menu-bar])
		      (define-key (current-local-map) [menu-bar]
			(make-keymap))))))
	(when map
	  (apply (if (functionp 'define-key-after)
		     'define-key-after
		   'define-key)
		 map
		 [tla-buffers]
		 (cons "Tla-Buffers" (tla--buffers-menu))
		 nil)))
      (add-hook 'menu-bar-update-hook 'tla--install-buffer-menu nil t))))

;; TODO: Use tla--archive-tree.
(defun tla--version-head (archive category branch version)
  "Return the newest revision for ARCHIVE/CATEGORY--BRANCH--VERSION."
  (tla--run-tla-sync (list "revisions"
                           (tla--name-construct
                            archive
                            category
                            branch
                            version))
                     :finished (lambda (output error status arguments)
                                 (with-current-buffer output
                                   (goto-char (point-max))
                                   (re-search-backward "^.")
                                   (buffer-substring-no-properties
                                    (point) (line-end-position))))))

;; ----------------------------------------------------------------------------
;; Archive tree manipulators
;; ----------------------------------------------------------------------------

(defvar tla--archive-tree nil
  "Arch archive/category/branch/version/revision are stored in assoc list:

 ((\"xsteve@nit.at--public\" \"http://arch.xsteve.at/2004\")
 [...]
  (\"mbp@sourcefrog.net--2004\"
   \"http://sourcefrog.net/arch/mbp@sourcefrog.net--2004\"
   (\"xtla\")
   (\"tilly\")
 [...]
   (\"dupes\"
    (\"mainline\"
     (\"0.1\")))
 [...]
   (\"archzoom\"))
  (\"mark@dishevelled.net--2003-mst\"
   \"http://members.iinet.net.au/~mtriggs/arch/\")
  (\"lord@emf.net--2004\"
   \"http://regexps.srparish.net/{archives}/lord@emf.net--2004\")
 [...]
  (\"Matthieu.Moy@imag.fr--public\"
   \"http://www-verimag.imag.fr/webdav/moy/public\"
   (\"xtla\"
    (\"main\"
     (\"0.1\"
      (\"patch-228\"
       \"Merged from Robert (patch8-9), Milan (patch21-22), Stefan (patch5-8)\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-06-03 20:13:11 GMT\")
      (\"patch-227\"
       \"Fix default-directory in tla--run-tla-sync, fix in tla-changes-ediff\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-06-03 15:26:15 GMT\")
 [...]
      (\"patch-1\"
       \"typo\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-04-07 22:57:00 GMT\")
      (\"base-0\"
       \"tag of xsteve@nit.at--public/xtla--main--0.1--patch-5\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\" \"2004-04-07 22:52:39 GMT\")))))
 [...]
   )

This list is initially empty, and is built/rebuilt on demand.")

;; Utilities
(defun tla--archive-tree-setcdr (parent value &optional rest)
  "In PARENT, update VALUE.
REST are the items that are already present."
  (let* ((current (cdr parent))
         (list-details (assoc value current)))
    (if (or (null current) (null list-details))
        ;; rest is '("summary" "creator" "date") when value is "patch-N"
        (setcdr parent (cons (cons value rest) current))
      (if (and list-details rest)
          ;; Field already there. update details.
          (setcdr list-details rest)))))

(defun tla--archive-tree-setcddr (parent value)
  "In PARENT, update VALUE."
  (let ((current (cddr parent)))
    (if (or (null current) (null (assoc value current)))
        (setcdr (cdr parent) (cons (cons value nil) current)))))

;; Archive
(defun tla--archive-tree-add-archive (archive location &optional old)
  "Add ARCHIVE  at LOCATION to the archive tree.
If OLD is provided, it is an old archive tree from which some
information can be found (this is useful to keep the category/branch/version
info for existing archives)."
  (if (tla--archive-tree-get-archive archive)
      (let* ((a (tla--archive-tree-get-archive archive))
             (val (cdr a))
             (oldlocation (car val))
             (category (cdr val)))
        (setcdr a (cons (or location oldlocation) category)))
    (let ((oldinfo (tla--archive-tree-get-archive archive old))
          (newinfo (list archive location)))
      (when oldinfo
        (setcdr (cdr newinfo) (cddr oldinfo))) ;; list of versions.
      (setq tla--archive-tree (cons newinfo
                                    tla--archive-tree)))))

(defun tla--archive-tree-get-archive (archive &optional archive-tree)
  "Get the value of ARCHIVE from ARCHIVE-TREE.
If ARCHIVE-TREE is not given, `tla--archive-tree' is used."
  (assoc archive (or archive-tree tla--archive-tree)))

;; Category
(defun tla--archive-tree-add-category (archive category)
  "Add a new category to ARCHIVE named CATEGORY."
  (tla--archive-tree-add-archive archive nil)
  (tla--archive-tree-setcddr
   (tla--archive-tree-get-archive archive)
   category))

(defun tla--archive-tree-get-category (archive category)
  "From ARCHIVE, get CATEGORY."
  (assoc category (cdr (cdr (tla--archive-tree-get-archive archive)))))

;; Branch
(defun tla--archive-tree-add-branch (archive category branch)
  "Add a new branch to ARCHIVE's CATEGORY named BRANCH."
  (tla--archive-tree-add-category archive category)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-category archive category)
   branch))

(defun tla--archive-tree-get-branch (archive category branch)
  "Get a branch from ARCHIVE's CATEGORY named BRANCH."
  (assoc branch (cdr (tla--archive-tree-get-category
                      archive category))))

;; Version
(defun tla--archive-tree-add-version (archive category branch version)
  "Add a new version to ARCHIVE CATEGORY BRANCH named VERSION."
  (tla--archive-tree-add-branch archive category branch)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-branch archive category branch )
   version))

(defun tla--archive-tree-get-version (archive category branch version)
  "Get a version from ARCHIVE CATEGORY BRANCH named VERSION."
  (assoc version (cdr (tla--archive-tree-get-branch
                       archive category branch))))

;; Revision
(defun tla--archive-tree-add-revision (archive category branch version revision
                                               &optional summary creator date)
  "Add a new revision to ARCHIVE CATEGORY BRANCH VERSION named REVISION."
  (tla--archive-tree-add-version archive category branch version)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-version archive category branch version)
   revision (list summary creator date)))

(defun tla--archive-tree-get-revision (archive category branch version revision)
  "Get a revision from ARCHIVE CATEGORY BRANCH VERSION named REVISION."
  (assoc revision (cdr (tla--archive-tree-get-version
                        archive category branch version))))

;; Archive tree builders
(defun tla--archive-tree-build (basename &optional use-cache ignore-error)
  "Generic version of tla--archive-tree-build-*.
BASENAME is used as a base for this tree.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (stringp basename)
    (setq basename (tla--name-split basename)))
  (let ((archive (tla--name-archive basename))
        (category (tla--name-category basename))
        (branch (tla--name-branch basename))
        (version (tla--name-version basename)))
  (cond
   (version
    (tla--archive-tree-build-revisions archive
                                       category
                                       branch
                                       version
                                       use-cache
                                       ignore-error))
   (branch
    (tla--archive-tree-build-versions archive
                                      category
                                      branch
                                      use-cache
                                      ignore-error))
   (category
    (tla--archive-tree-build-branches archive
                                      category
                                      use-cache
                                      ignore-error))
   (archive
    (tla--archive-tree-build-categories archive
                                        use-cache
                                        ignore-error))
   (t
    (tla--archive-tree-build-archives use-cache
                                      ignore-error)))))

(defun tla--archive-tree-build-archives (&optional use-cache ignore-error)
  "Builds the list of archives.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not tla--archive-tree))
    (tla--run-tla-sync '("archives")
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
    (let ((old-archive-tree tla--archive-tree))
      (setq tla--archive-tree nil)
      (save-excursion
        (let ((archive-name)
              (archive-location))
          (set-buffer tla--last-process-buffer)
          (goto-char (point-min))
          (while (> (line-end-position) (line-beginning-position))
            (setq archive-name (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            (beginning-of-line-text 2)
            (setq archive-location (buffer-substring-no-properties
                                    (point) (line-end-position)))
            (forward-line 1)
            (tla--archive-tree-add-archive archive-name
                                           archive-location
                                           old-archive-tree)))))))

(defun tla--archive-tree-build-categories (archive &optional
                                                   use-cache
                                                   ignore-error)
  "Build the list of categories for ARCHIVE in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not (cddr (tla--archive-tree-get-archive archive))))
    (let ((basename archive))
      (message "building categories for `%s'..." basename)
      (tla--run-tla-sync (list "categories" "-A" basename)
                         :finished 'tla--null-handler
                         :error
                         (if ignore-error
                             'tla--null-handler
                           'tla--default-error-function))
      (message "building categories for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer tla--last-process-buffer
      (let (category)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq category (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-category archive category)
          )))))

(defun tla--archive-tree-build-branches (archive category
                                                 &optional
                                                 use-cache
                                                 ignore-error)
  "Build the list of branches for ARCHIVE/CATEGORY in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-category archive category))))
    (let ((basename (tla--name-construct archive category)))
      (message "building branches for `%s'..." basename)
      (tla--run-tla-sync (list "branches" basename)
                         :finished 'tla--null-handler
                         :error
                         (if ignore-error
                             'tla--null-handler
                           'tla--default-error-function))
      (message "building branches for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer tla--last-process-buffer
      (let (branch)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq branch (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
          (tla--archive-tree-add-branch
           archive
           category
           (if (looking-at ".*--")
               (tla--name-branch (tla--name-split-semi-qualified
                                  branch))
             ;; unnamed branch
             ""))
          (forward-line 1))))))

(defun tla--archive-tree-build-versions (archive category branch
                                                 &optional
                                                 use-cache
                                                 ignore-error)
  "Build the version list in ARCHIVE/CATEGORY--BRANCH in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-branch archive category
                                                    branch))))
    (let ((basename (tla--name-construct archive category branch)))
      (message "building versions for `%s'..." basename)
      (tla--run-tla-sync (list "versions" basename)
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
      (message "building versions for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer tla--last-process-buffer
      (let (version)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq version (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-version
           archive
           category
           branch
           (tla--name-version (tla--name-split-semi-qualified version))))))))

(defun tla--archive-tree-build-revisions (archive category branch version
                                                  &optional
                                                  use-cache
                                                  ignore-error)

  "Build the revision list in ARCHIVE/CATEGORY--BRANCH--VERSION.
Updates `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-version archive category branch
                                                     version))))
    (let ((details (or tla-revisions-shows-summary
                       tla-revisions-shows-date
                       tla-revisions-shows-creator))
          (basename (tla--name-construct
                     archive category branch version)))
      (message "building revisions for `%s'..." basename)
      (if details
          (progn
            (tla--run-tla-sync (list "revisions"
                                     "--summary" "--date" "--creator"
                                     basename)
                               :finished 'tla--null-handler
                               :error (if ignore-error
                                          'tla--null-handler
                                        'tla--default-error-function)))
        (progn
          (tla--run-tla-sync (list "revisions" basename)
                             :finished 'tla--null-handler
                             :error (if ignore-error
                                        'tla--null-handler
                                      'tla--default-error-function))))
      (message "building revisions for `%s'...done" basename)
      (sit-for 0)
      (message nil)
      (with-current-buffer tla--last-process-buffer
        (let (revision date creator summary)
          (goto-char (point-min))
          (while (> (line-end-position) (line-beginning-position))
            (setq revision (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
            (forward-line 1)
            (when details
              (skip-chars-forward " ")
              (setq date (buffer-substring-no-properties (point)
                                                         (line-end-position)))
              (forward-line 1)
              (skip-chars-forward " ")
              (setq creator (buffer-substring-no-properties (point)
                                                            (line-end-position)))
              (forward-line 1)
              (skip-chars-forward " ")
              (setq summary (buffer-substring-no-properties
                             (point)
                             (progn (re-search-forward "^\\([^ \t]\\|$\\)")
                                    (previous-line 1)
                                    (end-of-line)
                                    (point))))
              (forward-line 1))
            (tla--archive-tree-add-revision
             archive
             category
             branch
             version
             revision
             summary
             creator
             date)))))))


(defun tla--revisions-tree-contains-details
  (archive category branch version)
  "Whether VERSION has already been listed full details.
Details include summary lines, dates, and creator in the archive tree."
  (let ((vtree (tla--archive-tree-get-version archive category branch
                                              version)))
    (and (cdr vtree) ;; revision list is here
         (cadr (cadr vtree))))) ;; summary line also

;; ----------------------------------------------------------------------------
;; Revlib tree manipulators
;; ----------------------------------------------------------------------------
(defvar tla--revlib-tree nil)
(defun tla--revlib-tree-get-archive (archive &optional archive-tree)
  "Get ARCHIVE from ARCHIVE-TREE.
If ARCHIVE-TREE is not given, `tla--revlib-tree' is used instead."
  (assoc archive (or archive-tree tla--revlib-tree)))

(defun tla--revlib-tree-build-archives (&optional use-cache ignore-error)
  "Build the list of archives in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for archives are updated."
  (when (or (not use-cache)
            (not tla--revlib-tree))
    (tla--run-tla-sync '("library-archives")
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
    (let ((old-revlib-tree tla--revlib-tree) )
      (setq tla--revlib-tree nil)
      (save-excursion
        (let ((archive-name)
              (tmp tla--archive-tree)
              (tla--archive-tree tla--revlib-tree)
              result)
          (set-buffer tla--last-process-buffer)
          (goto-char (point-min))
          (while (> (line-end-position) (line-beginning-position))
            (setq result t)
            (setq archive-name (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            (forward-line 1)
            (tla--archive-tree-add-archive archive-name
                                           nil
                                           old-revlib-tree))
          (setq tla--revlib-tree tla--archive-tree
                tla--archive-tree tmp)
          result)))))

(defun tla--revlib-tree-get-category (archive category)
  "Get a category from ARCHIVE named CATEGORY."
  (assoc category (cdr (cdr (tla--revlib-tree-get-archive archive)))))

(defun tla--revlib-tree-build-categories (archive &optional
                                                  use-cache
                                                  ignore-error)
  "Builds the list of categories for an ARCHIVE in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for categories are updated."
  (when (or (not use-cache)
            (not (cddr (tla--revlib-tree-get-archive archive))))
    (tla--run-tla-sync (list "library-categories" "-A" archive)
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
    (with-current-buffer tla--last-process-buffer
      (let (category
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq category (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-category archive category))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-branch (archive category branch)
  "From ARCHIVE/CATEGORY, get BRANCH."
  (assoc branch (cdr (tla--revlib-tree-get-category
                      archive category))))

(defun tla--revlib-tree-build-branches (archive category
                                                &optional
                                                use-cache
                                                ignore-error)
  "Build the list of branches for ARCHIVE/CATEGORY in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for branches are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-category archive category))))
    (tla--run-tla-sync (list "library-branches" "-A" archive category)
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
    (with-current-buffer tla--last-process-buffer
      (let (branch
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq branch (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-branch
           archive
           category
           (tla--name-branch (tla--name-split-semi-qualified branch))))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-version (archive category branch version)
  "Get ARCHIVE/CATEGORY--BRANCH--VERSION from the revlib tree."
  (assoc version (cdr (tla--revlib-tree-get-branch
                       archive category branch))))

(defun tla--revlib-tree-build-versions (archive category branch
                                                &optional
                                                use-cache
                                                ignore-error)
  "Build the versions list in ARCHIVE/CATEGORY/BRANCH in `tla--archive-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for versions are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-branch archive category
                                                   branch))))
    (tla--run-tla-sync (list "library-versions"
                             (tla--name-construct
                              archive category branch))
                       :finished 'tla--null-handler
                       :error
                       (if ignore-error
                           'tla--null-handler
                         'tla--default-error-function))
    (with-current-buffer tla--last-process-buffer
      (let (version
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq version (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-version
           archive
           category
           branch
           (tla--name-version (tla--name-split-semi-qualified version))))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-revision (archive category branch version revision)
  "Get ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION from the revlib tree."
  (assoc revision (cdr (tla--revlib-tree-get-version
                        archive category branch version))))

(defun tla--revlib-tree-build-revisions (archive category branch version
                                                 &optional
                                                 use-cache
                                                 ignore-error)

  "Build the revision list of ARCHIVE/CATEGORY--BRANCH--VERSION.
Updates `tla--revlib-tree'.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for revisions are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-version archive category branch
                                                    version))))
    (tla--run-tla-sync (list "library-revisions"
                             "--summary" "--date" "--creator"
                             (tla--name-construct
                              archive category branch version))
                       :finished 'tla--null-handler
                       :error (if ignore-error
                                  'tla--null-handler
                                'tla--default-error-function))
    (with-current-buffer tla--last-process-buffer
      (let (revision
            date
            creator
            summary
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)

        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq revision (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (skip-chars-forward " ")
          (setq date (buffer-substring-no-properties (point)
                                                     (line-end-position)))
          (forward-line 1)
          (skip-chars-forward " ")
          (setq creator (buffer-substring-no-properties (point)
                                                        (line-end-position)))
          (forward-line 1)
          (skip-chars-forward " ")
          (setq summary (buffer-substring-no-properties
                         (point)
                         (progn (re-search-forward "^\\([^ \t]\\|$\\)")
                                (previous-line 1)
                                (end-of-line)
                                (point))))
          (forward-line 1)
          (tla--archive-tree-add-revision
           archive
           category
           branch
           version
           revision
           summary
           creator
           date))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result
        ))))

;; ----------------------------------------------------------------------------
;; Name reading engine
;; ----------------------------------------------------------------------------
;;Currently only able to read a full revision starting from nothing.
(defun tla-name-read-refresh-cache ()
  "Function to be called from the minibuffer while reading a name."
  (interactive)
  (tla--archive-tree-build
   (tla--name-construct
    (butlast (delete nil (tla--name-split (minibuffer-contents))))))
  (setq tla--archive-tree nil))

(defvar tla--name-read-arguments "This value should not be refereed."
  "Used to suppress warnings from the byte code compiler.
This variable is a just placeholder introduced to suppress the
warnings from byte code compiler.  Variable `tla--name-read-arguments'
should be bound in `let'.  Variable `tla--name-read-arguments' is used
for passing information from `tla-name-read' to functions called internally
from `tla-name-read'.  Use function `tla--name-read-arguments' to get the
information")

(defun tla--name-read-arguments (key)
  "Get `tla-name-read' context information associated to KEY.
`tla-name-read' calls some functions to read a tla name.
In the functions, the arguments passed to `tla-name-read'(context information)
are needed to know.  However, `tla-name-read' cannot pass the context
information directly to the functions because the functions are something to do
with Emacs's completion mechanism; and the mechanism specifies the number
of arguments of the functions.  So the context information is passed via
a local variable, `tla--name-read-arguments', defined in let.

Symbol `archive', `category', `branch', `version', or `revision' are
acceptable as KEY."
  (cdr (assoc key tla--name-read-arguments)))


(defun tla--name-read-complete (string predicate what)
  "Completion function for name reading.

Displays STRING and prompts for something satisfying PREDICATE.

This function uses the free variables archive, category, branch,
version, and revision.  If one of these variables is non-nil, it means
the corresponding value must be read from keyboard.

REMINDER: this function may be called several times, with different
values for WHAT:

 - nil : The function must return the longest prefix
 - t : The function must return the list of completions
 - 'lambda : The function must return t if the completion correspond
   to an exact match, nil otherwise.  (so that Emacs can distinguish
   between \"sole completion\" and \"complete, but not unique\"."
  (if (and (eq what 'lambda)
           (string-match "/\\(.*--\\)?$" string))
      ;; The caller just want to know whether this is a full
      ;; completion. This can not be the case with such suffix.
      nil
    (let* ((empty-branch nil)
           (use-cache (not current-prefix-arg))
           (splited (tla--name-split string))
           (archive-loc  (tla--name-archive  splited))
           (category-loc (tla--name-category splited))
           (branch-loc   (tla--name-branch   splited))
           (version-loc  (tla--name-version  splited))
           (revision-loc (tla--name-revision splited))
           (suffix (cond
                    ((and (tla--name-read-arguments 'category)
                          (not category-loc) "/"))
                    ((and (tla--name-read-arguments 'branch)
                          (not branch-loc)   "--"))
                    ((and (tla--name-read-arguments 'version)
                          (not version-loc)  "--"))
                    ((and (tla--name-read-arguments 'revision)
                          (not revision-loc) "--"))
                    (t nil)))
           (maybep (cond
                    ((eq 'maybe (tla--name-read-arguments 'category))
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'branch))
                          archive-loc category-loc)
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'version))
                          archive-loc category-loc branch-loc)
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'revision))
                          archive-loc category-loc branch-loc version-loc)
                     t)
                    (t nil)))
           (completions
            (cond
             ;; If the user started to write a revision ...
             (revision-loc
              ;; ... and if the user is supposed to be prompted a
              ;; revision
              (when (tla--name-read-arguments 'revision)
                (let ((tla-revisions-shows-summary nil)
                      (tla-revisions-shows-date nil)
                      (tla-revisions-shows-creator nil))
                  (tla--archive-tree-build-revisions
                   archive-loc category-loc branch-loc version-loc use-cache t))
                (cdr (tla--archive-tree-get-version
                      archive-loc category-loc branch-loc version-loc))))
             (version-loc
              (when (tla--name-read-arguments 'version)
                (tla--archive-tree-build-versions
                 archive-loc category-loc branch-loc use-cache t)
                (cdr (tla--archive-tree-get-branch
                      archive-loc category-loc branch-loc))))
             ;; If the user started a branch ...
             (branch-loc
              ;; And a branch is needed
              (when (tla--name-read-arguments 'branch)
                (tla--archive-tree-build-branches
                 archive-loc category-loc use-cache t)
                (let ((result (cdr (tla--archive-tree-get-category
                                    archive-loc category-loc))))
                  (when (and (string= branch-loc "")
                             (tla--name-read-arguments 'version)
                             (let ((empty-br-exists nil))
                               (dolist (branch
                                        (cdr (tla--archive-tree-get-category
                                              archive-loc category-loc)))
                                 (when (string= (car branch) "")
                                   (setq empty-br-exists t)))
                               empty-br-exists))
                    (tla--archive-tree-build-versions
                     archive-loc category-loc "")
                    (setq empty-branch (tla--archive-tree-get-branch
                                        archive-loc category-loc ""))
                    (when empty-branch
                      ;; Remove the "" branch to avoid the ----
                      ;; completion.
                      (let ((tmp result))
                        (setq result nil)
                        (while tmp
                          (when (not (string= (caar tmp) ""))
                            (setq result (cons (car tmp) result)))
                          (setq tmp (cdr tmp))))))
                  result)))
             (category-loc
              (when (tla--name-read-arguments 'category)
                (tla--archive-tree-build-categories archive-loc use-cache t)
                (cddr (tla--archive-tree-get-archive archive-loc))))
             (t
              (when (tla--name-read-arguments 'archive)
                (tla--archive-tree-build-archives use-cache t)
                tla--archive-tree)))))
      (let* ((base (mapcar (lambda (x)
                             (tla--name-construct
                              (delete
                               nil
                               (list
                                (when category-loc archive-loc)
                                (when branch-loc category-loc)
                                (when version-loc branch-loc)
                                (when revision-loc version-loc)
                                (car x)))))
                           completions))
             (sans-suffix
              (and maybep suffix))
             (empty-branch-versions
              (and empty-branch
                   (mapcar (lambda (x)
                             (tla--name-construct
                              archive-loc category-loc "" (car x)))
                           (cdr empty-branch))))
             (completions (funcall 'all-completions
                                   string
                                   (nconc (mapcar
                                           (lambda (x)
                                             (list (concat x suffix)))
                                           base)
                                          (when sans-suffix
                                            (mapcar
                                             (lambda (x) (list x))
                                             base))
                                          (when empty-branch
                                            (mapcar
                                             (lambda (x) (list x))
                                             empty-branch-versions)))
                                   predicate)))
        (let ((result
               (cond ((eq what t)
                      ;; We just want the list of completions
                      completions)
                     ((eq (length completions) 1)
                      ;; There's only one completion
                      (if (eq what 'lambda)
                          (string= (car completions) string)
                        (cond ((string= (car completions) string) t)
                              (t (car completions)))))
                     ;; there are several possible completions
                     (t (if (eq what 'lambda)
                            ;; complete, but not unique ?
                            (member string completions)
                          (try-completion string (mapcar 'list
                                                         completions)))))))
;;          (tla--trace "string=%s predicate=%S what=%s ==> result=%S\ncompletions=%S"
;;                      string predicate what result completions)
          result)))))

;; Test cases
;; (tla-name-read "enter category: " "Matthieu.Moy@imag.fr--public" 'prompt)
;; (tla-name-read "branch: " "lord@emf.net--2004" 'prompt 'prompt)
;; (tla-name-read "revision: " 'prompt 'prompt 'prompt 'prompt 'prompt)
;; (tla-name-read "revision or version: " 'prompt 'prompt 'prompt 'prompt 'maybe)
;; (tla-name-read "revision or version: " "jet@gyve.org--xtla" "xtla" "jet" 'prompt 'maybe)
;;
(defvar tla--name-read-history nil)     ; TODO: multiple history list?
(defvar tla--name-read-debug nil
  "If non-nil, `condition-case' in `tla-name-read' is made disabled.")
(defun tla-name-read (&optional prompt archive category
                                 branch version revision)
  "Read a name.
To get help on the user interface of `tla-name-read', please type
M-x tla-name-read-help RET.

Function reading an archive location from keyboard.
Read name is expressed in a list built by `tla--name-split'.

First argument PROMPT is the prompt the user will get. Next arguments
ARCHIVE CATEGORY BRANCH VERSION and REVISION are either the default
value, or a request for a value. They can take four values:

 - A string means the default value, and will be used as an initial
   input.

 - The symbol 'prompt means the value will be prompted from the user.
   The user will HAVE to give this value.

 - The symbol 'maybe means the value will be prompted, but is optional
   for the user.

 - nil means the value won't be prompted.

They should appear in the same order as above.

Example:
- Read a category in archive \"Matthieu.Moy@imag.fr--public\":
 (tla-name-read \"enter category: \" \"Matthieu.Moy@imag.fr--public\" 'prompt)
- Read a revision, anywhere:
 (tla-name-read \"revision: \" 'prompt 'prompt 'prompt 'prompt 'prompt)
- Read either a revision or a version:
 (tla-name-read \"revision: \" 'prompt 'prompt 'prompt 'prompt 'maybe)

While prompting, a menu \"Xtla\" is added to the menubar. The
following commands are available:

\\{tla--name-read-minibuf-map}"
  (let ((tla--name-read-arguments `((archive  . ,archive)
                                    (category . ,category)
                                    (branch   . ,branch)
                                    (version  . ,version)
                                    (revision . ,revision))))
    (if tla--name-read-debug
        (tla--name-read-internal prompt archive category branch version revision)
      (condition-case reason
          (tla--name-read-internal prompt archive category branch version revision)
        ((quit error)
         (run-hooks 'tla-name-read-error-hook)
         (signal (car reason) (cdr reason)))))))

(defun tla--name-read-internal (prompt archive category branch version revision)
  "See `tla-name-read'."
  (run-hooks 'tla-name-read-init-hook)
  (let* ((minibuffer-local-completion-map tla--name-read-minibuf-map)
         (result (tla--name-construct
                  (delete
                   'maybe
                   (delete 'prompt (list archive category
                                         branch version)))))
         (first-try t)
         not-finished too-long last-empty)
    ;; Without in some case 'maybe is ignored by tla--prompt-not-finished
    ;; and never the control flow enters the while loop.
    ;; We need C language's do-while loop.
    (while (or first-try
               not-finished
               too-long
               last-empty)
      (unless first-try
        (unless (eq this-command 'choose-completion)
          (ding)
          (message (cond (not-finished "%s%s [incomplete input: %s]")
                         (too-long "%s%s [too long input for: %s]")
                         (last-empty (concat "%s%s [empty " last-empty
                                             " name]"))
                         (t (error
                             (concat "case not managed."
                                     " Please submit a bug report"))))
                   prompt result
                   (tla--name-read-required-input archive
                                                  category
                                                  branch
                                                  version
                                                  revision))
          (sit-for 2)
          (message nil)))

      (setq result (completing-read
                    (or prompt "Location: ")
                    'tla--name-read-complete
                    nil nil result
                    'tla--name-read-history)
            first-try nil)
      (setq not-finished (tla--prompt-not-finished
                          result archive category branch
                          version revision))
      (setq too-long (tla--prompt-too-long
                      result archive category branch
                      version revision))
      (setq last-empty (tla--prompt-last-empty result)))

    (when result
      (setq result (tla--name-split result)))
    (run-hook-with-args 'tla-name-read-final-hook result)
    result))

(defun tla--prompt-not-finished (result archive category branch
                                        version revision)
  "Check whether user input is complete.
True if RESULT (a string) is not sufficient when the user is
prompted for ARCHIVE CATEGORY BRANCH VERSION REVISION."
  (let ((res-split (tla--name-split result)))
    (or (and (eq archive 'prompt) ;; archive required
             (not (tla--name-archive res-split))) ;; but not provided
        (and (eq category 'prompt)
             (not (tla--name-category res-split)))
        (and (eq branch 'prompt)
             (not (tla--name-branch res-split)))
        (and (eq version 'prompt)
             (not (tla--name-version res-split)))
        (and (eq revision 'prompt)
             (not (tla--name-revision res-split))))))

(defun tla--prompt-too-long (result archive category branch
                                    version revision)
  "Check whether the user has entered too many elements.
True if RESULT (a string) contains too many elements when the user
is prompted for ARCHIVE CATEGORY BRANCH VERSION REVISION.

For example, will return true if the user entered
foo@bar--2004/xtla--main while prompted only for a category."
  (let ((res-split (tla--name-split result)))
    (or (and (not revision) ;; revision not needed
             (tla--name-revision res-split)) ;; but provided
        (and (not version)
             (tla--name-version res-split))
        (and (not branch)
             (tla--name-branch res-split))
        (and (not category)
             (tla--name-category res-split))
        (and (not archive)
             (tla--name-archive res-split)))))

(defun tla--prompt-last-empty (result)
  "Check whether the last field is empty.
Non-nil if RESULT (a string) is terminated by \"--\" or \"/\". This
means the user entered a delimiter but not the element after.

When non-nil, the returned value is a string giving the name of the
item that is currently empty. (eg: archive, category, ...)"
  (let ((res-split (tla--name-split result)))
    (cond ((equal (tla--name-archive  res-split) "") "archive" )
          ((equal (tla--name-category res-split) "") "category")
          ((and (equal (tla--name-branch res-split) "")
                (not (tla--name-version res-split))) "branch"  )
          ((equal (tla--name-version  res-split) "") "version" )
          ((equal (tla--name-revision res-split) "") "revision")
          (t nil))))


(defun tla--name-read-required-input (archive
                                      category
                                      branch
                                      version
                                      revision)
  "Return string which represents the elements to be readin `tla-name-read'.
If ARCHIVE, CATEGORY, BRANCH, VERSION or REVISION are equal to 'maybe, the
corresponding element will be optionally read.
If any of these are non-nil (but not 'maybe), the corresponding element will be
required.
If any of these are nil, the correpsonding element is not required."
  (concat
   (cond ((eq archive 'maybe) "[A]")
         (archive "A")
         (t ""))
   (cond ((eq category 'maybe) "[/C]")
         (category "/C")
         (t ""))
   (cond ((eq branch 'maybe) "[--B]")
         (branch "--B")
         (t ""))
   (cond ((eq version 'maybe) "[--V]")
         (version "--V")
         (t ""))
   (cond ((eq revision 'maybe) "[--R]")
         (revision "--R")
         (t ""))))



(defun tla--location-type (location)
  "Return the type of LOCATION."
  (cond
   ((string-match "^ftp://" location) 'ftp)
   ((string-match "^sftp://" location) 'sftp)
   ((string-match "^http://" location) 'http)
   (t 'local)))

(defun tla--archive-type (archive)
  "Return the type of ARCHIVE."
  (cond
   ((string-match "SOURCE$" archive) 'source)
   ;; archive-MIRROR, archive-MIRROR-2 should be treated as mirror
   ((string-match ".+-MIRROR" archive) 'mirror)
   (t 'normal)))

;; (tla--archive-name-source "a")
;; (tla--archive-name-source "a-SOURCE")
;; (tla--archive-name-source "a-MIRROR")
(defun tla--archive-name-source (archive &optional existence-check)
  "Make source archive name from ARCHIVE.
If EXISTENCE-CHECK is non-nil, check whether the made source archive name
already exists or not; return nil if it doesn't exists.
Example:
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla\")
\"jet@gyve.org--xtla-SOURCE\"
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla-MIRROR\")
\"jet@gyve.org--xtla\"
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla-SOURCE\")
nil"
  (let* ((type (tla--archive-type archive))
         (source (cond
                 ((eq 'normal type)
                  (concat archive "-SOURCE"))
                 ((eq 'mirror type)
                  (string-match "\\(.*\\)-MIRROR$" archive)
                  (match-string 1 archive))
                 (t nil))))
    (if existence-check
        (progn
          (tla--archive-tree-build-archives t)
          (when (and source (tla--archive-tree-get-archive source))
            source))
      source)))

;; (tla--archive-name-mirror "a")
;; (tla--archive-name-mirror "a-SOURCE")
;; (tla--archive-name-mirror "a-MIRROR")
(defun tla--archive-name-mirror (archive &optional existence-check)
  "Make mirror archive name from ARCHIVE.
If EXISTENCE-CHECK is non-nil, check whether the made mirror archive name
already exists or not; return nil if it doesn't exists.
Example:
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla\")
\"jet@gyve.org--xtla-MIRROR\"
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla-SOURCE\")
\"jet@gyve.org--xtla\"
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla-MIRROR\")
nil"
  (let* ((type (tla--archive-type archive))
         (mirror (cond
                  ((eq 'normal type)
                   (concat archive "-MIRROR"))
                  ((eq 'source type)
                   (string-match "\\(.*\\)-SOURCE" archive)
                   (match-string 1 archive))
                  (t nil))))
    (if existence-check
        (progn
          (tla--archive-tree-build-archives t)
          (when (and mirror (tla--archive-tree-get-archive mirror))
            mirror))
      mirror)))

(defun tla-compute-direct-ancestor (&optional revision)
  "Compute the direct ancestor of REVISION.
REVISION must be provided as a list, and a list is returned.
If revision is nil, return the ancestor of the last revision
of the local tree."
  (interactive
   (list (tla-name-read "Compute direct ancestor of: "
                         'prompt 'prompt 'prompt 'prompt 'prompt)))
  (let ((ancestor
         (tla--run-tla-sync (list "ancestry-graph" "--immediate"
                                  (and revision
                                       (tla--name-construct revision)))
                            :finished (lambda (output error status arguments)
                                        (tla--name-split
                                         (tla--buffer-content
                                          output))))))
    (when (interactive-p)
      (message "Ancestor of: %s\n         is: %s"
               (tla--name-construct ancestor)
               (tla--name-construct revision)))
    ancestor))

;; Copied from ediff-mouse-event-p. I prefer keeping this duplication
;; to avoid one more dependancy on ediff.el (whose interface may
;; change one day ...)
(defsubst tla--mouse-event-p (event)
  "Return true if EVENT is a mouse-related event."
  (if (featurep 'xemacs)
      (tla--do-in-xemacs (button-event-p event))
    (tla--do-in-gnu-emacs
      (string-match "mouse" (format "%S" (event-basic-type event))))))

;; ----------------------------------------------------------------------------
;; Face manipulators
;; ----------------------------------------------------------------------------
(defsubst tla--face-add (str face &optional keymap menu help)
  "Add to string STR the face FACE.
Optionally, also add the text properties KEYMAP, MENU and HELP.

If KEYMAP is a symbol, (symbol-value KEYMAP) is used
as a keymap; and `substitute-command-keys' result
against (format \"\\{%s}\" (symbol-name keymap)) is appended to HELP.

If HELP is nil and if MENU is non nil, the MENU title is used as HELP."
  (if tla-highlight
      (let* ((strcpy (copy-sequence str))
             (key-help (when (symbolp keymap)
                         (substitute-command-keys (format "\\{%s}" (symbol-name keymap)))))
             (prefix-help (if help help (when (and menu (stringp (cadr menu))) (cadr menu))))
             (long-help (if key-help
                            (if prefix-help (concat prefix-help "\n"
                                             ;; Sigh. Font used on tooltips in GNU Emacs with Gtk+
                                             ;; is a proportional.
                                             ;; (make-string (length help) ?=) "\n"
                                             "================" "\n"
                                             key-help) key-help)
                          help))
             (keymap (if (symbolp keymap) (symbol-value keymap) keymap)))
        (add-text-properties 0 (length strcpy)
                             `(face ,face
;;; Even if we define a face in a buffer, it seems that
;;; font-lock mode just ignore it or remove the face property.
;;; I don't know the detail but in tla-inventory buffer,
;;; I cannot make both font-lock keywords and faces put by tl--face-add
;;; highlight at once. When font-lock-face is defined, I can do.
;;; See "Special Properties" subsection in the emacs lisp reference manual.
;;; `font-lock-face' property is new in Emacs 21.4. However, I guess there is
;;; no wrong side effect if I define font-lock-face property here.
                                    font-lock-face ,face
                                    ,@(when keymap
                                        `(mouse-face highlight
                                                     keymap ,keymap
                                                     help-echo ,long-help))
                                    ,@(when menu
                                        `(menu ,menu))
                                    )
                             strcpy)
        strcpy)
    str))

;; ----------------------------------------------------------------------------
;; Debugging facilities
;; ----------------------------------------------------------------------------
(defvar tla--debug t)

(defun tla--trace (&rest msg)
  "Display the trace message MSG.
Same as `message' if `tla--debug' is non-nil.
Does nothing otherwise.  Please use it for your debug messages."
  (when tla--debug
    (apply 'message (concat "xtla: " (car msg)) (cdr msg))))

(defun tla-reload (&optional directory)
  "Reload xtla (usually for debugging purpose).

With prefix arg, prompts for the DIRECTORY in which xtla should be
loaded.  Useful to switch from one branch to the other.

If a Makefile is present in the directory where xtla is to be loaded,
run \"make\"."
  (interactive
   (list (when current-prefix-arg
           (let* ((other (tla--read-directory-name
                         "Load xtla from: "))
                  (lispdir (concat (file-name-as-directory other)
                                   "lisp")))
             (if (file-directory-p lispdir)
                 lispdir
               other)))))
  (when directory
    (let ((current-path (file-name-directory (locate-library
                                              "xtla"))))
      (setq load-path
            (cons directory (remove current-path load-path)))))
  (let ((default-directory (file-name-directory (locate-library "xtla"))))
    (when (file-exists-p
           "Makefile")
      (shell-command "make")))
  (when (featurep 'xtla-tips)   (unload-feature 'xtla-tips   t))
  (when (featurep 'xtla-browse) (unload-feature 'xtla-browse t))
  (when (featurep 'xtla)        (unload-feature 'xtla        t))
  (when (featurep 'xtla-core)   (unload-feature 'xtla-core   t))
  (when (featurep 'xtla-defs)   (unload-feature 'xtla-defs   t))
  (when (featurep 'xtla-xemacs) (unload-feature 'xtla-xemacs t))
  (when (featurep 'xtla-emacs)  (unload-feature 'xtla-emacs  t))
  (require 'xtla))

;; ----------------------------------------------------------------------------
;; Wether tla tag needs the "--setup" option
;; ----------------------------------------------------------------------------
;; --setup is necessary for tla 1.3, and raises an error for tla 1.4rc1
(defvar tla--tag-needs-setup nil
  "Wether \"tla tag\" needs the --setup option.

Possible values are nil (don't know), 'yes, or 'no.  Don't use this
variable directly.  Use `tla-tag-needs-setup' instead.")

(defun tla-tag-needs-setup ()
  "Wether \"tla tag\" needs the --setup option.

Returns 't or nil.

If `tla--tag-needs-setup' is non-nil, use its value. Otherwise, test
if \"--setup\" is listed by \"tla tag --help\", and memorize the result in
`tla--tag-needs-setup'."
  (interactive)
  (let ((answer
         (cond ((eq tla--tag-needs-setup 'yes) t)
               ((eq tla--tag-needs-setup 'no) nil)
               (t (tla--run-tla-sync
                   '("tag" "--help")
                   :finished (lambda (output error status arguments)
                               (with-current-buffer output
                                 (goto-char (point-min))
                                 (search-forward "  -S, --setup"
                                                 nil t))))))))
    (when (interactive-p)
      (message (if answer "Yes" "No")))
    (setq tla--tag-needs-setup
          (if answer 'yes 'no))
    answer))

;; ----------------------------------------------------------------------------
;; Supports spaces in filenames
;; ----------------------------------------------------------------------------
(defvar tla--supports-spaces-in-filenames nil
  "Wether tla supports spaces in filenames.

Possible values are nil (don't know), 'yes, or 'no.  Don't use this
variable directly.  Use `tla-supports-spaces-in-filenames' instead.")

(defun tla-supports-spaces-in-filenames ()
  "Wether tla supports spaces in filenames.

Returns 't or nil.

If `tla--supports-spaces-in-filenames' is non-nil, use its value.
Otherwise, test if \"escape\" is listed by \"tla help\", and memorize
the result in `tla--supports-spaces-in-filenames'"
  (interactive)
  (let ((answer
         (cond ((eq tla--supports-spaces-in-filenames 'yes) t)
               ((eq tla--supports-spaces-in-filenames 'no) nil)
               (t (tla--run-tla-sync
                   '("help")
                   :finished (lambda (output error status arguments)
                               (with-current-buffer output
                                 (goto-char (point-min))
                                 (search-forward "escape :"
                                                 nil t))))))))
    (when (interactive-p)
      (message (if answer "Yes" "No")))
    (setq tla--supports-spaces-in-filenames
          (if answer 'yes 'no))
    answer))

(defun tla-escape (string &optional unescape message)
  "Return the pika escaped value of STRING.
If pika escaping is not supported by tla, return STRING.
If UNESCAPE is non-nil, returns the unescaped version of string.
If MESSAGE is non-nil or if run interactively, also display the value
as a message."
  (interactive "sString to escape: ")
  (let ((res (if (and (string-match (if unescape "\\\\"
                                      "[^a-zA-Z._+,{}-]") string)
                      ;; We need to do the (un)escaping
                      (tla-supports-spaces-in-filenames))
                 (tla--run-tla-sync
                  (list "escape" (when unescape "--unescaped") string)
                  :finished (lambda (output error status arguments)
                              (tla--buffer-content output)))
               string)))
    (when (or (interactive-p) message)
      (message res))
    res))

(defun tla-unescape (string)
  "Run \"tla escape --unescaped\" on STRING.

Return STRING if \"tla escape\" is not available."
  (interactive "sString to unescape: ")
  (tla-escape string t (interactive-p)))

(defun tla-regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else.
Special characters are escaped to leave STRING in a suitable form for
Arch."
  (let ((quoted (regexp-quote string)))
    (replace-regexp-in-string
     "\\([{}()|]\\)"
     (concat "\\\\"                     ; leading slash
             "\\1")                     ; quoted character
     quoted)))

;; ----------------------------------------------------------------------------
;; Saving and loading state variables
;; ----------------------------------------------------------------------------
(defun tla-pp-to-string (sexp)
  "Return sexp pretty printed by `pp-to-string'."
  (let ((print-readably t)
        print-level print-length)
    (pp-to-string sexp)))

(defun tla-save-state (&optional vars state-file pp)
  "Save variables from VARS list to file STATE-FILE.
The default for VARS is `tla-state-variables-list'
The default for STATE-FILE is `tla-state-file-name'.
If PP is non-nil use `tla-pp-to-string' to format object.

The file will contain a setq setting the vars during loading by
`tla-load-state'."
  (let ((state-file (or state-file
                        (expand-file-name tla-state-file-name
                                          tla-config-directory)))
        (vars (or vars tla-state-variables-list))
        v)
    (if (not (file-exists-p (file-name-directory state-file)))
        (make-directory (file-name-directory state-file) t))
    (save-excursion
      (set-buffer (get-buffer-create " *tla-state*"))
      (erase-buffer)
      (insert ";; Generated file. Do not edit!!!\n(setq\n")
      (if pp
          (while vars
            (setq v (car vars) vars (cdr vars))
            (insert (format "%s\n'%s"
                            (symbol-name v)
                            (tla-pp-to-string (symbol-value v)))))
        (while vars
          (setq v (car vars) vars (cdr vars))
          (insert (format "      %s '%S\n"
                          (symbol-name v)
                          (symbol-value v)))))
      (insert "      )")
      (write-region (point-min) (point-max) state-file))))

(defun tla-load-state (&optional state-file)
  "Load `tla-state-file-name`, i.e. evaluate its content."
  (let ((state-file (or state-file
                        (expand-file-name tla-state-file-name
                                          tla-config-directory))))
    (if (file-exists-p state-file)
        (load state-file nil t t))))

;; (setq tla--archive-tree nil)
;; (setq tla--revlib-tree nil)
(provide 'xtla-core)
;; arch-tag: c9e35f5a-6aea-409d-a157-c0d73d92f9b0
;;; xtla-core.el ends here
