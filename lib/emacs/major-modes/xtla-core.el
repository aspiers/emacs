;;; xtla-core.el --- Core of xtla

;; Copyright (C) 2003-2004 by Stefan Reichoer

;; Author: Stefan Reichoer, <xsteve@nit.at>
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

;;; Code:

(eval-and-compile (require 'cl))
(require 'xtla-defs)
(require 'ewoc)

(defconst tla--buffer-type-alist
  '((changes "*tla-changes*")
    (inventory "*tla-inventory*")
    (missing "*tla-missing*")
    (cat-log "*tla-cat-log*")
    (generic "*tla-process*"))
  "List of pairs (type name) used to generate a name for a buffer")

(defvar tla--buffers-tree nil
  "List in the form
 ((type1 (\"path1\" buffer)
         (\"path2\" buffer2))
  (type2 (\"path1\" buffer3)
         (\"path3\" buffer4)))
Used to keep track of all the tla related buffers.")

(defun tla--get-buffer-create (type &optional path)
  "Gets a buffer of type TYPE for the path PATH. Reuse one if it
exists, otherwise, call `create-file-buffer' to create the buffer"
  ;; Inspired from `cvs-get-buffer-create'
  (let ((path (and path
                   (expand-file-name (if (file-directory-p path)
                                         (concat path "/")
                                       path))))
        (name (cadr (assoc type tla--buffer-type-alist))))
    (or (let* ((list-path (assoc type tla--buffers-tree))
               (buffer (cadr (assoc path
                                    (cdr list-path)))))
          (when buffer
            (if (buffer-live-p buffer) buffer
              (setcdr list-path
                      (delq (assoc path (cdr list-path))
                            (cdr list-path)))
              (tla--get-buffer-create type path))))
        (with-current-buffer
            (let* ((default-directory (or path default-directory))
                   (buffer (create-file-buffer name))
                   (current-assoc (assoc type tla--buffers-tree)))
              (if current-assoc
                  (setcdr current-assoc
                          (cons (list path buffer)
                                (cdr current-assoc)))
                (setq tla--buffers-tree
                      (cons `(,type (,path ,buffer)) tla--buffers-tree)))
              buffer)))))

;;
;; Process buffers
;;

;;;###autoload
(defcustom tla-process-buffer " *tla-process*"
  "*Name of the process buffer"
  :type 'string
  :group 'xtla-internal)

;;;###autoload
(defcustom tla-error-buffer " *tla-errors*"
  "*Name of the buffer to which tla's stderr is redirected"
  :type 'string
  :group 'xtla-internal)

(defcustom tla-number-of-dead-process-buffer 20
  "Number of process buffers to keep after process termination. When
thera is more than this number of buffer, the most ancient is killed.
This includes both the process buffer and the error buffer (to which
stderr is redirected).

A nil value here means \"Never kill any process buffer\". Usefull for
debugging, but this will eat the memory of your computer ;-)"
  :type 'integer
  :group 'xtla-internal)

(defvar tla--dead-process-buffer-queue nil
  "List of process buffer for which the process terminated. When the
list is greater than `tla-number-of-dead-process-buffer', the last
ones are killed.")

(defun tla--kill-process-buffer (buffer)
  "Don't actually kill BUFFER, but add it to
`tla--dead-process-buffer-queue'. It will be killed when
`tla-number-of-dead-process-buffer' will also have been killed this
way"
  (add-to-list 'tla--dead-process-buffer-queue buffer t)
  (when tla-number-of-dead-process-buffer
    (while (> (length tla--dead-process-buffer-queue)
              (max 2 tla-number-of-dead-process-buffer))
      (kill-buffer (car tla--dead-process-buffer-queue))
      (setq tla--dead-process-buffer-queue
            (cdr tla--dead-process-buffer-queue)))))

(defvar tla--last-process-buffer nil
  "Last created process-buffer")

(defvar tla--last-error-buffer nil
  "Last created process-buffer")

(defun tla--new-process-buffer (to-be-deleted)
  "Creates a new process buffer"
  (let ((buffer (create-file-buffer tla-process-buffer)))
    (setq tla--last-process-buffer buffer)
    (when to-be-deleted (tla--kill-process-buffer buffer))
    buffer))

(defun tla--new-error-buffer (to-be-deleted)
  "Creates a new error buffer"
  (let ((buffer (create-file-buffer tla-error-buffer)))
    (setq tla--last-error-buffer buffer)
    (when to-be-deleted (tla--kill-process-buffer buffer))
    buffer))

;;
;; Process management
;;
(defun tla--default-error-function (output error status)
  "Default function called when a tla process ends with a non-zero
status."
  (tla-switch-to-buffer error)
  (error "tla failed: %s" status))

(defun tla--default-killed-function (output error status)
  "Default function called when a tla process is killed."
  (tla-switch-to-buffer error)
  (error "tla process killed !"))

(defun tla--null-handler (output error status)
  "Function doing nothing. Candidate as an argument for one of the
keywords :finished, :error or :killed in `tla--run-tla-sync' or
`tla--run-tla-async'."
  nil
  )

(defun tla--status-handler (output error status)
  "returns an integer value represented by STATUS. Candidate as an
argument for one of the keywords :finished, :error or :killed in
`tla--run-tla-sync' or `tla--run-tla-async'."
  (cond ((numberp status) status)
        ((string-match "^exited abnormally with code \\(.*\\)" status)
         (string-to-int (match-string 1)))
        (t (error status))))


(defun tla--default-finish-function (output error status)
  "Default function called when a tla process terminates."
  (tla-switch-to-buffer output)
  (message "tla process finished !"))

(defvar tla--log-cookie nil)

(defstruct (tla--event) output-buffer error-buffer related-buffer
  command tree event time)

(defun tla--strip-final-newline (string)
  "Strip the final newline from string if there's one"
  (if (eq (aref string (- (length string) 1)) ?\n)
      (substring string 0 (- (length string) 1))
    string))

(defun tla--log-printer (elem)
  "tla event printer"
  (insert "Command: " (tla--event-command elem)
          "\nDirectory: " (tla--event-tree elem)
          "\nDate: " (format-time-string "%c" (tla--event-time elem)))
  (when (not (string= (tla--event-event elem)
                      "started"))
    (insert "\nEvent: " (tla--event-event elem)))
  (insert "\n"))

(defmacro tla--switch-to-buffer-macro (function accessor)
  `(defun ,function ()
     "In a log buffer, pops to the output or error buffer corresponding to the
process at point"
     (interactive)
     (let ((buffer (,accessor
                    (ewoc-data (ewoc-locate tla--log-cookie)))))
       (if (buffer-live-p buffer)
           (tla-switch-to-buffer buffer)
         (error "Buffer has been killed")))))

(tla--switch-to-buffer-macro tla-switch-to-output-buffer
                             tla--event-output-buffer)

(tla--switch-to-buffer-macro tla-switch-to-error-buffer
                             tla--event-error-buffer)

(tla--switch-to-buffer-macro tla-switch-to-related-buffer
                             tla--event-related-buffer)

(defun tla--log-event (output error command tree event)
  "Logs an event in the `tla-log-buffer' buffer"
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
        (ewoc-refresh tla--log-cookie)))))

(defun tla-log-next () (interactive)
  (let ((next (ewoc-next tla--log-cookie
                         (ewoc-locate tla--log-cookie))))
    (when next (goto-char (ewoc-location next)))))

(defun tla-log-prev () (interactive)
  (let ((prev (ewoc-prev tla--log-cookie
                         (ewoc-locate tla--log-cookie))))
    (when prev (goto-char (ewoc-location prev)))))

(define-derived-mode tla-log-buffer-mode fundamental-mode "Xtla Log"
  "Major mode for the Xtla log buffer"
  (toggle-read-only 1))

(defmacro tla--with-keywords (keywords plist &rest body)
  (flet ((keyword-to-symbol (keyword)
                            (intern (subseq (symbol-name keyword) 1))))
    (let ((keyword (gensym))
          (default (gensym)))
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
             (set (intern (subseq (symbol-name ,keyword) 1))
                  (or (cadr (member ,keyword ,plist))
                      ,default))))
         ,@body))))
(put 'tla--with-keywords 'lisp-indent-function 1)

(defun tla--build-tla-command (list-args)
  "Builds a string representing the shell command to execute to run
tla with args LIST-ARGS"
  (mapconcat 'shell-quote-argument
             (cons tla-tla-executable
                   (delq nil list-args))
             " "))

(defun tla--run-tla-async (arguments &rest keys)
  "Run tla asynchrounously. ARGUMENTS is a list of arguments. nil
values in this list are removed. KEYS is a list of keywords and
values. Possible keywords are

 :finished ....... Function run when the process finishes. If none
                   specified, `tla--default-finish-function' is run.

 :killed ......... Function run when the process is killed. If none
                   specified, `tla--default-killed-function' is run.

 :error .......... Function run when the process exits with a non 0
                   status. If none specified,
                   `tla--default-error-function' is run.

All these functions take 3 arguments : output, error, and status.

   - \"output\" is the output buffer
   - \"error\" is the buffer where standard error is redirected
   - \"status\" is a string reprensenting the exit status. (same
              string as `set-process-sentinel')

   `tla--null-handler' can be used here if there's nothing to do.

 :output-buffer .. Buffer where the output of the process should be
                   redirected. If none specified, a new one is
                   created, and will be entered in
                   `tla--dead-process-buffer-queue' to be killed
                   later.

 :error-buffer ... Buffer where the standard error of the process
                   should be redirected.

Example:
  (tla--run-tla-async `(\"changes\" ,(unless arg \"--diffs\"))
                      :finished
                      (lambda (output error status)
                        (message \"No changes in this working copy\"))
                      :error
                      (lambda (output error status)
                        (tla-show-changes-buffer output)))
"
  (tla--with-keywords (:finished :killed :error :output-buffer :error-buffer)
    keys
    (let ((output-buf (or (and output-buffer (get-buffer-create output-buffer))
                          (tla--new-process-buffer nil)))
          (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                          (tla--new-error-buffer nil)))
          (error-file (make-temp-name "/tmp/arch-errors"))
          (command (tla--build-tla-command arguments))
          (default-directory (expand-file-name (concat default-directory "/"))))
      (let* ((process (start-process
                       tla-tla-executable output-buf
                       "sh" "-c"
                       (format "%s 2> %s"
                               command error-file))))
        (message "running process %s in %s" command default-directory)
        (tla--log-event output-buf error-buf command default-directory "started")
        (set-process-sentinel
         process
         `(lambda (process event)
            (tla--log-event ,output-buf ,error-buf ,command
                            ,default-directory
                            (tla--strip-final-newline event))
            (when (file-exists-p ,error-file)
              (with-current-buffer ,error-buf
                (insert-file ,error-file))
              (delete-file ,error-file))
            (unwind-protect
                (cond ((string= event "finished\n")
                       (with-current-buffer ,(current-buffer)
                         (funcall (or (quote ,finished)
                                      'tla--default-finish-function)
                                  ,output-buf ,error-buf
                                  event)))
                      ((string= event "killed\n")
                       (with-current-buffer ,(current-buffer)
                         (funcall (or (quote ,killed)
                                      'tla--default-killed-function
                                      ,output-buf ,error-buf event))))
                      ((string-match "exited abnormally" event)
                       (with-current-buffer ,(current-buffer)
                         (funcall (or (quote ,error)
                                      'tla--default-error-function)
                                  ,output-buf ,error-buf event))))
              ;; Schedule any buffers we created for killing
              (unless ,output-buffer (tla--kill-process-buffer ,output-buf))
              (unless ,error-buffer (tla--kill-process-buffer ,error-buf)))))
        process))))

(defun tla--run-tla-sync (arguments &rest keys)
  "Run tla synchronously. See `tla--run-tla-async' for details"
  (tla--with-keywords (:finished :killed :error :output-buffer :error-buffer)
    keys
    (let ((output-buf (or (and output-buffer (get-buffer-create output-buffer))
                          (tla--new-process-buffer t)))
          (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                          (tla--new-error-buffer t)))
          (command (tla--build-tla-command arguments))
          (error-file (make-temp-name "/tmp/arch-errors")))
      (tla--log-event output-buf error-buf command default-directory "started")
      (let ((status (call-process "sh" nil output-buf nil "-c"
                                  (format "%s 2> %s"
                                          command
                                          error-file))))
        (when (file-exists-p error-file)
          (with-current-buffer error-buf
            (insert-file error-file))
          (delete-file error-file))
        (unwind-protect
            (cond ((stringp status)
                   (when (string= status "Terminated")
                     (funcall (or killed 'tla--default-killed-function)
                              output-buf error-buf status)))
                  ((numberp status)
                   (if (zerop status)
                       (funcall (or finished 'tla--default-finish-function)
                                output-buf error-buf status)
                     (funcall (or error 'tla--default-error-function)
                              output-buf error-buf status)))
                  (t (message "Unknown status - %s" status)))
          ;; Schedule any buffers we created for killing
          (unless output-buffer (tla--kill-process-buffer output-buf))
          (unless error-buffer (tla--kill-process-buffer error-buf)))))))

(defun tla--buffer-content (buffer)
  "Returns the content of BUFFER as a string, striping the final
newline if there is one"
  (with-current-buffer buffer
    (buffer-substring-no-properties
     (point-min)
     (progn (goto-char (point-max))
            (if (eq (char-before) ?\n)
                (- (point) 1)
              (point))))))


(defun tla-switch-to-buffer (buffer)
  "Allow customizable buffer switching."
  (cond ((eq tla-switch-to-buffer-mode 'pop-to-buffer)
         (pop-to-buffer buffer))
        ((eq tla-switch-to-buffer-mode 'single-window)
         (switch-to-buffer buffer))
        ;; TODO : dedicated frame.
        (t
         (error "Switch mode %s not implemented" tla-switch-to-buffer-mode))))

(defun tla--show-last-process-buffer (&optional type mode)
  "Switch to the last used process buffer in a new buffer of type
TYPE. If MODE is specified, it is a function that will be run in the
new buffer. Otherwise, the buffer will remain in fundamental mode, in
read-only."
  (when (buffer-live-p tla--last-process-buffer)
    (let ((content (with-current-buffer tla--last-process-buffer
                     (buffer-string))))
      (tla-switch-to-buffer (tla--get-buffer-create (or type 'generic)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (if mode
        (funcall mode)
      (toggle-read-only 1))))

(provide 'xtla-core)
;; arch-tag: c9e35f5a-6aea-409d-a157-c0d73d92f9b0
;;; xtla-core.el ends here
