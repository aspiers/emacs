;;; xtla.el --- Arch interface for emacs

;; Copyright (C) 2003-2004 by Stefan Reichoer

;; Author: Stefan Reichoer, <xsteve@nit.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; xtla.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xtla.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some documentation can be found on the wiki here
;;     http://wiki.gnuarch.org/moin.cgi/xtla

;; There is a project page at
;;     https://gna.org/projects/xtla-el
;; You can subscribe to the mailing list via
;;     https://mail.gna.org/listinfo/xtla-el-dev

;; Usage:
;; put the following in your .emacs: (require 'xtla)

;; The main commands are available with the prefix key C-x T.
;; Type C-x T C-h for a list.

;; M-x tla-inventory shows a tla inventory
;; In this inventory buffer the following commands are available:
;; e ... tla-edit-log
;; d ... tla-changes
;; l ... tla-changelog
;; L ... tla-logs

;; To Edit a logfile issue: M-x tla-edit-log
;; In this mode you can hit C-c C-d to show the changes
;; Edit the log file
;; After that you issue M-x tla-commit (bound to C-c C-c) to commit the files

;; M-x tla-archives starts the interactive archive browser

;; M-x tla-make-archive creates a new archive directory
;; Many commands are available from here. Look at the menus, they're
;; very helpfull to begin.

;; M-x tla-bookmarks RET
;; Is another good starting point. This is the place where you put the
;; project you work on most often, and you can get a new version, see
;; the missing patches, and a few other usefull features from here.
;; Use `a' to add a bookmark. Add your own projects, and your
;; contributor's projects too. Select several related projects with
;; `m' (unselect with M-u or M-del). Make them partners with 'M-p'.
;; Now, with your cursor on a bookmark, view the uncommited changes,
;; the missing patches from your archive and your contributors with
;; 'M'.

;; M-x tla-file-ediff RET
;; Is an wrapper to tla file-diff, ediff to view the changes
;; interactively.

;; Misc commands:
;; tla-insert-arch-tag inserts a arch-tag entry generated with uuidgen

;; There is an arch archive for xtla.el at: http://xsteve.nit.at/tla
;; Just type: M-x tla-register-archive <RET> http://xsteve.nit.at/tla <RET>
;;            M-x tla-archives <RET>
;; With these commands you can start to browse the archive

;; If you find xtla.el useful, and you have some ideas to improve it
;; please share them with me (Patches are preferred :-))

;; todo:
;; Many things
;;
;; * autoload marks
;; * interface for tla abrowse
;; * dired extension for tla files and operations
;; * modes for tla related files.
;; * run tla-tree-lint in compilation mode.

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'ediff))

(require 'sendmail)
(require 'pp)
(require 'ewoc)
(require 'diff-mode)

(require 'xtla-core)
(require 'xtla-defs)

(eval-and-compile
  (when (locate-library "xtla-version")
    (load-library "xtla-version")))

(autoload 'dired-recursive-delete-directory "dired")

;; ----------------------------------------------------------------------------
;; Internal variables
;; ----------------------------------------------------------------------------
(defvar tla-edit-arch-command nil)
(defvar tla-pre-commit-window-configuration nil)
(defvar tla-log-edit-file-name nil)
(defvar tla-log-edit-file-buffer nil)
(defvar tla-my-id-history nil)
(defvar tla-archive-tree nil)

(defvar tla-buffer-archive-name nil)
(defvar tla-buffer-category-name nil)
(defvar tla-buffer-branch-name nil)
(defvar tla-buffer-version-name nil)
(defvar tla-buffer-refresh-function nil
  "Variable should be local to each buffer. Function used to refresh
the current buffer")
(defvar tla-buffer-marked-file-list nil
  "List of marked files in the current buffer.")
(defvar tla-get-revision-info-at-point-function nil
  "Variable should be local to each buffer.
Function used to get the revision info at point")

(defvar tla-mode-line-process "")
(defvar tla-mode-line-process-status "")

(defvar tla-partner-select-was-interactive nil)

;; Overlay category
(put 'tla-default-button 'mouse-face 'highlight)
(put 'tla-default-button 'evaporate t)
;;(put 'tla-default-button 'rear-nonsticky t)
;;(put 'tla-default-button 'front-nonsticky t)

;; ----------------------------------------------------------------------------
;; Compatibility stuff
;; ----------------------------------------------------------------------------
(if (featurep 'xemacs)
    (require 'xtla-xemacs))

;; Move this to a xtla-emacs.el file if too many stuff come here.
(unless (functionp 'read-directory-name)
  (defalias 'read-directory-name 'read-file-name))

;; ----------------------------------------------------------------------------
;; Face manipulators
;; ----------------------------------------------------------------------------
(defun tla-add-face (str face &optional keymap)
  (if tla-highlight
      (let ((strcpy (format "%s" str)))
        (add-text-properties 0 (length strcpy)
                             `(face ,face
                                    ,@(when keymap
                                        `(mouse-face highlight
                                                     local-map ,keymap)))
                             strcpy)
        strcpy)
    str))

(defun tla-choose-face-to-add (condition text face1 face2)
  "If condition then add face1 to text, else add face2 to text."
  (if condition
      (tla-add-face text face1)
    (tla-add-face text face2)))

;; ----------------------------------------------------------------------------
;; Macros
;; ----------------------------------------------------------------------------
(defmacro tla-toggle-list-entry (list entry)
  "Either add or remove the entry from list"
  `(if (member ,entry ,list)
       (setq ,list (delete ,entry ,list))
     (add-to-list ',list ,entry)))

(defvar tla-last-command ""
  "Last command ran by `tla--run-arch'")

(defvar tla-err-file ""
  "File in which tla redirects its stdout")

(defvar tla-process-sentinel-var 'tla-process-sentinel
  "Function used as a sentinel for tla asynchron process. Don't use
setq on this variable, but set it temporarily with
 (let ((tla-process-sentinel-var 'other-function))
   (tla--run-arch ...))")

;; ----------------------------------------------------------------------------
;; Tla process handler
;; ----------------------------------------------------------------------------
;; If tla is run synchronous, `tla--run-arch' returns the exit status.
(defvar tla-process-associated-buffer nil
  "Buffer from where the process was started. This variable is local
to the process in which the tla process executes, and points to
another buffer. In the process sentinel, we switch back to this
buffer for some tasks")

(defvar tla-process-cmd nil
  "Typd of process being run")

(defvar tla-proc nil
  "Current tla process")

(defvar tla--error-buffer nil)

(defun tla--run-arch (run-asynchron clear-process-buffer cmdtype &rest arglist)
  (let ((current-dir default-directory)
        (from-buffer (current-buffer)))
    (when tla-edit-arch-command
      (setq arglist (append arglist
                            (split-string
                             (read-from-minibuffer
                              (format "arch %s %S " cmdtype arglist)))))
      (when (eq tla-edit-arch-command t)
        (tla-toggle-edit-cmd-flag t))
      (message "tla--run-arch %s: %S" cmdtype arglist))
    (let* ((proc-buf (tla--new-process-buffer (not
                                               run-asynchron)))
           (err-buffer (tla--new-error-buffer (not run-asynchron)))
           (err-file (make-temp-name "/tmp/arch-errors")))
      (when (listp (car arglist))
        (setq arglist (car arglist)))
      (save-excursion
        (set-buffer proc-buf)
        (cd current-dir)
        (setq buffer-read-only nil)
        (fundamental-mode)
        (if clear-process-buffer
            (erase-buffer)
          (goto-char (point-max)))
        (set (make-local-variable
              'tla-process-associated-buffer)
             from-buffer)
        (set (make-local-variable
              'tla--error-buffer) err-buffer)
        (with-current-buffer tla--last-process-buffer
          (set (make-local-variable 'tla-process-cmd) cmdtype))
        (setq tla-mode-line-process-status (format " running %s" cmdtype))
        (tla-update-mode-line)
                                        ;              (sit-for 0)
        (setq tla-last-command tla-tla-executable)
        (dolist (x arglist)
          (setq tla-last-command
                (concat tla-last-command
                        " " (shell-quote-argument x))))
        (message "Using obsolete tla--run-arch to run %s ..." tla-last-command)
        (if run-asynchron
            (progn
              (error "Asynchronous processes are now all managed by tla--run-tla-async")
              (setq tla-err-file err-file)
              (setq tla-proc (apply 'start-process "arch"
                                    proc-buf
                                    "sh" "-c" (list (concat
                                                     tla-last-command " 2>"
                                                     err-file))))
              (set-process-sentinel tla-proc
                                    tla-process-sentinel-var))
          ;;(message "running synchron: tla %S" arglist)
          (prog1
              (apply 'call-process tla-tla-executable nil
                     `(,(buffer-name proc-buf) ,err-file)
                     nil arglist)
            (when (file-exists-p err-file)
              (save-window-excursion
                (set-buffer err-buffer)
                (erase-buffer)
                (let ((msg (progn (insert-file err-file)
                                  (buffer-substring-no-properties
                                   (point-min)
                                   (point-max)))))
                  (when msg (message msg))))
              (delete-file err-file))
            (setq tla-mode-line-process-status "")
            (tla-update-mode-line)))))))

;; DEPRECATED
(defun tla-process-sentinel (process event)
  ;;(princ (format "Process: %s had the event `%s'" process event)))
  ;;(save-excursion
  (error "This function is now deprecated")
  (let* ((process-buffer (process-buffer process))
         (error-buffer (with-current-buffer process-buffer
                         tla--error-buffer)))
    (when tla-log-commands
      (with-current-buffer tla-log-buffer
        (goto-char (point-max))
        (insert "\nProcess \"")
        (dolist (x (process-command process))
          (insert x " ")) (delete-backward-char 1)
          (insert "\"\n")
          (insert "Event: " event
                  "Date: " (current-time-string) "\n")))
    (when (file-exists-p tla-err-file)
      (save-window-excursion
        (set-buffer error-buffer)
        (erase-buffer)
        (let ((msg (progn (insert-file tla-err-file)
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max)))))
          (when msg (message msg))))
      (delete-file tla-err-file))
    (let ((tla-process-cmd (with-current-buffer process-buffer
                             tla-process-cmd)))
      (set-buffer (process-buffer process))
      (set-buffer tla-process-associated-buffer)
      (setq tla-mode-line-process-status "")
      (tla-update-mode-line)
      (cond ((string= event "finished\n")
             (case tla-process-cmd
               (archive-mirror (tla-show-process-buffer-internal t)
                               (message "tla archive-mirror finished"))
               ;; Now obsolete.
               (star-merge (tla-show-process-buffer-internal t)
                           (tla-show-changes-buffer process-buffer)
                           (message "tla star-merge finished")
                           ;; TODO is it really the right directory ?
                           (tla-revert-some-buffers default-directory))
               ;; Now obsolete.
               (replay (tla-show-process-buffer-internal t)
                       (tla-show-changes-buffer process-buffer)
                       (message "tla replay finished")
                       ;; TODO is it really the right directory ?
                       (tla-revert-some-buffers default-directory))
               ;; Now obsolete.
               (sync-tree (tla-show-process-buffer-internal t)
                          (message "tla sync-tree finished")
                          ;; TODO is it really the right directory ?
                          (tla-revert-some-buffers default-directory))
               ;; Now obsolete.
               (delta (let ((no-changes))
                        (save-excursion
                          (set-buffer tla--last-process-buffer)
                          (setq no-changes (= (- (point-max) (point-min)) 1)))
                        (if no-changes
                            (message "tla delta finished: No changes in this arch working copy")
                          (tla-show-changes-buffer process-buffer)
                          (message "tla delta finished"))))
               (changes (message "No changes in this working copy"))
               (t (message "tla command finished"))))
            ((string= event "killed\n")
             (message "tla process killed"))
            ((string-match "exited abnormally" event)
             (cond ((and (or (eq tla-process-cmd 'star-merge)
                             (eq tla-process-cmd 'changes)
                             (eq tla-process-cmd 'replay)
                             (string-match "code 1$" event)))
                    (tla-show-changes-buffer process-buffer))
                   (t
                    (while (accept-process-output process 0 100))
                    ;; find last error message and show it.
                    (goto-char (point-max))
                    (message "tla failed: %s"
                             (if (re-search-backward "^tla: \\(.*\\)\n" nil t)
                                 (match-string 1)
                               event)))))
            (t
             (message "tla process had unknown event: %s" event))
            (tla-show-process-buffer-internal t)))
    (tla--kill-process-buffer process-buffer)
    (tla--kill-process-buffer process-buffer)))

(defun tla-show-process-buffer ()
  (interactive)
  (tla-show-process-buffer-internal))


(defun tla-show-process-buffer-internal (&optional scroll-to-top new-buffer-name mode-func)
  "Show the result of tla process.
If SCROLL-TO-TOP is non-nil, the point is moved to the top of buffer.
If NEW-BUFFER-NAME(a string) is given, the shown buffer is renamed to it.
If MODE-FUNC(a function with no argument) is given, it is called with selecting shown buffer.
MODE-FUNC is assumed to use set up a buffer mode. "
  (tla--show-last-process-buffer)
  (when new-buffer-name
    (if  (get-buffer new-buffer-name)
        (kill-buffer new-buffer-name))
    (rename-buffer new-buffer-name))
  (if mode-func
      (funcall mode-func))
  (when scroll-to-top
    (goto-char (point-min)))
  (other-window 1))

(defun tla--show-error-buffer (buffer)
  "Pops up the error buffer"
  (tla-switch-to-buffer buffer))

(defun tla-get-process-output ()
  (save-excursion
    (set-buffer tla--last-process-buffer)
    (if (> (point-max) (point-min))
        (buffer-substring-no-properties (point-min) (- (point-max) 1))
      "")))

(defun tla-update-mode-line ()
  (setq tla-mode-line-process tla-mode-line-process-status)
  ;; The following is not yet needed:
  ;; (concat tla-status-mode-line-process-edit-flag tla-mode-line-process-status))
  (force-mode-line-update))


;; ----------------------------------------------------------------------------
;; Common used functions for many xtla modes
;; ----------------------------------------------------------------------------
(defun tla-kill-all-buffers ()
  "Kill all xtla buffers.
These buffers match the regexp \"^ ?\\*tla-.+\\*\"."
  (interactive)
  (mapcar '(lambda (buf)
             (when (string-match "^ ?\\*tla-.+\\*" (buffer-name buf))
               (kill-buffer buf))) (buffer-list)))

(defun tla-buffer-quit ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun tla-edit-=tagging-method ()
  "Edit the {arch}/=tagging-method file."
  (interactive)
  (find-file (concat (tla-tree-root) "/{arch}/=tagging-method")))

(defun tla-ewoc-delete (cookie elem)
  "Remove element ELEM from COOKIE"
  (ewoc-filter cookie
               '(lambda (x) (not (eq x (ewoc-data elem))))))

(defun tla-generic-refresh ()
  "Calls the function specified by `tla-buffer-refresh-function'"
  (interactive)
  (funcall tla-buffer-refresh-function))

(defun tla--get-info-at-point ()
  "Get the version information that point is on."
  (when (fboundp tla-get-revision-info-at-point-function)
    (funcall tla-get-revision-info-at-point-function)))

(defvar tla-window-config nil
  "Used for inter-function communication.")

(defun tla--ediff-buffers (bufferA bufferB)
  "Wrapper around `ediff-buffers'"
  (let ((tla-window-config (current-window-configuration)))
    (ediff-buffers bufferA bufferB
                   '(tla-ediff-startup-hook) 'tla-ediff)))

;; should replace: tla-read-archive-category-name
(defun tla--complete-category (archive &optional prompt)
  (tla-categories-build-archive-tree archive)
  (completing-read
   (or prompt (concat "category for " archive ": "))
   (cddr (tla-archive-tree-get-archive archive))
   nil nil nil
   'tla-read-archive-category-history))

(defun tla--insert-right-justified (string count &optional face)
  "Inserts STRING preceded by spaces so that the line ends exaclty at
COUNT characters (or after if STRING is too long."
  (insert-char ?\  (max 0 (- count (length string))))
  (insert (if face (tla-add-face string face) string))
  )

;; should replace: tla-read-archive-category-branch-name
(defun tla--complete-branch (archive category &optional prompt)
  (tla-branches-build-archive-tree archive category)
  (let ((branch (completing-read
                 (or prompt (concat "branch for " archive "/" category ": "))
                 (cdr (tla-archive-tree-get-category archive category))
                 nil nil nil
                 'tla-read-archive-category-branch-history)))
    (if (string= branch "")
        nil
      branch)))

;; should replace: tla-read-archive-category-branch-version-name
(defun tla--complete-version (archive category branch &optional prompt)
  (tla-versions-build-archive-tree archive category branch)
  (let ((version (completing-read
                  (or prompt (concat "version for " (tla-fully-qualified-revision
                                                     archive category branch) ": "))
                  (cdr (tla-archive-tree-get-branch archive category branch))
                  nil nil nil
                  'tla-read-archive-category-branch-version-history)))
    (if (string= version "")
        nil
      version)))

;; should replace: tla-read-archive-category-branch-version-revision-name
(defun tla--complete-revision (archive category branch version &optional prompt)
  (tla-revisions-build-archive-tree archive category branch version)
  (let ((revision (completing-read
                   (or prompt (concat "revision for " (tla-fully-qualified-revision
                                                       archive category branch version) ": "))
                   (cdr (tla-archive-tree-get-version archive category branch version))
                   nil nil nil
                   'tla-read-archive-category-branch-version-revision-history)))
    (if (string= revision "")
        nil
      revision)))

(defun tla--get-archive (rev)
  "Get the archive part of a fully qualified revision"
  (if rev
      (nth 0 (tla-split-revision-name (cadr rev)))
    (car (tla-read-archive-name))))

(defun tla--get-category (rev)
  "Get the archive/category part of a fully qualified revision"
  (let* ((revs (tla-split-revision-name rev))
         (archive (tla-archive-name revs))
         (category (and archive
                        (or (tla-category-name revs)
                            (tla--complete-category archive)))))
    (tla-fully-qualified-revision archive category)))

(defun tla--get-branch (rev)
  "Get the archive/category-branch part of a fully qualified revision"
  (let* ((revs (tla-split-revision-name rev))
         (archive (tla-archive-name revs))
         (category (and archive
                        (or (tla-category-name revs)
                            (tla--complete-category archive))))
         (branch (and category
                      (or (tla-branch-name revs)
                          (tla--complete-branch archive category)))))
    (tla-fully-qualified-revision archive category branch)))

(defun tla--get-version (rev)
  "Get the archive/category-branch-version part of a fully qualified revision"
  (let* ((revs (tla-split-revision-name rev))
         (archive (tla-archive-name revs))
         (category (and archive
                        (or (tla-category-name revs)
                            (tla--complete-category archive))))
         (branch (and category
                      (or (tla-branch-name revs)
                          (tla--complete-branch archive category))))
         (version (and branch
                       (or (tla-version-name revs)
                           (tla--complete-version archive category branch)))))
    (tla-fully-qualified-revision archive category branch version)))

(defun tla--get-revision (rev)
  "Get the archive/category-branch-version-revision part of a fully qualified revision"
  (let* ((revs (tla-split-revision-name rev))
         (archive (tla-archive-name revs))
         (category (and archive
                        (or (tla-category-name revs)
                            (tla--complete-category archive))))
         (branch (and category
                      (or (tla-branch-name revs)
                          (tla--complete-branch archive category))))
         (version (and branch
                       (or (tla-version-name revs)
                           (tla--complete-version archive category branch))))
         (revision (and version
                        (or (tla-revision-name revs)
                            (tla--complete-revision archive category branch version)))))
    (tla-fully-qualified-revision archive category branch version revision)))

(defun tla-tree-root (&optional location no-error)
  "Returns the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an {arch}
directory in a parent buffer of LOCATION. This is therefore very
fast."
  (setq location (or location default-directory))
  (let ((pwd location))
    (while (not (or (string= pwd "/")
                    (file-exists-p (concat pwd "/{arch}"))))
      (setq pwd (expand-file-name (concat pwd "/.."))))
    (if (file-exists-p (concat pwd "/{arch}/=tagging-method"))
          (expand-file-name
           (replace-regexp-in-string "/$" "" pwd))
      (if no-error
          nil
        (error "%S is not in an arch-managed tree!" location)))))

(defun tla-save-some-buffers (&optional tree)
  "Saves all buffers visiting a file in TREE"
  (let ((ok t)
	(tree (or (tla-tree-root tree t)
		  tree)))
    (unless tree
      (error "Not in a project tree."))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (buffer-modified-p)
          (let ((file (buffer-file-name)))
            (when file
              (let ((root (tla-tree-root (file-name-directory file) t))
                    (tree-exp (expand-file-name tree)))
                (when (and (string= root tree-exp)
                           ;; buffer is modified and in the tree TREE.
                           (or tla-do-not-prompt-for-save
                               (y-or-n-p (concat "Save buffer "
                                                 (buffer-name)
                                                 "? "))
                               (setq ok nil)))
                  (save-buffer))))))))
    ok))

(defun tla-revert-some-buffers (&optional tree)
  "Reverts all buffers visiting a file in TREE that aren't modified.
To be ran after an update or a merge."
  (let ((tree (tla-tree-root tree)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (not (buffer-modified-p))
          (let ((file (buffer-file-name)))
            (when file
              (let ((root (tla-tree-root (file-name-directory file) t))
                    (tree-exp (expand-file-name tree)))
                (when (and (string= root tree-exp)
                           ;; buffer is modified and in the tree TREE.
                           tla-automatically-revert-buffers)
                  ;; TODO
                  ;; what should we do if file is not existed? >> Matthieu
                  (if (file-exists-p file)
                      (revert-buffer t t)))))))))))

;; ----------------------------------------------------------------------------
;; tla help system for commands that get input from the user via the minibuffer
;; ----------------------------------------------------------------------------

;; When the user is asked for input in the minibuffer, a help for the
;; command will be shown, if the user hits f1
;; This functionality is not only for xtla commands available
;; it is available for all emacs commands
;; to check: we should use some other binding for this, perhaps f1 C-m

;; GENERIC: This functionality should be in emacs itself. >> Masatake

(defun tla-display-command-help (command &optional current-prompt)
  (with-electric-help
   (lambda ()
     (let ((cmd-help (when (fboundp command)
                       (documentation command))))
       (delete-region (point-min) (point-max))
       (insert (if cmd-help
                   (format "Help for %S:\n%s" command cmd-help)
                 (format "No help available for %S" command)))))
   " *tla-command-help*"))

(defvar tla-command-stack nil)

(defun tla-minibuffer-setup ()
  (push  this-command tla-command-stack))

(defun tla-minibuffer-exit ()
  (pop tla-command-stack))

(defun tla-show-command-help ()
  (interactive)
  (tla-display-command-help (car tla-command-stack)))

(when tla-install-command-help-system
  (define-key minibuffer-local-map [f1] 'tla-show-command-help)
  (define-key minibuffer-local-completion-map [f1] 'tla-show-command-help)
  (define-key minibuffer-local-must-match-map [f1] 'tla-show-command-help)
  (add-hook 'minibuffer-setup-hook 'tla-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'tla-minibuffer-exit))

;; ----------------------------------------------------------------------------
;; Top level tla commands
;; ----------------------------------------------------------------------------
(defcustom tla-make-log-function 'tla-default-make-log-function
  "*function used to create the log buffer. Must return a string which
is the name absolute of the log file. This function is called only
when the log file doesn't exist already. The default is
`tla-default-make-log-function', which just calls \"tla make-log\". If
you want to override this function, you may just write a wrapper
around `tla-default-make-log-function'."
  :type 'function
  :group 'xtla)

(defun tla-make-log ()
  (interactive)
  (let* ((version (tla-tree-version-list))
         (file (concat (tla-tree-root) "/++log."
                       (tla-category-name version) "--"
                       (tla-branch-name   version) "--"
                       (tla-version-name  version) "--"
                       (tla-archive-name  version))))
    (if (file-exists-p file)
        file
      (funcall tla-make-log-function))))

(defun tla-default-make-log-function ()
  (tla--run-tla-sync '("make-log")
                     :finished
                     (lambda (output error status)
                       (tla--buffer-content output))))

(defun tla-pop-to-inventory ()
  (interactive)
  (tla-inventory nil t))

(defvar tla-inventory-cookie nil)
(defvar tla-inventory-list nil
  "Full list for the inventory.")

(defun tla-inventory-goto-file (file)
  "Put cursor on FILE. nil return means the file hasn't been found"
  (goto-char (point-min))
  (let ((current (ewoc-locate tla-inventory-cookie)))
    (while (and current (not (string= (caddr (ewoc-data current))
                                      file)))
      (setq current (ewoc-next tla-inventory-cookie current)))
    (when current (tla-inventory-cursor-goto current))
    current))


(defun tla-inventory-make-toggle-fn-and-var (variable function)
  "Defines the variable and the toggle function for type TYPE."
  (eval `(defvar ,variable t))
  (eval `(defun ,function ()
           (interactive)
           (setq ,variable (not ,variable))
           (tla-inventory-redisplay))))



(dolist (type-arg tla-inventory-file-types-manipulators)
  (tla-inventory-make-toggle-fn-and-var (cadr type-arg) (caddr type-arg)))

(defun tla-inventory-redisplay ()
  (let* ((elem (ewoc-locate tla-inventory-cookie))
         (file (when elem (caddr (ewoc-data elem))))
         (pos (point)))
    (tla-inventory-display)
    (or (and file
             (tla-inventory-goto-file file))
        (goto-char pos))
    (tla-inventory-cursor-goto (ewoc-locate tla-inventory-cookie))))


(defun tla-inventory-set-toggle-variables (new-value)
  "Set all tla-inventory-display-* variables.
If NEW-VALUE is 'toggle set the values to (not tla-inventory-display-*
Otherwise set it to NEW-VALUE."
  (dolist (type-arg tla-inventory-file-types-manipulators)
    (eval `(setq ,(cadr type-arg)
                 (if (eq new-value 'toggle)
                     (not ,(cadr type-arg))
                   new-value)))))

(defun tla-inventory-set-all-toggle-variables ()
  (interactive)
  (tla-inventory-set-toggle-variables t)
  (tla-inventory-redisplay))

(defun tla-inventory-reset-all-toggle-variables ()
  (interactive)
  (tla-inventory-set-toggle-variables nil)
  (tla-inventory-redisplay))

(defun tla-inventory-toggle-all-toggle-variables ()
  (interactive)
  (tla-inventory-set-toggle-variables 'toggle)
  (tla-inventory-redisplay))


;;;###autoload
(defun tla-inventory (&optional directory arg)
  "Show a tla inventory at DIRECTORY.
When called with a prefix arg, pop to the inventory buffer.
DIRECTORY defaults to the current one when within an arch managed tree,
unless you gave two prefix args."
  (interactive (list (let ((tree-root (tla-tree-root nil t)))
                       (if (or (not tree-root)
                               (and current-prefix-arg
                                    (> (car current-prefix-arg) 4)))
                           (read-directory-name "TLA Inventory (directory): "
                                                nil nil t)
                         default-directory))
                     current-prefix-arg))
  ;;    (tla--run-arch nil t 'inventory "inventory" "--kind" "--ids")
  (let ((directory (or directory default-directory)))
    (if arg
        (pop-to-buffer (tla--get-buffer-create 'inventory directory))
      (switch-to-buffer (tla--get-buffer-create 'inventory directory)))
    (cd directory))
  (tla-inventory-mode)
  (tla--run-tla-sync
   '("inventory" "--both")
   :finished
   (lambda (output error status)
     (let ((list (split-string (tla--buffer-content output) "\n"))
           (inventory-list '()))
       (mapc
        (lambda (item)
          (when (string-match "\\([A-Z]\\)\\([\\? ]\\) \\(.*\\)" item)
            (push (list (string-to-char (match-string 1 item))
                        (string= (match-string 2 item) "?")
                        (match-string 3 item))
                  inventory-list)))
        list)
       (setq inventory-list (reverse inventory-list))
       (set (make-local-variable 'tla-inventory-list)
            inventory-list)
       (tla-inventory-display)))))

(defun tla-inventory-display ()
  (interactive)
  (let (buffer-read-only)
    (erase-buffer)
    (set (make-local-variable 'tla-inventory-cookie)
         (ewoc-create 'tla-inventory-printer))
    (tla-inventory-insert-headers)
    (dolist (elem tla-inventory-list)
      (let ((type (car elem)))
        (if (eval (cadr (assoc type
                               tla-inventory-file-types-manipulators)))
            (ewoc-enter-last tla-inventory-cookie elem)))))
  (goto-char (point-min)))

(defun tla-inventory-printer (elem)
  (let ((type (car elem))
        (question (cadr elem))
        (file (caddr elem)))
    (insert (format "%s%c%s  %s"
                    (if (member file tla-buffer-marked-file-list)
                        " * " "   ")
                    type (if question "?" " ") file))))

(defun tla-inventory-mark-file ()
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie))
        (file (tla--get-file-info-at-point)))
    (add-to-list 'tla-buffer-marked-file-list file)
    (ewoc-refresh tla-inventory-cookie)
    (tla-inventory-cursor-goto (or (ewoc-next tla-inventory-cookie
                                              current)
                                   current))))

(defun tla-inventory-unmark-file ()
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie))
        (file (tla--get-file-info-at-point)))
    (setq tla-buffer-marked-file-list
          (delete file tla-buffer-marked-file-list))
    (ewoc-refresh tla-inventory-cookie)
    (tla-inventory-cursor-goto (or (ewoc-next tla-inventory-cookie
                                              current)
                                   current))))

(defun tla-inventory-unmark-all ()
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie)))
    (setq tla-buffer-marked-file-list nil)
    (ewoc-refresh tla-inventory-cookie)
    (tla-inventory-cursor-goto current)))

(defvar tla-get-file-info-at-point-function nil
  "Function used to get the file at point, anywhere")

(defun tla--get-file-info-at-point ()
  (funcall tla-get-file-info-at-point-function))

(defun tla-inventory-get-file-info-at-point ()
  (caddr (ewoc-data (ewoc-locate tla-inventory-cookie))))

(defun tla-inventory-insert-headers ()
  (ewoc-set-hf tla-inventory-cookie
               (tla-add-face (format "tla inventory for %s\n" default-directory)
                             'tla-archive-name)
               "\nend."))

(defvar tla-buffer-source-buffer nil
  "Buffer from where a command was called")

;;;###autoload
(defun tla-edit-log (&optional insert-changelog source-buffer)
  "Edit the tla log file.
With an optional prefix argument, insert the last group of entries from the
ChangeLog file. SOURCE-BUFFER, if non-nil, is the buffer from which
the function was called. It is used to get the list of marked files,
and potentially run a selected file commit."
  (interactive "P")
  (setq tla-pre-commit-window-configuration (current-window-configuration))
  (when (and tla--last-process-buffer (get-buffer tla--last-process-buffer))
    (kill-buffer tla--last-process-buffer))
  (setq tla-log-edit-file-name (tla-make-log))
  (tla-switch-to-buffer
   (find-file-noselect tla-log-edit-file-name))
  (when insert-changelog
    (goto-char (point-max))
    (let ((buf (find-file-noselect (find-change-log))))
      (insert-buffer buf))
    (when (re-search-forward "^2" nil t)
      (delete-region (line-beginning-position)
                     (line-beginning-position 3)))
    (when (re-search-forward "^2" nil t)
      (delete-region (line-beginning-position) (point-max)))
    (goto-char (point-min)))
  (tla-log-edit-mode)
  (set (make-local-variable 'tla-buffer-source-buffer)
       source-buffer)
  (end-of-line))

;;;###autoload
(defun tla-add-log-entry ()
  "Add new tla log ChangeLog style entry."
  (interactive)
  (save-restriction
    (tla-add-log-entry-internal)))

(defun tla-add-log-entry-internal ()
  ;; This is mostly copied from add-log.el.  Perhaps it would be better to
  ;; split add-change-log-entry into several functions and then use them, but
  ;; that wouldn't work with older versions of Emacs.
  (require 'add-log)
  (let* ((defun (add-log-current-defun))
	 (buf-file-name (if (and (boundp 'add-log-buffer-file-name-function)
                                 add-log-buffer-file-name-function)
                            (funcall add-log-buffer-file-name-function)
			  buffer-file-name))
	 (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
	 (file-name (tla-make-log))
	 ;; Set ENTRY to the file name to use in the new entry.
	 (entry (if (functionp 'add-log-file-name)
                    (add-log-file-name buffer-file file-name)
                  (file-relative-name buffer-file (tla-tree-root))))
         beg
	 bound
	 narrowing)
    (tla-edit-log)
    (undo-boundary)
    (goto-char (point-min))
    (when (re-search-forward "^Patches applied:" nil t)
      (narrow-to-region (point-min) (match-beginning 0))
      (setq narrowing t)
      (goto-char (point-min)))
    (re-search-forward "\n\n\\|\\'")
    (setq beg (point))
    (setq bound
	  (progn
            (if (looking-at "\n*[^\n* \t]")
                (skip-chars-forward "\n")
	      (if (and (boundp 'add-log-keep-changes-together)
                       add-log-keep-changes-together)
                  (goto-char (point-max))
		(forward-paragraph))) ; paragraph delimits entries for file
	    (point)))
    (goto-char beg)
    (forward-line -1)
    ;; Now insert the new line for this entry.
    (cond ((re-search-forward "^\\s *\\*\\s *$" bound t)
	   ;; Put this file name into the existing empty entry.
	   (if entry
	       (insert entry)))
	  ((let (case-fold-search)
             (re-search-forward
              (concat (regexp-quote (concat "* " entry))
                      ;; Don't accept `foo.bar' when
                      ;; looking for `foo':
                      "\\(\\s \\|[(),:]\\)")
              bound t))
	   ;; Add to the existing entry for the same file.
	   (re-search-forward "^\\s *$\\|^\\s \\*")
	   (goto-char (match-beginning 0))
	   ;; Delete excess empty lines; make just 2.
	   (while (and (not (eobp)) (looking-at "^\\s *$"))
	     (delete-region (point) (line-beginning-position 2)))
	   (insert-char ?\n 2)
	   (forward-line -2)
	   (indent-relative-maybe))
	  (t
	   ;; Make a new entry.
           (if tla-log-insert-last
               (progn
                 (goto-char (point-max))
                 (re-search-backward "^.")
                 (end-of-line)
                 (insert "\n\n* ")
                 )
             (forward-line 1)
             (while (looking-at "\\sW")
               (forward-line 1))
             (while (and (not (eobp)) (looking-at "^\\s *$"))
               (delete-region (point) (line-beginning-position 2)))
             (insert-char ?\n 3)
             (forward-line -2)
             (indent-to left-margin)
             (insert "* "))
           (if entry (insert entry))))
    (if narrowing (widen))
    ;; Now insert the function name, if we have one.
    ;; Point is at the entry for this file,
    ;; either at the end of the line or at the first blank line.
    (if defun
	(progn
	  ;; Make it easy to get rid of the function name.
	  (undo-boundary)
	  (unless (save-excursion
		    (beginning-of-line 1)
		    (looking-at "\\s *$"))
	    (insert ?\ ))
	  ;; See if the prev function name has a message yet or not
	  ;; If not, merge the two entries.
	  (let ((pos (point-marker)))
	    (if (and (skip-syntax-backward " ")
		     (skip-chars-backward "):")
		     (looking-at "):")
		     (progn (delete-region (+ 1 (point)) (+ 2 (point))) t)
		     (> fill-column (+ (current-column) (length defun) 3)))
		(progn (delete-region (point) pos)
		       (insert ", "))
	      (goto-char pos)
	      (insert "("))
	    (set-marker pos nil))
	  (insert defun "): "))
      ;; No function name, so put in a colon unless we have just a star.
      (unless (save-excursion
		(beginning-of-line 1)
		(looking-at "\\s *\\(\\*\\s *\\)?$"))
	(insert ": ")))))

;;;###autoload
(defun tla-changes (&optional summary against)
  "Run tla changes.
When called without a prefix argument: show the detailed diffs also.
When called with a prefix argument: do not show detailed diffs.
When AGAINST is non-nil, use it as comparison tree."
  (interactive "P")
  (tla-save-some-buffers)
  (let* ((root (tla-tree-root))
         (buffer (tla--get-buffer-create 'changes root)))
    (with-current-buffer buffer
      (let (buffer-read-only) (erase-buffer))))
  (tla--run-tla-async `("changes" ,(unless summary "--diffs") ,(when against against))
                      :finished
                      (lambda (output error status)
                        (message "No changes in this working copy"))
                      :error
                      (lambda (output error status)
                        (let ((status-number
                               (progn
                                 (string-match "with code \\(.*\\)"
                                               status)
                                 (string-to-int
                                  (substring status (match-beginning
                                                     1))))))
                          (if (equal 1 status-number)
                              (tla-show-changes-buffer output)
                            (tla--default-error-function output error
                                                         status))))))

(defun tla-changes-printer (elem)
  "ewoc pretty-printer"
  (insert (if (member (car elem) tla-buffer-marked-file-list) "* " "  ")
          (cadr elem) (caddr elem) " " (car elem))
  )

(defvar tla-changes-cookie nil
  "ewoc cookie for the changes buffer.")

(defconst tla-verbose-format-spec
  '(("added files"    "A" " ")
    ("modified files" "M" " "))
  "Internal variable used to parse the output of tla show-changeset"
  )

(defun tla-show-changes-buffer (buffer &optional verbose-format)
  "Show the *tla-changes* buffer built from the *tla-process* buffer.
If VERBOSE-FORMAT is non-nil, the format of the *tla-process* buffer
should be the one of tla show-changeset."
  (let* ((root (with-current-buffer buffer
                 (tla-tree-root default-directory t)))
         (changes-buffer (tla--get-buffer-create 'changes root))
         (header ""))
    (tla-switch-to-buffer changes-buffer)
    (let (buffer-read-only)
      (erase-buffer)
      (tla-changes-mode)
      (with-current-buffer buffer
        (if verbose-format
            (progn
              (goto-char (point-min))
              (while (re-search-forward
                      (concat "^\\* \\(" (regexp-opt
                                          (mapcar 'car tla-verbose-format-spec))
                              "\\)\n")
                      nil t)
                (let* ((elem (assoc (match-string 1)
                                    tla-verbose-format-spec))
                       (modif (cadr elem))
                       (dir (caddr elem)))
                  (if (string= modif "M")
                      (while (re-search-forward "^--- orig/\\(.*\\)$"
                                                nil t)
                        (let ((file (match-string 1)))
                          (with-current-buffer changes-buffer
                            (ewoc-enter-last tla-changes-cookie
                                             (list file modif dir)))))
                    (while (looking-at "^$") (forward-line 1))
                    (while (looking-at
                            "^ +\\([^ ].*\\)$")
                      (let ((file (match-string 1)))
                        (with-current-buffer changes-buffer
                          (ewoc-enter-last tla-changes-cookie
                                           (list file modif dir)))
                        (forward-line 1))))))
              (goto-char (point-min))
              (re-search-forward "^---")
              (forward-line -1))
          (setq header (buffer-substring-no-properties
                        (goto-char (point-min))
                        (progn (re-search-forward "^[^*]" nil t)
                               (beginning-of-line)
                               (point))))
          (while (looking-at "^\\(.\\)\\([ /-]\\) +\\(.*\\)$")
            (let ((file (match-string 3))
                  (modif (match-string 1))
                  (dir (match-string 2)))
              (with-current-buffer changes-buffer
                (ewoc-enter-last tla-changes-cookie
                                 (list file modif dir))))
            (forward-line 1)))
        (let ((footer (concat
		       ;; - is a keyword for diff output.
		       "________________________________________________________________________\n\n"
		       (buffer-substring-no-properties
                       (point) (point-max)))))
          (with-current-buffer changes-buffer
            (ewoc-set-hf tla-changes-cookie header footer)
            (if root (cd root)))))
      ))
  (toggle-read-only 1)
  (when (or (and (boundp 'global-font-lock-mode)
		 global-font-lock-mode)
	    (and (boundp 'font-lock-maximum-decoration)
		 font-lock-maximum-decoration))
    (font-lock-fontify-buffer))
  (if (ewoc-nth tla-changes-cookie 0)
      (goto-char (ewoc-location (ewoc-nth tla-changes-cookie 0)))))


;;;###autoload
(defun tla-delta (from to)
  "Runs tla delta FROM TO"
  (interactive (list
                (apply 'tla-fully-qualified-revision
                       (tla-read-archive-category-branch-version-revision-name))
                (apply 'tla-fully-qualified-revision
                       (tla-read-archive-category-branch-version-revision-name))))
  (tla--run-tla-async (list "delta" "--diffs" from to)
                      :finished
                      (lambda (output error status)
                        (with-current-buffer output
                          (let ((no-changes
                                 ;; There were no changes if the last line of
                                 ;; the buffer is "* changeset report"
                                 (save-excursion
                                   (goto-char (point-max))
                                   (previous-line 1)
                                   (beginning-of-line)
                                   (looking-at "^* changeset report"))))
                            (if no-changes
                                (message
                                 (concat "tla delta finished: No "
                                         "changes in this arch working copy"))
                              (tla-show-changes-buffer output)
                              (message "tla delta finished")))))))

;;;###autoload
(defun tla-get-changeset (revision justshow &optional destination
                                   without-diff)
  "When JUSTSHOW is non-nil, just show the diff. Otherwise, store
changeset in DESTINATION"
  (interactive "sRevision to view: \np")
  (let ((dest (or destination
                  (make-temp-name "/tmp/tla-changeset"))))
    (tla--run-arch nil t 'get-changeset "get-changeset" revision
                  dest)
    (when justshow
      (if without-diff
          (tla--run-arch nil t 'show-changeset "show-changeset" dest)
        (tla--run-arch nil t 'show-changeset "show-changeset"
                      "--diffs" dest))
      (tla-show-changes-buffer tla--last-process-buffer t)
      (call-process "rm" nil nil nil "-rf"
                    dest))
    )
  )

;;;###autoload
(defun tla-file-ediff-revisions (file &optional revision1 revision2)
  "View changes between REVISION1 and REVISION2 in file FILE using ediff."
  (interactive (let ((version-list (tla-tree-version-list)))
                 (list (buffer-file-name)
                       (apply 'tla-fully-qualified-revision
                              (append version-list
                                      (list
                                       (apply 'tla--complete-revision
                                              version-list))))
                       (apply 'tla-fully-qualified-revision
                              (append version-list
                                      (list
                                       (apply 'tla--complete-revision
                                              version-list)))))))
  (tla--ediff-buffers
   (tla-file-get-original file revision1)
   (tla-file-get-original file revision2)))

;;;###autoload
(defun tla-file-diff (file &optional revision)
  "Run tla file-diff on file FILE. In interactive mode, the file is
the current buffer's file."
  (interactive (list (buffer-file-name)))
  (let ()
    (if revision
        (tla--run-arch nil t 'file-diffs "file-diffs" file revision)
      (tla--run-arch nil t 'file-diffs "file-diffs" file))
    (if (with-current-buffer tla--last-process-buffer
          (= (point-max) (point-min)))
        (message "No changes in this arch working copy")
      (tla-show-process-buffer-internal t "*tla-file-diffs*"
                                        'diff-mode)
      (pop-to-buffer "*tla-file-diffs*"))))

(defvar tla-mine-string "TREE")
(defvar tla-his-string "MERGE-SOURCE")

;;;###autoload
(defun tla-view-conflicts (buffer)
  "*** WARNING : Use this function if you like, but M-x smerge-mode
RET is actually better for the same task ****

Graphical view of conflicts after tla star-merge --three-way. The
buffer given as an argument must be the content of a file with
conflicts markers like.

    <<<<<<< TREE
    my text
    =======
    his text
    >>>>>>> MERGE-SOURCE

Priority is given to your file by default. (This means all conflicts
will be rejected if you do nothing).
"
  (interactive (list (find-file (read-file-name "View conflicts in: "))))
  (let ((mine-buffer buffer)
        (his-buffer (get-buffer-create "*tla-his*")))
    (with-current-buffer his-buffer
      (erase-buffer)
      (insert-buffer mine-buffer)
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
                                        (regexp-quote tla-mine-string) "$")
                                nil t)
        (beginning-of-line)
        (delete-region (point) (progn
                                 (re-search-forward "^=======\n")))
        (re-search-forward
         (concat "^>>>>>>> "
                 (regexp-quote tla-his-string) "$"))
        (beginning-of-line)
        (delete-region (point) (1+ (line-end-position)))
        )
      )
    (with-current-buffer mine-buffer
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
                                        (regexp-quote tla-mine-string) "$")
                                nil t)
        (beginning-of-line)
        (delete-region (point) (1+ (line-end-position)))
        (re-search-forward "^=======$")
        (beginning-of-line)
        (delete-region (point) (progn
                                 (re-search-forward
                                  (concat "^>>>>>>> "
                                          (regexp-quote tla-his-string) "\n"))))
        ))
    (tla--ediff-buffers mine-buffer his-buffer)
    ))

(defun tla-file-get-original-file (file &optional revision)
  "Get the last-committed version of FILE. Returns (original-file
unmodified temporary). unmodified is non-nil if the file wasn't
modified since last commit. temporary is non-nil when the file is
temporary and should be deleted."
  (let* ((default-directory (tla-tree-root file))
         (original (progn (if revision
                              (tla--run-arch nil t 'file-find
                                            "file-find" file revision)
                            (tla--run-arch nil t 'file-find
                                          "file-find" file))
                          (with-current-buffer tla--last-process-buffer
                            (goto-char (point-min))
                            (re-search-forward "^[^*]")
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
         (original-to-be-removed nil)
         file-unmodified-p)
    (unless (file-exists-p original)
      ;; Probably tla is ran remotely or whatever. Well, get the
      ;; file using the old good tla file-diff | patch -R -o ...
      (setq original (make-temp-name "/tmp/tla-ediff")
            original-to-be-removed t)
      (if revision
          (tla--run-arch nil t 'file-diffs "file-diffs" file revision)
        (tla--run-arch nil t 'file-diffs "file-diffs" file))
      (with-current-buffer tla--last-process-buffer
        (if (= (point-min) (point-max))
            (setq file-unmodified-p t))
        (call-process-region (point-min) (point-max)
                             tla-patch-executable
                             nil nil nil
                             "-R" "-o" original file)))
    (list original file-unmodified-p original-to-be-removed)))

(defun tla-file-revert (file &optional revision)
  "Reverts the file FILE to the last committed version. Warning: You
use version control to keep backups of your files. This function will
by definition not keep any backup in the archive.

Most of the time, you should not use this function. Call
`tla-file-ediff' instead, and undo the changes one by one with the key
`b', then save your buffer.

As a last chance, tla-file-revert keeps a backup of the last-saved in
~ backup file."
  (interactive (list (progn (when (and (buffer-modified-p)
                                       (y-or-n-p (format "Save buffer %s? "
                                                         (buffer-name
                                                          (current-buffer)))))
                              (save-buffer))
                            (buffer-file-name))))
  (copy-file file (car (find-backup-file-name file)) t)
  (let* ((file-unmo-temp (tla-file-get-original-file file revision))
         (original (car file-unmo-temp))
         (unmodified (cadr file-unmo-temp)))
    (when unmodified
      (error "File not modified"))
    (unless (yes-or-no-p (format "Are you sure you want to revert %s? "
                                 file))
      (error "Not reverting file"))
    (copy-file original file t)
    (let ((buf (get-file-buffer file)))
      (when buf (with-current-buffer buf (revert-buffer))))))

;;;###autoload
(defun tla-file-ediff (file &optional revision)
  "Interactive view of differences since last commit (or REVISION if
specified) using ediff."
  (interactive (list (progn (when (and (buffer-modified-p)
                                       (y-or-n-p (format "Save buffer %s? "
                                                         (buffer-name
                                                          (current-buffer)))))
                              (save-buffer))
                            (buffer-file-name))))
  (tla--ediff-buffers (or (get-file-buffer file)
                          (find-file file))
                      (tla-file-get-original file revision)))

;;;###autoload
(defun tla-file-view-original (file &optional revision)
  "Gets the last-committed version of FILE in a buffer."
  (interactive (list (buffer-file-name)))
  (pop-to-buffer (tla-file-get-original file revision)))

(defun tla-file-get-original (file &optional revision)
  "Gets the last committed version of file in a buffer. Returned value
is the buffer"
  (let* ((default-directory (tla-tree-root))
         (modified-buf (or (get-file-buffer file)
                           (find-file-noselect file)))
         (file-unmo-temp (tla-file-get-original-file file revision))
         (original (car file-unmo-temp))
         (unmodified (cadr file-unmo-temp))
         (original-to-be-removed (caddr file-unmo-temp)))
    (when unmodified
      (error "No modification in this file"))
    (let ((buffer-orig (get-buffer-create
                        (concat (file-name-nondirectory file)
                                "<" (or revision
                                        "original") ">"))))
      (with-current-buffer buffer-orig
        (erase-buffer)
        (insert-file-contents original)
        (when original-to-be-removed
          (delete-file original))
        (when (string= (with-current-buffer modified-buf
                         (buffer-substring-no-properties (point-min)
                                                         (point-max)))
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))
          (error "No modification in this file")))
      buffer-orig)))

(defun tla-ediff-startup-hook ()
  ;; ediff-after-quit-hook-internal is local to an ediff session.
  (add-hook 'ediff-after-quit-hook-internal
            `(lambda ()
               (set-window-configuration
                ,tla-window-config))
            nil 'local))

;;;###autoload
(defun tla-commit ()
  "Runs tla commit.
Returns the exit status of tla"
  (interactive)
  (or (tla-save-some-buffers)
      (y-or-n-p
       "Commit with unsaved changes is a bad idea. Continue anyway? ")
      (error "Not committing"))
  (let* ((file-list (and (buffer-live-p tla-buffer-source-buffer)
                         (with-current-buffer tla-buffer-source-buffer
                           tla-buffer-marked-file-list)))
         arglist)
    (when file-list (setq arglist (append arglist (cons "--" file-list))))
    (let ((status (tla--run-tla-sync
                   (cons "commit"
                         (cons (when tla-strict-commits "--strict")
                               (when file-list (cons "--"
                                                     file-list))))
                   :finished 'tla--status-handler
                   :error 'tla--status-handler)))
      (if (equal status 0)
          (tla--show-last-process-buffer)
        (pop-to-buffer tla--last-error-buffer))
      status)))

;;;###autoload
(defun tla-rm (file)
  "Calls tla rm on file FILE. Prompts for confirmation before"
  (when (yes-or-no-p (format "Delete file %s? " file))
    (tla--run-arch nil t 'rm "rm" file)))

(defun tla-pristines ()
  (interactive)
  (tla--run-arch nil t 'pristines "pristines")
  (tla-show-process-buffer-internal t))

;;;###autoload
(defun tla-changelog ()
  (interactive)
  (tla--run-arch nil t 'changelog "changelog")
  (tla-show-process-buffer-internal t "*tla-changelog*" 'tla-changelog-mode))

(defvar tla-logs-flag-list '("--summary" "--date" "--creator"))

;;;###autoload
(defun tla-logs ()
  "Run tla logs"
  (interactive)
  (tla--run-arch nil t 'logs (append (list "logs") tla-logs-flag-list))
  (tla-show-process-buffer-internal t "*tla-logs*" 'tla-logs-mode))

;;;###autoload
(defun tla-tree-lint ()
  "Audit an arch source tree."
  (interactive)
  (let ((no-warnings))
    (when (get-buffer tla--last-process-buffer)
      (kill-buffer tla--last-process-buffer))
    (tla--run-arch nil t 'tree-lint "tree-lint")
    (save-excursion
      (set-buffer tla--last-process-buffer)
      (setq no-warnings (= (- (point-max) (point-min)) 0)))
    (if no-warnings
        (message "No tree-lint warnings for this arch working copy")
      (tla-show-process-buffer-internal t))))

(defun tla-tree-version-list-tla ()
  "Returns the tree version, or nil if not in a project tree"
  (tla--run-arch nil t 'tree-version "tree-version")
  (with-current-buffer tla--last-process-buffer
    (and
     (goto-char (point-min))
     (re-search-forward "\\(.*\\)/\\(.*\\)--\\(.*\\)--\\(.*\\)" nil t)
     (list (match-string 1)
           (match-string 2)
           (match-string 3)
           (match-string 4)))))

(defun tla-tree-version-list ()
  "Elisp implementation of `tla-tree-version-list-tla'"
  (with-temp-buffer
    (insert-file-contents (concat (tla-tree-root)
                                  "/{arch}/++default-version"))
    (and
     (goto-char (point-min))
     (re-search-forward "\\(.*\\)/\\(.*\\)--\\(.*\\)--\\(.*\\)" nil t)
     (list (match-string 1)
           (match-string 2)
           (match-string 3)
           (match-string 4)))))

(defun tla-tree-root-tla ()
  "Run tla tree-root."
  (interactive)
  (tla--run-arch nil t 'tree-root "tree-root")
  (when (interactive-p)
    (message "tla tree-root is: %s" (tla-get-process-output)))
  (tla-get-process-output))

(defun tla-tree-version (&optional interactive)
  (interactive (list t))
  (let ((version
         (or (if (boundp 'tla-buffer-version-name)
                 tla-buffer-version-name)
             (with-temp-buffer
               (insert-file-contents (concat (tla-tree-root)
                                             "/{arch}/++default-version"))
               (buffer-substring-no-properties (point-min)
                                               (- (point-max) 1))))))
    (if interactive
        (message version)
      version)))

;;;###autoload
(defun tla-my-id (&optional arg)
  "Run tla my-id.
When called without a prefix argument, just print the my-id from tla.
When called with a prefix argument, ask for a new my-id.

The my-id should have the following format:

Your id is recorded in various archives and log messages
as you use arch. It must consist entirely of printable
characters and fit on one line. By convention, it should
have the form of an email address, as in this example:

Jane Hacker <jane.hacker@gnu.org>
"
  (interactive "P")
  (let ((id (tla--run-tla-sync '("my-id")
                               :finished
                               (lambda (output error status)
                                 (tla--buffer-content output)))))
    (if arg
        ;; Set the user's ID
        (let ((new-id (read-string "New arch my-id: "
                                   id tla-my-id-history id)))
          (if (string= id new-id)
              (message "Id unchanged! Id = %s" new-id)
            (message "Setting id to: %s" new-id)
            (tla--run-tla-sync (list "my-id" new-id)
                               :finished (lambda (output error status)
                                           (message "Id changed"))))
          new-id)
      (cond (id (when (interactive-p)
                  (message "Arch my-id: %s" id))
                id)
            (t (message (concat "Arch my-id has not been given yet. "
                                "Call `%s' with prefix arguments to set.")
                        this-command))))))

(defun tla-set-my-id ()
  "Set tla's my-id. "
  (interactive)
  (tla-my-id 1))

;;
;; Library
;;

;;;###autoload
(defun tla-my-revision-library (&optional arg)
  "Run tla my-revision-library.
When called without a prefix argument, just print the my-revision-library from tla.
When called with a prefix argument, ask for a new my-revision-library.

my-revision-library specifies a path, where the revision library is stored to
speed up tla. For example ~/tmp/arch-lib.

You can configure the parameters for the library via tla-library-config."
  (interactive "P")
  (let ((result (tla--run-arch nil t 'my-revision-library
                              "my-revision-library"))
        (rev-lib (tla-get-process-output)))

    (when (not (= result 0))
      (pop-to-buffer " *tla-errors*")
      (error "running \"tla my-revision-library\" failed"))

    (if arg
	(tla-set-revision-library-interactive rev-lib)
      (if (and rev-lib (string= "" rev-lib))
          (message "Arch my-revision-library has not been given yet. Call `%s' with prefix arguments to set."
                   this-command)
        (when (interactive-p) (message "Arch my-revision-library: %s" rev-lib)))
      rev-lib)))

(defun tla-set-revision-library-interactive (&optional old-rev-lib)
  (unless old-rev-lib (setq old-rev-lib ""))
  (let ((new-rev-lib (expand-file-name (read-directory-name
					"New arch revision library: " old-rev-lib))))
    (if (not (string= old-rev-lib new-rev-lib))
	(progn
	  (message "Setting my-revision-library to: %s" new-rev-lib)
	  (tla-set-revision-library new-rev-lib))
      old-rev-lib)))

(defun tla-set-revision-library (new-rev-lib)
  (let ((dir-attr (file-attributes new-rev-lib)))
    (unless dir-attr
      (make-directory new-rev-lib t))
    (tla--run-arch nil t 'my-revision-library "my-revision-library" new-rev-lib)
    new-rev-lib))

(defun tla-library-config (&optional arg)
  "Run tla library-config.
When called without a prefix argument, just print the config.
When called with a prefix argument, let the user change the config."
  (interactive "P")
  (let ((rev-lib (tla-my-revision-library))
        (config-param))
    (if arg
        (progn
          (setq config-param (completing-read "tla library config "
                                              (mapcar 'list '("--greedy" "--sparse"))
                                              nil t "--greedy"))
          (tla--run-arch nil t 'library-config "library-config" config-param rev-lib))
      (tla--run-arch nil t 'library-config "library-config" rev-lib)
      (message (tla-get-process-output)))))

(defun tla-library-add (archive category branch version revision)
  (tla--show-last-process-buffer)
  (tla--run-tla-async `("library-add"
                        ,(tla-fully-qualified-revision archive category
                                                       branch version
                                                       revision))))

(defun tla-library-find (archive category branch version revision
                                 &optional silent)
  "Run tla library-find.
If the revision is found, return the path for it. Else return nil."
  (if (zerop (if silent
                 (tla--run-arch nil t 'library-find "library-find" "--silent" "-A" archive
                               (tla-name-construct category branch version
                                                   revision))
               (tla--run-arch nil t 'library-find "library-find" "-A" archive
                             (tla-name-construct category branch version revision))))
      (tla-get-process-output)))

;; completing-read: tagline, explicit, names, implicit
(defvar tla-id-tagging-method-history nil)
;;;###autoload
(defun tla-id-tagging-method (arg)
  "View or change the id-tagging method.
When called without a prefix argument: show the actual tagging method.
When called with a prefix argument: Ask the user for the new tagging method."
  (interactive "P")
  (let ((tm (progn (tla--run-arch nil t 'id-tagging-method "id-tagging-method")
                   (tla-get-process-output)))
        (new-tagging-method))
    (if arg
        (progn
          (setq new-tagging-method
                (completing-read "New id tagging method: "
                                 (mapcar 'list '("tagline" "explicit" "names" "implicit"))
                                 nil t "tagline" tla-id-tagging-method-history))
          (when (not (string= tm new-tagging-method))
            (message "Setting tagging method to: %s" new-tagging-method)
            (tla--run-arch nil t 'id-tagging-method "id-tagging-method" new-tagging-method)))
      (message "Arch id tagging method: %s" tm))))

(defun tla-archive-mirror (archive &optional category branch version from)
  "Synchronize the mirror for the archive ARCHIVE. Limit to
CATEGORY--BRANCH--VERSION. If FROM is provided, mirror from it."
  (interactive (tla-read-archive-name))
  (tla--run-tla-async (list "archive-mirror"
                            archive
                            (let ((name (tla-fully-qualified-revision
                                         nil category branch version)))
                              (if (string= name "") nil name))
                            from)))

(defun tla-star-merge (from &optional to-tree)
  ;;(message "run tla star-merge %s" from)
  (or (tla-save-some-buffers (or to-tree default-directory))
      (y-or-n-p
       "Update may delete unsaved changes. Continue anyway? ")
      (error "Not updating"))
  (let ((default-directory (or to-tree default-directory))
        (arglist '()))
    (when tla-three-way-merge (add-to-list 'arglist "--three-way"))
    (when tla-use-forward-option (add-to-list 'arglist "--forward"))
    (tla--run-tla-async `("star-merge" ,@arglist ,from)
                        :finished `(lambda (output error status)
                                     (tla-show-process-buffer-internal t)
                                     (tla-show-changes-buffer output)
                                     (message "tla star-merge finished")
                                     (tla-revert-some-buffers ,to-tree))
                        :error (lambda (output error status)
                                 (tla-show-changes-buffer output)))))

(defun tla-replay (from &optional to-tree)
  (or (tla-save-some-buffers (or to-tree default-directory))
      (y-or-n-p
       "Replay may delete unsaved changes. Continue anyway? ")
      (error "Not replaying"))
  (tla--show-last-process-buffer)
  (when to-tree (cd to-tree))
  (tla--run-tla-async `("replay"
                        ,@(if tla-use-forward-option
                              (list "--forward")
                            nil)
                        ,from)
                      :finished `(lambda (output error status)
                                   (tla-show-process-buffer-internal t)
                                   (tla-show-changes-buffer output)
                                   (message "tla replay finished")
                                   (tla-revert-some-buffers ,to-tree))
                      :error (lambda (output error status)
                               (tla-show-changes-buffer output))))

(defun tla-sync-tree (from &optional to-tree)
  (or (tla-save-some-buffers (or to-tree default-directory))
      (y-or-n-p
       "Update may delete unsaved changes. Continue anyway? ")
      (error "Not updating"))
  (tla--show-last-process-buffer)
  (when to-tree (cd to-tree))
  (tla--run-tla-async `("sync-tree" ,from)
                      :finished `(lambda (output error status)
                                  (tla-show-process-buffer-internal t)
                                  (message "tla sync-tree finished")
                                  (tla-revert-some-buffers ,to-tree))
                      :error (lambda (output error status)
                               (tla-show-changes-buffer output))))

(defun tla-tag (source-revision tag-version)
  "Runs tla tag --setup"
  (tla--run-tla-sync (list "tag" "--setup"
                           source-revision tag-version))
  (tla--show-last-process-buffer))

;;
;; Xtla bookmarks
;;

(defvar tla-bookmarks-loaded nil
  "wether tla-bookmarks have been loaded from file")

(defvar tla-bookmarks-alist nil
  "Alist containing xtla bookmarks.")

(defvar tla-bookmarks-show-details nil
  "Wether tla-bookmarks shoudl show bookmark details")

(defvar tla-bookmarks-cookie nil
  "ewoc dll")

(defvar tla-missing-buffer-todolist nil
  "List of (kind info) which can be
 (separator \"label\" bookmark \"local-tree\")
 (changes \"local-tree\")
 (missing \"local-tree\" \"location\" \"bookmark-name\")")

(defvar tla-bookmarks-marked-list nil
  "list of marked bookmaks")

(defun tla-bookmarks-load-from-file (&optional force)
  (when (or force (not tla-bookmarks-loaded))
    (let ((file (expand-file-name tla-bookmarks-file-name)))
      (save-excursion
        (unless (file-exists-p file)
          (with-temp-buffer
            (insert "()")
            (write-file file)))
        (unless (file-readable-p file)
          (error "Xtla bookmark file not readable"))
        (with-temp-buffer
          (insert-file-contents file)
          (setq tla-bookmarks-alist (read (current-buffer))
                tla-bookmarks-loaded t))))))

(defun tla-bookmarks-save-to-file ()
  "Saves `tla-bookmarks-alist' to a file"
  (let ((print-readably t)
        print-level print-length
        (file (expand-file-name tla-bookmarks-file-name)))
    (with-temp-buffer
      (insert (pp-to-string tla-bookmarks-alist))
      (write-file file))))

(defun tla-bookmarks-toggle-details (&optional val)
  "toggles the value of `tla-bookmarks-toggle-details'"
  (interactive "P")
  (let ((current-bookmark (ewoc-locate tla-bookmarks-cookie)))
    (setq tla-bookmarks-show-details
          (if val
              (if (> val 0) t
                (if (< val 0) nil
                  (not tla-bookmarks-show-details)))
            (not tla-bookmarks-show-details)))
    (ewoc-refresh tla-bookmarks-cookie)
    (tla-bookmarks-cursor-goto current-bookmark)))

(defun tla-bookmarks-printer (element)
  "Pretty printer used by ewoc, printing an entry of the bookmark
list"
  (insert (if (member element tla-bookmarks-marked-list) " *" "  "))
  (tla--insert-right-justified (concat (car element) ": ") 17
                               'tla-bookmark-name)
  (insert (tla-add-face (apply 'tla-fully-qualified-revision
                               (cdr (assoc 'location (cdr element))))
                        'tla-revision-name))
  (when tla-bookmarks-show-details
    (newline)
    (insert-char ?\  17)
    (insert (cdr (assoc 'timestamp (cdr element))))
    (newline)
    (let ((notes (assoc 'notes (cdr element))))
      (when notes
        (insert-char ?\  17)
        (insert (cdr notes))
        (newline)))
    (let ((nickname (assoc 'nickname (cdr element))))
      (when nickname
        (tla--insert-right-justified "nickname: " 17)
        (insert (cadr nickname))
        (newline)))
    (let ((partners (assoc 'partners (cdr element))))
      (when partners
        (tla--insert-right-justified "partners: " 17)
        (insert (cadr partners))
        (dolist (x (cddr partners))
          (insert ",\n")
          (insert-char ?\  17)
          (insert x))
        (newline)))
    (let ((local-tree (assoc 'local-tree (cdr element))))
      (when local-tree
        (tla--insert-right-justified "local trees: " 17)
        (insert (cadr local-tree))
        (dolist (x (cddr local-tree))
          (insert ", " x ))
        (newline)))
    (let ((groups (assoc 'groups (cdr element))))
      (when groups
        (tla--insert-right-justified "Groups: " 17)
        (insert (cadr groups))
        (dolist (x (cddr groups))
          (insert ", " x ))
        (newline)))))

(defvar tla-revision-list-cookie nil
  "ewoc cookie for tla-bookmark-missing")

(defun tla-bookmarks-read-local-tree (bookmark arg)
  "Reads a local tree from keyboard for bookmark BOOKMARK, and
possibly add it to the bookmarks. If arg is non-nil, user will be
prompted anyway. Otherwise, just use the default if it exists."
  (let* ((local-trees (assoc 'local-tree (cdr bookmark))))
    (cond
     ((not local-trees)
      (let ((dir (read-directory-name
                  (format "Local tree for \"%s\": "
                          (car bookmark)))))
        (when (y-or-n-p "Add this tree in your bookmarks? ")
          (tla-bookmarks-add-tree bookmark dir))
        dir))
     (arg
      ;; multiple local trees.
      (let ((dir (completing-read
                  (format "Local tree for \"%s\": "
                          (car bookmark))
                  (mapcar #'(lambda (x) (cons x nil))
                          (cdr local-trees))
                  nil nil nil nil (cadr local-trees))))
        (when (and (not (member dir (cdr local-trees)))
                   (y-or-n-p "Add this tree in your bookmarks? "))
          (tla-bookmarks-add-tree bookmark dir))
        (when (and (not (string=
                         dir (cadr local-trees)))
                   (y-or-n-p "Make this the default? "))
          (tla-bookmarks-delete-tree bookmark dir)
          (tla-bookmarks-add-tree bookmark dir))
        dir))
     (t (cadr local-trees)))))

(defun tla-bookmarks-missing (&optional arg)
  "Show the missing patches from your partners.
The missing patches are received via tla missing.
Additionaly the local changes in your working copy are also shown."
  (interactive "P")
  (let ((list (or tla-bookmarks-marked-list
                  (list (ewoc-data (ewoc-locate
                                    tla-bookmarks-cookie))))))
    (set-buffer (get-buffer-create "*tla-missing*"))
    (tla-revision-list-mode)
    (set (make-local-variable 'tla-buffer-refresh-function)
         'tla-missing-refresh)
    (let ((tla-bookmarks-missing-buffer-list-elem
           (mapcar
            #'(lambda (elem)
                (cons
                 elem
                 (tla-bookmarks-read-local-tree elem arg)))
            list)))
      (set (make-local-variable 'tla-missing-buffer-todolist)
           (reverse
            (apply 'append
                   (mapcar (lambda (elem)
                             (tla-bookmarks-missing-elem
                              (car elem) arg (cdr elem) t t))
                           tla-bookmarks-missing-buffer-list-elem))))
      (tla-missing-refresh))))


(defun tla-missing-refresh (&optional arg)
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (set (make-local-variable 'tla-revision-list-cookie)
         (ewoc-create 'tla-revision-list-printer))
    (dolist (item tla-missing-buffer-todolist)
        (case (car item)
          (missing
           ;; This item is a version that we want to check for missing patches.
           ;; ITEM is of the form:
           ;; (missing <local tree> <fully qualified version> [bookmark name])
           (let* ((local-tree (nth 1 item))
                  (version (nth 2 item))
                  (bookmark-name (nth 3 item))
                  (text (if bookmark-name
                            (format "Missing patches for partner %s:"
                                    bookmark-name)
                          (concat "Missing patches for archive " version)))
                  (node (ewoc-enter-last tla-revision-list-cookie
                                         (list 'separator (concat
                                                           text)
                                               'partner))))
             (ewoc-enter-last tla-revision-list-cookie
                              '(entry-change "Checking for missing patches..."))
             (let ((default-directory local-tree))
               (message default-directory)
               (tla--run-tla-async
                `("missing" "--full" "--summary" "--creator" "--date"
                  ,version)
                :finished
                `(lambda (output error status)
                   (when (get-buffer "*tla-missing*")
                     (with-current-buffer "*tla-missing*"
                       (when (ewoc-p tla-revision-list-cookie)
                         (tla-bookmarks-missing-parse-missing output ,node)
                         (ewoc-refresh tla-revision-list-cookie)))))
                :killed
                (lambda (output error status)
                  (error "Command killed unexpectedly!"))
                :error
                (lambda (output error status)
                  (tla-switch-to-buffer output)
                  (tla--show-error-buffer error)
                  (error status))))))
          (separator
           ;; This item is a separator -- the name of a bookmark.
           ;; ITEM is of the form:
           ;; (separator <text> bookmark <local tree>)
           (let* ((text (nth 1 item))
                  (local-tree (nth 3 item)))
             (ewoc-enter-last tla-revision-list-cookie
                              (list 'separator
                                    text
                                    'bookmark
                                    local-tree))))
          (changes
           ;; This item is a local-tree that should be checked for changes.
           ;; ITEM is of the form:
           ;; (changes <local tree>)
           (let ((default-directory (nth 1 item)))
             (tla--run-tla-async
              '("changes")
              :error `(lambda (output error status)
                        (tla-bookmarks-missing-parse-changes
                         output ,(ewoc-nth tla-revision-list-cookie
                                           -1)))
              :finished 'tla--null-handler))))
        (ewoc-set-hf tla-revision-list-cookie ""
                     (concat "\n" (tla-add-face "end." 'tla-separator))))))


(defun tla-bookmarks-missing-elem (data arg local-tree header
                                        &optional changes-too)
  "Show missing patches for one element"
  (let* ((partners (assoc 'partners (cdr data)))
         (location (cdr (assoc 'location (cdr data)))))
    (tla-switch-to-buffer "*tla-missing*")
    (cd local-tree)
    (let ((item '()))
      (add-to-list 'item
                   `(separator
                           ,(format "Bookmark %s (%s):"
                                    (car data)
                                    (apply
                                     'tla-fully-qualified-revision
                                     location))
                           bookmark
                           ,local-tree))
      (when changes-too
        (add-to-list 'item `(changes ,local-tree)))
      (dolist (partner (cons (apply 'tla-fully-qualified-revision
                                    (cdr (assoc 'location (cdr data)))) ; Me
                             (cdr partners))) ; and my partners
        (let ((bookmark-name
               (some (lambda (bookmark)
                       (and (string= partner
                                     (apply 'tla-fully-qualified-revision
                                            (cdr (assoc 'location bookmark))))
                            (car bookmark)))
                     tla-bookmarks-alist)))
          (add-to-list 'item `(missing ,local-tree ,partner ,bookmark-name))))
      item)))

(defun tla-bookmarks-missing-parse-missing (buffer parent-node)
  (let ((cookie tla-revision-list-cookie)
        revision date creator summary)
    (let ((missing-buffer (with-current-buffer buffer
                            (clone-buffer))))
      (with-current-buffer missing-buffer
        (goto-char (point-min))
        (re-search-forward ".*/.*--.*--.*--.*" nil t)
        (beginning-of-line)
        (let ((last-node (ewoc-next cookie parent-node)))
          (while (progn (current-buffer) (> (point-max) (point)))
            (setq revision (buffer-substring-no-properties
                            (point) (line-end-position)))
            (forward-line 1)
            (re-search-forward " +\\(.*[^ ]\\)   *\\(.*\\)"
                               (line-end-position))
            (setq date (match-string-no-properties 1))
            (setq creator (match-string-no-properties 2))
            (forward-char 1)
            (if (re-search-forward " +" (line-end-position) t)
                (progn (setq summary (buffer-substring-no-properties
                                      (point) (line-end-position)))
                       (forward-line 1))
              (setq summary ""))
            (with-current-buffer "*tla-missing*"
              (setq last-node
                    (ewoc-enter-after cookie last-node
                                      (list 'entry-patch nil
                                            (tla-split-revision-name revision)
                                            summary creator date))))))
        (ewoc--node-delete (ewoc-next cookie parent-node)))
      (kill-buffer missing-buffer))))

(defun tla-bookmarks-missing-parse-changes (buffer parent-node)
  (with-current-buffer buffer
    (let ((changes
           (progn (goto-char (point-min))
                  (when (re-search-forward "^[^\\*]" nil t)
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (point-max)))))
          (local-tree default-directory))
      (when changes
        (with-current-buffer "*tla-missing*"
          (ewoc-enter-after tla-revision-list-cookie
                            parent-node
                            (list 'entry-change
                                  changes
                                  local-tree)))))))

(defun tla-bookmarks--get-local-tree ()
  (let ((local-trees
         (cdr (assoc 'local-tree (ewoc-data (ewoc-locate tla-bookmarks-cookie))))))
    ;; todo: add support for multiple local-trees
    (car local-trees)))

(defun tla-bookmarks-open-tree ()
  "Open a local tree in a dired buffer."
  (interactive)
  (dired-other-window (tla-bookmarks--get-local-tree)))

(defun tla-bookmarks-inventory ()
  "Run tla-inventory on a local tree."
  (interactive)
  (let ((default-directory (tla-bookmarks--get-local-tree)))
    (tla-inventory t)))

(defmacro tla-make-move-fn (ewoc-direction function cookie)
  `(defun ,function ()
     (interactive)
     (let* ((elem (ewoc-locate ,cookie))
            (next (or (,ewoc-direction ,cookie elem) elem)))
       (while (and next
                   (eq (car (ewoc-data next)) 'separator)
                   (,ewoc-direction ,cookie next))
         (setq next (,ewoc-direction ,cookie next)))
       (while (and next (eq (car (ewoc-data next)) 'separator))
         (setq next (,(if (eq ewoc-direction 'ewoc-next)
                          'ewoc-prev
                        'ewoc-next) ,cookie next)))
       (when next (goto-char (ewoc-location next)))))
  )

(tla-make-move-fn ewoc-next tla-bookmarks-missing-next
                  tla-revision-list-cookie)

(tla-make-move-fn ewoc-prev tla-bookmarks-missing-prev
                  tla-revision-list-cookie)


;;;###autoload
(defun tla-bookmarks (&optional arg)
  "Displays xtla bookmarks in a buffer. Non-nil prefix argument to
reload the file from disk."
  (interactive "P")
  (tla-bookmarks-load-from-file arg)
  (pop-to-buffer "*tla-bookmarks*")
  (let ((pos (point)))
    (toggle-read-only -1)
    (erase-buffer)
    (set (make-local-variable 'tla-bookmarks-cookie)
         (ewoc-create 'tla-bookmarks-printer))
    (set (make-local-variable 'tla-bookmarks-marked-list) nil)
    (dolist (elem tla-bookmarks-alist)
      (ewoc-enter-last tla-bookmarks-cookie elem))
    (tla-bookmarks-mode)
    (if (equal pos (point-min))
        (if (ewoc-nth tla-bookmarks-cookie 0)
            (tla-bookmarks-cursor-goto (ewoc-nth tla-bookmarks-cookie 0))
          (message "You have no bookmarks, create some in the other buffers"))
      (goto-char pos))))


(defun tla-bookmarks-mode ()
  "Major mode to show xtla bookmarks.

You can add a bookmark with '\\<tla-bookmarks-mode-map>\\[tla-bookmarks-add]', and remove one with '\\[tla-bookmarks-delete]'. After
marking a set of files with '\\[tla-bookmarks-mark]', make them partners with '\\[tla-bookmarks-marked-are-partners]', and
you will then be able to use '\\[tla-bookmarks-missing]' to view the missing patches.

Commands:
\\{tla-bookmarks-mode-map}
"
  (interactive)
  (use-local-map tla-bookmarks-mode-map)
  (setq major-mode 'tla-bookmarks-mode)
  (setq mode-name "tla-bookmarks")
  (toggle-read-only 1)
  (run-hooks 'tla-bookmarks-mode-hook))

(defun tla-bookmarks-cursor-goto (ewoc-bookmark)
  "Move cursor to the ewoc location of EWOC-BOOKMARK"
  (interactive)
  (goto-char (ewoc-location ewoc-bookmark))
  (search-forward ":"))

(defun tla-bookmarks-next ()
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla-bookmarks-cursor-goto next)))

(defun tla-bookmarks-previous ()
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla-bookmarks-cursor-goto previous)))

(defun tla-bookmarks-move-down ()
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (data (ewoc-data elem))
         (oldname (car data))
         (next (ewoc-next cookie elem)))
    (unless next
      (error "Can't go lower"))
    (tla-ewoc-delete cookie elem)
    (goto-char (ewoc-location
                (ewoc-enter-after cookie next data)))
    (let ((list tla-bookmarks-alist)
          newlist)
      (while list
        (if (string= (caar list) oldname)
            (progn
              (setq newlist (cons (car (cdr list)) newlist))
              (setq newlist (cons (car      list)  newlist))
              (setq list (cdr list)))
          (setq newlist (cons (car list) newlist)))
        (setq list (cdr list)))
      (setq tla-bookmarks-alist (reverse newlist)))
    (search-forward ":")))

(defun tla-bookmarks-move-up ()
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (data (ewoc-data elem))
         (oldname (car data))
         (previous (ewoc-prev cookie elem)))
    (unless previous
      (error "Can't go upper"))
    (tla-ewoc-delete cookie elem)
    (goto-char (ewoc-location
                (ewoc-enter-before cookie previous data)))
    (let ((list tla-bookmarks-alist)
          newlist)
      (while list
        (if (string= (caar (cdr list)) oldname)
            (progn
              (setq newlist (cons (car (cdr list)) newlist))
              (setq newlist (cons (car      list)  newlist))
              (setq list (cdr list)))
          (setq newlist (cons (car list) newlist)))
        (setq list (cdr list)))
      (setq tla-bookmarks-alist (reverse newlist)))
    (search-forward ":")))

(defun tla-bookmarks-get (directory)
  (interactive (list (expand-file-name (read-directory-name "Get in directory: "))))
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem))))
    (tla-get directory t
             (tla-archive-name  location)
             (tla-category-name location)
             (tla-branch-name  location)
             (tla-version-name location))))

(defun tla-bookmarks-goto ()
  (interactive)
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem)))
         (archive  (tla-archive-name  location))
         (category (tla-category-name location))
         (branch   (tla-branch-name  location))
         (version  (tla-version-name location)))
    (cond (version  (tla-revisions archive category branch version))
          (branch   (tla-versions  archive category branch))
          (category (tla-branches  archive category))
          (archive  (tla-categories archive))
          (t (error "Nothing specified for this bookmark")))))

(defun tla-bookmarks-star-merge (arg)
  (interactive "P")
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem)))
         (local-tree (read-directory-name "Star-merge into: ")))
    (tla-star-merge (apply 'tla-fully-qualified-revision location)
                    local-tree)))

(defun tla-bookmarks-replay (arg)
  (interactive "P")
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem)))
         (local-tree (read-directory-name "Replay into: ")))
    (tla-replay (apply 'tla-fully-qualified-revision location)
                local-tree)))

(defun tla-bookmarks-add-elem (name info)
  "Internal function. Adds the association (name . info) to the list
of bookmarks, and saves it"
  (when (assoc name tla-bookmarks-alist)
    (error (concat "Already got a bookmark " name)))
  (let ((elem (cons name info)))
    (add-to-list 'tla-bookmarks-alist elem t)
    (tla-bookmarks-save-to-file)
    (ewoc-enter-last tla-bookmarks-cookie elem)
    ))

(defun tla-bookmarks-add (name archive &optional category branch version)
  "Adds a bookmark named NAME for location ARCHIVE/CATEGORY--BRANCH--VERSION"
  (interactive (cons (read-string "Name of the bookmark: ")
                     (tla-read-archive-category-branch-version-name)))
  (unless (get-buffer "*tla-bookmarks*")
    (tla-bookmarks))
  (with-current-buffer "*tla-bookmarks*"
    (let* ((info (list (cons 'location
                             (list archive category branch version))
                       (cons 'timestamp (current-time-string)))))
      (tla-bookmarks-add-elem name info))))

(defun tla-bookmarks-mark ()
  "Marks bookmark at point"
  (interactive)
  (let ((pos (point)))
    (add-to-list 'tla-bookmarks-marked-list
                 (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos))
  (tla-bookmarks-next))

(defun tla-bookmarks-unmark ()
  "Marks bookmark at point"
  (interactive)
  (let ((pos (point)))
    (setq tla-bookmarks-marked-list
          (delq (ewoc-data (ewoc-locate tla-bookmarks-cookie))
                tla-bookmarks-marked-list))
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos))
  (tla-bookmarks-next))

(defun tla-bookmarks-unmark-all ()
  "Unmarks all bookmarks in current buffer"
  (interactive)
  (let ((pos (point)))
    (setq tla-bookmarks-marked-list nil)
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos)))

(defun tla-bookmarks-marked-are-partners ()
  "All marked bookmarks becomes mutual partners"
  (interactive)
  (let ((list-arch (mapcar
                    #'(lambda (x)
                        (format "%s"
                                (apply 'tla-fully-qualified-revision
                                       (cdr (assoc 'location x)))))
                    tla-bookmarks-marked-list)))
    (dolist (book tla-bookmarks-marked-list)
      (let ((myloc (apply 'tla-fully-qualified-revision
                          (cdr (assoc 'location book)))))
        (message myloc)
        (dolist (arch list-arch)
          (unless (string= myloc arch)
            (tla-bookmarks-add-partner book arch t))))))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))

(defun tla-bookmarks-cleanup-local-trees ()
  "Remove fields local-tree from bookmarks when they don't exist"
  (interactive)
  (dolist (book tla-bookmarks-alist)
    (let ()
      (dolist (local-tree (cdr (assoc 'local-tree book)))
        (when (and (not (file-exists-p local-tree))
                   (or tla-bookmarks-cleanup-dont-prompt
                       (y-or-n-p
                        (format
                         "Remove tree %s from bookmarks %s? "
                         local-tree
                         (car book)))))
          (tla-bookmarks-delete-tree book local-tree t)))))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))

(defun tla-bookmarks-delete (elem &optional force)
  "Deletes bookmark at point"
  (interactive (list (ewoc-locate tla-bookmarks-cookie)))
  (let* ((data (ewoc-data elem)))
    (when (or force
              (yes-or-no-p (format "Delete bookmark \"%s\"? " (car data))))
      (tla-ewoc-delete tla-bookmarks-cookie elem)
      (let ((list tla-bookmarks-alist)
            newlist)
        (while list
          (unless (string= (caar list) (car data))
            (setq newlist (cons (car list) newlist)))
          (setq list (cdr list)))
        (setq tla-bookmarks-alist (reverse newlist)))
      ;; TODO could be optimized
      (tla-bookmarks-save-to-file)
      )))

(defun tla-bookmarks-find-bookmark (location)
  "Finds the bookmark whose location is LOCATION"
  (let ((list tla-bookmarks-alist)
        result)
    (while list
      (when (string= (apply 'tla-fully-qualified-revision
                            (cdr (assoc 'location (cdar list))))
                     location)
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defmacro tla-bookmarks-make-add-fn (name field message-already message-add)
  `(defun ,name (bookmark value &optional dont-save)
     "Adds the direcotry VALUE to the list of local trees of bookmark
BOOKMARK."
     (let ((local-trees (assoc ,field (cdr bookmark))))
       (if local-trees
           (if (member value (cdr local-trees))
               (message ,message-already)
             (progn
               (message ,message-add)
               (setcdr local-trees (cons value
                                         (cdr local-trees)))))
         (progn
           (message ,message-add)
           (setcdr bookmark (cons (list ,field value)
                                  (cdr bookmark)))))
       (unless dont-save
         (tla-bookmarks-save-to-file)
         (save-window-excursion
           (tla-bookmarks)))))
  )

(tla-bookmarks-make-add-fn tla-bookmarks-add-tree
                           'local-tree
                           "Local tree already in the list"
                           "Local tree added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-partner
                           'partners
                           "Partner already in the list"
                           "Partner added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-group
                           'groups
                           "Group already in the list"
                           "Group added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-nickname
                           'nickname
                           "Nickname already in the list"
                           "Nickname added to your bookmark")

(defmacro tla-bookmarks-make-delete-fn (name field)
  `(defun ,name (bookmark value &optional dont-save)
     "Deletes the directory VALUE to the list of local trees of bookmark
BOOKMARK."
     (let ((local-trees (assoc ,field (cdr bookmark))))
       (when local-trees
         (let ((rem-list (delete value (cdr (assoc ,field
                                                   bookmark)))))
           (if rem-list
               (setcdr local-trees rem-list)
             ;; Remove the whole ('field ...)
             (setcdr bookmark (delq local-trees (cdr bookmark))))))
       (unless dont-save
         (tla-bookmarks-save-to-file)
         (save-window-excursion
           (tla-bookmarks)))))
  )

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-tree
                              'local-tree)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-partner
                              'partners)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-group
                              'groups)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-nickname
                              'nickname)

(defun tla-bookmarks-add-partner-interactive ()
  "Adds a partner to the current or marked bookmarks"
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie)))))
        (partner (apply 'tla-fully-qualified-revision
                        (tla-read-archive-category-branch-version-name))))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-partner bookmark partner t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-delete-partner-interactive ()
  "Deletes a partner from the current or marked bookmarks"
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (choices (apply 'append
                         (mapcar #'(lambda (x)
                                     (cdr (assoc 'partners
                                                 (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar #'(lambda (x) (list x)) choices))
         (partner (completing-read "Partner to remove: " choices-alist)))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-delete-partner bookmark partner t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-add-tree-interactive ()
  "Adds a local tree to the current or marked bookmarks"
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie)))))
        (local-tree (read-directory-name "Local tree to add: ")))
    (unless (file-exists-p (concat local-tree "/{arch}"))
      (error (concat local-tree " is not an arch local tree.")))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-tree bookmark local-tree t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-delete-tree-interactive ()
  "Adds a local tree to the current or marked bookmarks"
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (choices (apply 'append
                         (mapcar #'(lambda (x)
                                     (cdr (assoc 'local-tree
                                                 (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar #'(lambda (x) (list x)) choices))
         (local-tree (completing-read "Local tree to remove: " choices-alist)))
    (unless (file-exists-p (concat local-tree "/{arch}"))
      (error (concat local-tree " is not an arch local tree.")))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-delete-tree bookmark local-tree t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-list-groups ()
  "Returns the list of groups currently used by bookmarks"
  (let ((list (apply 'append
                     (mapcar #'(lambda (x)
                                 (cdr (assoc 'groups
                                             (cdr x))))
                             tla-bookmarks-alist)))
        result)
    ;; Make elements unique
    (dolist (elem list)
      (add-to-list 'result elem))
    result))

(defun tla-bookmarks-add-group-interactive ()
  "Add a group entry in the current or marked bookmarks"
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (group (completing-read "Group of bookmarks: "
                                 (mapcar #'(lambda (x) (list x))
                                         (tla-bookmarks-list-groups)))))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-group bookmark group t)))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))


(defun tla-bookmarks-delete-group-interactive ()
  "Deletes a group of bookmark entry from the current or marked bookmarks"
  (interactive)
    (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
           (choices (apply 'append
                           (mapcar #'(lambda (x)
                                     (cdr (assoc 'groups
                                                 (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar #'(lambda (x) (list x)) choices))
         (group (completing-read "Group to remove: " choices-alist)))
      (dolist (bookmark bookmarks)
        (tla-bookmarks-delete-group bookmark group t)))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks)))

(defun tla-bookmarks-select-by-group (group)
  "Select all bookmarks in the group GROUP"
  (interactive (list (completing-read "Group to select: "
                                      (mapcar #'(lambda (x) (list x))
                                              (tla-bookmarks-list-groups)))))
  (dolist (bookmark tla-bookmarks-alist)
    (when (member group (cdr (assoc 'groups bookmark)))
      (add-to-list 'tla-bookmarks-marked-list bookmark))
    )
  (ewoc-refresh tla-bookmarks-cookie))

(defun tla-bookmarks-add-nickname-interactive ()
  "Adds a nickname to the current bookmark"
  (interactive)
  (let* ((bookmark (ewoc-data (ewoc-locate
                               tla-bookmarks-cookie)))
         (prompt (format "Nickname for %S: " (assoc 'location bookmark))))
    (tla-bookmarks-add-nickname bookmark (read-string prompt) t)
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-delete-nickname-interactive ()
  "Deletes the nickname of the current bookmark"
  (interactive)
  (let* ((bookmark (ewoc-data (ewoc-locate
                               tla-bookmarks-cookie)))
         (nickname (cadr (assoc 'nickname bookmark))))
    (tla-bookmarks-delete-nickname bookmark nickname t)
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defvar tla-buffer-bookmark nil
  "Bookmark manipulated in current buffer")

(defun tla-bookmarks-edit ()
  "Edit bookmark at point"
  (interactive)
  (let* ((elem (ewoc-locate tla-bookmarks-cookie))
         (data (ewoc-data elem)))
    (pop-to-buffer (concat "*xtla bookmark " (car data) "*"))
    (erase-buffer)
    (emacs-lisp-mode)
    (make-local-variable 'tla-buffer-bookmark)
    (setq tla-buffer-bookmark elem)
    (insert ";; Edit the current bookmark. C-c C-c to finish\n\n")
    (pp data (current-buffer))
    (goto-char (point-min)) (forward-line 2) (forward-char 2)
    (local-set-key [(control ?c) (control ?c)]
                   #'(lambda () (interactive)
                       (goto-char (point-min))
                       (let* ((newval (read (current-buffer)))
                              (elem tla-buffer-bookmark)
                              (oldname (car (ewoc-data elem))))
                         (kill-buffer (current-buffer))
                         (pop-to-buffer "*tla-bookmarks*")
                         (setcar (ewoc-data elem) (car newval))
                         (setcdr (ewoc-data elem) (cdr newval))
                         (let ((list tla-bookmarks-alist)
                               newlist)
                           (while list
                             (if (string= (caar list) oldname)
                                 (setq newlist (cons newval newlist))
                               (setq newlist (cons (car list) newlist)))
                             (setq list (cdr list)))
                           (setq tla-bookmarks-alist (reverse newlist)))
                         (ewoc-refresh tla-bookmarks-cookie)
                         (tla-bookmarks-save-to-file)
                         )))))

;;
;; Archives
;;
;;;###autoload
(defun tla-archives ()
  (interactive)
  (tla-archives-build-archive-tree)
  ;;(message "archives: %S" tla-archive-tree)
  (tla-switch-to-buffer "*tla-archives*")
  (let ((a-list tla-archive-tree)
        (my-default-archive (tla-my-default-archive))
        defaultp
        archive-name
        archive-location
        p)
    (toggle-read-only -1)
    (erase-buffer)
    (while a-list
      (setq archive-name (caar a-list)
            archive-location (cadar a-list)
            a-list (cdr a-list)
            defaultp (string= archive-name my-default-archive))
      (if defaultp (setq p (point)))
      (tla-archives-insert-item archive-name archive-location defaultp))
    (delete-backward-char 1)
    (when p (goto-char p))
    (tla-archive-list-mode)))

(defun tla-archives-insert-item (archive location defaultp)
  (let ((start-pos (point))
        overlay)
    (insert (if defaultp "*" " ")
            "  "
            (tla-choose-face-to-add
             defaultp
             archive 'tla-marked 'tla-archive-name))
    (newline)
    (insert "      " location)
    (newline)
    (setq overlay (make-overlay start-pos (point)))
    (overlay-put overlay 'category 'tla-default-button)
    (overlay-put overlay 'tla-archive-info archive)))

;; Just update tla-archive-tree.
(defun tla-archives-build-archive-tree ()
  (tla--run-arch nil t 'archives "archives")
  (setq tla-archive-tree nil)
  (save-excursion
    (let ((archive-name)
          (archive-location))
      (set-buffer tla--last-process-buffer)
      (goto-char (point-min))
      (while (> (line-end-position) (line-beginning-position))
        (setq archive-name (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (beginning-of-line-text 2)
        (setq archive-location (buffer-substring-no-properties (point) (line-end-position)))
        (forward-line 1)
        (tla-archive-tree-add-archive archive-name archive-location)))))

(defun tla-get-archive-info (&optional property)
  (unless property
    (setq property 'tla-archive-info))
  (let ((overlay (car (overlays-at (point)))))
    (when overlay
      (overlay-get overlay property))))

(defun tla-my-default-archive (&optional new-default)
  "Set or get the default archive.
When called with a prefix argument: Ask the user for the new default archive.
When called with a string argument: Set the default archive to this string.
When called with no argument: return the name of the default argument.
When called interactively, with no argument: Show the name of the default archive."
  (interactive "P")
  (when (or (numberp new-default) (and (listp new-default) (> (length new-default) 0)))
    (setq new-default (car (tla-read-archive-name))))
  (cond ((stringp new-default)
         (message "Setting arch default archive to: %s" new-default)
         (tla--run-arch nil t 'my-default-archive "my-default-archive" new-default))
        (t
         (tla--run-arch nil t 'my-default-archive "my-default-archive")
         (when (interactive-p) (message "Default arch archive: %s" (tla-get-process-output)))
         (tla-get-process-output))))

(defun tla-whereis-archive (&optional archive)
  "Call tla whereis-archive.
"
  (interactive "P")
  (let ((location))
    (unless archive
      (setq archive (tla--get-archive (tla--get-info-at-point))))
    (tla--run-arch nil t 'whereis-archive "whereis-archive" archive)
    (setq location (tla-get-process-output))
    (when (interactive-p)
      (message "archive location for %s: %s" archive location))
    location))

(defun tla-register-archive (location &optional archive)
  "Register arch archive.
LOCATION should be either a local directory or a remote path.
ARCHIVE is the name is archive. If ARCHIVE is not given or an empty string,
the default name is used."
  (interactive "sLocation: \nsArchive (empty for default): ")
  (if (and archive (eq 0 (length archive)))
      (setq archive nil))
  (let ((result (if archive
                    (tla--run-arch nil t 'register-archive "register-archive" archive location)
                  (tla--run-arch nil t 'register-archive "register-archive" location)
                  )))
    (when (eq result 0)
      (tla-show-process-buffer-internal t)
      (run-hooks 'tla-new-archive-hook))))

(defun tla-make-archive (name location)
  "Create a new arch archive.
NAME is the global name for the archive.  It must be an
email address with a fully qualified domain name, optionally
followed by \"--\" and a string of letters, digits, periods
and dashes.
LOCATION specifies the path, where the archive should be created.

Examples for name are:
foo.bar@flups.com--public
foo.bar@flups.com--public-2004"
  (interactive "sArchive name: \nFLocation: ")
  (setq location (expand-file-name location))
  (if (file-directory-p location)
      (error "directory is existing: %s" location))
  (tla--run-arch nil t 'make-archive "make-archive" name location)
  (tla-show-process-buffer-internal t)
  (run-hooks 'tla-new-archive-hook))

(defun tla-mirror-archive ( &optional archive location mirror)
  "Creates a mirror for the archive ARCHIVE"
  (interactive)
  (let* ((archive-loc (or archive (car (tla-read-archive-name "Archive to mirror: "))))
         (location-loc (or location (read-string "Location of the mirror: ")))
         (mirror-loc (or mirror (read-string "Name of the mirror: "
                                             (concat archive "-MIRROR")))))
    (tla--run-arch nil t 'make-archive "make-archive"
                  "--mirror" archive-loc mirror-loc location-loc)
    (tla-show-process-buffer-internal t)
    (run-hooks 'tla-new-archive-hook)))

(defvar tla-read-archive-history nil)
(defun tla-read-archive-name (&optional archive-prompt default)
  (let ((my-default-archive (or default (tla-my-default-archive)))
        a)
    (unless archive-prompt
      (setq archive-prompt "Archive name: "))
    (list
     (progn
       (tla-archives-build-archive-tree)
       (setq a (completing-read
                archive-prompt
                tla-archive-tree
                nil nil
                my-default-archive
                'tla-read-archive-history
                my-default-archive))
       (if (string= "" a) (setq a nil))
       a))))

;;
;; Categories
;;
(defun tla-categories-build-archive-tree (archive)
  (tla--run-arch nil t 'categories "categories" "-A" archive)
  (with-current-buffer tla--last-process-buffer
    (let (category)
      (goto-char (point-min))
      (while (> (line-end-position) (line-beginning-position))
        (setq category (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (tla-archive-tree-add-category archive category)))))

(defun tla-categories (archive)
  (interactive (tla-read-archive-name))
  (unless archive
    (setq archive (tla-my-default-archive)))
  (tla-categories-build-archive-tree archive)
  (tla-switch-to-buffer "*tla-categories*")
  (let ((list (cddr (tla-archive-tree-get-archive archive)))
        category start-pos overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-archives.
    (insert (format "Archive: %s\n%s\n" archive
                    (make-string (+ (length archive)
                                    (length "Archive: ")) ?=)))
    (save-excursion
      (while list
        (setq category (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (tla-add-face category 'tla-category-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'tla-category-info category))
      (delete-backward-char 1)))
  (tla-category-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name)
       archive))

(defun tla-make-category (archive category)
  "Make new category."
  (interactive (tla-read-archive-category-name))
  (tla--run-arch nil t 'make-category "make-category" "-A" archive category)
  (tla-show-process-buffer-internal t)
  (let ((tla-buffer-archive-name archive))
    (run-hooks 'tla-make-category-hook)))

(defvar tla-read-archive-category-history nil)
(defun tla-read-archive-category-name (&optional archive-prompt
                                                 category-prompt)
  (unless category-prompt
    (setq category-prompt "Category name: "))
  (let* ((l (tla-read-archive-name archive-prompt))
         (a (car l))
         c)
    (when a
      (tla-categories-build-archive-tree a)
      (setq c (completing-read
               category-prompt
               (cddr (tla-archive-tree-get-archive a))
               nil nil nil
               'tla-read-archive-category-history))
      (if (string= "" c) (setq c nil)))
    (list a c)))

;;
;; Branches
;;
(defun tla-branches-build-archive-tree (archive category)
  (tla--run-arch nil t 'branches "branches" "-A" archive category)
  (with-current-buffer tla--last-process-buffer
    (let (branch)
      (goto-char (point-min))
      (while (> (line-end-position) (line-beginning-position))
        (setq branch (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (tla-archive-tree-add-branch
         archive
         category
         (car (last (tla-name-split-components branch))))))))

(defun tla-branches (archive category)
  (interactive (tla-read-archive-category-name))
  (tla-branches-build-archive-tree archive category)
  (tla-switch-to-buffer "*tla-branches*")
  (let ((list (cdr (tla-archive-tree-get-category archive category)))
        alength
        clength
        branch
        start-pos
        overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-categories and tla-archives
    (setq alength (+ (length archive)  (length "Archive: "))
          clength (+ (length category) (length "Category: ")))
    (insert (format "Archive: %s\nCategory: %s\n%s\n" archive category
                    (make-string (max alength clength) ?=)))
    (save-excursion
      (while list
        (setq branch (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (tla-add-face branch 'tla-branch-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'tla-branch-info branch))
      (delete-backward-char 1)))
  (tla-branch-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name)
       archive)
  (set (make-local-variable 'tla-buffer-category-name)
       category))

(defvar tla-read-archive-category-branch-history nil)
(defun tla-read-archive-category-branch-name (&optional archive-prompt
                                                        category-prompt
                                                        branch-prompt)
  (unless branch-prompt
    (setq branch-prompt "Branch name: "))
  (let* ((l (tla-read-archive-category-name
             archive-prompt
             category-prompt))
         (a (car l))
         (c (cadr l))
         b)
    (when c
      (tla-branches-build-archive-tree a c)
      (setq b (completing-read
               branch-prompt
               (cdr (tla-archive-tree-get-category a c))
               nil nil nil
               'tla-read-archive-category-branch-history))
      (if (string= "" b) (setq b nil)))
    (list a c b)))

(defun tla-make-branch (archive category branch)
  "Make new branch."
  (interactive (tla-read-archive-category-branch-name))
  (tla--run-arch nil t 'make-branch "make-branch" "-A" archive
                (tla-name-construct category branch))
  (tla-show-process-buffer-internal t)
  (let ((tla-buffer-archive-name archive)
        (tla-buffer-category-name category))
    (run-hooks 'tla-make-branch-hook)))

;;
;; Versions
;;
(defun tla-versions (archive category branch)
  (interactive (tla-read-archive-category-branch-name))
  (tla-versions-build-archive-tree archive category branch)
  (tla-switch-to-buffer "*tla-versions*")
  (let ((list (cdr (tla-archive-tree-get-branch
                    archive category branch)))
        alength
        clength
        blength
        version
        start-pos
        overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-categories and tla-archives
    (setq alength (+ (length archive)  (length "Archive: "))
          clength (+ (length category) (length "Category: "))
          blength (+ (length branch)   (length "Branch: ")))
    (insert (format "Archive: %s\nCategory: %s\nBranch: %s\n%s\n"
                    archive category branch
                    (make-string (max alength clength blength) ?=)))
    (save-excursion
      (while list
        (setq version (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (tla-add-face version 'tla-version-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'tla-version-info version))
      (delete-backward-char 1)))
  (tla-version-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name) archive)
  (set (make-local-variable 'tla-buffer-category-name) category)
  (set (make-local-variable 'tla-buffer-branch-name) branch))

(defun tla-versions-build-archive-tree (archive category branch)
  (tla--run-arch nil t 'versions "versions" "-A" archive
                (tla-name-construct category branch))
  (with-current-buffer tla--last-process-buffer
    (let (version)
      (goto-char (point-min))
      (while (> (line-end-position) (line-beginning-position))
        (setq version (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (tla-archive-tree-add-version
         archive
         category
         branch
         (car (last (tla-name-split-components version))))))))

(defvar tla-read-archive-category-branch-version-history nil)
(defun tla-read-archive-category-branch-version-name (&optional archive-prompt
                                                                category-prompt
                                                                branch-prompt
                                                                version-prompt)
  (unless version-prompt
    (setq version-prompt "Version name: "))
  (let* ((l (tla-read-archive-category-branch-name
             archive-prompt
             category-prompt
             branch-prompt))
         (a (car l))
         (c (cadr l))
         (b (caddr l))
         v)
    (when b
      (tla-versions-build-archive-tree a c b)
      (setq v (completing-read
               version-prompt
               (cdr (tla-archive-tree-get-branch a c b))
               nil nil nil
               'tla-read-archive-category-branch-version-history
               ))
      (if (string= "" v) (setq v nil)))
    (list a c b v)))

(defun tla-make-version (archive category branch version)
  "Make new version."
  (interactive (tla-read-archive-category-branch-version-name))
  (tla--run-arch nil t 'make-branch "make-version" "-A" archive
                (tla-name-construct category branch version))
  (tla-show-process-buffer-internal t)
  (let ((tla-buffer-archive-name archive)
        (tla-buffer-category-name category)
        (tla-buffer-branch-name branch))
    (run-hooks 'tla-make-version-hook)))

;;
;; Revisions
;;
(defvar tla-revisions-shows-summary t
  "* Wether summary should be displayed for `tla-revisions'")
(defvar tla-revisions-shows-creator t
  "* Wether creator should be displayed for `tla-revisions'")
(defvar tla-revisions-shows-date t
  "* Wether date should be displayed for `tla-revisions'")
(defvar tla-revisions-shows-library nil
  "* Wether the presence of this revision in the library should be
displayed for `tla-revisions'")


;; elem should be
;; ('separator "string" kind)
;; or
;; ('entry-patch nil revision summary creator date)
;; ('entry-change "changes")
;; The second element tells if the element is marked or not.
(defun tla-revision-list-printer (elem)
  (case (car elem)
    (entry-patch (insert (if (cadr elem) " *" "  ")
			 ;; The revision is in library?
			 (if (and tla-revisions-shows-library
                                  (apply 'tla-library-find
                                         (append (caddr elem) '(t))))
			     "L " "  ")
                         (tla-add-face (apply 'tla-fully-qualified-revision
                                              (caddr elem))
                                       'tla-revision-name))
                 (when tla-revisions-shows-summary
                   (insert "\n      " (cadddr elem)))
                 (when tla-revisions-shows-creator
                   (insert "\n      " (cadddr (cdr elem))))
                 (when tla-revisions-shows-date
                   (insert "\n      " (cadddr (cddr elem)))))
    (entry-change (insert  (cadr elem)))
    (separator
     (case (caddr elem)
       (partner (insert "\n" (tla-add-face (cadr elem)
                                           'tla-separator)))
       (bookmark (insert "\n" (tla-add-face
                               (concat "*** "
                                       (cadr elem)
                                       " ***")
                               'tla-separator) "\n"))))
    ))

(defun tla-tree-revisions ()
  "Calls `tla-revisions` in the current tree"
  (interactive)
  (let ((version (tla-tree-version-list)))
    (unless version
      (error "not in a project tree"))
    (apply 'tla-revisions version)))

(defvar tla-revisions-tree-contains-details nil
  "Wether the revision tree contains summary/date/creator
information")

;;;###autoload
(defun tla-revisions (archive category branch version &optional update-display)
  (interactive (tla-read-archive-category-branch-version-name))
  (unless (and update-display
               (or tla-revisions-tree-contains-details
                   (not (or tla-revisions-shows-summary
                            tla-revisions-shows-creator
                            tla-revisions-shows-date))))
    (tla-revisions-build-archive-tree archive category branch version))
  (tla-switch-to-buffer "*tla-revisions*")
  (let ((list (cdr (tla-archive-tree-get-version
                    archive category branch version)))
        first
        alength
        clength
        blength
        vlength
        revision
        summary
        creator
        date)
    (tla-revision-list-mode)
    (toggle-read-only -1)
    (erase-buffer)
    (set (make-local-variable 'tla-revision-list-cookie)
         (ewoc-create 'tla-revision-list-printer))
    (set (make-local-variable 'tla-buffer-refresh-function)
         'tla-revision-refresh)
    ;; TODO: button to invoke tla-categories and tla-archives
    (setq alength (+ (length archive)  (length "Archive: "))
          clength (+ (length category) (length "Category: "))
          blength (+ (length branch)   (length "Branch: "))
          vlength (+ (length version)   (length "Version: "))
          )
    (ewoc-set-hf tla-revision-list-cookie
                 (format "Archive: %s\nCategory: %s\nBranch: %s\nVersion: %s\n%s\n"
                         archive category branch version
                         (make-string (max alength clength blength
                                           vlength) ?=))
                 (make-string (max alength clength blength
                                   vlength) ?=))

    (while list
      (setq revision (car (car list))
            summary (car (cdr (car list)))
            creator (car (cddr (car list)))
            date (car (cdddr (car list)))
            list (cdr list))
        (ewoc-enter-last tla-revision-list-cookie
                         (list 'entry-patch nil
                               (list archive
                                     category
                                   branch
                                   version
                                   revision)
                               summary creator date))
      (if first
          (goto-char first)
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line 1)
        (setq first (point)))
      (sit-for 0)))

  (set (make-local-variable 'tla-buffer-archive-name) archive)
  (set (make-local-variable 'tla-buffer-category-name) category)
  (set (make-local-variable 'tla-buffer-branch-name) branch)
  (set (make-local-variable 'tla-buffer-version-name) version)
  (toggle-read-only t))

(defun tla-revisions-build-archive-tree (archive category branch version)
  (let ((details (or tla-revisions-shows-summary
                     tla-revisions-shows-date
                     tla-revisions-shows-creator)))
    (if details
        (progn
          (tla--run-arch nil t 'revisions "revisions" "-A" archive
                        "--summary" "--date" "--creator"
                        (tla-name-construct category branch version))
          (setq tla-revisions-tree-contains-details t))
      (progn
        (tla--run-arch nil t 'revisions "revisions" "-A" archive
                      (tla-name-construct category branch version))
        (setq tla-revisions-tree-contains-details nil)))
    (with-current-buffer tla--last-process-buffer
      (let (revision date creator summary)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq revision (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
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
            (setq summary (buffer-substring-no-properties (point)
                                                          (line-end-position)))
            (forward-line 1))
          (tla-archive-tree-add-revision
           archive
           category
           branch
           version
           revision
           summary
           creator
           date))))))

;;;###autoload
(defun tla-missing (local-tree location)
  "Runs tla missing in the directory LOCAL-TREE"
  (interactive (let ((dir (expand-file-name
                           (read-directory-name
                            "Search missing patches in directory: "
                            default-directory default-directory t nil))))
                 (list dir
                       (let ((default-directory dir))
                         (read-string "From location: " (tla-tree-version))))))
  (pop-to-buffer (get-buffer-create "*tla-missing*"))
  (tla-revision-list-mode)
  (set (make-local-variable 'tla-buffer-refresh-function)
       'tla-missing-refresh)
  (set (make-local-variable 'tla-missing-buffer-todolist)
       `((missing ,local-tree ,location nil)))
  (tla-missing-refresh))

(defun tla-fully-qualified-revision (archive &optional
                                             category
                                             branch
                                             version
                                             revision)
  "Creates the fully qualified revision name :
archive/category--branch--version--revision. The arguments may be
nil."
  (concat
   (and archive (concat archive "/"))
   (tla-name-construct category branch version revision)))

(defun tla-archive-name (list)
  "LIST must be a full revision in the form of a list."
  (car list))

(defun tla-category-name (list)
  "LIST must be a full revision in the form of a list."
  (cadr list))

(defun tla-branch-name (list)
  "LIST must be a full revision in the form of a list."
  (caddr list))

(defun tla-version-name (list)
  "LIST must be a full revision in the form of a list."
  (cadddr list))

(defun tla-revision-name (list)
  "LIST must be a full revision in the form of a list."
  (cadddr (cdr list)))

(defun tla-split-revision-name (name)
  "Parses a fully qualified revision name, but possibly incomplete.
email@address.com--arch/cat--branch--ver ->
  (\"email@address.com--arch\" \"cat\" \"branch\" \"ver\")
email@address.com--arch/cat ->
  (\"email@address.com--arch\" \"cat\" nil nil)"
  (if (string-match "\\(.*\\)/\\(.*\\)" name)
      (cons (match-string 1 name)
            (tla-name-split-components (match-string 2 name) 4))))

(defvar tla-read-archive-category-branch-version-revision-history nil)
(defun tla-read-archive-category-branch-version-revision-name (&optional archive-prompt
                                                                         category-prompt
                                                                         branch-prompt
                                                                         version-prompt
                                                                         revision-prompt)
  (unless revision-prompt
    (setq revision-prompt "Revision name: "))
  (let* ((l (tla-read-archive-category-branch-version-name
             archive-prompt
             category-prompt
             branch-prompt
             version-prompt))
         (a (tla-archive-name l))
         (c (tla-category-name l))
         (b (tla-branch-name l))
         (v (tla-version-name l))
         r)
    (when v
      (tla-revisions-build-archive-tree a c b v)
      (setq r (completing-read
               revision-prompt
               (cdr (tla-archive-tree-get-version a c b v))
               nil nil nil
               'tla-read-archive-category-branch-version-revision-history)))
    (if (string= "" r) (setq r nil))
    (list a c b v r)))

;;
;; Rbrowse interface
;;
;; TODO: Use tree-widget.
(defun tla-browse-archive (archive)
  (interactive (tla-read-archive-name))
  (unless archive
    (setq archive (tla-my-default-archive)))
  (tla--run-arch nil t 'rbrowse "rbrowse" "-A" archive)
  (tla-show-process-buffer-internal t))

;;
;; Get
;;
(defun tla-get (directory run-dired-p archive category branch
                          &optional version revision)
  ;; run-dired-p => t, nil, ask
  (interactive (let* ((l (tla-read-archive-category-branch-version-revision-name))
                      (name (apply 'tla-name-construct (remove nil l)))
                      (d (read-directory-name (format "Store \"%s\" to: " name))))
                 (cons d (cons 'ask l))))
  (let* ((name (tla--get-revision
                (apply 'tla-fully-qualified-revision
                       (list archive category branch version
                             revision))))
         (result (tla--run-arch nil t 'get "get" "-A" archive name
                               directory)))
    (when (eq 0 result)
      (tla--run-arch nil t 'get "get" name directory)
      (let ((bookmark (tla-bookmarks-find-bookmark (tla-fully-qualified-revision
                                                    archive category branch
                                                    version))))
        (when bookmark
          (tla-bookmarks-add-tree bookmark directory)))
      (case run-dired-p
        (ask (when (y-or-n-p (format "Run dired at %s? " directory))
               (dired directory)))
        (t   (dired directory))))))

;;
;; Cacherev
;;
;; TODO:
;; - run this asynchronous
;; - provide the way to run interactively
;; - show progress
;;
(defun tla-cache-revision (archive category branch version revision)
  (let ((result (tla--run-arch nil t 'cacherev "cacherev" "-A" archive
                              (tla-name-construct category branch version revision))))
    (tla--show-last-process-buffer)
    (message "Exit status: %d" result)
    result))

;;
;; Add
;;
(defun tla-add (id &rest files)
  (interactive (let ((name
                      (read-file-name "Add file as source: "
                                      nil nil t
                                      (file-name-nondirectory (or
                                                               (buffer-file-name) ""))))
                     (id (read-string "id (empty for default): ")))
                 (list id name)))
  (if (and id (string= id ""))
      (setq id nil))
  (setq files (mapcar 'expand-file-name files))
  (if id
      (apply 'tla--run-arch nil t 'add "add" "--id" id files)
    (apply 'tla--run-arch nil t 'add "add" files)))

;;
;; Remove
;;
(defun tla-remove (only-id &rest files)
  (interactive (let ((name
                      (read-file-name "Remove file: "
                                      nil nil t
                                      (file-name-nondirectory (or
                                                               (buffer-file-name) ""))))
                     (only-id (not (y-or-n-p "Delete the file locally also? "))))
                 (list only-id name)))
  (setq files (mapcar 'expand-file-name files))
  (dolist (f files)
    (when (equal 0 (tla--run-tla-sync (list "id" "--explicit" f)
                                      :finished 'tla--status-handler
                                      :error 'tla--status-handler))
      (tla--run-tla-sync (list "delete-id" f)))
    (unless only-id
      (delete-file f))))

;;
;; Move
;;
(defun tla-move (from to only-id)
  (interactive
   (list (from (read-file-name "Move file: "
                               nil nil t
                               (file-name-nondirectory
                                (or (buffer-file-name) ""))))))

  (setq to (or to (read-file-name (format "Move file %S to: " from)
                                  nil nil nil))
        only-id (if (eq only-id 'ask)
                    (not (y-or-n-p "Move the file locally also? "))
                  only-id)
        from (expand-file-name from)
        to   (expand-file-name to))

  (let ((buffer (get-file-buffer from))
        (cmd (if only-id "move-id" "mv")))

    (if buffer
        (save-excursion
          (set-buffer buffer)
          (set-visited-file-name to)))

    (apply 'tla--run-arch nil t 'delete cmd (list from to))))

;;
;; Update
;;
(defun tla-update (tree)
  (interactive (list (expand-file-name
                      (read-directory-name "Update tree: " nil nil nil ""))))
  (or (tla-save-some-buffers tree)
      (y-or-n-p
       "Update may delete unsaved changes. Continue anyway? ")
      (error "Not updating"))
  (cd tree)
  (tla--run-tla-sync (list "update" (when tla-use-forward-option "--forward")))
  (tla-revert-some-buffers tree))

;;
;; Import
;;
;;;###autoload
(defun tla-start-project ()
  "Start a new project.
Prompts for the root directory of the project and the fully
qualified version name to use.  Sets up and imports the tree and
displays an inventory buffer to allow the project's files to be
added and committed."
  (interactive)
  (let* ((base (read-directory-name "Directory containing files to import: "
                                    (or default-directory
                                        (getenv "HOME"))))
         (l (tla-read-archive-category-branch-version-name))
         (project (if (member nil l)
                      (error "Need a fully qualified version name")
                    (apply 'tla-fully-qualified-revision l))))
    (let ((default-directory (concat base "/")))
      (tla--run-tla-sync (list "init-tree" project))
      (tla--run-tla-sync (list "import" "--setup"))
      (tla-inventory base t))))


;; ----------------------------------------------------------------------------
;; xtla partner stuff
;; ----------------------------------------------------------------------------
(defun tla-partner-find-partner-file ()
  "Do find-file tla-parterns file and return the buffer."
  (interactive)
  (find-file (concat (tla-tree-root) "/++tla-partners")))

(defun tla-partner-read (&optional prompt)
  (apply 'tla-fully-qualified-revision
         (tla-read-archive-category-branch-version-name
          (when prompt (concat prompt "[Archive name] "))
          (when prompt (concat prompt "[Category name] "))
          (when prompt (concat prompt "[Branch name] "))
          (when prompt (concat prompt "[Version name] "))
          )))

(defun tla-partner-add (partner)
  "Add a partner for this xtla working copy.
Return nil if PARTNER is alerady in partners file.
For example: Franz.Lustig@foo.bar--public/tla--main--0.1"
  (interactive (list (tla-partner-read)))
  (let ((list (tla-partner-list)))
    (if (member partner list)
        nil
      (with-current-buffer (tla-partner-find-partner-file)
        (goto-char (point-min))
        (insert partner)
        (newline)
        (save-buffer)
        (kill-buffer (current-buffer)))
      partner)))

(defun tla-partner-list ()
  (with-current-buffer (tla-partner-find-partner-file)
    (let ((partners (split-string (buffer-substring (point-min) (point-max)) "\n")))
      (prog1 (remove "" partners)
        (kill-buffer (current-buffer))))))

(defvar tla-partner-select-history nil)
(defvar tla-partner-select-history-including-self nil)
(defun tla-partner-select (&optional prompt including-self)
  "Select a partner from the xtla partner file
If INCLUDING-SELF is non-nil and a user gives an empty string from minibuffer, 
this function can return `self'."
  (unless prompt
    (setq prompt "Enter xtla partner: "))
  (save-window-excursion
    (let ((partners (tla-partner-list)))
      (if including-self
	  (cond ((= 0 (length partners))
			       (setq tla-partner-select-was-interactive nil)
			       'self)
			      (t
			       (setq tla-partner-select-was-interactive t)
			       (completing-read 
				prompt
				(mapcar 'list partners) nil t nil 
				'tla-partner-select-history-including-self
				'self)))
	(cond ((= 0 (length partners))
			     (setq tla-partner-select-was-interactive t)
			     (tla-partner-add (tla-partner-read prompt)))
			    ((= 1 (length partners))
			     (setq tla-partner-select-was-interactive nil)
			     (car partners))
			    (t
			     (setq tla-partner-select-was-interactive t)
			     (completing-read prompt
					      (mapcar 'list partners) nil t (car partners)
					      'tla-partner-select-history
					      )))))))


;; (tla-partner-popup-menu)
(defun tla-partner-popup-menu (&optional prompt)
  (let ((list (tla-partner-list)))
    (popup-menu (easy-menu-create-menu prompt
                                       (mapcar
                                        (lambda (item)
                                          (let ((v (make-vector 3 nil)))
                                            (aset v 0 item) ; name
                                            (aset v 1 item) ; callback
                                            (aset v 2 t) ; enable
                                            ;;(aset v 3 :style)
                                            ;;(aset v 4 'radio)
                                            ;;(aset v 5 :selected)
                                            ;;(aset v 6 (if ...))
                                            v))
                                        list)))))

;; ----------------------------------------------------------------------------
;; tla-inventory-mode:
;; ----------------------------------------------------------------------------

(defun tla-inventory-mode ()
  "Major Mode to show the inventory of a tla working copy.

This allows you to view the list of files in your local tree. You can
display only some particular kinds of files with 't' keybindings:
'\\<tla-inventory-mode-map>\\[tla-inventory-toggle-source]' to toggle show sources,
'\\[tla-inventory-toggle-precious]' to toggle show precious, ...

Use '\\[tla-inventory-mark-file]' to mark files, and '\\[tla-inventory-unmark-file]' to unmark.
If you commit from this buffer (with '\\[tla-inventory-edit-log]'), then, the list of selected
files in this buffer at the time you actually commit with
\\<tla-log-edit-mode-map>\\[tla-log-edit-done].

Commands:
\\{tla-inventory-mode-map}
"
  (interactive)
  (use-local-map tla-inventory-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-inventory-font-lock-keywords t))
  (set (make-local-variable 'tla-buffer-refresh-function)
       'tla-inventory)
  (make-local-variable 'tla-buffer-marked-file-list)
  (easy-menu-add tla-inventory-mode-menu)
  (setq major-mode 'tla-inventory-mode)
  (setq mode-name "tla-inventory")
  (setq mode-line-process 'tla-mode-line-process)
  (set (make-local-variable 'tla-get-file-info-at-point-function)
       'tla-inventory-get-file-info-at-point)

  (toggle-read-only 1)
  (run-hooks 'tla-inventory-mode-hook))

(defun tla-inventory-cursor-goto (ewoc-inv)
  "Move cursor to the ewoc location of EWOC-BOOKMARK"
  (interactive)
  (if ewoc-inv
      (progn (goto-char (ewoc-location ewoc-inv))
             (forward-char 6))
    (goto-char (point-min))))

(defun tla-inventory-next ()
  (interactive)
  (let* ((cookie tla-inventory-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla-inventory-cursor-goto next)))

(defun tla-inventory-previous ()
  (interactive)
  (let* ((cookie tla-inventory-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla-inventory-cursor-goto previous)))

(defun tla-inventory-edit-log (&optional insert-changelog)
  "Wrapper around `tla-edit-log', setting the source buffer to current
buffer."
  (interactive "P")
  (tla-edit-log insert-changelog (current-buffer)))

(defun tla-inventory-add (files)
  (interactive
   (list
    (if tla-buffer-marked-file-list
        (progn
          (unless (y-or-n-p (if (eq 1 (length tla-buffer-marked-file-list))
				(format "Add %s? "
				      (car tla-buffer-marked-file-list))
			      (format "Add %s files? "
				      (length tla-buffer-marked-file-list))))
            (error "Not adding any file"))
          tla-buffer-marked-file-list)
      (list (read-file-name "Add file: " default-directory
                            nil nil
                            (tla--get-file-info-at-point))))))
  (if (eq 0 (apply 'tla-add nil files))
      (tla-inventory)
    (tla-show-process-buffer-internal t)))

(defun tla-inventory-remove (files id-only)
  (interactive
   (list
    (if tla-buffer-marked-file-list
        (progn
          (unless (y-or-n-p (format "Remove %s files? "
                                    (length tla-buffer-marked-file-list)))
            (error "Not removing any files"))
          tla-buffer-marked-file-list)
      (list (read-file-name "Remove file: " default-directory
                            nil nil
                            (tla--get-file-info-at-point))))
    (not (y-or-n-p "Delete files also locally? "))))
  (if (eq 0 (apply 'tla-remove id-only files))
      (tla-inventory)
    (tla--show-last-process-buffer)))

(defun tla-inventory-delete (files no-questions)
  "Delete files locally.
This is here for convenience to delete left over, temporary files or files
avoiding a commit or conflicting with tree-lint.

It is not meant to delete tla managed files, i.e. files with IDs will be
passed to `tla-inventory-remove'!

When called with a prefix arg, do not annoy the user with questions, but just
delete the files."
  (interactive
   (list
    (if tla-buffer-marked-file-list
        (progn
          (or current-prefix-arg
              (unless (yes-or-no-p
                       (format "Delete %d files permanently? "
                               (length tla-buffer-marked-file-list)))
                (error "Not deleting any files")))
          tla-buffer-marked-file-list)
      (if (or current-prefix-arg
              (yes-or-no-p (format "Delete file %S permanently? "
                                   (tla--get-file-info-at-point))))
          (list (tla--get-file-info-at-point))))
    current-prefix-arg))
  (while files
    (let ((f (car files)))
      (if (= 0 (tla--run-tla-sync (list "id" f)
                                  :finished 'tla--status-handler
                                  :error 'tla--status-handler))
          (if (or no-questions
                  (y-or-n-p (format (concat "File %s is arch managed! "
                                            "Delete it with its id?") f)))
              (tla-inventory-remove (list f) nil))
        (if (file-directory-p f)
            (condition-case nil
                (delete-directory f)
              (file-error
               (if (or no-questions
                       (y-or-n-p (format "Delete non-emtpy directory %S? " f)))
                   (dired-recursive-delete-directory f))))
          (delete-file f))))
    (setq files (cdr files)))
  (tla-inventory))

(defun tla-inventory-move ()
  "Rename a file and if present also its ID."
  (interactive)
  (if (eq 0 (tla-move (tla--get-file-info-at-point) nil 'ask))
      (tla-inventory)
    (tla-show-process-buffer-internal t)))

(defun tla-file-has-conflict-p (file-name)
  (let ((rej-file-name (concat default-directory (file-name-nondirectory file-name) ".rej")))
    (file-exists-p rej-file-name)))

(defun tla-inventory-find-file ()
  (interactive)
  (let* ((file (tla--get-file-info-at-point)))
    (cond
     ((not file)
      (error "no file at point"))
     ((car (file-attributes file)) ; file is a directory
      (tla-inventory (expand-file-name file)))
     (t
      (find-file file)
      (when (tla-file-has-conflict-p file)
        (smerge-mode t))))))

(defun tla-inventory-parent-directory ()
  "Go to parent directory in inventory mode"
  (interactive)
  (cd "..")
  (tla-inventory))

(defun tla-inventory-find-file-other-window ()
  (interactive)
  (let ((file (tla--get-file-info-at-point)))
    (if file
        (progn
          (find-file-other-window file)
          (when (tla-file-has-conflict-p file)
            (smerge-mode t)))
      (error "no file at point"))))

(defun tla-inventory-view-file ()
  (interactive)
  (let ((file (tla--get-file-info-at-point)))
    (if file
        (view-file-other-window file)
      (error "no file at point"))))

(defun tla-inventory-mirror ()
  (interactive)
  (let ((tree-version (tla-tree-version-list)))
    (tla-archive-mirror (tla-archive-name  tree-version)
                        (tla-category-name tree-version)
                        (tla-branch-name   tree-version)
                        (tla-version-name  tree-version))))

(defun tla-inventory-star-merge ()
  "Run tla star-merge.
Either use a partner in (concat (tla-tree-root) \"/++tla-partners\")
Or ask the user for the merge partner."
  (interactive)
  (let ((merge-partner (tla-partner-select "Star-merge with: ")))
    (when (or tla-partner-select-was-interactive
              (y-or-n-p (format "Star-merge with %s ? " merge-partner)))
      (tla-star-merge merge-partner))))

(defun tla-inventory-changes (summary)
  "Run tla changes.
Either use a partner in (concat (tla-tree-root) \"/++tla-partners\")
Or run against default revision." 
  (interactive "P")
  (let ((compare-partner (tla-partner-select "Compare with(default is your tree): " t)))
    (when (eq 'self compare-partner)
      (setq compare-partner nil))
    (tla-changes summary compare-partner)))

(defun tla-inventory-replay ()
  "Run tla replay.
Either use a partner in (concat (tla-tree-root) \"/++tla-partners\")
Or ask the user for the merge partner."
  (interactive)
  (let ((merge-partner (tla-partner-select "Replay from: ")))
    (when (or tla-partner-select-was-interactive
              (y-or-n-p (format "Replay from %s ? " merge-partner)))
      (tla-replay merge-partner))))

(defun tla-inventory-missing ()
  "Run tla missing in default-directory."
  (interactive)
  (let ((missing-partner (tla-partner-select "Check missing against: ")))
    (when (or tla-partner-select-was-interactive
              (y-or-n-p (format "Check missing against %s ? " missing-partner)))
      (tla-missing default-directory missing-partner))))

(defun tla-inventory-file-ediff (&optional file)
  (interactive (list (caddr (ewoc-data (ewoc-locate tla-inventory-cookie)))))
  (tla-file-ediff file))

;; ----------------------------------------------------------------------------
;; tla-logs-mode:
;; ----------------------------------------------------------------------------
(defun tla-logs-mode ()
  "Major Mode to show the logs for the actual working copy history.
Commands:
\\{tla-logs-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-logs-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-logs-font-lock-keywords t))
  (setq major-mode 'tla-logs-mode)
  (setq mode-name "tla-logs")
  (toggle-read-only 1)
  (run-hooks 'tla-logs-mode-hook))

(defun tla-logs-toggle-date ()
  (interactive)
  (save-window-excursion
    (tla-toggle-list-entry tla-logs-flag-list "--date")
    (other-window -1)
    (tla-logs)))

(defun tla-logs-toggle-creator ()
  (interactive)
  (save-window-excursion
    (tla-toggle-list-entry tla-logs-flag-list "--creator")
    (other-window -1)
    (tla-logs)))

(defun tla-logs-toggle-summary ()
  (interactive)
  (save-window-excursion
    (tla-toggle-list-entry tla-logs-flag-list "--summary")
    (other-window -1)
    (tla-logs)))

(defun tla-logs-toggle-reverse ()
  (interactive)
  (save-window-excursion
    (tla-toggle-list-entry tla-logs-flag-list "--reverse")
    (other-window -1)
    (tla-logs)))

(defvar tla-revision-regexp "^[^ \t]*\\(\\(patch\\|base\\|version\\(fix\\)?\\)-[0-9]+\\)")
(defvar tla-logs-patch-start-regexp "^[^ \t]*\\(\\(patch\\|base\\|version\\(fix\\)?\\)-[0-9]+\\)")

(defun tla-logs-prev-revision ()
  (interactive)
  (or (re-search-backward tla-logs-patch-start-regexp nil t)
      (not (interactive-p))
      (ding)))

(defun tla-logs-next-revision ()
  (interactive)
  (end-of-line)
  (or (re-search-forward tla-logs-patch-start-regexp nil t)
      (not (interactive-p))
      (ding))
  (beginning-of-line))

(defun tla-logs-cat-log ()
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward tla-logs-patch-start-regexp))
  (tla-cat-log (match-string 1)))

(defun tla-cat-log (revision-spec)
  (interactive "sRevision spec: ")
  (tla--run-tla-sync (list "cat-log" revision-spec))
  (tla--show-last-process-buffer 'cat-log 'tla-cat-log-mode))

(defun tla-cat-archive-log (revision-spec)
  "Run cat-archive-log."
  (interactive "sRevision spec: ")
  (tla--run-tla-sync (list "cat-archive-log" revision-spec))
  (tla--show-last-process-buffer 'cat-log 'tla-cat-log-mode))

;; ----------------------------------------------------------------------------
;; tla-cat-log-mode:
;; ----------------------------------------------------------------------------
(defun tla-cat-log-mode ()
  "Major Mode to show a specific log mesage
Commands:
\\{tla-cat-log-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-cat-log-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-cat-log-font-lock-keywords t))
  (setq major-mode 'tla-cat-log-mode)
  (setq mode-name "tla-cat-log")
  (toggle-read-only 1)
  (run-hooks 'tla-cat-log-mode-hook))

;; ----------------------------------------------------------------------------
;; tla-log-edit-mode:
;; ----------------------------------------------------------------------------
(defun tla-log-edit-next-field ()
  "Go to next field in a log edition"
  (interactive)
  (let ((in-field (string-match "^\\([A-Z][A-Za-z]*\\(: ?\\)?\\)?$"
                                (buffer-substring
                                 (line-beginning-position) (point)))))
    (if (and in-field
             (string-match "^[A-Z][A-Za-z]*: $"
                           (buffer-substring
                            (line-beginning-position) (point))))
        (forward-line))
    (if in-field (beginning-of-line) (forward-line 1))
    (or (and (looking-at "^[A-Z][a-zA-Z]*: ")
             (goto-char (match-end 0)))
        (and (looking-at "^[A-Z][a-zA-Z]*:$")
             (goto-char (match-end 0))
             (progn (insert " ") t))
        (goto-char (point-max)))))

(defun tla-log-goto-summary ()
  "Go to the Summary field in a log file."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^Summary: "))

(defun tla-log-goto-keywords ()
  "Go to the Summary field in a log file."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^Keywords: "))

(defun tla-log-goto-body ()
  "Go to the Body in a log file."
  (interactive)
  (goto-char (point-min))
  (forward-line 3))

(defun tla-log-kill-body ()
  "Kill the content of the log file body."
  (interactive)
  (tla-log-goto-body)
  (kill-region (point) (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\+\\+log\\." . tla-log-edit-mode))
;;;###autoload
(define-derived-mode tla-log-edit-mode text-mode "tla-log-edit"
  "Major Mode to edit xtla log messages.
Commands:
\\{tla-log-edit-mode-map}
"
  (use-local-map tla-log-edit-mode-map)
  (easy-menu-add tla-log-edit-mode-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-log-edit-font-lock-keywords t))
  (setq fill-column 73)
  (run-hooks 'tla-log-edit-mode-hook))

(defun tla-log-edit-abort ()
  (interactive)
  (bury-buffer)
  (set-window-configuration tla-pre-commit-window-configuration))

(defun tla-log-edit-done ()
  (interactive)
  (save-buffer)
  (if (equal (tla-commit) 0)
      (kill-buffer (current-buffer))))

(defun tla-archive-maintainer-name (archive)
  "Return the maintainer name for a given ARCHIVE.
This functions looks in the bookmarks file for the nickname field and returns it.
If the nickname field is not present, just return ARCHIVE."
  (tla-bookmarks-load-from-file)
  (dolist (elem tla-bookmarks-alist)
    (when (string= (cadr (assoc 'location elem)) archive)
      (return (or (cadr (assoc 'nickname elem)) archive)))))

(defun tla-merge-summary-line (mergelist)
  (concat "Merged from "
          (mapconcat '(lambda (m)
                        (format "%s (patch%s%s)" (car m)
                                (cadr m)
                                (if (caddr m) (concat "-" (caddr m)) "")))
                     mergelist", ")))

(defun tla-merge-summary-line-for-log ()
  (save-excursion
    (let ((rev-list)
          (maintainer)
          (rev)
          (patch-list))
      (goto-char (point-min))
      (while (re-search-forward "^ \\* \\(.+@.+--.+/.+--.+\\)$" nil t)
        (setq rev-list (tla-split-revision-name (match-string-no-properties 1)))
        (setq maintainer (tla-archive-maintainer-name (tla-archive-name rev-list)))
        (setq rev (cadr (split-string (tla-revision-name rev-list) "-")))
        (add-to-list 'patch-list (list maintainer rev)))
      (add-to-list 'patch-list (list nil nil))
      (setq patch-list (nreverse patch-list))
      (let ((last-maintainer)
            (patch-start)
            (patch-end)
            (rev patch-list)
            (mergelist))
        (while rev
          (if (string= (caar rev) last-maintainer)
              (setq patch-end (cadar rev))
            (when last-maintainer
              (add-to-list 'mergelist (list last-maintainer patch-start patch-end) t))
            (setq last-maintainer (caar rev))
            (setq patch-start (cadar rev)))
          (setq rev (cdr rev)))
        (tla-merge-summary-line mergelist)))))

(defun tla-log-edit-insert-log-for-merge (arg)
  "Insert the output of tla log-for-merge at POINT.
When called with a prefix argument:
Create a standard Merged from line as Summary."
  (interactive "P")
  (let ((on-summary-line (= 1 (count-lines (point-min) (point))))
        (old-pos (point)))
    (tla--run-tla-sync '("log-for-merge")
                       :finished
                       `(lambda (output error status)
                          (let ((content (tla--buffer-content
                                          output)))
                            (if (= 0 (length content))
                                (error "There was no merge!"))
                            (with-current-buffer ,(current-buffer)
                              (if on-summary-line (tla-log-goto-body) (goto-char old-pos))
                              (insert content)))
                          (when arg
                            (tla-log-goto-summary)
                            (delete-region (point) (line-end-position))
                            (insert
                             (with-current-buffer output
                               (tla-merge-summary-line-for-log)))
                            (tla-log-goto-keywords)
                            (delete-region (point) (line-end-position))
                            (insert "merge"))))))

;; ----------------------------------------------------------------------------
;; tla-archive-list-mode:
;; ----------------------------------------------------------------------------
(defun tla-archive-mirror-archive ()
  "Mirrors archive at point"
  (interactive)
  (let ((archive-info (tla-get-archive-info)))
    (when archive-info
      (tla-mirror-archive archive-info))))

(defun tla-archive-synchronize-archive ()
  "Synchronizes the mirror for the archive at point"
  (interactive)
  (let ((archive-info (tla-get-archive-info)))
    (when archive-info
      (tla-archive-mirror archive-info))))

(defun tla-archive-list-mode ()
  "Major Mode to show arch archives:
\\{tla-archive-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-archive-list-mode-map)
  (easy-menu-add tla-archive-list-mode-menu)
  (setq major-mode 'tla-archive-list-mode)
  (setq mode-name "tla-archives")
  (add-hook 'tla-new-archive-hook 'tla-archives)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'tla-get-revision-info-at-point-function)
       'tla--get-archive-info-at-point)
  (run-hooks 'tla-archive-list-mode-hook))

(defun tla--get-archive-info-at-point ()
  "Get archive information."
  (list 'archive (tla-get-archive-info)))

(defun tla-archive-select-default ()
  (interactive)
  (when (tla-get-archive-info)
    (let ((pos (point)))
      (tla-my-default-archive (tla-get-archive-info))
      (tla-archives)
      (goto-char pos))))

(defun tla-archive-unregister-archive ()
  "Delete the registration of the selected archive."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (when (yes-or-no-p (format "Delete the registration of %s? " archive))
          (tla--run-tla-sync  (list "register-archive" "--delete" archive))
          (message "Deleted the registration of %s" archive)
          (tla-archives))
      (error "No archive under the point"))))

(defun tla-archive-list-categories ()
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (tla-categories archive)
      (error "No archive under the point"))))

(defun tla-archive-browse-archive ()
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (tla-browse-archive archive)
      (error "No archive under the point"))))

(defun tla-archive-next ()
  (interactive)
  (forward-line 2)
  (beginning-of-line))

(defun tla-archive-previous ()
  (interactive)
  (forward-line -2)
  (beginning-of-line))

;; ----------------------------------------------------------------------------
;; tla-category-list-mode:
;; ----------------------------------------------------------------------------
(defun tla-category-list-mode ()
  "Major Mode to show arch categories:
\\{tla-category-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-category-list-mode-map)
  (easy-menu-add tla-category-list-mode-menu)
  (setq major-mode 'tla-category-list-mode)
  (setq mode-name "tla-category")
  (add-hook 'tla-make-category-hook 'tla-category-update)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'tla-get-revision-info-at-point-function)
       'tla--get-category-info-at-point)
  (run-hooks 'tla-category-list-mode-hook))

(defun tla--get-category-info-at-point ()
  "Get archive/category--branch information."
  (let ((buffer-version (tla-fully-qualified-revision
                         tla-buffer-archive-name
                         (tla-get-archive-info 'tla-category-info))))
    (list 'category buffer-version)))

(defun tla-category-list-branches ()
  (interactive)
  (let ((category (tla-get-archive-info 'tla-category-info)))
    (if category
        (tla-branches tla-buffer-archive-name category)
      (error "No category under the point"))))

(defun tla-category-make-category (category)
  (interactive "sCategory name: ")
  (tla-make-category tla-buffer-archive-name category))

(defun tla-category-update ()
  (interactive)
  (tla-categories tla-buffer-archive-name))

(defun tla-category-next ()
  (interactive)
  (forward-line 1)
  (beginning-of-line))

(defun tla-category-previous ()
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (unless (looking-at "^   ")
    (forward-line 1)))

(defun tla-category-mirror-archive ()
  (interactive)
  (let ((category (tla-get-archive-info 'tla-category-info)))
    (unless category
      (error "no category at point"))
    (tla-archive-mirror tla-buffer-archive-name
                        category)))

;; ----------------------------------------------------------------------------
;; tla-branch-list-mode
;; ----------------------------------------------------------------------------
(defun tla-branch-list-mode ()
  "Major Mode to show arch branches:
\\{tla-branch-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-branch-list-mode-map)
  (easy-menu-add tla-branch-list-mode-menu)
  (setq major-mode 'tla-branch-list-mode)
  (setq mode-name "tla-branch")
  (add-hook 'tla-make-branch-hook 'tla-branch-update)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'tla-get-revision-info-at-point-function)
       'tla--get-branch-info-at-point)
  (run-hooks 'tla-branch-list-mode-hook))

(defun tla--get-branch-info-at-point ()
  "Get archive/category--branch--version information."
  (let ((buffer-version (tla-fully-qualified-revision
                         tla-buffer-archive-name
                         tla-buffer-category-name
                         (tla-get-archive-info 'tla-branch-info))))
    (list 'branch buffer-version)))

(defun tla-branch-make-branch (branch)
  (interactive "sBranch name: ")
  (tla-make-branch tla-buffer-archive-name
                   tla-buffer-category-name
                   branch))

(defun tla-branch-update ()
  (interactive)
  (tla-branches
   tla-buffer-archive-name
   tla-buffer-category-name))

(defun tla-branch-list-parent-category ()
  (interactive)
  (tla-categories tla-buffer-archive-name))

(defun tla-branch-list-versions ()
  (interactive)
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (if branch
        (tla-versions tla-buffer-archive-name
                      tla-buffer-category-name
                      branch)
      (error "No branch under the point"))))

(defun tla-branch-mirror-archive ()
  (interactive)
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (unless branch
      (error "No branch under the point"))
    (tla-archive-mirror tla-buffer-archive-name
                        tla-buffer-category-name
                        branch)))

(defun tla-branch-get-branch (directory)
  (interactive (list (expand-file-name
                      (read-directory-name
                       (format "Restore \"%s\" to: "
                               (let ((branch
                                      (tla-get-archive-info 'tla-branch-info)))
                                 (unless branch
                                   (error "No branch under the point"))
                                 (tla-fully-qualified-revision
                                  tla-buffer-archive-name
                                  tla-buffer-category-name
                                  branch)))))))
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (if branch
        (tla-get directory
                 t
                 tla-buffer-archive-name
                 tla-buffer-category-name
                 branch)
      (error "No branch under the point"))))

(defun tla-branch-bookmarks-add (name)
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     tla-buffer-archive-name
                     tla-buffer-category-name)
  (message "bookmark %s added." name))




;; ----------------------------------------------------------------------------
;; tla-version-list-mode
;; ----------------------------------------------------------------------------
(defun tla-version-list-mode ()
  "Major Mode to show arch versiones:
\\{tla-version-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-version-list-mode-map)
  (easy-menu-add tla-version-list-mode-menu)
  (setq major-mode 'tla-version-list-mode)
  (setq mode-name "tla-version")
  (add-hook 'tla-make-version-hook 'tla-version-update)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'tla-get-revision-info-at-point-function)
       'tla--get-version-info-at-point)
  (run-hooks 'tla-version-list-mode-hook))

(defun tla--get-version-info-at-point ()
  "Get archive/category--branch--version--revision information."
  (let ((buffer-version (tla-fully-qualified-revision
                         tla-buffer-archive-name
                         tla-buffer-category-name
                         tla-buffer-branch-name
                         (tla-get-archive-info 'tla-version-info))))
    (list 'version buffer-version)))

(defun tla-version-update ()
  (interactive)
  (tla-versions
   tla-buffer-archive-name
   tla-buffer-category-name
   tla-buffer-branch-name))

(defun tla-version-list-parent-branch ()
  (interactive)
  (tla-branches tla-buffer-archive-name
                tla-buffer-category-name))

(defun tla-version-list-revisions ()
  (interactive)
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-revisions tla-buffer-archive-name
                       tla-buffer-category-name
                       tla-buffer-branch-name
                       version)
      (error "No version under the point"))))

(defun tla-version-make-version (version)
  (interactive "sVersion name: ")
  (tla-make-version tla-buffer-archive-name
                    tla-buffer-category-name
                    tla-buffer-branch-name
                    version))

(defun tla-version-bookmarks-add (name)
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     tla-buffer-archive-name
                     tla-buffer-category-name
                     tla-buffer-branch-name)
  (message "bookmark %s added." name))

(defun tla-version-get-version (directory)
  (interactive (list (expand-file-name
                      (read-directory-name
                       (format "Restore \"%s\" to: "
                               (let ((version
                                      (tla-get-archive-info 'tla-version-info)))
                                 (unless version
                                   (error "No version under the point"))
                                 (tla-fully-qualified-revision
                                  tla-buffer-archive-name
                                  tla-buffer-category-name
                                  tla-buffer-branch-name
                                  version)))))))
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-get directory
                 t
                 tla-buffer-archive-name
                 tla-buffer-category-name
                 tla-buffer-branch-name
                 version)
      (error "No version under the point"))))

(defun tla-version-mirror-archive ()
  (interactive)
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-archive-mirror tla-buffer-archive-name
                            tla-buffer-category-name
                            tla-buffer-branch-name
                            version))))

;; ----------------------------------------------------------------------------
;; tla-revision-list-mode
;; ----------------------------------------------------------------------------
(defun tla-revision-list-mode ()
  "Major Mode to show arch revisions:
\\{tla-revision-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-revision-list-mode-map)
  (easy-menu-add tla-revision-list-mode-menu)
  (setq major-mode 'tla-revision-list-mode)
  (setq mode-name "tla-revision")
  (add-hook 'tla-make-revision-hook 'tla-revision-refresh)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'tla-get-revision-info-at-point-function)
       'tla--get-revision-info-at-point)
  (setq mode-line-process 'tla-mode-line-process)
  (run-hooks 'tla-revision-list-mode-hook))

(defun tla--get-revision-info-at-point ()
  "Get archive/category--branch--version--revision--patch information."
  (let* ((elem (ewoc-data (ewoc-locate tla-revision-list-cookie)))
         (full (caddr elem))
         (buffer-version
          (tla-fully-qualified-revision (tla-archive-name  full)
                                        (tla-category-name full)
                                        (tla-branch-name   full)
                                        (tla-version-name  full)
                                        (tla-revision-name full))))
    (list 'revision buffer-version)))

(defun tla-revision-refresh ()
  (interactive)
  (tla-revisions
   tla-buffer-archive-name
   tla-buffer-category-name
   tla-buffer-branch-name
   tla-buffer-version-name))

(defun tla-revision-list-parent-version ()
  (interactive)
  (tla-versions tla-buffer-archive-name
                tla-buffer-category-name
                tla-buffer-branch-name))

(defun tla-revision-get-revision (directory archive category branch
                                            version revision)
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate tla-revision-list-cookie)))
          (full (caddr elem))
          (revision (tla-revision-name full))
          (archive (tla-archive-name full))
          (category (tla-category-name full))
          (branch (tla-branch-name full))
          (version (tla-version-name full))
	  dir)
     (unless revision
       (error "No revision under the point"))
     (setq dir (expand-file-name
                (read-directory-name
                 (format "Restore \"%s\" to: "
			 (tla-fully-qualified-revision
                          archive category branch version revision)))))
     (if (file-exists-p dir)
	 (error "Directory %s already exists." dir))
     (list dir archive category branch version revision)))
  (if revision
      (tla-get directory t archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-cache-revision (archive category branch version revision)
  "Create a cached revision for the revision at point"
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate tla-revision-list-cookie)))
          (full (caddr elem))
          (revision (tla-revision-name full))
          (archive (tla-archive-name full))
          (category (tla-category-name full))
          (branch (tla-branch-name full))
          (version (tla-version-name full)))
     (unless revision
       (error "No revision under the point"))
     (list archive category branch version revision)))
  (if revision
      (tla-cache-revision archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-add-to-library (archive category branch version revision)
  "Add the revision at point to library"
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate tla-revision-list-cookie)))
          (full (caddr elem))
          (revision (tla-revision-name full))
          (archive (tla-archive-name full))
          (category (tla-category-name full))
          (branch (tla-branch-name full))
          (version (tla-version-name full)))
     (unless revision
       (error "No revision under the point"))
     (list archive category branch version revision)))
  (if revision
      (tla-library-add archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-toggle-date ()
  (interactive)
  (setq tla-revisions-shows-date (not tla-revisions-shows-date))
  (ewoc-refresh tla-revision-list-cookie))

(defun tla-revision-toggle-summary ()
  (interactive)
  (setq tla-revisions-shows-summary (not tla-revisions-shows-summary))
  (ewoc-refresh tla-revision-list-cookie))

(defun tla-revision-toggle-creator ()
  (interactive)
  (setq tla-revisions-shows-creator (not tla-revisions-shows-creator))
  (ewoc-refresh tla-revision-list-cookie))

(defun tla-revision-toggle-library ()
  (interactive)
  (setq tla-revisions-shows-library (not tla-revisions-shows-library))
  (ewoc-refresh tla-revision-list-cookie))

(defun tla-revision-next ()
  (interactive)
  (end-of-line)
  (re-search-forward (concat "^   " tla-revision-regexp))
  (beginning-of-line)
  (forward-char 1))

(defun tla-revision-previous ()
  (interactive)
  (beginning-of-line)
  (re-search-backward (concat "^   " tla-revision-regexp))
  (beginning-of-line)
  (forward-char 1))

(defun tla-revision-changeset (arg)
  "Gets and display the changeset at point in a revision list buffer.
If used with a prefix arg, don't include the diffs from the output."
  (interactive "P")
  (let* ((cookie tla-revision-list-cookie)
         (full (caddr (ewoc-data (ewoc-locate cookie))))
         (revision (apply 'tla-fully-qualified-revision full)))
    (tla-get-changeset revision t nil arg)))

(defvar tla-buffer-marked-revision nil)

(defun tla-revision-delta ()
  "Runs tla delta from marked revision to revision at point"
  (interactive)
  (unless tla-buffer-marked-revision
    (error "please mark a revision first"))
  (let* ((elem (ewoc-data (ewoc-locate tla-revision-list-cookie)))
         (full (caddr elem))
         (version (tla-tree-version))
         (buffer-version
          (tla-fully-qualified-revision (tla-archive-name  full)
                                        (tla-category-name full)
                                        (tla-branch-name   full)
                                        (tla-version-name  full))))
    (if (and version
             (string= version buffer-version))
        (tla-delta (tla-revision-name tla-buffer-marked-revision)
                   (tla-revision-name full))
      (tla-delta (concat buffer-version "--" (tla-revision-name tla-buffer-marked-revision))
                 (concat buffer-version "--" (tla-revision-name full)))))
  )

(defun tla-revision-bookmarks-add (name)
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     tla-buffer-archive-name
                     tla-buffer-category-name
                     tla-buffer-branch-name
                     tla-buffer-version-name)
  (message "bookmark %s added." name))

(defun tla-revision-sync-tree (arg)
  "Unify a tree's patch log with the current revision.  With an argument, use
the latest version instead."
  (interactive "P")
  (let ((local-tree default-directory) ;; Default value
        (current (ewoc-locate tla-revision-list-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (caddr (ewoc-data current))
                              'bookmark))))
      (setq current (ewoc-prev tla-revision-list-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (caddr (ewoc-data current)) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((to-tree (read-directory-name "Sync with tree: " local-tree)))
      (let* ((elem (ewoc-data (ewoc-locate
                               tla-revision-list-cookie)))
             (full (caddr elem)))
        (tla-sync-tree (apply 'tla-fully-qualified-revision
                              (if arg (butlast full) full))
                       to-tree)))))

(defun tla-revision-star-merge (arg)
  "Run star-merge from the current location.  With an argument, merge all
missing revisions from this version."
  (interactive "P")
  (let ((local-tree default-directory) ;; Default value
        (current (ewoc-locate tla-revision-list-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (caddr (ewoc-data current))
                              'bookmark))))
      (setq current (ewoc-prev tla-revision-list-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (caddr (ewoc-data current)) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((to-tree (read-directory-name "Merge to tree: " local-tree)))
      (let* ((elem (ewoc-data (ewoc-locate
                               tla-revision-list-cookie)))
             (full (caddr elem)))
        (tla-star-merge (apply 'tla-fully-qualified-revision
                               (if arg (butlast full) full))
                        to-tree)))))

(defun tla-revision-replay (arg)
  "Run replay from the current location.  With an argument, replay all missing
revisions from this version."
  (interactive "P")
  (let ((local-tree default-directory) ;; Default value
        (current (ewoc-locate tla-revision-list-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (caddr (ewoc-data current))
                              'bookmark))))
      (setq current (ewoc-prev tla-revision-list-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (caddr (ewoc-data current)) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((to-tree (read-directory-name "Replay to tree: " local-tree)))
      (let* ((elem (ewoc-data (ewoc-locate
                               tla-revision-list-cookie)))
             (full (caddr elem)))
        (tla-replay (apply 'tla-fully-qualified-revision
                           (if arg (butlast full) full))
                    to-tree)))))

(defun tla-revision-mark-revision ()
  "Mark revision at point for a further `tla-delta'"
  (interactive)
  (let ((pos (point))
        (data (ewoc-data (ewoc-locate
                          tla-revision-list-cookie))))
    (ewoc-map #'(lambda (x) (when (eq (car x) 'entry-patch)
                              (setcar (cdr x) nil)))
              tla-revision-list-cookie)
    (setcar (cdr data) t)
    (ewoc-refresh tla-revision-list-cookie)
    (setq tla-buffer-marked-revision (caddr data))
    (goto-char pos))
  )

(defun tla-revision-tag (tag-archive tag-category tag-branch tag-version)
  "Run tla tag from the current location"
  (interactive (tla-read-archive-category-branch-version-name "Tag to: "))
  (let ((tag-name (tla-fully-qualified-revision tag-archive
                                                tag-category
                                                tag-branch
                                                tag-version)))
    (if tla-buffer-archive-name
        (tla-tag (tla-fully-qualified-revision
                  tla-buffer-archive-name
                  tla-buffer-category-name
                  tla-buffer-branch-name
                  tla-buffer-version-name) tag-name)
      (let* ((elem (ewoc-data (ewoc-locate
                               tla-revision-list-cookie)))
             (full (caddr elem)))
        (tla-tag (apply 'tla-fully-qualified-revision
                        full)
                 tag-name)))
    (when (y-or-n-p "Tag created. Get a copy of this revision? ")
      (tla-get (read-directory-name "Get a copy in: ")
               'ask
               tag-archive tag-category tag-branch tag-version))))


(defun tla-revision-return ()
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate
                          tla-revision-list-cookie))))
    (case (car elem)
      (entry-patch (tla-revision-cat-log))
      (entry-change (progn (cd (caddr elem)) (tla-changes)))))
  )

(defun tla-revision-cat-log ()
  (interactive)
  (let* ((elem (ewoc-data (ewoc-locate
                           tla-revision-list-cookie)))
         (full (caddr elem))
         (revision (tla-revision-name full))
         (version (tla-tree-version))
         (buffer-version
          (tla-fully-qualified-revision (tla-archive-name  full)
                                        (tla-category-name full)
                                        (tla-branch-name   full)
                                        (tla-version-name  full))))
    (if (and (not (string= version ""))
             (string= version buffer-version))
        (tla-cat-log revision)
      (tla-cat-archive-log (concat buffer-version "--" revision)))))

(defun tla-revision-update ()
  "Runs tla update"
  (interactive)
  (let ((local-tree default-directory) ;; Default value
        (current (ewoc-locate tla-revision-list-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (caddr (ewoc-data current))
                              'bookmark))))
      (setq current (ewoc-prev tla-revision-list-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (caddr (ewoc-data current)) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((buffer (current-buffer)))
      (tla-update (read-directory-name "Update tree: " local-tree))
      (pop-to-buffer buffer))
    (tla-generic-refresh)))

;; ----------------------------------------------------------------------------
;; tla-changes-mode
;; ----------------------------------------------------------------------------
(define-derived-mode tla-changes-mode fundamental-mode "tla-changes"
  "Major mode to display changesets. Derives from `diff-mode'.

Use '\\<tla-changes-mode-map>\\[tla-changes-mark-file]' to mark files, and '\\[tla-changes-unmark-file]' to unmark.
If you commit from this buffer (with '\\[tla-changes-edit-log]'), then, the list of selected
files in this buffer at the time you actually commit with
\\<tla-log-edit-mode-map>\\[tla-log-edit-done].

Commands:
\\{tla-changes-mode-map}
"
  (let ((diff-mode-shared-map (copy-keymap tla-changes-mode-map))
        major-mode mode-name)
    (diff-mode))
  ;;
  (let ((keywords (append tla-changes-font-lock-keywords
			  diff-font-lock-keywords)))
    (set (make-local-variable 'font-lock-defaults) `(,keywords t)))
  (set (make-local-variable 'tla-get-file-info-at-point-function)
       'tla-changes-get-file-at-point)
  (set (make-local-variable 'tla-buffer-refresh-function)
       'tla-changes)
  (set (make-local-variable 'tla-changes-cookie)
       (ewoc-create 'tla-changes-printer))
  (make-local-variable 'tla-buffer-marked-file-list)
  (easy-menu-add tla-changes-mode-menu)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(defun tla-changes-return (&optional other-file)
  "Switch to the corresponding file and location of the change."
  (interactive "P")
  (let ((file (tla--get-file-info-at-point)))
    (if file
        (find-file file)
      (diff-goto-source other-file))))

(defun tla-changes-view-source (&optional other-file)
  "Show the corresponding file and location of the change.
This function does not switch to the file, but it places the cursor
temporarily at the location of the change and will stay in the changes
buffer.  Thus you can quickly see more context on a specific change without
switching buffers."
  (interactive "P")
  (let ((diff-window (selected-window)))
    (save-excursion
      (diff-goto-source other-file)
      (if (functionp 'highline-on) (highline-on))
      (sit-for 1000)
      (if (functionp 'highline-off) (highline-off))
      (select-window diff-window))))

(defun tla-changes-edit-log (&optional insert-changelog)
  "Wrapper around `tla-edit-log', setting the source buffer to current
buffer."
  (interactive "P")
  (tla-edit-log insert-changelog (current-buffer)))

(defun tla-changes-rm ()
  (interactive)
  (let ((file (tla--get-file-info-at-point)))
    (unless file
      (error "No file at point"))
    (tla-rm file)))

(defun tla-changes-mark-file ()
  (interactive)
  (let ((current (ewoc-locate tla-changes-cookie))
        (file (tla--get-file-info-at-point)))
    (add-to-list 'tla-buffer-marked-file-list file)
    (ewoc-refresh tla-changes-cookie)
    (goto-char (ewoc-location (or (ewoc-next tla-changes-cookie
                                             current)
                                  current)))))

(defun tla-changes-unmark-file ()
  (interactive)
  (let ((current (ewoc-locate tla-changes-cookie))
        (file (tla--get-file-info-at-point)))
    (setq tla-buffer-marked-file-list
          (delete file tla-buffer-marked-file-list))
    (ewoc-refresh tla-changes-cookie)
    (goto-char (ewoc-location (or (ewoc-next tla-changes-cookie
                                             current)
                                  current)))))

(defun tla-changes-diff ()
  "Runs tla file-diff on the file at point in *tla-changes*"
  (interactive)
  (let ((on-modified-file (tla--get-file-info-at-point)))
    (if on-modified-file
        (tla-file-diff on-modified-file)
      (error "not on a modified file"))))

(defun tla-changes-next ()
  (interactive)
  (let ((cur-location (ewoc-location (ewoc-locate tla-changes-cookie)))
        (next (ewoc-next tla-changes-cookie
                         (ewoc-locate tla-changes-cookie))))
    (cond
     ((> cur-location (point))
      (goto-char cur-location))
     (next
      (goto-char (ewoc-location next)))
     (t
      (diff-hunk-next)))))

(defun tla-changes-prev ()
  (interactive)
  (let* ((current (ewoc-locate tla-changes-cookie))
         (cur-location (ewoc-location current))
         (prev (ewoc-prev tla-changes-cookie current))
         (next (ewoc-next tla-changes-cookie current)))
    (cond (next
           (if prev (goto-char (ewoc-location prev))
             (goto-char cur-location)))
          ((condition-case nil (progn (diff-hunk-prev) t) (error nil)))
          ((> (line-beginning-position) cur-location)
           (goto-char cur-location))
          (prev
           (goto-char (ewoc-location prev)))
          (t
           (goto-char cur-location)))
    ))

(defun tla-changes-ediff (&optional other-file)
  (interactive "P")
  (let ((on-modified-file (tla--get-file-info-at-point))
        (loc (point)))
    (if on-modified-file
        (tla-file-ediff on-modified-file)
      (re-search-backward "^--- orig/")
      (re-search-forward "^--- orig/")
      (let ((file (buffer-substring-no-properties (point)
                                                  (line-end-position)))
            (hunk 1))
        (diff-hunk-next)
        (while (<= (re-search-forward "\\(^[\\+-].*\n\\)+" nil t) loc)
          (setq hunk (1+ hunk)))
        (goto-char loc)
        (with-current-buffer (tla-file-ediff file)
          (ediff-jump-to-difference hunk))))))

(defun tla-changes-get-file-at-point ()
  "Find file at point in *tla-changes*. Error when not on a file."
  (let ((elem (ewoc-locate tla-changes-cookie (point))))
    (or (when (and elem
                   (>= (ewoc-location elem) (line-beginning-position)))
          (car (ewoc-data elem)))
        (let ((loc (diff-find-source-location)))
          (buffer-file-name (car loc))))))

(defun tla-changes-revert ()
  "Reverts file at point"
  (interactive)
  (let* ((file (tla--get-file-info-at-point))
         (absolute (if (file-name-absolute-p file)
                       file
                     (expand-file-name
                      (concat default-directory "/" file)))))
    (tla-file-revert absolute)))

;; ----------------------------------------------------------------------------
;; tla-changelog-mode
;; ----------------------------------------------------------------------------
(define-derived-mode tla-changelog-mode change-log-mode "tla-changelog"
  (let ((keywords (append tla-changelog-font-lock-keywords
                          change-log-font-lock-keywords)))
    (set (make-local-variable 'font-lock-defaults) `(,keywords t)))
  (use-local-map tla-changelog-mode-map)

  (toggle-read-only 1)
  (set-buffer-modified-p nil))

;; ----------------------------------------------------------------------------
;; tla-inventory-file-mode
;; ----------------------------------------------------------------------------
;;;###autoload
(defun tla-inventory-file-mode ()
  "Major Mode to edit tla inventory files (=tagging-method, .arch-inventory)."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-inventory-file-font-lock-keywords t))
  (setq major-mode 'tla-inventory-file-mode
        mode-name "tla-inventory-file"
        comment-start "# ")
  (run-hooks 'tla-inventory-file-mode-hook))

;; ----------------------------------------------------------------------------
;; Arch tree manipulators
;; ----------------------------------------------------------------------------
;; Arch archive/category/branch/version/revision are stored in assoc list:
;; -------------------------
;; ("archive1" "localtion1"
;;  (("category1"
;;    (("branch1"
;;      (("version1"
;;        (("revision1" "summary" "creator" "date") ("revision2" "summary" "creator" "date") ...))
;;       ("version2"
;;        (("revision3" "summary" "creator" "date") ("revision4" "summary" "creator" "date") ...))
;;       ...))
;;     ("branch2"
;;      (("version1"
;;        (("revision1" "summary" "creator" "date") ("revision2" "summary" "creator" "date") ...))
;;       ("version2"
;;        (("revision3" "summary" "creator" "date") ("revision4" "summary" "creator" "date") ...))
;;       ...)
;;      ...)
;;     ...)
;;    ...)
;;   ...)
;;  ...)

;; Utilities
(defun tla-archive-tree-setcdr (parent value &optional rest)
  (let* ((current (cdr parent))
         (list-details (assoc value current)))
    (if (or (null current) (null list-details))
        ;; rest is '("summary" "creator" "date") when value is "patch-N"
        (setcdr parent (cons (cons value rest) current))
      (if (and list-details rest)
          ;; Field already there. update details.
          (setcdr list-details rest)))))

(defun tla-archive-tree-setcddr (parent value)
  (let ((current (cddr parent)))
    (if (or (null current) (null (assoc value current)))
        (setcdr (cdr parent) (cons (cons value nil) current)))))

;; Archive
(defun tla-archive-tree-add-archive (archive location)
  (if (tla-archive-tree-get-archive archive)
      (let* ((a (tla-archive-tree-get-archive archive))
             (val (cdr a))
             (oldlocation (car val))
             (category (cdr val)))
        (setcdr a (cons (or location oldlocation) category)))
    (setq tla-archive-tree (cons (list archive location)
                                 tla-archive-tree))))

(defun tla-archive-tree-get-archive (archive)
  (assoc archive tla-archive-tree))

;; Category
(defun tla-archive-tree-add-category (archive category)
  (tla-archive-tree-add-archive archive nil)
  (tla-archive-tree-setcddr
   (tla-archive-tree-get-archive archive)
   category))

(defun tla-archive-tree-get-category (archive category)
  (assoc category (cdr (cdr (tla-archive-tree-get-archive archive)))))

;; Branch
(defun tla-archive-tree-add-branch (archive category branch)
  (tla-archive-tree-add-category archive category)
  (tla-archive-tree-setcdr
   (tla-archive-tree-get-category archive category)
   branch))

(defun tla-archive-tree-get-branch (archive category branch)
  (assoc branch (cdr (tla-archive-tree-get-category
                      archive category))))

;; Version
(defun tla-archive-tree-add-version (archive category branch version)
  (tla-archive-tree-add-branch archive category branch)
  (tla-archive-tree-setcdr
   (tla-archive-tree-get-branch archive category branch )
   version))

(defun tla-archive-tree-get-version (archive category branch version)
  (assoc version (cdr (tla-archive-tree-get-branch
                       archive category branch))))

;; Revision
(defun tla-archive-tree-add-revision (archive category branch version revision
                                              &optional summary creator date)
  (tla-archive-tree-add-version archive category branch version)
  (tla-archive-tree-setcdr
   (tla-archive-tree-get-version archive category branch version)
   revision (list summary creator date)))

(defun tla-archive-tree-get-revision (archive category branch version revision)
  (assoc revision (cdr (tla-archive-tree-get-version
                        archive category branch version))))



;; ----------------------------------------------------------------------------
;; Arch name manipulators
;; ----------------------------------------------------------------------------

;; * tla-name-sans-separator
;; (tla-name-sans-separator "name--category--")
;; => "name-category"
;; (tla-name-sans-separator "name--category")
;; => "name-category"
(defun tla-name-sans-separator (name)
  (replace-regexp-in-string "--$" "" name))

;; * tla-name-split-components
;; ELISP> (tla-name-split-components "name--category--branch" 4)
;; ("name" "category" "branch" nil)
;;
;; ELISP> (tla-name-split-components "name--category--branch" 5)
;; ("name" "category" "branch" nil nil)
;;
;; ELISP> (tla-name-split-components "name--category--branch" 3)
;; ("name" "category" "branch")
;;
;; ELISP> (tla-name-split-components "name--category--branch--revision")
;; ("name" "category" "branch" "revision")
;;
;; ELISP> (tla-name-split-components "name--category--branch--")
;; ("name" "category" "branch" "")
(defun tla-name-split-components (name &optional fill-with-nil)
  "Split \"--\" connected string into list.
You can give a number to FILL-WITH-NIL.
If FILL-WITH-NIL is given, empty string element of the result
list is replaced with nil; and nil is added to the result list
till the list length is equals to FILL-WITH-NIL."
  (let ((list (tla-name-split-components-internal name)))
    (when fill-with-nil
      (setq list (mapcar (lambda (elt)
                           (unless (string= "" elt) elt))
                         list))
      (while (> fill-with-nil (length list))
        (setq list (cons nil list))))
    (nreverse list)))

(defun tla-name-split-components-internal (name)
  (if (string-match "^\\(.+\\)--\\(.*\\)" name)
      (cons (match-string 2 name)
            (tla-name-split-components-internal
             (match-string 1 name)))
    (cons name nil)))

;; * tla-name-construct
;; (tla-name-construct "name" "category" "branch")
;; => "name--category--branch"
;; tla-fully-qualified-revision does the same, but better.
(defun tla-name-construct (&rest comp)
  (mapconcat 'identity (remove nil comp) "--"))



;; ----------------------------------------------------------------------------
;; Find file hook
;; ----------------------------------------------------------------------------
;; just 99% cut&paste from vc-follow-link in vc-hook.el, but this way there is
;; no need to load it thus avoiding interfering with VC ...
(defun tla-follow-link ()
  ;; If the current buffer visits a symbolic link, this function makes it
  ;; visit the real file instead.  If the real file is already visited in
  ;; another buffer, make that buffer current, and kill the buffer
  ;; that visits the link.
  (let* ((truename (abbreviate-file-name (file-truename buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
	 (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
	(progn
	  (kill-buffer this-buffer)
	  ;; In principle, we could do something like set-visited-file-name.
	  ;; However, it can't be exactly the same as set-visited-file-name.
	  ;; I'm not going to work out the details right now. -- rms.
	  (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

;;;###autoload
(defun tla-find-file-hook ()
  (let (link file result)
    (when (and (if (boundp 'vc-ignore-vc-files)
                   (not vc-ignore-vc-files)
                 t)
               tla-follow-symlinks
               buffer-file-name
               (setq link (file-symlink-p buffer-file-name)))
      (setq file (file-truename link)
            result (cond ((equal tla-follow-symlinks 'tree)
                          (tla-tree-root file t))
                         ((equal tla-follow-symlinks 'id)
                          (= 0 (tla--run-tla-sync
                                (list "id" file)
                                :finished 'tla--status-handler
                                :error 'tla--status-handler)))))

      (if result
          (cond ((eq tla-follow-symlinks-mode 'warn)
                 (message
                  "Warning: symbolic link to arch-controlled source file: %s"
                  file))
                ((or (eq tla-follow-symlinks-mode 'follow)
                     (find-buffer-visiting file))
                 (tla-follow-link)
                 (message "Followed link to %s" buffer-file-name))
                ((eq tla-follow-symlinks-mode 'ask)
                 (if (yes-or-no-p "Symbolic link to arch-controlled source file; follow link? ")
                     (progn
                       (tla-follow-link)
                       (message "Followed link to %s" buffer-file-name))
                   (message
                    "Warning: editing through the link bypasses version control")))
                (t (error "unknown mode for tla-follow-symlinks-mode=%s"
                          tla-follow-symlinks-mode)))
        ))))

;; ----------------------------------------------------------------------------
;; Misc functions
;; ----------------------------------------------------------------------------
;;;###autoload
(defun tla-insert-arch-tag ()
  "Insert a unique arch-tag in the current file."
  (interactive)
  (let ((uuid (shell-command-to-string "uuidgen"))
        (in-comment-p (nth 4 (parse-partial-sexp (point) (point-min))))
        (header "")
        (footer ""))
    (unless in-comment-p
      (setq header (if comment-start
                       (concat comment-start
                               (if (string-match " $" comment-start)
                                   "" " "))
                     "")
            footer (if (and comment-end (not (string= "" comment-end)))
                       (format "%s(do not change this comment)%s%s"
                               (make-string (length header) ?\ )
                               comment-end
                               (if (string-match "^ " comment-end)
                                   "" " "))
                     "")))
    (insert (concat header "arch-tag: " uuid footer))))

;;;###autoload
(defun tla-ediff-add-log-entry ()
  (interactive)
  (pop-to-buffer ediff-buffer-A)
  (tla-add-log-entry))

(defvar tla-arch-version nil
  "Version of tla version.")

(defun tla-arch-version ()
  "Returns the TLA (arch) version."
  (interactive)
  (setq tla-arch-version
        (tla--run-tla-sync '("-V")
                           :finished
                           (lambda (output error status)
                             (tla--buffer-content output)))))

(defun tla-version ()
  "Returns the XTLA version."
  (interactive)
  (let ((version
         (or (when (locate-library "xtla-version")
               (load-library "xtla-version")
               (when (boundp 'tla-version)
                 tla-version))
             (let ((default-directory
                     (file-name-directory (locate-library "xtla"))))
               (defvar tla-version nil "Version of xtla")
               (tla--run-tla-sync '("logs" "-f" "-r")
                                  :finished
                                  (lambda (output error status)
                                    (set-buffer output)
                                    (goto-char (point-min))
                                    (setq tla-version
                                          (buffer-substring-no-properties
                                           (point)
                                           (line-end-position))))
                                  :error
                                  (lambda (output error status)
                                    (setq tla-version "unknown")))))))
    (if (not version)
        (progn
          (message "We did not find xtla-version.el nor the arch-tree containing xtla.el!")
          (sit-for 2)
          (message "Are you using a developer version of XTLA?")
          (sit-for 2))
      (if (interactive-p)
          (message tla-version))
      tla-version)))

;;;###autoload
(defun tla-submit-bug-report ()
  "Submit a bug report, with pertinent information to the xtla-el-dev list."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  (tla-version)
  (tla-arch-version)
  (reporter-submit-bug-report
   "xtla-el-dev@gna.org"
   (concat "XTLA " tla-version)
   (append
    ;; non user variables
    '(emacs-version
      tla-version
      tla-arch-version
      )
    ;; user variables
    (sort (apropos-internal "^tla-" 'user-variable-p)
          (lambda (v1 v2) (string-lessp (format "%s" v1) (format "%s" v2))))
    ;; see what the user had loaded
    (list 'features)
    )
   nil
   nil
   "Please change the Subject header to a concise bug description or feature request.\nIn this report, remember to cover the basics, that is, what you \nexpected to happen and what in fact did happen.\nPlease remove these instructions from your message.")

  ;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\n\n")
      (insert-buffer-substring backtrace)))

  (goto-char (point-min))
  (mail-position-on-field "Subject")
  (insert "[BUG/FEATURE] "))



(defun tla-arch-version ()
  "Returns the TLA (arch) version."
  (interactive)
  (setq tla-arch-version
        (tla--run-tla-sync '("-V")
                           :finished
                           (lambda (output error status)
                             (tla--buffer-content output)))))

(defun tla-version ()
  "Returns the XTLA version."
  (interactive)
  (if (boundp 'tla-version)
      tla-version
    (if (locate-library "xtla-version")
        (load-library "xtla-version")
      (if (locate-library "xtla")
          (save-excursion
            (let ((default-directory
                    (file-name-directory (locate-library "xtla"))))
              (tla--run-tla-sync '("revisions" "-f" "-r")
                                 :finished
                                 (lambda (output error status)
                                   (set-buffer output)
                                   (goto-char (point-min))
                                   (re-search-forward "\n")
                                   (delete-region (point) (point-max))
                                   (setq tla-version (tla--buffer-content output)))
                                 :error
                                 (lambda (output error status)
                                   (setq tla-version "unknown")))))
        (setq tla-version "unknown"))))
  
  (when (string= xtla-version "unknown")
    (message "We did not find xtla-version.el nor the arch-tree containing xtla.el!")
    (sit-for 2)
    (message "Are you using a developer version of XTLA?")
    (sit-for 2))
  
  (if (interactive-p)
      (message tla-version))
  tla-version)

;;;###autoload
(defun tla-submit-bug-report ()
  "Submit a bug report, with pertinent information to the xtla-el-dev list."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  (tla-version)
  (tla-arch-version)
  (reporter-submit-bug-report
   "xtla-el-dev@gna.org"
   (concat "XTLA " tla-version)
   (append
    ;; non user variables
    '(emacs-version
      tla-version
      tla-arch-version
      )
    ;; user variables
    (sort (apropos-internal "^tla-" 'user-variable-p)
          (lambda (v1 v2) (string-lessp (format "%s" v1) (format "%s" v2))))
    ;; see what the user had loaded
    (list 'features)
    )
   nil
   nil
   "Please change the Subject header to a concise bug description.\nIn this report, remember to cover the basics, that is, what you \nexpected to happen and what in fact did happen.\nPlease remove these instructions from your message.")

  ;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\n\n")
      (insert-buffer-substring backtrace)))

  (goto-char (point-min))
  (mail-position-on-field "Subject")
  (insert "[BUG] "))



(provide 'xtla)

;; Local Variables:
;; arch-tag: f2eee8c5-0f20-4fc7-b1c1-6cef4dff8a5a
;; End:
