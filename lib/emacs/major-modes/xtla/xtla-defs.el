;;; xtla-defs.el --- UI Xtla's element definitions

;; Copyright (C) 2003-2005 by Stefan Reichoer

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; This file is part of Xtla.
;;
;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; In order to keep UI consistency, especially about key binding,
;; we gather all UI definition in this separated file.
;;


;;; History:
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (autoload 'ad-add-advice "advice")
  (require 'ediff)
  (require 'diff-mode)
  (require 'font-lock)
  (require 'add-log)
  (require 'ffap))


;;;###autoload
(eval-and-compile
  (require 'easymenu))

;; Macros to generate correct code for different emacs variants
;; This doesn't really belong here, but then again it doesn't "fit"
;; anywhere else.
;; (progn ...) is here to have autoload generation actually insert the
;; code in the autoload file.
;;;###autoload
(progn
  (defmacro tla--do-in-gnu-emacs (&rest body)
    "Execute BODY if in GNU/Emacs."
    (unless (featurep 'xemacs) `(progn ,@body))))
(put 'tla--do-in-gnu-emacs 'lisp-indent-hook 'defun)

;;;###autoload
(progn
  (defmacro tla--do-in-xemacs (&rest body)
    "Execute BODY if in XEmacs."
    (when (featurep 'xemacs) `(progn ,@body))))
(put 'tla--do-in-xemacs 'lisp-indent-hook 'defun)

(defmacro tla--funcall-if-exists (function &rest args)
  "Call FUNCTION with ARGS as parameters if it exists."
  (if (fboundp function)
      `(funcall ',function ,@args)))

(unless (functionp 'clone-process)
  (defun clone-process (process &optional newname)
    "Create a twin copy of PROCESS.
If NEWNAME is nil, it defaults to PROCESS' name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.
If PROCESS is associated with a buffer, the new process will be associated
  with the current buffer instead.
Returns nil if PROCESS has already terminated."
    (setq newname (or newname (process-name process)))
    (if (string-match "<[0-9]+>\\'" newname)
        (setq newname (substring newname 0 (match-beginning 0))))
    (when (memq (process-status process) '(run stop open))
      (let* ((process-connection-type (process-tty-name process))
             (old-kwoq (process-kill-without-query process nil))
             (new-process
              (if (memq (process-status process) '(open))
                  (apply 'open-network-stream newname
                         (if (process-buffer process) (current-buffer)))
                (apply 'start-process newname
                       (if (process-buffer process) (current-buffer))
                       (process-command process)))))
        (process-kill-without-query new-process old-kwoq)
        (process-kill-without-query process old-kwoq)
        (set-process-filter new-process (process-filter process))
        (set-process-sentinel new-process (process-sentinel process))
        new-process))))

;; ----------------------------------------------------------------------------
;; Key bindings
;; ----------------------------------------------------------------------------
;;
;; Conventions
;;
;; - Meta Rules:
;; 0. If you feel a binding odd more than 3 times, report it to xtla dev mailing
;;    list. Especially about some danger functions like undo, merge; and functions
;;    taking longer time to be executed.
;;
;; 1. Our key binding should not surprise "general users" even if the
;;    binding is convenience. Instead, provide hooks for customization.
;;    We know it is difficult to define "general users".
;;
;; 2. Write the result of discussion here.
;;
;; 3. See http://mail.gnu.org/archive/html/emacs-devel/2004-03/msg00608.html
;;
;;
;; - Generic Rules:
;;
;; 1. xtla-inventory should have similar key bindings to pcl-cvs.
;;    If a pcl-cvs's binding is too odd, talk it in xtla dev mailing list.
;;
;; 2. Define common prefix for command groups like '>'.
;;    So a key binding for a grouped command has following structure:
;;
;;    ?{prefix} ?{suffix}
;;
;;    e.g. `get something commands' should have `>' as prefix.
;;
;;    About suffix part, ? should show the help for the groups.
;;
;;    e.g. `help for `get something commands'' is >?.
;;
;;    BTW, The prefix ? is for help related command.
;;    So `? >' can stand for "show the help for get-something related
;;    command". In other word, prefix and suffix is swappable if
;;    prefix or suffix is `?'.
;;
;; 3. Upper case for commands taking longer time to be executed.
;; 4. Lower case for commands taking shorter time to be executed.
;; 5. dired's binding is also helpful.
;;
;;
;; - Concrete Rules:
;;

;; t  ?    list all toggles
;; c       tla-edit-log
;; RET     Open the thing at point
;;

;;
;; Definitions for key concrete rules
;;

;; common keys
;;;###autoload
(defvar tla--key-help        ??)        ; help
(defvar tla--key-mark-prefix ?*)   ; other mark related command prefix
(defvar tla--key-add-bookmark    ?b)    ; add this to bookmark
(defvar tla--key-get      ?>)           ; prefix for getting something
(defvar tla--key-reflect  ?<)           ; mirror, apply, install...
(defvar tla--key-parent   ?^)       ; visit uppper XXX. e.g. directory
;;;###autoload
(defvar tla--key-diff     ?=)           ; one shot

(defvar tla--key-add      ?a)           ; prefix for adding something
;;;###autoload
(defvar tla--key-show-bookmark ?b)      ; show bookmark
(defvar tla--key-diff-prefix ?d)
;;;###autoload
(defvar tla--key-file-diff ?d)
;;;###autoload
(defvar tla--key-tree-lint ?l)
;;;###autoload
(defvar tla--key-logs      ?L)
;;;###autoload
(defvar tla--key-ediff     ?e)
;;;###autoload
(defvar tla--key-log-entry ?a)
(defvar tla--key-refresh   ?g)           ; refresh buffer
;;;###autoload
(defvar tla--key-inventory ?i)           ; inventory
(defvar tla--key-mark      ?m)           ; mark
(defvar tla--key-next      ?n)           ; next item
(defvar tla--key-previous  ?p)           ; previous item
(defvar tla--key-quit      ?q)           ; quit
(defvar tla--key-remove    ?r)           ; prefix for remove something
(defvar tla--key-move      ?R)           ; prefix for move/rename something
(defvar tla--key-toggle    ?t)           ; prefix for toggle
(defvar tla--key-unmark    ?u)           ; unmark
(defvar tla--key-popup-menu ?\C-j)
(defvar tla--key-kill-ring-prefix ?w)
;;;###autoload
(defvar tla--key-commit    ?c)          ; actually edit-log, but
                                        ; that's what you do when you
                                        ; want to commit.
;;;###autoload
(defvar tla--key-update     ?u)           ; to run tla update
(defvar tla--key-replay     ?r)           ; to run tla replay
(defvar tla--key-star-merge ?s)           ; to run tla star-merge
(defvar tla--key-missing    ?m)           ; to run tla missing

(defvar tla--key-buffer-prefix ?B)   ; perfix for switching XXX buffer
(defvar tla--key-directory-prefix ?D)
(defvar tla--key-merge-prefix ?M)
(defvar tla--key-tag ?T)
(defvar tla--key-revert ?U)
(defvar tla--key-working-copy ?W)       ; Affecting on working copy
(defvar tla--key-partner-file-prefix ?f)
(defvar tla--key-tagging-method-prefix ?#)
(defvar tla--key-id ?t)                 ; `t' for `t'ag.

;; functions for creating key groups
(defun tla--key-group (prefix &rest keys)
  (apply 'vector prefix keys))

(defun  tla--prefix-toggle (&rest keys)
  (tla--key-group tla--key-toggle keys))

(defun tla--prefix-add (&rest keys)
  (tla--key-group tla--key-add keys))

(defun tla--prefix-remove (&rest keys)
  (tla--key-group tla--key-remove keys))

(defun tla--prefix-move (&rest keys)
  (tla--key-group tla--key-move keys))

(defun tla--prefix-mark (&rest keys)
  (tla--key-group tla--key-mark-prefix keys))

(defun tla--prefix-diff (&rest keys)
  (tla--key-group tla--key-diff-prefix keys))

(defun tla--prefix-merge (&rest keys)
  (tla--key-group tla--key-merge-prefix keys))

(defun tla--prefix-directory (&rest keys)
  (tla--key-group tla--key-directory-prefix keys))

(defun tla--prefix-kill-ring (&rest keys)
  (tla--key-group tla--key-kill-ring-prefix keys))

(defun tla--prefix-buffer (&rest keys)
  (tla--key-group tla--key-buffer-prefix keys))

(defun tla--prefix-working-copy (&rest keys)
  (tla--key-group tla--key-working-copy keys))

(defun tla--prefix-partner-file (&rest keys)
  (tla--key-group tla--key-partner-file-prefix keys))

(defun tla--prefix-tag (&rest keys)
  (tla--key-group tla--key-tag keys))

(defun tla--prefix-tagging-method (&rest keys)
  (tla--key-group tla--key-tagging-method-prefix keys))

;; predefined key vectors
(defvar tla--keyvec-toggle-set     (tla--prefix-toggle ?+))
(defvar tla--keyvec-toggle-reset   (tla--prefix-toggle ?-))
(defvar tla--keyvec-toggle-invert  (tla--prefix-toggle ?~))

;;;###autoload
(defvar tla--keyvec-help    (vector tla--key-help))
(defvar tla--keyvec-parent  (vector tla--key-parent))
(defvar tla--keyvec-add     (vector tla--key-add))
(defvar tla--keyvec-remove  (vector tla--key-remove))
(defvar tla--keyvec-get     (vector tla--key-get))
(defvar tla--keyvec-refresh (vector tla--key-refresh))

(defvar tla--keyvec-next     (vector tla--key-next))
(defvar tla--keyvec-previous (vector tla--key-previous))

(defvar tla--keyvec-mark     (vector tla--key-mark))
(defvar tla--keyvec-unmark   (vector tla--key-unmark))
(defvar tla--keyvec-mark-all (tla--prefix-mark ?*))
(defvar tla--keyvec-unmark-all (tla--prefix-mark ?!))
(defvar tla--keyvec-quit (vector tla--key-quit))
(defvar tla--keyvec-popup-menu   (vector tla--key-popup-menu))


;;;###autoload
(defvar tla--keyvec-ediff (vector tla--key-ediff))
;;;###autoload
(defvar tla--keyvec-tree-lint (vector tla--key-tree-lint))
;;;###autoload
(defvar tla--keyvec-logs      (vector tla--key-logs))
;;;###autoload
(defvar tla--keyvec-log-entry (vector tla--key-log-entry))
;;;###autoload
(defvar tla--keyvec-diff (vector tla--key-diff))
;;;###autoload
(defvar tla--keyvec-file-diff (vector tla--key-file-diff))
;;;###autoload
(defvar tla--keyvec-file-diff (vector tla--key-file-diff))
;;;###autoload
(defvar tla--keyvec-commit (vector tla--key-commit))
;;;###autoload
(defvar tla--keyvec-update     (vector tla--key-update))
(defvar tla--keyvec-replay     (vector tla--key-replay))
(defvar tla--keyvec-star-merge (vector tla--key-star-merge))

(defvar tla--keyvec-reflect  (vector tla--key-reflect))
(defvar tla--keyvec-revert   (vector tla--key-revert))

;;;###autoload
(defvar tla--keyvec-inventory (vector tla--key-inventory))

;;;###autoload
(defvar tla--keyvec-show-bookmark (vector tla--key-show-bookmark))
(defvar tla--keyvec-add-bookmark (vector tla--key-add-bookmark))

(defvar tla--keyvec-tag (vector tla--key-tag))
(defvar tla--keyvec-kill-ring (vector tla--key-kill-ring-prefix))

(defvar tla--keyvec-id (vector tla--key-id))
(defvar tla--keyvec-toggle (vector tla--key-toggle))
;;
;; Global
;;
;;;###autoload
(defvar tla-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?U]                      'tla-undo)
    (define-key map [?R]                      'tla-redo)
    (define-key map tla--keyvec-log-entry     'tla-add-log-entry)
    (define-key map [?A] 'tla-archives)
    (define-key map tla--keyvec-file-diff     'tla-file-diff)
    (define-key map tla--keyvec-ediff         'tla-file-ediff)
    (define-key map [?o]                      'tla-file-view-original)
    (define-key map tla--keyvec-diff          'tla-changes)
    (define-key map tla--keyvec-commit        'tla-edit-log)
    (define-key map [?t]                      'tla-tag-insert)
    (define-key map tla--keyvec-inventory     'tla-inventory)
    (define-key map [?r]                      'tla-tree-revisions)
    (define-key map tla--keyvec-logs          'tla-logs)
    (define-key map tla--keyvec-tree-lint     'tla-tree-lint)
    (define-key map tla--keyvec-update        'tla-update)
    (define-key map tla--keyvec-show-bookmark 'tla-bookmarks)
    (define-key map tla--keyvec-help          'tla-help)
    map)
  "Global keymap used by Xtla.")

;;;###autoload
(define-key ctl-x-4-map [?T] 'tla-add-log-entry)

;;
;; Minibuffer(for reading engine)
;;
(defvar xtla--name-read-partner-menu (cons "Insert Partner Version" nil))
(fset 'xtla--name-read-partner-menu (cons 'keymap xtla--name-read-partner-menu))
(defvar xtla--name-read-bookmark-menu (cons "Insert Version in Bookmarks" nil))
(fset 'xtla--name-read-bookmark-menu (cons 'keymap xtla--name-read-bookmark-menu))

;;;###autoload
(defvar tla--name-read-extension-keydefs
  '(([(control r)] . tla-name-read-refresh-cache)
    ([(meta *)]    . tla-name-read-insert-default-archive)
    ([(meta \.)]   . tla-name-read-insert-info-at-point)
    ([(meta \;)]   . tla-name-read-insert-version-associated-with-default-directory)
    ([(control n)] . tla-name-read-insert-partner-next)
    ([(control p)] . tla-name-read-insert-partner-previous)
    ([(control v)] . tla-name-read-insert-bookmark-next)
    ([(meta v)]    . tla-name-read-insert-bookmark-previous)
    ([(meta ^)]    . tla-name-read-insert-ancestor)
    ([(control h)] . tla-name-read-help)
    ([(meta \?)]    . tla-name-read-inline-help))
    "Key definitions table for `tla--name-read-minibuf-map'.
The reason these definitions are defined separately from
`tla--name-read-minibuf-map' is that to reuse these definitions
in `tla-name-read-help'. Don't forget to evalute
`tla--name-read-minibuf-map' again after updating this value.")

;;;###autoload
(defvar tla--name-read-minibuf-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    ;; Keys
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (func (cdr pair)))
         (define-key map key func)))
     tla--name-read-extension-keydefs)
    ;; Menus
    (define-key map [menu-bar xtla]
      (cons "Xtla" (make-sparse-keymap "Xtla")))
    (define-key map [menu-bar xtla refresh]
      (list 'menu-item "Refresh Completion Cache"
            'tla-name-read-refresh-cache))
    (define-key map [menu-bar xtla ancestor]
      (list 'menu-item "Insert Ancestor"
            'tla-name-read-insert-ancestor
            :enable '(and
                      (minibufferp)
                      (equal "" (minibuffer-contents))
                      (member archive '(prompt maybe))
                      (not (eq this-command 'tla-compute-direct-ancestor))
                      )))
    (define-key map [menu-bar xtla default]
      (list 'menu-item "Insert Default Archive"
            'tla-name-read-insert-default-archive
            :enable '(and
                      (minibufferp)
                      (equal "" (minibuffer-contents))
                      (member archive '(prompt maybe)))))
    (define-key map [menu-bar xtla here]
      (list 'menu-item "Insert Thing at Point"
            'tla-name-read-insert-info-at-point
            :enable '(and (minibufferp)
                          (equal "" (minibuffer-contents))
                          tla-name-read-insert-info-at-point)))
    (define-key map [menu-bar xtla bookmark]
      (list 'menu-item "Insert Version in Bookmark" 'xtla--name-read-bookmark-menu
            :enable '(let* ((l (condition-case nil
                                   (let ((default-version (tla-tree-version-list default-directory)))
                                     (tla-bookmarks-get-partner-versions default-version))
                                 (error nil))))
                       (and l (< 0 (length l))))))
    (define-key map [menu-bar xtla partner]
      (list 'menu-item "Insert Partner Version" 'xtla--name-read-partner-menu
            :enable '(let* ((l (condition-case nil (tla-partner-list)
                                 (error nil))))
                       (and l (< 0 (length l))))))
    map)
  "Keymap to input a gnuarch revision at the minibuffer.")

;;
;; Context keymap template
;;
(defvar tla--context-map-template
  (let ((map (make-sparse-keymap)))
    ;; TODO: [return[, "\C-m" => tla--generic-context-action
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map [down-mouse-3] 'tla--generic-popup-menu)
    (define-key map tla--keyvec-popup-menu 'tla--generic-popup-menu-by-keyboard)
    map)
  "Template for keymaps used in items, files, changes, etc.")

;;
;; Bookmarks mode
;;
(defvar tla-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    ;; Move
    (define-key map tla--keyvec-next       'tla-bookmarks-next)
    (define-key map tla--keyvec-previous   'tla-bookmarks-previous)
    (define-key map [?N] 'tla-bookmarks-move-down)
    (define-key map [?P] 'tla-bookmarks-move-up)
    ;; Actions
    (define-key map (tla--prefix-merge tla--key-star-merge)
      'tla-bookmarks-star-merge)
    (define-key map (tla--prefix-merge tla--key-replay)
      'tla-bookmarks-replay)
    (define-key map (tla--prefix-merge tla--key-update)
      'tla-bookmarks-update)
    (define-key map (tla--prefix-merge tla--key-missing)
      'tla-bookmarks-missing)
    (define-key map (tla--prefix-merge tla--key-tag)
      'tla-bookmarks-tag)
    (define-key map [?o] 'tla-bookmarks-open-tree)
    (define-key map [(control x) (control f)] 'tla-bookmarks-find-file)
    (define-key map tla--keyvec-diff 'tla-bookmarks-changes)
    (define-key map tla--keyvec-get  'tla-bookmarks-get)
    (define-key map "\C-m"           'tla-bookmarks-goto)
    ;; Marks
    (define-key map tla--keyvec-mark       'tla-bookmarks-mark)
    (define-key map tla--keyvec-unmark     'tla-bookmarks-unmark)
    (define-key map tla--keyvec-unmark-all 'tla-bookmarks-unmark-all)
    (define-key map (tla--prefix-mark ?g)  'tla-bookmarks-select-by-group)
    ;; Partners
    (define-key map [(meta p)] 'tla-bookmarks-marked-are-partners)
    (define-key map (tla--prefix-add    ?p)
      'tla-bookmarks-add-partner-interactive)
    (define-key map (tla--prefix-remove ?p)
      'tla-bookmarks-delete-partner-interactive)
    (define-key map (tla--prefix-partner-file ?r)
      'tla-bookmarks-add-partners-from-file)
    (define-key map (tla--prefix-partner-file ?w)
      'tla-bookmarks-write-partners-to-file)
    ;; Bookmark manipulation
    (define-key map (tla--prefix-add    ?b) 'tla-bookmarks-add)
    (define-key map (tla--prefix-remove ?b) 'tla-bookmarks-delete)
    (define-key map [?e] 'tla-bookmarks-edit)
    (define-key map tla--keyvec-toggle  'tla-bookmarks-toggle-details)
    ;; Fields
    (define-key map (tla--prefix-add    ?t)
      'tla-bookmarks-add-tree-interactive)
    (define-key map (tla--prefix-remove ?t)
      'tla-bookmarks-delete-tree-interactive)
    (define-key map (tla--prefix-add    ?g)
      'tla-bookmarks-add-group-interactive)
    (define-key map (tla--prefix-remove ?g)
      'tla-bookmarks-delete-group-interactive)
    (define-key map (tla--prefix-add    ?n)
      'tla-bookmarks-add-nickname-interactive)
    (define-key map (tla--prefix-remove ?n)
      'tla-bookmarks-delete-nickname-interactive)
    (define-key map [?s] 'tla-bookmarks-edit-summary)
    ;; Switch to other buffers
    (define-key map tla--keyvec-inventory 'tla-bookmarks-inventory)
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in `tla-bookmarks-mode' buffers.")

(defvar tla-bookmarks-entry-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-bookmarks-goto-by-mouse)
    map)
  "Keymap used on entries in `tla-bookmarks-mode' buffers.")

;;
;; Inventory mode
;;
(defvar tla-inventory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help    'describe-mode)
    (define-key map tla--keyvec-refresh 'tla-generic-refresh)
    (define-key map tla--keyvec-add     'tla-inventory-add-files)
    (define-key map tla--keyvec-remove  'tla-inventory-remove-files)
    (define-key map tla--keyvec-quit    'tla-buffer-quit)
    (define-key map tla--keyvec-next     'tla-inventory-next)
    (define-key map tla--keyvec-previous 'tla-inventory-previous)
    (define-key map tla--keyvec-parent   'tla-inventory-parent-directory)
    ;;
    ;;
    ;;
    (define-key map [?X] 'tla-inventory-delete-files)
    (define-key map (tla--prefix-move tla--key-move) 'tla-inventory-move)
    (define-key map tla--keyvec-commit 'tla-inventory-edit-log)
    (define-key map [?l] 'tla-changelog)
    (define-key map tla--keyvec-logs 'tla-logs)
    ;;
    ;; Find file group
    ;;
    (define-key map [?f] 'tla-inventory-find-file)
    (define-key map [return] 'tla-inventory-find-file)
    (define-key map "\C-m" 'tla-inventory-find-file)
    (define-key map [?o] 'tla-generic-find-file-other-window)
    (define-key map [?v] 'tla-generic-view-file)
    ;;
    ;; Diffs group
    ;;
    (define-key map (tla--prefix-merge tla--key-missing)
      'tla-inventory-missing)
    (define-key map (tla--prefix-diff tla--key-diff)
      'tla-inventory-changes)
    (define-key map (tla--prefix-diff ?l) 'tla-changes-last-revision)
    (define-key map (tla--prefix-diff tla--key-ediff)
      'tla-inventory-file-ediff)
    (define-key map (tla--prefix-diff tla--key-get)
      'tla-inventory-delta)
    ;; Alias for above bindings
    (define-key map tla--keyvec-diff    'tla-inventory-changes)
    (define-key map tla--keyvec-ediff   'tla-inventory-file-ediff)
    ;;
    (define-key map tla--keyvec-reflect 'tla-inventory-mirror)
    ;;
    ;; Merge group
    ;;
    (define-key map (tla--prefix-merge tla--key-star-merge)
      'tla-inventory-star-merge)
    (define-key map (tla--prefix-merge tla--key-replay)
      'tla-inventory-replay)
    (define-key map (tla--prefix-merge tla--key-update)
      'tla-inventory-update)
    (define-key map (tla--prefix-merge tla--key-reflect)
      'tla-inventory-apply-changeset)
    ;;
    ;; Buffers group
    ;;
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)
    ;;
    ;; Undo and redo group
    ;;
    (define-key map tla--keyvec-revert        'tla-inventory-revert)
    (define-key map (tla--prefix-working-copy tla--key-revert) 'tla-inventory-undo)
    (define-key map (tla--prefix-working-copy ?R) 'tla-inventory-redo)
    ;;
    ;; Patches group
    ;;
    (define-key map (tla--prefix-working-copy ?S) 'tla-changes-save)
    (define-key map (tla--prefix-working-copy ?s) 'tla-changes-save-as-tgz)
    (define-key map (tla--prefix-working-copy ?V) 'tla-show-changeset)
    (define-key map (tla--prefix-working-copy ?v) 'tla-show-changeset-from-tgz)
    (define-key map (tla--prefix-working-copy ?A) 'tla-inventory-apply-changeset)
    (define-key map (tla--prefix-working-copy ?a) 'tla-inventory-apply-changeset-from-tgz)
    ;;
    ;; Kill ring group
    ;;
    (define-key map (tla--prefix-kill-ring ?a) 'tla-save-archive-to-kill-ring)
    ;;
    ;; Tree lint
    ;;
    (define-key map (tla--prefix-working-copy tla--key-tree-lint)
      'tla-tree-lint)
    ;;
    ;; Mark group
    ;;
    (define-key map (tla--prefix-mark tla--key-mark) 'tla-inventory-mark-file)
    (define-key map (tla--prefix-mark tla--key-unmark) 'tla-inventory-unmark-file)
    ;; (define-key map tla--keyvec-mark-all      'tla-inventory-mark-all)
    (define-key map tla--keyvec-unmark-all    'tla-inventory-unmark-all)
    ;; Alias for above bindings
    (define-key map tla--keyvec-mark          'tla-inventory-mark-file)
    (define-key map tla--keyvec-unmark        'tla-inventory-unmark-file)
    ;;
    ;; Tagging method
    ;;
    (define-key map (tla--prefix-tagging-method ?=) 'tla-edit-=tagging-method-file)
    (define-key map (tla--prefix-tagging-method ?.) 'tla-edit-.arch-inventory-file)
    ;;
    ;; Exclude, junk, precious, unrecognized...
    ;;
    (define-key map (tla--prefix-move ?j) 'tla-inventory-make-junk)
    (define-key map (tla--prefix-move ?,) 'tla-inventory-make-junk)
    (define-key map (tla--prefix-move ?p) 'tla-inventory-make-precious)
    (define-key map (tla--prefix-move ?+) 'tla-inventory-make-precious)
    (define-key map (tla--prefix-tagging-method ?M) 'tla-generic-set-id-tagging-method)
    (define-key map (tla--prefix-tagging-method ?V) 'tla-generic-set-tree-version)
    (define-key map (tla--prefix-tagging-method ?x) 'tla-generic-add-to-exclude) ; alias
    (define-key map (tla--prefix-tagging-method ?e) 'tla-generic-add-to-exclude) ; alias
    (define-key map (tla--prefix-tagging-method ?j) 'tla-generic-add-to-junk)
    (define-key map (tla--prefix-tagging-method ?b) 'tla-generic-add-to-backup)
    (define-key map (tla--prefix-tagging-method ?~) 'tla-generic-add-to-backup) ; alias
    (define-key map (tla--prefix-tagging-method ?p) 'tla-generic-add-to-precious)
    (define-key map (tla--prefix-tagging-method ?u) 'tla-generic-add-to-unrecognized)
    ;;
    ;; Toggles
    ;;
    (define-key map tla--keyvec-toggle-set    'tla-inventory-set-all-toggle-variables)
    (define-key map tla--keyvec-toggle-reset  'tla-inventory-reset-all-toggle-variables)
    (define-key map tla--keyvec-toggle-invert 'tla-inventory-toggle-all-toggle-variables)
    map)
  "Keymap used in `tla-inventory-mode' buffers.")

(defvar tla-inventory-item-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-inventory-find-file-by-mouse)
    map)
  "Keymap used on items in `tla-inventory-mode' buffers.")

(defvar tla-inventory-default-version-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [return] 'tla-generic-set-tree-version)
    (define-key map "\C-m" 'tla-generic-set-tree-version)
    map)
  "Keymap used on the default version field in `tla-inventory-mode' buffers.")

(defvar tla-inventory-tagging-method-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-generic-set-id-tagging-method-by-mouse)
    (define-key map [return] 'tla-generic-set-id-tagging-method)
    (define-key map "\C-m" 'tla-inventory-id-tagging-method)
    map)
  "Keymap used on the tagging method field in `tla-inventory-mode' buffers.")

;;;###autoload
(defconst tla-inventory-file-types-manipulators
  '((?S tla-inventory-display-source
        tla-inventory-toggle-source ?s "source")
    (?P tla-inventory-display-precious
        tla-inventory-toggle-precious ?p "precious")
    (?J tla-inventory-display-junk
        tla-inventory-toggle-junk ?j "junk")
    (?B tla-inventory-display-backup
        tla-inventory-toggle-backup ?b "backup")
    (?T tla-inventory-display-tree
        tla-inventory-toggle-tree ?t "tree root")
    (?U tla-inventory-display-unrecognized
        tla-inventory-toggle-unrecognized ?u "unrecognized"))
  "List of possible file types in inventory.")

(dolist (type-arg tla-inventory-file-types-manipulators)
  (define-key tla-inventory-mode-map `[?t ,(cadddr type-arg)] (caddr type-arg)))

;;;###autoload
(dolist (type-arg tla-inventory-file-types-manipulators)
  (eval `(defcustom ,(cadr type-arg) t
           ,(concat "Wether " (nth 4 type-arg)
                    " should be printed in inventory")
           :group 'tla-inventory
           :type 'boolean)))

(defvar tla-tree-lint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help    'describe-mode)
    (define-key map tla--keyvec-refresh 'tla-generic-refresh)
    (define-key map tla--keyvec-add     'tla-tree-lint-add-files)
    (define-key map tla--keyvec-remove  'tla-tree-lint-delete-files)
    (define-key map tla--keyvec-quit    'tla-buffer-quit)
    (define-key map tla--keyvec-commit  'tla-edit-log)
    (define-key map tla--keyvec-next    'tla-tree-lint-next)
    (define-key map tla--keyvec-previous 'tla-tree-lint-previous)
    (define-key map [down]              'tla-tree-lint-next)
    (define-key map [up]                'tla-tree-lint-previous)
    (define-key map tla--keyvec-id      'tla-tree-lint-regenerate-id)
    (define-key map (tla--prefix-move ?j) 'tla-tree-lint-make-junk)
    (define-key map (tla--prefix-move ?,) 'tla-tree-lint-make-junk)
    (define-key map (tla--prefix-move ?p) 'tla-tree-lint-make-precious)
    (define-key map (tla--prefix-move ?+) 'tla-tree-lint-make-precious)
    ;;
    (define-key map (tla--prefix-tagging-method ?=) 'tla-edit-=tagging-method-file)
    (define-key map (tla--prefix-tagging-method ?.) 'tla-edit-.arch-inventory-file)
    (define-key map (tla--prefix-tagging-method ?M) 'tla-generic-set-id-tagging-method)
    (define-key map (tla--prefix-tagging-method ?V) 'tla-generic-set-tree-version)
    (define-key map (tla--prefix-tagging-method ?x) 'tla-generic-add-to-exclude) ; alias
    (define-key map (tla--prefix-tagging-method ?e) 'tla-generic-add-to-exclude) ; alias
    (define-key map (tla--prefix-tagging-method ?j) 'tla-generic-add-to-junk)
    (define-key map (tla--prefix-tagging-method ?b) 'tla-generic-add-to-backup)
    (define-key map (tla--prefix-tagging-method ?~) 'tla-generic-add-to-backup) ; alias
    (define-key map (tla--prefix-tagging-method ?p) 'tla-generic-add-to-precious)
    (define-key map (tla--prefix-tagging-method ?u) 'tla-generic-add-to-unrecognized)
    ;; Other commands
    (define-key map tla--keyvec-diff      'tla-changes)
    (define-key map tla--keyvec-inventory 'tla-inventory)
    ;;
    (define-key map [return]            'tla-generic-find-file-at-point)
    (define-key map "\C-m"              'tla-generic-find-file-at-point)
    (define-key map [?o]                'tla-generic-find-file-other-window)
    (define-key map [?v]                'tla-generic-view-file)
    ;;
    ;; Mark group
    ;;
    (define-key map (tla--prefix-mark tla--key-mark) 'tla-tree-lint-mark-file)
    (define-key map (tla--prefix-mark tla--key-unmark) 'tla-tree-lint-unmark-file)
    ;; TODO
    ;; (define-key map tla--keyvec-mark-all      'tla-tree-lint-mark-all)
    (define-key map tla--keyvec-unmark-all    'tla-tree-lint-unmark-all)
    ;; Alias for above bindings
    (define-key map tla--keyvec-mark          'tla-tree-lint-mark-file)
    (define-key map tla--keyvec-unmark        'tla-tree-lint-unmark-file)
    ;;
    ;; Buffers group
    ;;
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)
    map)
  "Keymap used in `tla-tree-lint-mode' buffers.")

(defvar tla-tree-lint-file-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-generic-find-file-at-point-by-mouse)
    map)
  "Keymap used on files in tla-lint-mode buffers.")

;;
;; Cat-Log mdoe
;;
(defvar tla-cat-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map tla--keyvec-inventory 'tla-pop-to-inventory)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in `tla-cat-log-mode' buffers.")

;;
;; Log edit mode
;;
(defvar tla-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'tla-log-edit-done)
    (define-key map [(control ?c) (control ?d)] 'tla-changes)
    (define-key map [(control ?c) (control ?l)] 'tla-changelog)
    (define-key map [(control ?c) (control ?m)] 'tla-log-edit-insert-log-for-merge)
    (define-key map [(control ?c)          ?m ]
      'tla-log-edit-insert-log-for-merge-and-headers)
    (define-key map [(control ?c) (control ?p)] 'tla-log-edit-insert-memorized-log)
    (define-key map [(control ?c) (control ?q)] 'tla-log-edit-abort)
    (define-key map [(control ?c) (control ?s)] 'tla-log-goto-summary)
    (define-key map [(control ?c) (control ?b)] 'tla-log-goto-body)
    (define-key map [(control ?c) (control ?k)] 'tla-log-edit-keywords)
    (define-key map "\t" 'tla-log-edit-next-field)
    map)
  "Keymap used in `tla-log-edit-mode' buffers.")

;;
;; Archive list mode
;;
(defvar tla-archive-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map (tla--prefix-kill-ring ?a) 'tla-save-archive-to-kill-ring)
    (define-key map "\C-m" 'tla-archive-list-categories)
    (define-key map [return] 'tla-archive-list-categories)

    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)

    (define-key map tla--keyvec-add-bookmark 'tla-bookmarks-add)
    (define-key map [?o] 'tla-archive-browse-archive)
    (define-key map [?*] 'tla-archive-select-default)
    (define-key map (tla--prefix-add ?r) 'tla-register-archive)
    (define-key map (tla--prefix-add ?a) 'tla-make-archive)
    (define-key map (tla--prefix-add ?m) 'tla-archive-mirror-archive)
    (define-key map tla--keyvec-remove   'tla-archive-unregister-archive)
    (define-key map [?g] 'tla-archives)
    (define-key map [?s] 'tla-archive-synchronize-archive)
    (define-key map [?e] 'tla-archive-edit-archive-location)
    (define-key map [down] 'tla-archive-next)
    (define-key map [up] 'tla-archive-previous)
    (define-key map [?n] 'tla-archive-next)
    (define-key map [?p] 'tla-archive-previous)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in `tla-archive-list-mode' buffers.")

(defvar tla-archive-archive-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'tla-archive-list-categories-by-mouse)
    map)
  "Keymap used archives in `tla-archive-list-mode' buffers.")

;;
;; Category list mode
;;
(defvar tla-category-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-category-list-branches)
    (define-key map [return] 'tla-category-list-branches)

    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)

    (define-key map tla--keyvec-add-bookmark 'tla-category-bookmarks-add-here)
    (define-key map [?^] 'tla-archives)
    (define-key map (tla--prefix-add ?c) 'tla-category-make-category)
    (define-key map [?g] 'tla-category-refresh)
    (define-key map [?s] 'tla-category-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in `tla-category-list-mode' buffers.")

(defvar tla-category-category-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'tla-category-list-branches-by-mouse)
    map)
  "Keymap used categories in `tla-category-list-mode' buffers.")

;;
;; Branch list mode section
;;
(defvar tla-branch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-branch-list-versions)
    (define-key map [return] 'tla-branch-list-versions)

    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)

    (define-key map tla--keyvec-parent 'tla-branch-list-parent-category)
    (define-key map (tla--prefix-add ?b) 'tla-branch-make-branch)
    (define-key map [?>] 'tla-branch-get-branch)
    (define-key map [?g] 'tla-branch-refresh)
    (define-key map [?s] 'tla-branch-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map tla--keyvec-add-bookmark 'tla-branch-bookmarks-add-here)
    map)
  "Keymap used in `tla-branch-list-mode' buffers.")

(defvar tla-branch-branch-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'tla-branch-list-versions-by-mouse)
    map)
  "Keymap used branches in `tla-branch-list-mode' buffers.")

;;
;; Version list mode
;;
(defvar tla-version-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-version-list-revisions)
    (define-key map [return] 'tla-version-list-revisions)

    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)

    (define-key map tla--keyvec-parent 'tla-version-list-parent-branch)
    (define-key map (tla--prefix-add ?v) 'tla-version-make-version)
    (define-key map [?>] 'tla-version-get-version)
    (define-key map [?g] 'tla-version-refresh)
    (define-key map [?s] 'tla-version-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map tla--keyvec-add-bookmark 'tla-version-bookmarks-add-here)
    (define-key map tla--keyvec-tag 'tla-version-tag)
    map)
  "Keymap used in `tla-version-list-mode' buffers.")

(defvar tla-version-version-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'tla-version-list-revisions-by-mouse)
    map)
  "Keymap used versions in `tla-version-list-mode' buffers.")

;;
;; Revision list mode
;;
(defvar tla-revision-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map tla--keyvec-parent 'tla-revision-list-parent-version)
    (define-key map [?> ?g] 'tla-revision-get-revision)
    (define-key map [?> ?C] 'tla-revision-cache-revision)
    (define-key map [?> ?L] 'tla-revision-add-to-library)

    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)

    (define-key map (tla--prefix-kill-ring ?r) 'tla-revision-save-revision-to-kill-ring)
    (define-key map (tla--prefix-kill-ring ?v) 'tla-revision-save-version-to-kill-ring)

    (define-key map tla--keyvec-add-bookmark 'tla-bookmarks-add)

    (define-key map (tla--prefix-toggle ??) 'tla-revision-toggle-help)
    (define-key map (tla--prefix-toggle ?d) 'tla-revision-toggle-date)
    (define-key map (tla--prefix-toggle ?c) 'tla-revision-toggle-creator)
    (define-key map (tla--prefix-toggle ?s) 'tla-revision-toggle-summary)
    (define-key map (tla--prefix-toggle ?l) 'tla-revision-toggle-library)
    (define-key map (tla--prefix-toggle ?m) 'tla-revision-toggle-merges)
    (define-key map (tla--prefix-toggle ?b) 'tla-revision-toggle-merged-by)
    (define-key map (tla--prefix-toggle ?r) 'tla-revision-toggle-reverse)

    ;;
    ;; Star merge
    ;; from here
    (define-key map tla--keyvec-star-merge 'tla-revision-star-merge)
    ;; from head
    (define-key map (tla--prefix-merge tla--key-star-merge)
      'tla-revision-star-merge-version)

    ;;
    ;; Replay
    ;; from here
    (define-key map tla--keyvec-replay 'tla-revision-replay)
    ;; from head
    (define-key map (tla--prefix-merge tla--key-replay)
      'tla-revision-replay-version)

    ;;
    ;; Sync tree
    (define-key map  [?y] 'tla-revision-sync-tree)
    ;;
    ;; Update
    (define-key map (tla--prefix-merge tla--key-update)
      'tla-revision-update)
    ;;
    ;; Tag
    ;; from here
    (define-key map tla--keyvec-tag 'tla-revision-tag-from-here)

    (define-key map [?g] 'tla-generic-refresh)
    (define-key map [down] 'tla-revision-next)
    (define-key map [up] 'tla-revision-prev)
    (define-key map [?n] 'tla-revision-next)
    (define-key map [?p] 'tla-revision-prev)
    (define-key map [?N] 'tla-revision-next-unmerged)
    (define-key map [?P] 'tla-revision-prev-unmerged)
    (define-key map [?l] 'tla-revision-cat-log)
    (define-key map "\C-m" 'tla-revision-show-changeset)
    (define-key map [return] 'tla-revision-show-changeset)
    (define-key map (tla--prefix-merge tla--key-missing) 'tla-missing-show-all-revisions)
    (define-key map tla--keyvec-mark   'tla-revision-mark-revision)
    (define-key map tla--keyvec-unmark 'tla-revision-unmark-revision)
    (define-key map (tla--prefix-diff tla--key-diff) 'tla-revision-delta)
    (define-key map (tla--prefix-diff tla--key-get)  'tla-revision-store-delta)
    (define-key map [?=] 'tla-revision-changeset)
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)
    (define-key map tla--keyvec-inventory 'tla-pop-to-inventory)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map tla--keyvec-add-bookmark 'tla-revision-bookmarks-add)
    map)
  "Keymap used in `tla-revision-list-mode' buffers.")

(defstruct (tla--revision)
  revision ;; The revision, as a list
  summary creator date
  merges ;; List of patches merged by this revision
  merged-by ;; List of patches merging this revision
  )

(defvar tla-revision-revision-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-revision-show-changeset-by-mouse)
    map)
  "Keymap used on revisions in `tla-revision-list-mode' buffers.")

;;
;; Changes mode
;;
(defvar tla-changes-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-changes-jump-to-change)
    (define-key map [return] 'tla-changes-jump-to-change)
    (define-key map [?=] 'tla-changes-diff)
    (define-key map tla--keyvec-ediff   'tla-changes-ediff)
    (define-key map tla--keyvec-refresh 'tla-generic-refresh)
    (define-key map tla--keyvec-commit  'tla-changes-edit-log)
    (define-key map [?I] 'tla-inventory)
    (define-key map tla--keyvec-inventory 'tla-pop-to-inventory)
    (define-key map tla--keyvec-next      'tla-changes-next)
    (define-key map tla--keyvec-previous  'tla-changes-prev)
    (define-key map tla--keyvec-revert    'tla-changes-revert)
    (define-key map tla--keyvec-quit      'tla-buffer-quit)
    (define-key map [?d] 'tla-changes-rm)
    (define-key map tla--keyvec-mark   'tla-changes-mark-file)
    (define-key map tla--keyvec-unmark 'tla-changes-unmark-file)
    (define-key map [?v] 'tla-changes-view-source)
    (define-key map tla--keyvec-parent 'tla-changes-master-buffer)
    (define-key map [?j] 'tla-changes-diff-or-list)
    ;; Buffers group
    (define-key map (tla--prefix-buffer ?p) 'tla-show-process-buffer)
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)
    (define-key map (tla--prefix-buffer tla--key-show-bookmark) 'tla-bookmarks)
    map)
  "Keymap used in `tla-changes-mode'.")

(defvar tla-changes-file-map
  (let ((map (copy-keymap tla--context-map-template)))
    (define-key map [mouse-2] 'tla-changes-jump-to-change-by-mouse)
    map)
  "Keymap used on files in `tla-changes-mode' buffers.")


;;
;; ChangeLog mode section
;;
(defvar tla-changelog-mode-map
  (let ((map (copy-keymap change-log-mode-map)))
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map [?n] 'tla-changelog-next-entry)
    (define-key map [?p] 'tla-changelog-previous-entry)
    map)
  "Keymap used in `tla-changelog-mode'.")


;;
;; Log buffer mode section
;;
(defvar tla-log-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map [?o] 'tla-switch-to-output-buffer)
    (define-key map "\C-m" 'tla-switch-to-output-buffer)
    (define-key map [?e] 'tla-switch-to-error-buffer)
    (define-key map [?r] 'tla-switch-to-related-buffer)
    (define-key map [?n] 'tla-log-next)
    (define-key map [?p] 'tla-log-prev)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in Xtla's log buffer.")

;;
;; Process buffer mode section
;;
(defvar tla-process-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (tla--prefix-buffer ?L) 'tla-open-internal-log-buffer)

    (define-key map tla--keyvec-inventory 'tla-show-inventory-buffer)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    map)
  "Keymap used in Xtla's log buffer.")

;;
;; Log edit buffer mode section
;;

(defvar tla-log-edit-keywords-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?n] 'tla-log-edit-keywords-next)
    (define-key map [?p] 'tla-log-edit-keywords-previous)
    (define-key map [?m] 'tla-log-edit-keywords-mark)
    (define-key map [?u] 'tla-log-edit-keywords-unmark)
    (define-key map [?t] 'tla-log-edit-keywords-toggle-mark)
    (define-key map [?* ?!] 'tla-log-edit-keywords-unmark-all)
    (define-key map [?* ?*] 'tla-log-edit-keywords-mark-all)
    (define-key map "\C-c\C-c" 'tla-log-edit-keywords-insert)
    map)
  "Keymap used in tla-log-edit-keywords-mode buffers.")


;; ----------------------------------------------------------------------------
;; Menu entries
;; ----------------------------------------------------------------------------
;;
;; Conventions
;;
;; 1. Each Nouns and verbs in menu items are should be capitalized.
;; 2. TODO: Consider menu items order.

;;
;; Common submenus
;;

(defconst tla-.arch-inventory-menu-list
  '("Put to .arch-inventory"
    ["Junk"         tla-generic-add-to-junk         t]
    ["Backup"       tla-generic-add-to-backup       t]
    ["Precious"     tla-generic-add-to-precious     t]
    ["Unrecognized" tla-generic-add-to-unrecognized t]))

(defconst tla-=tagging-method-menu-list
  '("Put to =tagging-method"
    ["Junk"         (tla-generic-add-to-junk t)     t]
    ["Backup"       (tla-generic-add-to-backup t)   t]
    ["Precious"     (tla-generic-add-to-precious t) t]
    ["Unrecognized" (tla-generic-add-to-junk t)     t]))


;;
;; Global
;;
;;;###autoload
(easy-menu-add-item
 (or (tla--do-in-gnu-emacs menu-bar-tools-menu) nil)
 (or (tla--do-in-xemacs '("Tools")) nil)
 '("Xtla"
   ["Browse Archives" tla-archives t]
   ["Show Bookmarks" tla-bookmarks t]
   ["Start New Project" tla-start-project t]
   "---"
   "Tree Commands:"
   ["View Changes" tla-changes t]
   ["View Inventory" tla-inventory t]
   ["View Tree Lint" tla-tree-lint t]
   ["Show Tree Revisions" tla-tree-revisions t]
   ["Edit Arch Log" tla-edit-log t]
   "---"
   "File Commands:"
   ["Insert Arch Tag" tla-tag-insert t]
   ["Add Log Entry" tla-add-log-entry t]
   ["View File Diff" tla-file-diff t]
   ["View File Ediff" tla-file-ediff t]
   ["View Original" tla-file-view-original t]
   ["View Conflicts" tla-view-conflicts t]
   "---"
   ("Quick Configuration"
    ["Three Way Merge" tla-toggle-three-way-merge
     :style toggle :selected tla-three-way-merge]
;;     ["Use --forward" tla-toggle-use-forward-option
;;      :style toggle :selected tla-use-forward-option]
    ["Use --skip-present" tla-toggle-use-skip-present-option
     :style toggle :selected tla-use-skip-present-option]
    )
   )
 "PCL-CVS")

;;
;; Bookmarks mode
;;
(defconst tla-bookmarks-entry-menu-list
  '("Bookmark Entry"
    ["Delete"         tla-bookmarks-delete    t]
    ["Goto Location"  tla-bookmarks-goto      t]
    ("File Tree"
     ["Find File"      tla-bookmarks-find-file t]
     ["Run Dired"      tla-bookmarks-open-tree t]
     ["Run Inventory"  tla-bookmarks-inventory t]
     ["View Changes"   tla-bookmarks-changes t]
     )
    ("Merge/Tag"
     ["View Missing Patches" tla-bookmarks-missing t]
     ["Replay"       tla-bookmarks-replay  t]
     ["Update"       tla-bookmarks-update  t]
     ["Star-merge"   tla-bookmarks-star-merge t]
     ["Tag"          tla-bookmarks-tag     t]
     )
    ("Edit"
     ["Edit Bookmark"    tla-bookmarks-edit t]
     ["Add Nickname"     tla-bookmarks-add-nickname-interactive    t]
     ["Remove Nickname"  tla-bookmarks-delete-nickname-interactive t]
     ["Add Local Tree"   tla-bookmarks-add-tree-interactive        t]
     ["Remove Local Tree" tla-bookmarks-delete-tree-interactive    t]
     ["Add Group"        tla-bookmarks-add-group-interactive       t]
     ["Remove Group"     tla-bookmarks-delete-group-interactive    t]
     ["Add Partner"      tla-bookmarks-add-partner-interactive     t]
     ["Remove Partner"   tla-bookmarks-delete-partner-interactive  t]
     )
    ("Partners"
     ["Add Partner"      tla-bookmarks-add-partner-interactive     t]
     ["Remove Partner"   tla-bookmarks-delete-partner-interactive  t]
     ["Write to Partner File" tla-bookmarks-write-partners-to-file t]
     ["Load from Partner File" tla-bookmarks-add-partners-from-file t]
     ["View Missing Patches" tla-bookmarks-missing t]
     ))
  "Used both for the local and the global menu."
  )

(easy-menu-define tla-bookmarks-mode-menu tla-bookmarks-mode-map
  "`tla-bookmarks-mode' menu"
  `("Tla-Bookmarks"
    ["Add Bookmark" tla-bookmarks-add t]
    ["Show Details" tla-bookmarks-toggle-details
     :style toggle :selected tla-bookmarks-show-details]
    ["Select by Group" tla-bookmarks-select-by-group t]
    ["Cleanup 'local-tree fields" tla-bookmarks-cleanup-local-trees t]
    ,tla-bookmarks-entry-menu-list
    ))

(easy-menu-define tla-bookmarks-entry-menu nil
  "Menu used on a tla bookmark entry."
  tla-bookmarks-entry-menu-list)

;;
;; Inventory mode
;;
(easy-menu-define tla-inventory-mode-partners-menu tla-inventory-mode-map
  "`tla-inventory-mode' partners menu"
  '("Partners"
    ["Add Partner..." tla-partner-add t]
    ("Set Tree Version" :filter (lambda (x)
                                  (tla--partner-create-menu
                                   'tla-generic-set-tree-version)))
    "--"
    ("Show Changes" :filter (lambda (x)
                              (tla--partner-create-menu
                               '(lambda (x)
                                  (tla-changes current-prefix-arg
                                               (list 'revision (tla--name-split x)))))))
    ("Show Missing" :filter (lambda (x)
                              (tla--partner-create-menu
                               '(lambda (x)
                                  (tla-missing default-directory x)))))
    "--"
    ("Replay" :filter (lambda (x)
                            (tla--partner-create-menu
                             'tla-inventory-replay)))
    ("Star-merge" :filter (lambda (x)
                            (tla--partner-create-menu
                             'tla-inventory-star-merge)))))

(defconst tla-inventory-item-menu-list
  `("Inventory Item"
    ["Open File" tla-inventory-find-file t]
    ["View File" tla-generic-view-file t]
    "--"
    ["Add"    tla-inventory-add-files    t]
    ["Move"   tla-inventory-move         t]
    ["Revert" tla-inventory-revert       t]
    ["Remove" tla-inventory-remove-files t]
    ["Delete" tla-inventory-delete-files t]
    "--"
    ["Make Junk"     tla-inventory-make-junk     t]
    ["Make Precious" tla-inventory-make-precious t]
    ,tla-.arch-inventory-menu-list
    ,tla-=tagging-method-menu-list)
  "Used both in the context and the global menu for inventory.")

(easy-menu-define tla-inventory-mode-menu tla-inventory-mode-map
  "`tla-inventory-mode' menu"
  `("Inventory"
    ["Edit Log" tla-inventory-edit-log t]
    "--"
    ["Show Changes"   tla-inventory-changes t]
    ["Show Changelog" tla-changelog t]
    ["Show Logs"      tla-logs t]
    ["Show Missing"   tla-inventory-missing t]
    "--"
    ,tla-inventory-item-menu-list
    "--"
    ["Update"     tla-inventory-update t]
    ["Replay"     tla-inventory-replay t]
    ["Star-merge" tla-inventory-star-merge t]
    ("Changesets"
     ["Save actual changes in directory" tla-changes-save t]
     ["Save actual changes in tarball" tla-changes-save-as-tgz t]
     ["View changeset from directory" tla-show-changeset t]
     ["View changeset from tarball" tla-show-changeset-from-tgz t]
     ["Apply changeset from directory" tla-inventory-apply-changeset t]
     ["Apply changeset from tarball" tla-inventory-apply-changeset-from-tgz t]
     )
    "--"
    ["Undo" tla-inventory-undo t]
    ["Redo" tla-inventory-redo t]
    "--"
    ["Synchronize Mirror" tla-inventory-mirror t]
    ("Taging Method"
     ["Edit .arch-inventory" tla-edit-.arch-inventory-file t]
     ["Edit =tagging-method" tla-edit-=tagging-method-file t]
     ["Set Tagging Method"   tla-generic-set-id-tagging-method t]
     ["Set Tree Version From Scratch" tla-generic-set-tree-version t]
     )
    ["Tree-lint" tla-tree-lint t]
    "--"
    ("Toggles"
     ["Set All Toggle Variables" tla-inventory-set-all-toggle-variables t]
     ["Reset All Toggle Variables" tla-inventory-reset-all-toggle-variables t]
     ["Toggle All Toggle Variables" tla-inventory-toggle-all-toggle-variables t] .
     ,(mapcar '(lambda (elem) `[,(concat "Toggle " (car (cddddr elem)))
                                ,(caddr elem)
                                :style toggle
                                :selected ,(cadr elem)])
              tla-inventory-file-types-manipulators))))

(easy-menu-define tla-inventory-item-menu nil
  "Menu used on a inventory item."
  tla-inventory-item-menu-list)

(easy-menu-define tla-inventory-tagging-method-menu nil
  "Menu used on the taggine method line in a inventory buffer."
  '("Switch Taggine Method"
    ["Tagline"  (tla-generic-set-id-tagging-method "tagline") t]
    ["Explicit" (tla-generic-set-id-tagging-method "explicit") t]
    ["Names"    (tla-generic-set-id-tagging-method "names") t]
    ["Implicit" (tla-generic-set-id-tagging-method "implicit") t]))

;;
;; Cat-log mode
;;
(easy-menu-define tla-cat-log-mode-menu tla-cat-log-mode-map
  "'tla-cat-log-mode' menu"
  '("Cat-Log"
    ["Inventory" tla-pop-to-inventory t]
    ["Quit" tla-buffer-quit t]
    ))

;;
;; Log edit mode
;;
(easy-menu-define tla-log-edit-mode-menu tla-log-edit-mode-map
  "`tla-log-edit-mode' menu"
  '("Log"
    ["Insert tla log-for-merge" tla-log-edit-insert-log-for-merge t]
    ["log-for-merge and headers"
     tla-log-edit-insert-log-for-merge-and-headers t]
    ["Insert memorized log"     tla-log-edit-insert-memorized-log t]
    ["Show changes"             tla-changes                       t]
    ["Commit"                   tla-log-edit-done                 t]
    ["Show Changelog"           tla-changelog                     t]
    ["Goto Summary Field"       tla-log-goto-summary              t]
    ["Goto Body"                tla-log-goto-body                 t]
    ["Edit Keywords Field"      tla-log-edit-keywords             t]
    ["Kill Body"                tla-log-kill-body                 t]
    ["Tree Lint"                tla-tree-lint                     t]
    ["Abort"                    tla-log-edit-abort                t]))

;;
;; Archive list mode
;;
(easy-menu-define tla-archive-list-mode-menu tla-archive-list-mode-map
  "`tla-archive-list-mode' menu"
  '("Archives"
    ["Register New Archive"        tla-register-archive t]
    ["Add a Bookmark"              tla-bookmarks-add t]
    ["Update Archives List"        tla-archives t]
    ["Set Default Archive"         tla-archive-select-default t]
    ["Remove Archive Registration" tla-archive-unregister-archive t]
    ["Edit Archive Location"       tla-archive-edit-archive-location t]
    ["Make New Archive..."         tla-make-archive t]
    ["Create a Mirror"             tla-archive-mirror-archive t]
    ["Use as default Mirror"       tla-archive-use-as-default-mirror t]
    ["Synchronize Mirror"          tla-archive-synchronize-archive t]
    ))

;;
;; Category list mode
;;
(easy-menu-define tla-category-list-mode-menu tla-category-list-mode-map
  "`tla-category-list-mode' menu"
  '("Categories"
    ["List Archives"          tla-archives                t]
    ["Update Categories List" tla-category-refresh         t]
    ["Make New Category..."   tla-category-make-category  t]
    ["Add a Bookmark"         tla-bookmarks-add           t]
    ["Synchronize Mirror"     tla-category-mirror-archive t]
    ))


;;
;; Branch list mode
;;
(easy-menu-define tla-branch-list-mode-menu tla-branch-list-mode-map
  "`tla-branch-list-mode' menu"
  '("Branches"
    ["Update Branches List" tla-branch-refresh               t]
    ["List Parent Category" tla-branch-list-parent-category t]
    ["Make New Branch..."   tla-branch-make-branch          t]
    ["Synchronize Mirror"   tla-branch-mirror-archive       t]
    ["Bookmark Branch under Point"    tla-branch-bookmarks-add        t]
    ["Get..."               tla-branch-get-branch           t]
    ))

;;
;; Version list mode
;;
(easy-menu-define tla-version-list-mode-menu tla-version-list-mode-map
  "`tla-version-list-mode' menu"
  '("Versions"
    ["Update Versions List" tla-version-refresh             t]
    ["Get..."               tla-version-get-version        t]
    ["Make New Version..."  tla-version-make-version       t]
    ["List Parent Branch"   tla-version-list-parent-branch t]
    ["Synchronize Mirror"   tla-version-mirror-archive     t]
    ["Bookmark Version under Point"    tla-version-bookmarks-add      t]
    ["Tag This Version"     tla-version-tag      t]))

;;
;; Revision list mode
;;
(easy-menu-define tla-revision-list-mode-menu tla-revision-list-mode-map
  "`tla-revision-list-mode' menu"
  '("Revisions"
    ["Refresh Revisions List" tla-generic-refresh t]
    ["List Parent Version"    tla-revision-list-parent-version t]
    ["Show all revisions" tla-missing-show-all-revisions t]
    "--"
    ["Bookmark Revision under Point"      tla-revision-bookmarks-add t]
    ("Mark"
     ["Mark Revision"   tla-revision-mark-revision t]
     ["Unmark Revision" tla-revision-unmark-revision t])
    "--"
    ["Show Log"                            tla-revision-cat-log t]
    ["Unify Patch Logs with This Revision" tla-revision-sync-tree t]
    ["View changeset"                      tla-revision-changeset t]
    ("Delta"
     ["View"  (tla-revision-delta t) t]
     ["Store to Directory" (tla-revision-store-delta t) t])
    "--"
    ["Update" tla-revision-update t]
    ("Replay"
     ["From Head Revision" tla-revision-replay-version t]
     ["From Revision under Point" tla-revision-replay t]
     ["Revision under Point Reversely" (tla-revision-replay 'reversely) t])
    ("Star-Merge"
     ["From Head Revision" tla-revision-star-merge-version t]
     ["From Revision under Point" tla-revision-star-merge t])
    ("Get"
     ["Get a Local Copy" tla-revision-get-revision t]
     ["Make Cache"       tla-revision-cache-revision t]
     ["Add to Library"   tla-revision-add-to-library t])
    ("Tag "
     ["From Head Revision" tla-revision-tag-from-head t]
     ["From Revision under Point" tla-revision-tag-from-here t])
    ["Send comment to author" tla-revision-send-comments t]
    "--"
    ("Filter Display"
     ["Date"    tla-revision-toggle-date
      :style toggle :selected tla-revisions-shows-date]
     ["Creator" tla-revision-toggle-creator
      :style toggle :selected tla-revisions-shows-creator]
     ["Summary" tla-revision-toggle-summary
     :style toggle :selected tla-revisions-shows-summary]
     ["Presence in Revlib" tla-revision-toggle-library
     :style toggle :selected tla-revisions-shows-library]
     ["Merged Patches"   tla-revision-toggle-merges
     :style toggle :selected tla-revisions-shows-merges]
     ["Patches Merging ..." tla-revision-toggle-merged-by
      :style toggle :selected tla-revisions-shows-merged-by])))

(easy-menu-define tla-revision-revision-menu nil
  "Menu used on a revision item in `tla-revision-list-mode' buffer"
  '("Revision"
     ["Show Log"        tla-revision-cat-log t]
     ["Unify Patch Logs with This Revision" tla-revision-sync-tree t]
     ["View changeset"  tla-revision-changeset t]
     ["Set Bookmark"    tla-revision-bookmarks-add t]
     ("Mark"
      ["Mark Revision"   tla-revision-mark-revision t]
      ["Unmark Revision"   tla-revision-unmark-revision t])
     ("Delta"
      ["In This Version"                     tla-revision-delta t]
      ["With Revision in Another Archive"    tla-revision-store-delta t])
     ("Merge"
      ["Star-Merge"       tla-revision-star-merge t]
      ["Replay"           tla-revision-replay t]
      ["Replay Reversely" (tla-revision-replay 'reversely) t])
     ("Get"
      ["Get a Local Copy" tla-revision-get-revision t]
      ["Make Cache"       tla-revision-cache-revision t]
      ["Add to Library"   tla-revision-add-to-library t])
     ["Send comment to author" tla-revision-send-comments t]
     ["Tag from Here"      tla-revision-tag-from-here]))

;;
;; Changes mode
;;
(defconst tla-changes-file-menu-list
  '("File Changes"
    ["Jump to File"                   tla-changes-jump-to-change t]
    ["Jump to Diffs"                  tla-changes-diff-or-list   t]
    ["View Diff in Separate Buffer"   tla-changes-diff           t]
    ["View Diff with Ediff"           tla-changes-ediff          t]
    "--"
    ["Delete File"                    tla-changes-rm             t]
    ["Revert File"                    tla-changes-revert         t]
    )
  "Used both in the global and the context menu of `tla-changes-mode'.")

(easy-menu-define tla-changes-mode-menu tla-changes-mode-map
  "`tla-changes' menu"
  `("Changes"
    ["Refresh Buffer" tla-generic-refresh t]
    ["Edit log before commit" tla-changes-edit-log t]
    ["View other revisions" tla-tree-revisions t]
    ,tla-changes-file-menu-list
    ))

(easy-menu-define tla-changes-file-menu nil
  "Menu used on a `tla-changes' file"
  tla-changes-file-menu-list)

;;
;; Lint mode
;;
(defconst tla-tree-lint-file-menu-list
  `("File"
    ["Jump to File"  tla-generic-find-file-at-point t]
    ("Mark"
     ["Mark File" tla-tree-lint-mark-file t]
     ["Unmark File" tla-tree-lint-unmark-file t])
    "--"
    ["Add File"      tla-tree-lint-add-files        t]
    ["Delete File"   tla-tree-lint-delete-files     t]
    ["Regenerate ID" tla-tree-lint-regenerate-id    t]
    "--"
    ["Make Junk"     tla-tree-lint-make-junk        t]
    ["Make Precious" tla-tree-lint-make-precious     t]
    ,tla-.arch-inventory-menu-list
    ,tla-=tagging-method-menu-list
    )
  "Used both for context and global menu.")

(easy-menu-define tla-tree-lint-file-menu nil
  "Menu used on files listed in `tla-tree-lint'"
  tla-tree-lint-file-menu-list
  )

(easy-menu-define tla-tree-lint-mode-menu tla-tree-lint-mode-map
  "`tla-tree-lint' menu"
  `("Tree Lint"
    ["Refresh Buffer"         tla-generic-refresh t]
    ,tla-tree-lint-file-menu-list
    ))

;;
;; Event Log buffer
;;
(easy-menu-define tla-log-buffer-mode-menu tla-log-buffer-mode-map
  "`tla-log-buffer' menu"
  '("Logs"
    ["Show Related Buffer" tla-switch-to-related-buffer t]
    ["Show Output Buffer"  tla-switch-to-output-buffer  t]
    ["Show Error Buffer"   tla-switch-to-error-buffer   t]
    ))


;; ----------------------------------------------------------------------------
;; User customization section
;; ----------------------------------------------------------------------------

;;;###autoload
(defgroup xtla nil
  "Arch interface for emacs."
  :group 'tools
  :prefix "tla-")

;;;###autoload
(defgroup tla-inventory nil
  "This group contains items used in inventory mode."
  :group 'xtla)

;;;###autoload
(defgroup tla-revisions nil
  "This group contains items used in revisions mode."
  :group 'xtla)

;;;###autoload
(defgroup tla-file-actions nil
  "This group contains items manipulating finding, saving and reverting files."
  :group 'xtla)

;;;###autoload
(defgroup tla-bindings nil
  "This group contains items related to key bindings."
  :group 'xtla)

;;;###autoload
(defgroup tla-faces nil
  "This group contains faces defined for Xtla."
  :group 'xtla)

;;;###autoload
(defcustom tla-prefix-key [(control x) ?T]
  "Prefix key for the Xtla commands in the global keymap."
  :type '(choice (const [(control x) ?T])
                 (const [(control x) ?t])
                 (const [(control x) ?v])
                 (const [(control x) ?V])
                 (const [(control x) ?v ?t])
                 (const [(super t)])
                 (const [(hyper t)])
                 (sexp))
  :group 'tla-bindings
  :set  (lambda (var value)
          (if (boundp var)
              (global-unset-key (symbol-value var)))
          (set var value)
          (global-set-key (symbol-value var) tla-global-keymap)))

;;;###autoload
(global-set-key tla-prefix-key tla-global-keymap)

;;;###autoload
(defcustom tla-tla-executable "tla"
  "*The name of the tla executable."
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-diff-executable "diff"
  "*The name of the diff executable."
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-patch-executable "patch"
  "*The name of the patch executable."
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-highlight t
  "*Use highlighting for tla buffers."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-install-command-help-system t
  "*Use f1 to display help for the actual function call during minibuffer input.
Note: this functionality is provided for all minibuffer prompts."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-do-not-prompt-for-save nil
  "*Whether or not xtla will prompt before saving.

If non nil, xtla will not prompt you before saving buffers of the
working local tree."
  :type 'boolean
  :group 'tla-file-actions)

;;;###autoload
(defcustom tla-automatically-revert-buffers t
  "*Whether or not xtla will automatically revert buffers.

If non nil, xtla will automatically revert unmodified buffers after an
arch operation modifying the file."
  :type 'boolean
  :group 'tla-file-actions)

;;;###autoload
(defcustom tla-changes-recursive t
  "*Whether or not xtla will compute changes recursively.

If non nil, `tla-changes' will be applied recursively to subprojects
of the current tree"
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-strict-commits nil
  "*If non-nil, commit operations are invoked with the --strict option."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-commit-check-log-buffer-functions
  '(tla-commit-check-empty-headers
    tla-commit-check-empty-line
    tla-commit-check-missing-space)
  "*List of functions to check the ++log.. buffer.

Each function is called, from the log buffer, with no argument. It
should raise an error if commit should be canceled."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-commit-headers-allowed-to-be-empty
  "^\\(Keywords\\)$"
  "*Headers allowed to be empty in the ++log.. buffer.

This should be a regexp matching the header names. Headers not
matching this regexp should not be empty when committing."
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-commit-fix-missing-space t
  "*Whether or not xtla will add missing spaces after header names.

If non-nil, missing spaces after a space will be inserted
automatically instead of raising an error when committing."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-three-way-merge t
  "*If non-nil, star-merge operations are invoked with --three-way."
  :type 'boolean
  :group 'xtla)

;; --forward is actually a no-op !
;; ;;;###autoload
;; (defcustom tla-use-forward-option nil
;;   "*If non-nil, use the --forward option with commands that allow it."
;;   :type 'boolean
;;   :group 'xtla)

;;;###autoload
(defcustom tla-use-skip-present-option nil
  "*If non-nil, use --skip-present with commands that allow it."
  :type 'boolean
  :group 'xtla)

;; ;;;###autoload
;; (defun tla-toggle-use-forward-option ()
;;   "Toggle the value of `tla-use-forward-option'."
;;   (interactive)
;;   (setq tla-use-forward-option (not tla-use-forward-option)))

;;;###autoload
(defun tla-toggle-use-skip-present-option ()
  "Toggle the value of `tla-use-skip-present-option'."
  (interactive)
  (setq tla-use-skip-present-option
        (not tla-use-skip-present-option)))

;;;###autoload
(defun tla-toggle-three-way-merge ()
  "Toggle the value of `tla-three-way-merge'."
  (interactive)
  (setq tla-three-way-merge (not tla-three-way-merge)))

;;;###autoload
(defgroup tla-bookmarks nil
  "xtla bookmarks allows you to save places (archive, category,
branch, version) in the archive that you use often. Try M-x
tla-bookmarks RET to see."
  :group 'xtla)

;;;###autoload
(defcustom tla-bookmarks-file-name "bookmarks.el"
  "*File in which xtla bookmarks will be saved.
The bookmark file is stored in the `tla-config-directory'"
  :type 'file
  :group 'tla-bookmarks)

;;;###autoload
(defcustom tla-tag-function 'tla-tag-uuid
  "Function called to generate the value of the arch-tag.

The function must take no argument, and return a string without a
final newline."
  :type '(choice (const tla-tag-uuid)
                 (const tla-tag-name-date-filename)
                 function)
  :group 'xtla)

;;;###autoload
(defcustom tla-config-directory "~/.xtla/"
  "*Directory in which the xtla config files will be stored."
  :type 'directory
  :group 'xtla)

;;;###autoload
(defcustom tla-log-library "~/.arch-log-library/"
  "*Directory in which the log library will be stored."
  :type 'directory
  :group 'tla-internal)

;;;###autoload
(defcustom tla-log-library-greedy t
  "*Whether log files are automatically saved in the log library.

If non-nil, then, whenever xtla needs to access a log file, this file
will be copied to the log library."
  :type 'boolean
  :group 'tla-internal)

;;;###autoload
(defcustom tla-cache-tla-get-changeset 2
  "*Cache `tla-get-changeset' calls.
When nil, don't cache.
When a number, cache only if the `tla-get-changeset' call takes
more seconds than the number.
Otherwise don't cache the results.
The cache is kept only in RAM."
  :type 'integer
  :group 'xtla)

;;;###autoload
(defcustom tla-bookmarks-cleanup-dont-prompt nil
  "*Whether xtla should prompt before cleaning a local tree.

non nil means `tla-bookmarks-cleanup-local-trees' shouldn't prompt
before removing a local-tree"
  :type 'boolean
  :group 'tla-bookmarks)

;;;###autoload
(defgroup tla-internal nil
  "This group contains items used mainly for debugging."
  :group 'xtla)

;;;###autoload
(defcustom tla-log-commands t
  "*Non nil means log all tla commands in the buffer *tla-log*."
  :type 'boolean
  :group 'tla-internal)

;;;###autoload
(defcustom tla-log-buffer " *tla-log*"
  "*Name of the buffer in which xtla logs main events."
  :type 'string
  :group 'tla-internal)

;;;###autoload
(defcustom tla-switch-to-buffer-mode 'pop-to-buffer
  "*Mode for switching to xtla buffers.
Recommended settings are: 'pop-to-buffer, and 'show-in-other-window
and 'single-window"
  :type '(choice (const pop-to-buffer)
                 (const single-window)
                 (const dedicated-frame)
                 (const show-in-other-window))
  :group 'xtla)

(defcustom tla-switch-to-changes-buffer nil
  "Switch to the changes buffer or stay in the current buffer."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-read-project-tree-mode 'sometimes
  "*Mode for prompting project tree directories. Possible values are:

- always: When running a tla command requiring a tree, always prompt
  for it.

- sometimes: If a command is ran inside a project tree, the tree root
  is used. Otherwise, prompt.

- never: If a command is run inside a project tree, use the tree root.
  Othwise, raise an error."
  :type '(choice (const always)
                 (const sometimes)
                 (const never))
  :group 'xtla)

;;;###autoload
(defcustom tla-read-directory-mode 'sometimes
  "*How prompting project directories should be done.

Works similarly to `tla-read-project-tree-mode', but this one is used
for commands like `tla-inventory' for which a subdirectory of a
project tree is accepted."
  :type '(choice (const always)
                 (const sometimes)
                 (const never))
  :group 'xtla)

;;;###autoload
(defcustom tla-switch-to-buffer-first t
  "*Switch to newly created buffer on creation of buffers?

If non-nil, xtla commands implementing this feature will switch to the
newly created buffer when the command is called. Further (potentially
asynchronous) processes are run without modifying your
window-configuration. Otherwise, xtla will switch to the new buffer on
command completion."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-buffer-quit-mode 'kill
  "*How *tla-...* buffer should be killed.
If the value is 'kill, buffers are actually killed. Otherwise, just
burry them."
  :type '(choice (const kill)
                 (const burry))
  :group 'xtla)

;;;###autoload
(defcustom tla-log-insert-last t
  "*If non-nil, insert changelog entries at the end of the log file."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defgroup tla-hooks nil
  "This group contains hooks into xtla."
  :group 'xtla)

;;;###autoload
(defcustom tla-commit-done-hook '()
  "*Hooks run after a successful commit via `tla-commit'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-archive-list-mode-hook '()
  "*Hooks run after switching to `tla-archive-list-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-bookmarks-mode-hook '()
  "*Hooks run after switching to `tla-bookmarks-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-branch-list-mode-hook '()
  "*Hooks run after switching to `tla-branch-list-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-cat-log-mode-hook '()
  "*Hooks run after switching to `tla-cat-log-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-category-list-mode-hook '()
  "*Hooks run after switching to `tla-category-list-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-inventory-file-mode-hook '()
  "*Hooks run after switching to `tla-inventory-file-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-inventory-mode-hook '()
  "*Hooks run after switching to `tla-inventory-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-log-edit-mode-hook '()
  "*Hooks run after switching to `tla-log-edit-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-logs-mode-hook '()
  "*Hooks run after switching to `tla-logs-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-revision-list-mode-hook '()
  "*Hooks run after switching to `tla-revision-list-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-version-list-mode-hook '()
  "*Hooks run after switching to `tla-version-list-mode'."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-make-branch-hook '()
  "*Hooks run after making a branch."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-make-category-hook '()
  "*Hooks run after making a category."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-make-version-hook '()
  "*Hooks run after making a version."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-make-archive-hook '()
  "*Hooks run after creating a new archive."
  :type 'hook
  :group 'tla-hooks)

;;;###autoload
(defcustom tla-name-read-init-hook '()
  "*Hooks run when the control enters to `tla-name-read'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-name-read-final-hook '()
  "*Hooks run when the control leaves `tla-name-read'.
The name read by `tla-name-read' is passed to functions connected
to this hook as an argument."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-name-read-error-hook '()
  "*Hooks run when an error is occurred in `tla-name-read'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-follow-symlinks 'tree
  "*Follow symlinks of this type."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Symlinks into an arch-managed tree" tree)
                 (const :tag "Symlinks to an arch-managed file" id))
  :group 'tla-file-actions)

(defcustom tla-follow-symlinks-mode 'follow
  "*Before following a symlink do this."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Follow" follow)
                 (const :tag "Warn" warn))
  :group 'tla-file-actions)

(defcustom tla-use-arrow-keys-for-navigation nil
  "*Enable left/right for navigation.
This works best if `tla-switch-to-buffer-mode' is set to 'single-window.

It enables binding for navigation allowing you to browse by only using the
cursor keys, which is much faster than n/p/return/^.  Use up/down to move to
an item, right to select it and left to go to its \"logical\" parent!

Got the idea?

See the variable `tla-use-arrow-keys-for-navigation-list' for a list of
bindings that will be installed."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled"  t)
                 (const :tag "Enabled with Shift" shift))
  :group 'tla-bindings)

(defcustom tla-revisions-shows-summary t
  "*Whether summary should be displayed for `tla-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-creator t
  "*Whether creator should be displayed for `tla-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-date t
  "*Whether date should be displayed for `tla-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-library t
  "*Display library information in revision lists.

If non-nil the presence of this revision in the library should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-merges nil
  "*Display merge information in revision lists.

If non-nil, the list of merged patches of this revision should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-merged-by t
  "*Display \"merged-by\" field in revision lists.

If non-nil the list of patches merged by this revision should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

;;;###autoload
(defcustom tla-log-edit-keywords
  '(
    ;; I am not sure how to group keywords ...
    "bugfix"                            ; should it be bugfix=BUGNO
    "docfix"
    "warnfix"
    "linting"                           ; whitespace only change
    ;;
    "newfeature"
    ;;
    "merge"
    "update"
    "rename"
    "delete"
    "newfile"
    )
  "A list of keywords for the Keywords field of a log message."
  :type '(repeat (string))
  :group 'xtla)

;;;###autoload
(defcustom tla-apply-patch-mapping nil
  "*Tree in which patches should be applied.

An alist of rules to match fully qualified revision names to target
directories.

That variable is used to offer a directory in `tla-gnus-apply-patch'.
Example setting: '(((nil \"xtla\" nil nil nil) \"~/work/tla/xtla\")))"
  :type '(repeat (cons :tag "Rule"
                       (cons :tag "Full revision (regexps)"
                        (choice (const nil) (regexp :tag "Archive"))
                        (cons
                         (choice (const nil) (regexp :tag "Category"))
                         (cons
                          (choice (const nil) (regexp :tag "Branch"))
                          (cons
                           (choice (const nil) (regexp :tag "Version"))
                           (cons (choice (const nil) (string :tag "Revision"))
                                 (const nil))))))
                  (cons (string :tag "tree") (const nil))))
  :group 'xtla)

;;
;; Tips
;;
;;;###autoload
(defgroup tla-tips nil
  "\"Tip of the day\" feature for Xtla"
  :group 'xtla)

;;;###autoload
(defcustom tla-tips-enabled t
  "*Set this to nil to disable tips."
  :type 'boolean
  :group 'tla-tips)

;;
;; State
;;
;;;###autoload
(defgroup tla-state nil
  "Saving Xtlas state between Emacs sessions."
  :group 'xtla)

;;;###autoload
(defcustom tla-state-file-name "state.el"
  "*File in which xtla saves state variables between Emacs sessions.
The file is stored in the `tla-config-directory'"
  :type 'file
  :group 'tla-state)

;;;###autoload
(defcustom tla-state-variables-list '(tla-tips-number)
  "*List of variables to store in the state file `tla-state-file-name'."
  :type '(repeat (symbol))
  :group 'tla-state)

;;;###autoload
(defgroup tla-merge nil
  "Merging with Xtla."
  :group 'xtla)

;;;###autoload
(defcustom tla-version-to-name-function nil
  "*Function returning a name for a version.

If non-nil, it must be a function that is called with the version as
an argument, and must return a string that will be used to instead of
the nickname.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)

;;;###autoload
(defcustom tla-generate-line-function nil
  "*Function generating a string summarizing the merge.

If non-nil, it must be a function that is called with a list like
\((\"Robert\" 167 168 170) (\"Masatake\" 209 213 214 215 217 218)) as
an argument, and must return a string.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)


;;;###autoload
(defcustom tla-format-line-function nil
  "*Function formatting the summary line.

If non-nil, it must be a function that is called with a string as an
argument, and returns another string (typically adding a \"Merges \"
comment in front of it.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)


;; ----------------------------------------------------------------------------
;; Face
;; ----------------------------------------------------------------------------
;;;###autoload
(defface tla-marked
  '((((type tty) (class color)) (:foreground "magenta" :weight light))
    (((class color) (background light)) (:foreground "magenta"))
    (((class color) (background dark)) (:foreground "yellow"))
    (t (:weight bold)))
  "Face to highlight a marked entry in xtla buffers"
  :group 'tla-faces)

;;;###autoload
(defface tla-archive-name
  '((((type tty) (class color)) (:foreground "lightblue" :weight light))
    (((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "lightskyblue1"))
    (t (:weight bold)))
  "Face to highlight xtla archive names."
  :group 'tla-faces)

;;;###autoload
(defface tla-source-archive-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla source archive names."
  :group 'tla-faces)

;;;###autoload
(defface tla-mirror-archive-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla mirror archive names."
  :group 'tla-faces)

;;;###autoload
(defface tla-category-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla category names."
  :group 'tla-faces)

;;;###autoload
(defface tla-branch-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla branch names."
  :group 'tla-faces)

;;;###autoload
(defface tla-version-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla version names."
  :group 'tla-faces)

;;;###autoload
(defface tla-revision-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla revision names."
  :group 'tla-faces)

;;;###autoload
(defface tla-local-directory
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla local directory."
  :group 'tla-faces)

;;;###autoload
(defface tla-buffer
  '((t (:inherit tla-archive-name)))
  "Face to highlight buffer names printed in xtla's buffer."
  :group 'tla-faces)

;;;###autoload
(defface tla-tagging-method
  '((t (:inherit tla-archive-name)))
  "Face to highlight taggine methods."
  :group 'tla-faces)

;;;###autoload
(defface tla-bookmark-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla revision names."
  :group 'tla-faces)

;;;###autoload
(defface tla-id
  '((t (:inherit tla-keyword)))
  "Face to highlight an arch id."
  :group 'tla-faces)

;;;###autoload
(defface tla-separator
  '((((type tty)) (:underline t :weight bold))
    ;(((background light)) (:strike-through t :weight bold))
    ;(((background dark))  (:strike-through t :weight bold)))
    (((background light)) (:underline t :weight bold))
    (((background dark))  (:underline t :weight bold)))
  "Face to highlight separators."
  :group 'tla-faces)

;;;###autoload
(defface tla-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight keywords."
  :group 'tla-faces)

;;;###autoload
(defface tla-comment
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight comments."
  :group 'tla-faces)

;;;###autoload
(defface tla-precious
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight precious entries"
  :group 'tla-faces)

;;;###autoload
(defface tla-unrecognized
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight unrecognized entries"
  :group 'tla-faces)

;;;###autoload
(defface tla-duplicate
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight files with duplicate IDs"
  :group 'tla-faces)

;;;###autoload
(defface tla-source
  '((t (:inherit font-lock-string-face)))
  "Face to highlight source code entries"
  :group 'tla-faces)

;;;###autoload
(defface tla-junk
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight junk entries"
  :group 'tla-faces)

;;;###autoload
(defface tla-nested-tree
  '((t (:inherit font-lock-type-face)))
  "Face to highlight nested trees"
  :group 'tla-faces)

;;;###autoload
(defface tla-to-add
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight a file that should probably be added to the
archive"
  :group 'tla-faces)

;;;###autoload
(defface tla-broken-link
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight a broken link"
  :group 'tla-faces)

;;;###autoload
(defface tla-unmerged
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight unmerged patches"
  :group 'tla-faces)

;;;###autoload
(defface tla-header
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight header in log mode for example"
  :group 'tla-faces)

;;;###autoload
(defface tla-conflict
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight conflicts"
  :group 'tla-faces)

;;;###autoload
(defface tla-modified
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight modified files"
  :group 'tla-faces)

;;;###autoload
(defface tla-move
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight moved files/directory"
  :group 'tla-faces)

;;;###autoload
(defface tla-deleted
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight deleted files"
  :group 'tla-faces)

;;;###autoload
(defface tla-added
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight added files"
  :group 'tla-faces)

;;;###autoload
(defface tla-meta-info
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight files with meta-info changes"
  :group 'tla-faces)

;;;###autoload
(defface tla-messages
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight messages in tla buffers"
  :group 'tla-faces)

;;;###autoload
(defface tla-highlight
  '((((class color) (background dark)) (:background "darkblue"))
    (((class color) (background light)) (:background "gold")))
  "Face to use as an alternative to `highlight' face.
If there could be more than two highlighted things, the user will confuse.
In such case use this face."
  :group 'tla-faces)

;;;###autoload
(defface tla-mark
  '((((class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:foreground "green3" :bold t))
    (t (:bold t)))
  "Xtla face used to highlight marked file indicator."
  :group 'tla-faces)


;; ----------------------------------------------------------------------------
;; Font lock keywords
;; ----------------------------------------------------------------------------

;;
;; Inventory file mode
;;
(defvar tla-inventory-file-font-lock-keywords
  '(
    ("^#.*$" . 'tla-comment)
    ("^[ \t]*\\(backup\\|exclude\\|junk\\|precious\\|unrecognized\\|source\\)\\>[  ]*\\(.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^[ \t]*\\(untagged-source\\)"
     (1 font-lock-builtin-face))
    ("^[ \t]*\\(untagged-source\\) \\(precious\\|source\\|backup\\|junk\\|unrecognized\\)\\>"
     (1 font-lock-builtin-face)
     (2 font-lock-keyword-face))
    ("^[ \t]*\\(explicit\\|tagline\\|names\\|implicit\\)\\>"
     (1 font-lock-builtin-face))
    )
  "Keywords in tla-inventory-file mode.")

;;
;; Logs mode
;;
(defvar tla-logs-font-lock-keywords
  '(("^[^ \t]*\\(base\\|patch\\|version\\(fix\\)?\\)-[0-9]+" .
     font-lock-function-name-face))
  "Keywords in tla-logs-mode.")

;;
;; Cat-Log mode
;;
(defvar tla-cat-log-font-lock-keywords
  '(("^\\(Revision\\|Archive\\|Creator\\|Date\\|Standard-date\\|Modified-files\\|New-patches\\|Summary\\|Keywords\\|New-files\\|New-directories\\|Removed-files\\|Removed-directories\\|Renamed-files\\|Renamed-directories\\|Modified-directories\\|Removed-patches\\):" . font-lock-function-name-face))
  "Keywords in `tla-cat-log-mode'.")

;;
;; Log edit mode
;;
(defvar tla-log-edit-font-lock-keywords
  '(("^Summary: " . 'tla-header)
    ("^Keywords: " . 'tla-header)
    ("^\t?\\* \\([^ ,:([\n]+\\)"
     (1 'change-log-file-face)
     ("\\=, \\([^ ,:([\n]+\\)" nil nil
      (1 'change-log-file-face))
     ("\\= (\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face))
     ("\\=, *\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face))))
  "Keywords in tla-log-edit mode.")

;;
;; Changes mode
;;
(defvar tla-changes-font-lock-keywords
  (append
   '(("^\\* looking for .* to compare with$" . font-lock-function-name-face)
     ("^\\* comparing to .*$" . font-lock-function-name-face)
     ("^\\* dir metadata changed$" . font-lock-function-name-face)
     ("^\\* file metadata changed$" . font-lock-function-name-face)
     ("^\\* modified files" . font-lock-function-name-face)
     ("^\\* added files" . font-lock-function-name-face)
     ("^\\* removed files" . font-lock-function-name-face)
     ("^ +-?-/ .*$" . 'tla-meta-info)
     ("^ +-- .*$" .   'tla-meta-info)
     ("^ *T. .*$" .  'tla-nested-tree))
   diff-font-lock-keywords)
  "Keywords in `tla-changes' mode.")

;;
;; ChangeLog mode
;;
(defvar tla-changelog-font-lock-keywords
  (append
   '(("    \\([^ ].+:\\)$" (1 'tla-keyword))
     ("\t\\(patch-[0-9]+\\)" (1 'tla-keyword))
     ("^#.*$" . 'tla-comment))
   change-log-font-lock-keywords)
  "Keywords in `tla-changelog' mode.")

;;
;; Tips mode
;;
(defvar tla-tips-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map tla--keyvec-next 'tla-tips-next-tip)
    (define-key map tla--keyvec-previous 'tla-tips-previous-tip)
    (define-key map [?c] 'tla-tips-customize)
    map))


;; ----------------------------------------------------------------------------
;; Auto-mode-alist entries
;; ----------------------------------------------------------------------------
;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\(=tagging-method\\|\\.arch-inventory\\)$" .
               tla-inventory-file-mode))

;; ----------------------------------------------------------------------------
;; Hooks into other packages and/or functions
;; ----------------------------------------------------------------------------

;;
;; ediff
;;
(defvar tla-ediff-keymap (copy-keymap tla-global-keymap)
  "Global keymap used by Xtla in the ediff control buffer.")

(define-key tla-ediff-keymap tla--keyvec-log-entry 'tla-ediff-add-log-entry)

(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (define-key ediff-mode-map tla-prefix-key tla-ediff-keymap)))

;;
;; find-file
;;
(add-hook 'find-file-hooks 'tla-find-file-hook)

;; ----------------------------------------------------------------------------
;; Enables arrow key navigation for left/right
;; ----------------------------------------------------------------------------
(defvar tla-use-arrow-keys-for-navigation-list
  '((tla-inventory-mode-map right 'tla-inventory-find-file)
    (tla-inventory-mode-map left 'tla-inventory-parent-directory)
    (tla-archive-list-mode-map right 'tla-archive-list-categories)
    (tla-archive-list-mode-map left 'tla-buffer-quit)
    (tla-category-list-mode-map right 'tla-category-list-branches)
    (tla-category-list-mode-map left 'tla-archives)
    (tla-branch-list-mode-map right 'tla-branch-list-versions)
    (tla-branch-list-mode-map left 'tla-branch-list-parent-category)
    (tla-version-list-mode-map right 'tla-version-list-revisions)
    (tla-version-list-mode-map left 'tla-version-list-parent-branch)
    (tla-revision-list-mode-map left 'tla-revision-list-parent-version)
    (tla-revision-list-mode-map right 'tla-revision-show-changeset)
    (tla-changes-mode-map left 'tla-changes-jump-to-change)
    (tla-changes-mode-map right 'tla-changes-view-source)
    (tla-changelog-mode-map left 'tla-buffer-quit)
    (tla-process-buffer-mode-map left 'tla-buffer-quit)
    (tla-bookmarks-mode-map right 'tla-bookmarks-inventory)
    ))

(defun tla-use-arrow-keys-for-navigation (&optional uninstall)
  "Bind the left/right keys for navigation.

This function will be called automatically if variable
`tla-use-arrow-keys-for-navigation' is non-nil.

If argument UNINSTALL is non-nil, undefine the keys instead of
defining it."
  (interactive "P")
  (let ((bl tla-use-arrow-keys-for-navigation-list) b
        (m tla-use-arrow-keys-for-navigation))
    (while bl
      (setq b (car bl)
            bl (cdr bl))
      (eval
       (append (list 'define-key
                     (car b))
               (cond ((eq nil m)
                      (list (vector (cadr b)) nil))
                     ((eq 'shift m)
                      (if uninstall
                          (list (vector (list 'shift (cadr b))) nil)
                        (list (vector (list 'shift (cadr b))) (caddr b))))
                     ((eq t m)
                      (if uninstall
                          (list (vector (cadr b)) nil)
                        (list (vector (cadr b)) (caddr b))))))))
    (if uninstall
        (message "%sleft/right bindings for xtla have been removed."
                 (if (eq 'shift m) "Shifted " ""))
      (message "%sleft/right bindings for xtla have been installed."
               (if (eq 'shift m) "Shifted " "")))))

;; install them if customized
(if tla-use-arrow-keys-for-navigation
    (tla-use-arrow-keys-for-navigation))

(provide 'xtla-defs)

;; arch-tag: fb2c4733-b5da-4d02-8a27-5e3eaa090442
;;; xtla-defs.el ends here
