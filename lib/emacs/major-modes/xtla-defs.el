;;; xtla-defs.el --- UI xtla's element definitions

;; Copyright (C) 2003-2004 by Stefan Reichoer

;; Author: Stefan Reichoer, <xsteve@nit.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; This file is part of xtla.
;;
;; xtla is free software; you can redistribute it and/or modify
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

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'diff-mode)
  (require 'ediff)
  (require 'add-log))

;;;###autoload
(eval-and-compile
  (require 'easymenu))

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
;;    So `? >' can stand for "show thehelp for get-something related 
;;    command". In other word, prefix and suffix is swappable if
;;    prefix or suffix is `?'.
;;
;; 3. Upper case for commands taking longer time to be executed.
;; 4. Lowwer case for commands taking shorter time to be executed.
;; 5. dired's binding is also helpful. 
;; 
;;
;; - Concrete Rules:
;;

;; t  ?    list all toggles
;; >       get something
;; c       tla-edit-log
;; e       call ediff
;; RET     Open the thing at point
;; i       Show tla inventory
;;

;;
;; Definitions for key concrete rules
;;

;; common keys
(defvar tla--key-toggle   ?t)		; prefix for toggle
(defvar tla--key-help     ??)		; help
(defvar tla--key-get      ?>)		; prefix for getting something
(defvar tla--key-add      ?+)		; prefix for adding something
(defvar tla--key-remove   ?-)		; prefix for remove something
(defvar tla--key-refresh  ?g)		; update or refresh buffer
(defvar tla--key-parent   ?^)		; visit uppper XXX. e.g. directory
(defvar tla--key-next     ?n)		; next item
(defvar tla--key-previous ?p)		; previous item
(defvar tla--key-quit     ?q)		; quit

;; functions for creating key groups
(defun  tla--prefix-toggle (&rest keys)
  (apply 'vector tla--key-toggle keys))

(defun tla--prefix-add (&rest keys)
  (apply 'vector tla--key-add keys))

(defun tla--prefix-remove (&rest keys)
  (apply 'vector tla--key-remove keys))

;; pre defined key vectors
(defvar tla--keyvec-toggle-set     (tla--prefix-toggle ?+))
(defvar tla--keyvec-toggle-reset   (tla--prefix-toggle ?-))
(defvar tla--keyvec-toggle-invert  (tla--prefix-toggle ?~))

(defvar tla--keyvec-help   (vector tla--key-help))
(defvar tla--keyvec-parent (vector tla--key-parent))

(defvar tla--keyvec-refresh (vector tla--key-refresh))

(defvar tla--keyvec-next     (vector tla--key-next))
(defvar tla--keyvec-previous (vector tla--key-previous))

(defvar tla--keyvec-quit (vector tla--key-quit))

;;
;; Global
;;
;;;###autoload
(defvar tla-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] 'tla-add-log-entry)
    (define-key map [?A] 'tla-archives)
    (define-key map [?b] 'tla-bookmarks)
    (define-key map [?d] 'tla-file-diff)
    (define-key map [?e] 'tla-file-ediff)
    (define-key map [?o] 'tla-file-view-original)
    (define-key map [?c] 'tla-changes)
    (define-key map [?C] 'tla-edit-log)
    (define-key map [?t] 'tla-insert-arch-tag)
    (define-key map [?i] 'tla-inventory)
    (define-key map [?r] 'tla-tree-revisions)
    (define-key map [?l] 'tla-tree-lint)
    (define-key map [?u] 'tla-update)
    map)
  "Global keymap used by Xtla")

;;;###autoload
(define-key ctl-x-4-map [?T] 'tla-add-log-entry)

;;
;; Bookmarks mode
;;
(defvar tla-bookmarks-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'describe-mode)
    (define-key map "\C-m" 'tla-bookmarks-goto)
    (define-key map [?S] 'tla-bookmarks-star-merge)
    (define-key map [?n] 'tla-bookmarks-next)
    (define-key map [?p] 'tla-bookmarks-previous)
    (define-key map [?N] 'tla-bookmarks-move-down)
    (define-key map [?P] 'tla-bookmarks-move-up)
    (define-key map [?M] 'tla-bookmarks-missing)
    (define-key map [?m] 'tla-bookmarks-mark)
    (define-key map [(meta p)] 'tla-bookmarks-marked-are-partners)
    (define-key map "\M-\C-?" 'tla-bookmarks-unmark-all)
    (define-key map [?* ?!] 'tla-bookmarks-unmark-all)
    (define-key map [?u] 'tla-bookmarks-unmark)
    (define-key map [?a] 'tla-bookmarks-add)
    (define-key map [?e] 'tla-bookmarks-edit)
    (define-key map [?d] 'tla-bookmarks-delete)
    (define-key map [?o] 'tla-bookmarks-open-tree)
    (define-key map [?i] 'tla-bookmarks-inventory)
    (define-key map [?q] 'tla-buffer-quit)
    (define-key map [?t] 'tla-bookmarks-toggle-details)
    (define-key map [?+ ?b] 'tla-bookmarks-add)
    (define-key map [?+ ?t] 'tla-bookmarks-add-tree-interactive)
    (define-key map [?- ?t] 'tla-bookmarks-delete-tree-interactive)
    (define-key map [?+ ?p] 'tla-bookmarks-add-partner-interactive)
    (define-key map [?- ?p] 'tla-bookmarks-delete-partner-interactive)
    (define-key map [?+ ?g] 'tla-bookmarks-add-group-interactive)
    (define-key map [?- ?g] 'tla-bookmarks-delete-group-interactive)
    (define-key map [?* ?g] 'tla-bookmarks-select-by-group)
    (define-key map [?+ ?n] 'tla-bookmarks-add-nickname-interactive)
    (define-key map [?- ?n] 'tla-bookmarks-delete-nickname-interactive)
    (define-key map [?>] 'tla-bookmarks-get)
    map)
  "Keymap used in tla-bookmarks-mode buffers.")

;;
;; Inventory mode
;;
(defvar tla-inventory-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help    'describe-mode)
    (define-key map tla--keyvec-refresh 'tla-generic-refresh)
    (define-key map (tla--prefix-add ?f)    'tla-inventory-add)
    (define-key map (tla--prefix-remove ?f) 'tla-inventory-remove)
    (define-key map [?X] 'tla-inventory-delete)
    (define-key map [?R] 'tla-inventory-move)
    (define-key map [?e] 'tla-inventory-file-ediff)
    (define-key map [?c] 'tla-inventory-edit-log) ;; mnemonic for commit
    (define-key map [?f] 'tla-inventory-find-file)
    (define-key map [return] 'tla-inventory-find-file)
    (define-key map "\C-m" 'tla-inventory-find-file)
    (define-key map [?o] 'tla-inventory-find-file-other-window)
    (define-key map [?v] 'tla-inventory-view-file)
    ;;  (define-key map [?d ?e] 'tla-inventory-file-ediff)
    (define-key map [?d ?m] 'tla-inventory-missing)
    (define-key map [?=] 'tla-inventory-changes)
    (define-key map [?l] 'tla-changelog)
    (define-key map [?L] 'tla-logs)
    (define-key map [?M] 'tla-inventory-mirror)
    (define-key map [?S] 'tla-inventory-star-merge)
    (define-key map [?A] 'tla-show-process-buffer)
    (define-key map tla--keyvec-quit 'tla-buffer-quit)
    (define-key map tla--keyvec-next 'tla-inventory-next)
    (define-key map tla--keyvec-previous 'tla-inventory-previous)
    (define-key map tla--keyvec-parent 'tla-inventory-parent-directory)
    (define-key map [?m] 'tla-inventory-mark-file)
    (define-key map [?u] 'tla-inventory-unmark-file)
    (define-key map [?* ?!] 'tla-inventory-unmark-all)
    (define-key map tla--keyvec-toggle-set    'tla-inventory-set-all-toggle-variables)
    (define-key map tla--keyvec-toggle-reset  'tla-inventory-reset-all-toggle-variables)
    (define-key map tla--keyvec-toggle-invert 'tla-inventory-toggle-all-toggle-variables)
    map)
  "Keymap used in tla-inventory-mode buffers.")

(defconst tla-inventory-file-types-manipulators
  '((?S tla-inventory-display-source
        tla-inventory-toggle-source ?s "source")
    (?P tla-inventory-display-precious
        tla-inventory-toggle-precious ?p "precious")
    (?J tla-inventory-display-junk
        tla-inventory-toggle-junk ?j "junk")
    (?T tla-inventory-display-tree
        tla-inventory-toggle-tree ?t "tree root")
    (?U tla-inventory-display-unrecognized
        tla-inventory-toggle-unrecognized ?u "unrecognized"))
  "List of possible file types in inventory")

(dolist (type-arg tla-inventory-file-types-manipulators)
  (define-key tla-inventory-mode-map `[?t ,(cadddr type-arg)] (caddr type-arg)))


;;
;; Logs mode
;;
(defvar tla-logs-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'describe-mode)
    (define-key map [?i] 'tla-pop-to-inventory)
    (define-key map [?d] 'tla-logs-toggle-date)
    (define-key map [?c] 'tla-logs-toggle-creator)
    (define-key map [?s] 'tla-logs-toggle-summary)
    (define-key map [?r] 'tla-logs-toggle-reverse)
    (define-key map [up] 'tla-logs-prev-revision)
    (define-key map [down] 'tla-logs-next-revision)
    (define-key map "\C-m" 'tla-logs-cat-log)
    (define-key map [return] 'tla-logs-cat-log)
    (define-key map [?q] 'tla-buffer-quit)
    map)
  "Keymap used in tla-logs-mode buffers.")

;;
;; Cat-Log mdoe
;;
(defvar tla-cat-log-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'describe-mode)
    (define-key map [?i] 'tla-pop-to-inventory)
    (define-key map [?q] 'tla-buffer-quit)
    map)
  "Keymap used in tla-cat-log-mode buffers.")

;;
;; Log edit mode
;;
(defvar tla-log-edit-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'tla-log-edit-done)
    (define-key map [(control ?c) (control ?d)] 'tla-changes)
    (define-key map [(control ?c) (control ?l)] 'tla-changelog)
    (define-key map [(control ?c) (control ?m)] 'tla-log-edit-insert-log-for-merge)
    (define-key map [(control ?c) (control ?q)] 'tla-log-edit-abort)
    (define-key map [(control ?c) (control ?s)] 'tla-log-goto-summary)
    (define-key map [(control ?c) (control ?b)] 'tla-log-goto-body)
    (define-key map "\t" 'tla-log-edit-next-field)
    map)
  "Keymap used in tla-log-edit-mode buffers.")

;;
;; Archive list mode
;;
(defvar tla-archive-list-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-archive-list-categories)
    (define-key map [return] 'tla-archive-list-categories)
    (define-key map [?A] 'tla-show-process-buffer)
    (define-key map [?o] 'tla-archive-browse-archive)
    (define-key map [?*] 'tla-archive-select-default)
    (define-key map [?+ ?r] 'tla-register-archive)
    (define-key map [?+ ?a] 'tla-make-archive)
    (define-key map [?+ ?m] 'tla-archive-mirror-archive)
    (define-key map [?d] 'tla-archive-unregister-archive)
    (define-key map [?g] 'tla-archives)
    (define-key map [?s] 'tla-archive-synchronize-archive)
    (define-key map [down] 'tla-archive-next)
    (define-key map [up] 'tla-archive-previous)
    (define-key map [?n] 'tla-archive-next)
    (define-key map [?p] 'tla-archive-previous)
    (define-key map [?q] 'tla-buffer-quit)
    map)
  "Keymap used in tla-archive-list-mode buffers.")

;;
;; Category list mode
;;
(defvar tla-category-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-category-list-branches)
    (define-key map [return] 'tla-category-list-branches)
    (define-key map [?^] 'tla-archives)
    (define-key map [?+ ?c] 'tla-category-make-category)
    (define-key map [?g] 'tla-category-update)
    (define-key map [?s] 'tla-category-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map [?q] 'tla-buffer-quit)
    map)
  "Keymap used in tla-category-list-mode buffers.")

;;
;; Branch list mode section
;;
(defvar tla-branch-list-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-branch-list-versions)
    (define-key map [return] 'tla-branch-list-versions)
    (define-key map [?^] 'tla-branch-list-parent-category)
    (define-key map [?+ ?b] 'tla-branch-make-branch)
    (define-key map [?>] 'tla-branch-get-branch)
    (define-key map [?g] 'tla-branch-update)
    (define-key map [?s] 'tla-branch-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map [?q] 'tla-buffer-quit)
    (define-key map [?.] 'tla-branch-bookmarks-add)
    map)
  "Keymap used in tla-branch-list-mode buffers.")

;;
;; Version list mode
;;
(defvar tla-version-list-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-version-list-revisions)
    (define-key map [return] 'tla-version-list-revisions)
    (define-key map [?^] 'tla-version-list-parent-branch)
    (define-key map [?+ ?v] 'tla-version-make-version)
    (define-key map [?>] 'tla-version-get-version)
    (define-key map [?g] 'tla-version-update)
    (define-key map [?s] 'tla-version-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map [?q] 'tla-buffer-quit)
    (define-key map [?.] 'tla-version-bookmarks-add)
    map)
  "Keymap used in tla-version-list-mode buffers.")

;;
;; Revision list mode
;;
(defvar tla-revision-list-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map tla--keyvec-help 'describe-mode)
    (define-key map [?^] 'tla-revision-list-parent-version)
    (define-key map [?> ?g] 'tla-revision-get-revision)
    (define-key map [?> ?C] 'tla-revision-cache-revision)
    (define-key map [?> ?L] 'tla-revision-add-to-library)
    (define-key map [?t ??] 'tla-revision-toggle-help)
    (define-key map [?t ?d] 'tla-revision-toggle-date)
    (define-key map [?t ?c] 'tla-revision-toggle-creator)
    (define-key map [?t ?s] 'tla-revision-toggle-summary)
    (define-key map [?t ?l] 'tla-revision-toggle-library)
    (define-key map [?t ?r] 'tla-revision-toggle-reverse)
    (define-key map [?t ??] 'tla-revision-list-toggles)
    (define-key map [?S] 'tla-revision-star-merge)
    (define-key map [?T] 'tla-revision-tag)
    (define-key map [?g] 'tla-generic-refresh)
    (define-key map [down] 'tla-bookmarks-missing-next)
    (define-key map [up] 'tla-bookmarks-missing-prev)
    (define-key map [?n] 'tla-bookmarks-missing-next)
    (define-key map [?p] 'tla-bookmarks-missing-prev)
    (define-key map [?l] 'tla-revision-cat-log)
    (define-key map [?u] 'tla-revision-update)
    (define-key map "\C-m" 'tla-revision-return)
    (define-key map [return] 'tla-revision-return)
    (define-key map [?m] 'tla-revision-mark-revision)
    (define-key map [?d] 'tla-revision-delta)
    (define-key map [?=] 'tla-revision-changeset)
    (define-key map [?A] 'tla-show-process-buffer)
    (define-key map [?i] 'tla-pop-to-inventory)
    (define-key map [?q] 'tla-buffer-quit)
    (define-key map [?.] 'tla-revision-bookmarks-add)
    map)
  "Keymap used in tla-revision-list-mode buffers.")

;;
;; Changes mode
;;
(defvar tla-changes-mode-map 
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map "\C-m" 'tla-changes-return)
    (define-key map [return] 'tla-changes-return)
    (define-key map [?=] 'tla-changes-diff)
    (define-key map [?e] 'tla-changes-ediff)
    (define-key map [?g] 'tla-generic-refresh)
    (define-key map [?c] 'tla-changes-edit-log)
    (define-key map [?I] 'tla-inventory)
    (define-key map [?i] 'tla-pop-to-inventory)
    (define-key map [?n] 'tla-changes-next)
    (define-key map [?p] 'tla-changes-prev)
    (define-key map [?U] 'tla-changes-revert)
    (define-key map [?q] 'tla-buffer-quit)
    (define-key map [?d] 'tla-changes-rm)
    (define-key map [?m] 'tla-changes-mark-file)
    (define-key map [?u] 'tla-changes-unmark-file)
    map)
  "Keymap used in tla-changes-mode")


;;
;; ChangeLog mode section
;;
(defvar tla-changelog-mode-map 
  (let ((map (copy-keymap change-log-mode-map)))
    (define-key map [?q] 'tla-buffer-quit)    
    map)
  "Keymap used in tla-changelog-mode")


;;
;; Log buffer mode section
;;
(defvar tla-log-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?o] 'tla-switch-to-output-buffer)
    (define-key map "\C-m" 'tla-switch-to-output-buffer)
    (define-key map [?e] 'tla-switch-to-error-buffer)
    (define-key map [?r] 'tla-switch-to-related-buffer)
    (define-key map [?n] 'tla-log-next)
    (define-key map [?p] 'tla-log-prev)
    map)
  "Keymap used in Xtla's log buffer")


;; ----------------------------------------------------------------------------
;; Menu entries
;; ----------------------------------------------------------------------------
;;
;; Conventions
;;
;; 1. Each Nouns and verbs in menu items are should be capitalized.
;; 2. TODO: Consider menu items order.

;;
;; Global
;;
;;;###autoload
(easy-menu-add-item 
 nil '("tools")
 '("XTLA"
   ["Browse Archives" tla-archives t]
   ["Show Bookmarks" tla-bookmarks t]
   ["Start New Project" tla-start-project t]
   "---"
   "Tree Commands:"
   ["View Inventory" tla-inventory t]
   ["View Tree Lint" tla-tree-lint t]
   ["Show Tree Revisions" tla-tree-revisions t]
   ["Edit Arch Log" tla-edit-log t]
   ["View Conflicts" tla-view-conflicts t]
   "---"
   "File Commands:"
   ["Insert Arch Tag" tla-insert-arch-tag t]
   ["Add Log Entry" tla-add-log-entry t]
   ["View File Diff" tla-file-diff t]
   ["View File Ediff" tla-file-ediff t]
   ["View Original" tla-file-view-original t]
   ("Quick Configuration"
    ["Three Way Merge" tla-toggle-three-way-merge
     :style toggle :selected tla-three-way-merge]
    ["Use --forward" tla-toggle-use-forward-option
     :style toggle :selected tla-use-forward-option]
    )
   )
 "PCL-CVS")

;;
;; Bookmarks mode
;;
(easy-menu-define tla-bookmarks-mode-menu tla-bookmarks-mode-map
  "`tla-bookmarks-mode' menu"
  '("Tla-Bookmarks"
    ["View Missing Patches" tla-bookmarks-missing t]
    ["Open Local Tree" tla-bookmarks-open-tree t]
    ["Inventory on Local Tree" tla-bookmarks-inventory t]
    ["Add Bookmark" tla-bookmarks-add t]
    ["Show Details" tla-bookmarks-toggle-details
     :style toggle :selected tla-bookmarks-show-details]
    ["Add Partner" tla-bookmarks-add-partner-interactive t]
    ["Remove Partner" tla-bookmarks-delete-partner-interactive t]
    ("Group"
     ["Add Group" tla-bookmarks-add-group-interactive t]
     ["Delete Group" tla-bookmarks-delete-group-interactive t]
     ["Select by Group" tla-bookmarks-select-by-group t]
     )
    ["Add Nickname" tla-bookmarks-add-nickname-interactive t]
    ["Remove Nickname" tla-bookmarks-delete-nickname-interactive t]
    ["Get" tla-bookmarks-get t]
    ["Cleanup 'local-tree fields" tla-bookmarks-cleanup-local-trees t]
    ))

;;
;; Inventory mode
;;
(easy-menu-define tla-inventory-mode-menu tla-inventory-mode-map
  "'tla-inventory-mode' menu"
  `("Tla-Inventory"
    ["Edit Log" tla-edit-log t]
    ["Show Changes" tla-changes t]
    ["Show Changelog" tla-changelog t]
    ["Show Logs" tla-logs t]
    ["Show Missing" tla-inventory-missing t]
    ["Add"    tla-inventory-add t]
    ["Mirror" tla-inventory-mirror t]
    ["Star-merge" tla-inventory-star-merge t]
    ["Edit =tagging-method" tla-edit-=tagging-method t]
    ["Tree-lint" tla-tree-lint t]
    ("Toggles"
     ["Set All Toggle Variables" tla-inventory-set-all-toggle-variables t]
     ["Reset All Toggle Variables" tla-inventory-reset-all-toggle-variables t]
     ["Toggle All Toggle Variables" tla-inventory-toggle-all-toggle-variables t] .
     ,(mapcar '(lambda (elem) `[,(concat "Toggle " (car (cddddr elem)))
				,(caddr elem)
				:style toggle
				:selected ,(cadr elem)])
	      tla-inventory-file-types-manipulators))))


;;
;; Logs mode
;;
(easy-menu-define tla-logs-mode-menu tla-logs-mode-map
  "`tla-logs-mode' menu"
  '("Tla-Logs"
    ["Show Date" tla-logs-toggle-date
     :style toggle :selected (member "--date" tla-logs-flag-list)]
    ["Show Creator" tla-logs-toggle-creator
     :style toggle :selected (member "--creator" tla-logs-flag-list)]
    ["Show Summary" tla-logs-toggle-summary
     :style toggle :selected (member "--summary" tla-logs-flag-list)]
    ["Reverse" tla-logs-toggle-reverse
     :style toggle :selected (member "--reverse" tla-logs-flag-list)]
    ["Inventory" tla-pop-to-inventory t]
    ["Quit" tla-buffer-quit t]
    ))

;;
;; Cat-log mode
;;
(easy-menu-define tla-cat-log-mode-menu tla-cat-log-mode-map
  "'tla-cat-log-mode' menu"
  '("Tla-Cat-Log"
    ["Inventory" tla-pop-to-inventory t]
    ["Quit" tla-buffer-quit t]
    ))

;;
;; Log edit mode
;;
(easy-menu-define tla-log-edit-mode-menu tla-log-edit-mode-map
  "'tla-log-edit-mode' menu"
  '("Tla-Log"
    ["Insert tla log-for-merge" tla-log-edit-insert-log-for-merge t]
    ["Show changes"             tla-changes                       t]
    ["Commit"                   tla-log-edit-done                 t]
    ["Show Changelog"           tla-changelog                     t]
    ["Goto Summary Field"       tla-log-goto-summary              t]
    ["Goto Body"                tla-log-goto-body                 t]
    ["Kill Body"                tla-log-kill-body                 t]
    ["Tree Lint"                tla-tree-lint                     t]
    ["Abort"                    tla-log-edit-abort                t]))

;;
;; Archive list mode
;;
(easy-menu-define tla-archive-list-mode-menu tla-archive-list-mode-map
  "'tla-archive-list-mode' menu"
  '("Tla-Archives"
    ["Update Archives List" tla-archives t]
    ["Set Default Archive"  tla-archive-select-default t]
    ["Register New Archive" tla-register-archive t]
    ["Make New Archive..."  tla-make-archive t]
    ["Create a Mirror"      tla-archive-mirror-archive t]
    ["Synchronize Mirror"   tla-archive-synchronize-archive t]
    ))

;;
;; Category list mode
;;
(easy-menu-define tla-category-list-mode-menu tla-category-list-mode-map
  "'tla-category-list-mode' menu"
  '("Tla-Categories"
    ["List Archives"          tla-archives                t]
    ["Update Categories List" tla-category-update         t]
    ["Make New Category..."   tla-category-make-category  t]
    ["Synchronize Mirror"     tla-category-mirror-archive t]
    ))


;;
;; Branch list mode
;;
(easy-menu-define tla-branch-list-mode-menu tla-branch-list-mode-map
  "'tla-branch-list-mode' menu"
  '("Tla-Branches"
    ["Update Branches List" tla-branch-update               t]
    ["List Parent Category" tla-branch-list-parent-category t]
    ["Make New Branch..."   tla-branch-make-branch          t]
    ["Synchronize Mirror"   tla-branch-mirror-archive       t]
    ["Set Bookmark Here"    tla-branch-bookmarks-add        t]
    ["Get..."               tla-branch-get-branch           t]
    ))

;;
;; Version list mode
;;
(easy-menu-define tla-version-list-mode-menu tla-version-list-mode-map
  "'tla-version-list-mode' menu"
  '("Tla-Versions"
    ["Update Versions List" tla-version-update         t]
    ["Get..."               tla-version-get-version    t]
    ["Make New Version..."  tla-version-make-version   t]
    ["List Parent Branch"   tla-version-list-parent    t]
    ["Synchronize Mirror"   tla-version-mirror-archive t]
    ["Set Bookmark Here"    tla-version-bookmarks-add  t]))

;;
;; Revision list mode
;;
(easy-menu-define tla-revision-list-mode-menu tla-revision-list-mode-map
  "'tla-revision-list-mode' menu"
  '("Tla-Revisions"
    ["Show Log"                            tla-revision-cat-log t]
    ["Update Revisions List"               tla-generic-refresh t]
    ["List Parent Version"                 tla-revision-list-parent-version t]
    ["Mark Revision"                       tla-revision-mark-revision t]
    ["Delta with Mark"                     tla-revision-delta t]
    ["Get a Local Copy"                    tla-revision-get-revision t]
    ["Make Cache"                          tla-revision-cache-revision t]
    ["Add to Library"                      tla-revision-add-to-library t]
    ["View changeset"                      tla-revision-changeset t]
    ["Make New Version..."                 tla-version-make-version t]
    ["Set Bookmark Here"                   tla-revision-bookmarks-add t]
    ["Unify Patch Logs with This Revision" tla-revision-sync-tree t]
    ["Star-Merge from Here"                tla-revision-star-merge t]
    ["Replay from Here"                    tla-revision-replay t]
    ["Tag from Here"                       tla-revision-tag t]
    ["Show Date"    tla-revision-toggle-date
     :style toggle :selected tla-revisions-shows-date]
    ["Show Creator" tla-revision-toggle-creator
     :style toggle :selected tla-revisions-shows-creator]
    ["Show Summary" tla-revision-toggle-summary
     :style toggle :selected tla-revisions-shows-summary]
    ["Show Presence in Revlib" tla-revision-toggle-library
     :style toggle :selected tla-revisions-shows-library]
    ))

;;
;; Changes mode
;;
(easy-menu-define tla-changes-mode-menu tla-changes-mode-map
  "`tla-changes' menu"
  '("Tla-Changes"
    ["Update Changes" tla-changes t]
    ["Edit log before commit" tla-changes-edit-log t]
    ["View other revisions" tla-tree-revisions t]
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
(defcustom tla-prefix-key [(control x) ?T]
  "Prefix key for most xtla commands"
  :type '(choice (const [(control x) ?T])
                 (const [(control x) ?t])
                 (const [(control x) ?v])
                 (const [(control x) ?V])
                 (const [(control x) ?v ?t])
                 (sexp))
  :group 'xtla
  :set  (lambda (var value)
          (if (boundp var)
              (global-unset-key (symbol-value var)))
          (set var value)
          (global-set-key (symbol-value var) tla-global-keymap)))

;;;###autoload
(global-set-key tla-prefix-key tla-global-keymap)

;;;###autoload
(defcustom tla-tla-executable "tla"
  "*The name of the tla executable"
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-diff-executable "diff"
  "*The name of the diff executable"
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-patch-executable "patch"
  "*The name of the patch executable"
  :type 'string
  :group 'xtla)

;;;###autoload
(defcustom tla-highlight t
  "*Use highlighting for tla buffers"
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
  "*If non nil, xtla will not prompt you before saving buffers of the
working local tree"
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-automatically-revert-buffers t
  "*If non nil, xtla will automatically revert unmodified buffers
after an arch operation modifying the file."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-strict-commits nil
  "*If non-nil, commit operations are invoked with the --strict option."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-three-way-merge t
  "*If non-nil, star-merge operations are invoked with the --three-way
option."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-use-forward-option nil
  "*If non-nil, use the --forward option with commands that allow it."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defun tla-toggle-use-forward-option ()
  "Toggles the value of `tla-use-forward-option'"
  (interactive)
  (setq tla-use-forward-option (not tla-use-forward-option)))

;;;###autoload
(defun tla-toggle-three-way-merge ()
  "Toggles the value of `tla-three-way-merge'"
  (interactive)
  (setq tla-three-way-merge (not tla-three-way-merge)))

;;;###autoload
(defgroup tla-bookmarks nil
  "xtla bookmarks allows you to save places (archive, category,
branch, version) in the archive that you use often. Try M-x
tla-bookmarks RET to see."
  :group 'xtla)

;;;###autoload
(defcustom tla-bookmarks-file-name "~/.tla-bookmarks.el"
  "*File in which xtla bookmarks will be saved"
  :type 'file
  :group 'xtla-bookmarks)

;;;###autoload
(defcustom tla-bookmarks-cleanup-dont-prompt nil
  "*non nil means `tla-bookmarks-cleanup-local-trees' shouldn't prompt
before removing a local-tree"
  :type 'boolean
  :group 'xtla-bookmarks)

;;;###autoload
(defgroup xtla-internal nil
  "This group contains items used mainly for debugging."
  :group 'xtla)

;;;###autoload
(defcustom tla-log-commands t
  "*Non nil means log all tla executed commands in the buffer
*tla-log*."
  :type 'boolean
  :group 'xtla-internal)

;;;###autoload
(defcustom tla-log-buffer " *tla-log*"
  "*Name of the buffer in which xtla logs main events"
  :type 'string
  :group 'xtla-internal)

;;;###autoload
(defcustom tla-switch-to-buffer-mode 'pop-to-buffer
  "*Mode for switching to xtla buffers."
  :type '(choice (const pop-to-buffer)
                 (const single-window)
                 (const dedicated-frame))
  :group 'xtla)

;;;###autoload
(defcustom tla-log-insert-last t
  "*If non-nil, new changelog entries will be inserted at the end of
the log file"
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-archive-list-mode-hook '()
  "*Hooks run after switching to `tla-archive-list-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-bookmarks-mode-hook '()
  "*Hooks run after switching to `tla-bookmarks-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-branch-list-mode-hook '()
  "*Hooks run after switching to `tla-branch-list-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-cat-log-mode-hook '()
  "*Hooks run after switching to `tla-cat-log-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-category-list-mode-hook '()
  "*Hooks run after switching to `tla-category-list-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-inventory-file-mode-hook '()
  "*Hooks run after switching to `tla-inventory-file-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-inventory-mode-hook '()
  "*Hooks run after switching to `tla-inventory-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-log-edit-mode-hook '()
  "*Hooks run after switching to `tla-log-edit-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-logs-mode-hook '()
  "*Hooks run after switching to `tla-logs-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-revision-list-mode-hook '()
  "*Hooks run after switching to `tla-revision-list-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-version-list-mode-hook '()
  "*Hooks run after switching to `tla-version-list-mode'."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-make-branch-hook '()
  "*Hooks run after making a branch."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-make-category-hook '()
  "*Hooks run after making a category."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-make-version-hook '()
  "*Hooks run after making a version."
  :type 'hook
  :group 'xtla)

;;;###autoload
(defcustom tla-make-archive-hook '()
  "*Hooks run after creating a new archive."
  :type 'hook
  :group 'xtla)

(defcustom tla-follow-symlinks 'tree
  "*Follow symlinks of this type."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Symlinks into an arch-managed tree" tree)
                 (const :tag "Symlinks to an arch-managed file" id))
  :group 'xtla)

(defcustom tla-follow-symlinks-mode 'follow
  "*Follow symlinks of this type."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Follow" follow)
                 (const :tag "Warn" warn))
  :group 'xtla)

(defcustom tla-use-arrow-keys-for-navigation nil
  "*Enable left/right for navigation."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled"  t)
                 (const :tag "Enabled with Shift" shift))
  :group 'xtla)


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
  :group 'xtla-faces)

;;;###autoload
(defface tla-archive-name
  '((((type tty) (class color)) (:foreground "lightblue" :weight light))
    (((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "lightskyblue1"))
    (t (:weight bold)))
  "Face to highlight xtla archive names."
  :group 'xtla-faces)

;;;###autoload
(defface tla-category-name
  '((t (:inherit xtla-archive-name)))
  "Face to highlight xtla category names."
  :group 'xtla-faces)

;;;###autoload
(defface tla-branch-name
  '((t (:inherit xtla-archive-name)))
  "Face to highlight xtla branch names."
  :group 'xtla-faces)

;;;###autoload
(defface tla-version-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla version names."
  :group 'xtla-faces)

;;;###autoload
(defface tla-revision-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla revision names."
  :group 'xtla-faces)

;;;###autoload
(defface tla-bookmark-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight xtla revision names."
  :group 'xtla-faces)


;;;###autoload
(defface tla-separator
  '((((background light)) (:underline t :bold t))
    (((background dark)) (:underline t :bold t)))
  "Face to highlight separators."
  :group 'xtla-faces)


;; ----------------------------------------------------------------------------
;; Font lock keywords
;; ----------------------------------------------------------------------------

;;
;; Inventory file mode
;;
(defvar tla-inventory-file-font-lock-keywords
  '(
    ("^#.*$" . font-lock-comment-face)
    ("^[ \t]*\\(backup\\|exclude\\|junk\\|precious\\|unrecognized\\|source\\)\\>[  ]*\\(.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^[ \t]*\\(untagged-source\\)"
     (1 font-lock-keyword-face))
    ("^[ \t]*\\(untagged-source\\) \\(precious\\|source\\|backup\\|junk\\|unrecognized\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
    ("^[ \t]*\\(explicit\\|tagline\\|names\\)\\>" (1 font-lock-builtin))
    )
  "Keywords in tla-inventory-file mode.")

;;
;; Inventory mode
;;
(defvar tla-inventory-font-lock-keywords
  '(("^[ \*]*P.*$" . font-lock-comment-face)
    ("^[ \*]*U.*$" . font-lock-warning-face)
    ("^[ \*]*S.*$" . font-lock-constant-face)
    ("^[ \*]*J.*$" . font-lock-function-name-face)
    ("^[ \*]*T.*$" . font-lock-type-face)
    ;; TODO: Shouldn't use font-lock standard faces ...
    )
  "Keywords in tla-inventory-mode.")

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
  "Keywords in tla-cat-log-mode.")

;;
;; Log edit mode
;;
(defvar tla-log-edit-font-lock-keywords
  '(("^Summary: " . font-lock-function-name-face)
    ("^Keywords: " . font-lock-function-name-face)
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
  '(("^\\* looking for .* to compare with$" . font-lock-function-name-face)
    ("^\\* comparing to .*$" . font-lock-function-name-face)
    ("^\\* dir metadata changed$" . font-lock-function-name-face)
    ;; FIXME: Next regexp should start with ^ ?
    ("-/ .*$" . font-lock-function-name-face)
    ("^A  .*$" . font-lock-function-name-face)
    ("^M  .*$" . font-lock-function-name-face)
    ("^C  .*$" . font-lock-warning-face)
    ("^\\* modified files" . font-lock-function-name-face)
    ("^\\* added files" . font-lock-function-name-face))
  "Keywords in tla-changes mode.")

;;
;; ChangeLog mode
;;
(defvar tla-changelog-font-lock-keywords
  '(("    \\([^ ].+:\\)$" (1 font-lock-function-name-face))
    ;;    ("^ +new files:" . font-lock-function-name-face)
    ;;    ("^ +new directories" . font-lock-function-name-face)
    ;;    ("^ +modified files:" . font-lock-function-name-face)
    ;;    ("^ +Revision:" . font-lock-function-name-face)
    ;;    ("^ +new patches:" . font-lock-function-name-face)
    ;;    ("  +removed files:" . font-lock-function-name-face)
    ("\t\\(patch-[0-9]+\\)" (1 font-lock-function-name-face))
    ("^#.*$" . font-lock-comment-face))
  "Keywords in tla-changelog mode.")


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
  "Global keymap used by Xtla in the ediff control buffer")

(define-key tla-ediff-keymap [?a] 'tla-ediff-add-log-entry)

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
    (tla-revision-list-mode-map right 'tla-revision-return)
    (tla-changes-mode-map left 'tla-changes-return)
    (tla-changes-mode-map right 'tla-changes-view-source)))


(defun tla-use-arrow-keys-for-navigation (&optional uninstall)
  "Bind the left/right keys for navigation.
Add a call to this function after the require for xtla."
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
;; xtla-defs.el ends here
