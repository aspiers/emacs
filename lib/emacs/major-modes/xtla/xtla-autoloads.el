

;;;### (autoloads (tla-submit-bug-report tla-prepare-patch-submission
;;;;;;  tla-tree-lint tla-ediff-add-log-entry tla-tag-regenerate
;;;;;;  tla-tag-insert tla-tag-string tla-find-file-hook tla-inventory-file-mode
;;;;;;  tla-log-edit-mode tla-start-project tla-update tla-missing
;;;;;;  tla-revisions tla-tree-revisions tla-make-archive tla-archives
;;;;;;  tla-bookmarks tla-tag tla-id-tagging-method tla-my-revision-library
;;;;;;  tla-my-id tla-tree-version tla-help tla-logs tla-changelog
;;;;;;  tla-rm tla-commit tla-file-view-original tla-file-ediff tla-view-conflicts
;;;;;;  tla-conflicts-finish tla-file-diff tla-file-ediff-revisions
;;;;;;  tla-apply-changeset tla-get-changeset tla-delta tla-changes-last-revision
;;;;;;  tla-changes-against tla-changes tla-add-log-entry tla-edit-log
;;;;;;  tla-inventory xtla) "xtla" "xtla.el" (17003 33542))
;;; Generated autoloads from xtla.el

(autoload (quote xtla) "xtla" "\
Displays a welcome message." t nil)

(autoload (quote tla-inventory) "xtla" "\
Show a tla inventory at DIRECTORY.
When called with a prefix arg, pop to the inventory buffer.
DIRECTORY defaults to the current one when within an arch managed tree,
unless prefix argument ARG is non-nil." t nil)

(autoload (quote tla-edit-log) "xtla" "\
Edit the tla log file.

With an optional prefix argument INSERT-CHANGELOG, insert the last
group of entries from the ChangeLog file.  SOURCE-BUFFER, if non-nil,
is the buffer from which the function was called.  It is used to get
the list of marked files, and potentially run a selected file commit." t nil)

(autoload (quote tla-add-log-entry) "xtla" "\
Add new tla log ChangeLog style entry." t nil)

(autoload (quote tla-changes) "xtla" "\
Run \"tla changes\".

When called without a prefix argument: show the detailed diffs also.
When called with a prefix argument SUMMARY: do not show detailed
diffs. When AGAINST is non-nil, use it as comparison tree." t nil)

(autoload (quote tla-changes-against) "xtla" "\
Wrapper for `tla-changes'.

When called interactively, SUMMARY is the prefix arg, and AGAINST is
read from the user." t nil)

(autoload (quote tla-changes-last-revision) "xtla" "\
Run `tla-changes' against the last but one revision.

The idea is that running this command just after a commit should be
equivalent to running `tla-changes' just before the commit.

SUMMARY is passed to `tla-changes'." t nil)

(autoload (quote tla-delta) "xtla" "\
Run tla delta BASE MODIFIED.
If DIRECTORY is a non-empty string, the delta is stored to it.
If DIRECTORY is ask, a symbol, ask the name of directory.
If DIRECTORY is nil or an empty string, just show the delta using --diffs." t nil)

(autoload (quote tla-get-changeset) "xtla" "\
Gets the changeset corresponding to REVISION.

When JUSTSHOW is non-nil (no prefix arg), just show the diff.
Otherwise, store changeset in DESTINATION.
If WITHOUT-DIFF is non-nil, don't use the --diff option to show the
changeset." t nil)

(autoload (quote tla-apply-changeset) "xtla" "\
Call \"tla apply-changeset\".

CHANGESET is the changeset to apply, TARGET is the directory in which
to apply the changeset. If REVERSE is non-nil, apply the changeset in
reverse." t nil)

(autoload (quote tla-file-ediff-revisions) "xtla" "\
View changes in FILE between BASE and MODIFIED using ediff." t nil)

(autoload (quote tla-file-diff) "xtla" "\
Run \"tla file-diff\" on file FILE.

In interactive mode, the file is the current buffer's file.
If REVISION is specified, it must be a string representing a revision
name, and the file will be diffed according to this revision." t nil)

(autoload (quote tla-conflicts-finish) "xtla" "\
Command to delete .rej file after conflicts resolution.
Asks confirmation if the file still has diff3 markers." t nil)

(autoload (quote tla-view-conflicts) "xtla" "\
*** WARNING: semi-deprecated function.
Use this function if you like, but M-x smerge-mode RET is actually
better for the same task ****

Graphical view of conflicts after tla star-merge --three-way. The
buffer given as an argument must be the content of a file with
conflicts markers like.

    <<<<<<< TREE
    my text
    =======
    his text
    >>>>>>> MERGE-SOURCE

Priority is given to your file by default. (This means all conflicts
will be rejected if you do nothing)." t nil)

(autoload (quote tla-file-ediff) "xtla" "\
Interactive view of differences in FILE with ediff.

Changes are computed since last commit (or REVISION if specified)." t nil)

(autoload (quote tla-file-view-original) "xtla" "\
Get the last-committed version of FILE in a buffer.

If REVISION is specified, it must be a cons representing the revision
for which to get the original." t nil)

(autoload (quote tla-commit) "xtla" "\
Run tla commit.

Optional argument HANDLER is the process handler for the commit
command.
When the commit finishes successful, `tla-commit-done-hook' is called." t nil)

(autoload (quote tla-rm) "xtla" "\
Call tla rm on file FILE.  Prompts for confirmation before." nil nil)

(autoload (quote tla-changelog) "xtla" "\
Run \"tla changelog\".

display the result in an improved ChangeLog mode." t nil)

(autoload (quote tla-logs) "xtla" "\
Run tla logs." t nil)

(autoload (quote tla-help) "xtla" "\
Run tla COMMAND -H." t nil)

(autoload (quote tla-tree-version) "xtla" "\
Equivalent of tla tree-version (but implemented in pure elisp).

Optional argument LOCATION is the directory in which the command must
be ran.  If NO-ERROR is non-nil, don't raise errors if ran outside an
arch managed tree." t nil)

(autoload (quote tla-my-id) "xtla" "\
Run tla my-id.

When called without a prefix argument ARG, just print the my-id from
tla and return it.  If MY-ID is not set yet, return an empty string.
When called with a prefix argument, ask for a new my-id.

The my-id should have the following format:

Your id is recorded in various archives and log messages as you use
arch.  It must consist entirely of printable characters and fit on one
line.  By convention, it should have the form of an email address, as
in this example:

Jane Hacker <jane.hacker@gnu.org>" t nil)

(autoload (quote tla-my-revision-library) "xtla" "\
Run tla my-revision-library.

When called without a prefix argument ARG, just print the
my-revision-library from tla.  When called with a prefix argument, ask
for a new my-revision-library.

my-revision-library specifies a path, where the revision library is
stored to speed up tla.  For example ~/tmp/arch-lib.

You can configure the parameters for the library via
`tla-library-config'." t nil)

(autoload (quote tla-id-tagging-method) "xtla" "\
View (and return) or change the id-tagging method.
When called without prefix argument ARG: show the actual tagging method.
When called with prefix argument ARG: Ask the user for the new tagging method." t nil)

(autoload (quote tla-tag) "xtla" "\
Create a tag from SOURCE-REVISION to TAG-VERSION.
Run tla tag --setup." t nil)

(autoload (quote tla-bookmarks) "xtla" "\
Display xtla bookmarks in a buffer.
With prefix argument ARG, reload the bookmarks file from disk." t nil)

(autoload (quote tla-archives) "xtla" "\
Start the archive browser." t nil)

(autoload (quote tla-make-archive) "xtla" "\
Call `tla--make-archive' interactively  then call `tla-archives'." t nil)

(autoload (quote tla-tree-revisions) "xtla" "\
Call `tla-revisions' in the current tree." t nil)

(autoload (quote tla-revisions) "xtla" "\
List the revisions of ARCHIVE/CATEGORY--BRANCH--VERSION." t nil)

(autoload (quote tla-missing) "xtla" "\
Search in directory LOCAL-TREE for missing patches from LOCATION.
If the current buffers default directory is in an arch managed tree use that
one unless called with a prefix arg.  In all other cases prompt for the local
tree and the location." t nil)

(autoload (quote tla-update) "xtla" "\
Run tla update in TREE.

After running update, execute HANDLE (function taking no argument)." t nil)

(autoload (quote tla-start-project) "xtla" "\
Start a new project.
Prompts for the root directory of the project and the fully
qualified version name to use.  Sets up and imports the tree and
displays an inventory buffer to allow the project's files to be
added and committed.
If ARCHIVE is given, use it when reading version.
Return a cons pair: its car is the new version name string, and
its cdr is imported location.
If SYNCHRONOUSLY is non-nil, run \"tla import\" synchronously.
Else run it asynchronously." t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\+\\+log\\." . tla-log-edit-mode)))

(autoload (quote tla-log-edit-mode) "xtla" "\
Major Mode to edit xtla log messages.
Commands:
\\{tla-log-edit-mode-map}
" t nil)

(autoload (quote tla-inventory-file-mode) "xtla" "\
Major mode to edit tla inventory files (=tagging-method, .arch-inventory)." t nil)

(autoload (quote tla-find-file-hook) "xtla" "\
Hook executed when opening a file.
Follow symlinked files/directories to the actual location of a file.
Enter smerge mode if the file has conflicts (detected by the presence
of a .rej file)." nil nil)

(autoload (quote tla-tag-string) "xtla" "\
Return a suitable string for an arch-tag.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after).

Interactively, you should call `tla-tag-insert', but this function can
be usefull to write template files." nil nil)

(autoload (quote tla-tag-insert) "xtla" "\
Insert a unique arch-tag in the current file.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after)" t nil)

(autoload (quote tla-tag-regenerate) "xtla" "\
Find an arch tag in the current buffer and regenerates it.
This means changing the ID of the file, which will usually be done after
copying a file in the same tree to avoid duplicates ID.

Raises an error when multiple tags are found or when no tag is found." t nil)

(autoload (quote tla-ediff-add-log-entry) "xtla" "\
Add a log entry." t nil)

(autoload (quote tla-tree-lint) "xtla" "\
Run tla tree-lint in directory ROOT." t nil)

(autoload (quote tla-prepare-patch-submission) "xtla" "\
Submit a patch to a tla working copy (at TLA-TREE-ROOT) via email.
With this feature it is not necessary to tag an tla archive.
You simply edit your checked out copy from your project and call this function.
The function will create a patch as *.tar.gz file (based on TARBALL-BASE-NAME)
and send it to the given email address EMAIL.
VERSION-STRING should indicate the version of tla that the patch applies to.
DESCRIPTION is a brief descsription of the patch.
SUBJECT is the subject for the email message.
For an example, how to use this function see: `tla-submit-patch'." t nil)

(autoload (quote tla-submit-bug-report) "xtla" "\
Submit a bug report, with pertinent information to the xtla-el-dev list." t nil)

;;;***

;;;### (autoloads (tla-error-buffer tla-process-buffer) "xtla-core"
;;;;;;  "xtla-core.el" (17003 33542))
;;; Generated autoloads from xtla-core.el

(defvar tla-process-buffer " *tla-process*" "\
*Name of the process buffer.")

(defvar tla-error-buffer " *tla-errors*" "\
*Name of the buffer to which tla's stderr is redirected.")

(add-to-list (quote minor-mode-alist) (quote (tla-process-running (:eval (if (equal (length tla-process-running) 1) " Tla running" (concat " Tla running(" (int-to-string (length tla-process-running)) ")"))))))

;;;***

;;;### (autoloads (tla-format-line-function tla-generate-line-function
;;;;;;  tla-version-to-name-function tla-state-variables-list tla-state-file-name
;;;;;;  tla-tips-enabled tla-apply-patch-mapping tla-log-edit-keywords
;;;;;;  tla-name-read-init-hook tla-make-archive-hook tla-make-version-hook
;;;;;;  tla-make-category-hook tla-make-branch-hook tla-version-list-mode-hook
;;;;;;  tla-revision-list-mode-hook tla-logs-mode-hook tla-log-edit-mode-hook
;;;;;;  tla-inventory-mode-hook tla-inventory-file-mode-hook tla-category-list-mode-hook
;;;;;;  tla-cat-log-mode-hook tla-branch-list-mode-hook tla-bookmarks-mode-hook
;;;;;;  tla-archive-list-mode-hook tla-commit-done-hook tla-log-insert-last
;;;;;;  tla-buffer-quit-mode tla-switch-to-buffer-first tla-read-directory-mode
;;;;;;  tla-read-project-tree-mode tla-switch-to-buffer-mode tla-log-buffer
;;;;;;  tla-log-commands tla-bookmarks-cleanup-dont-prompt tla-cache-tla-get-changeset
;;;;;;  tla-log-library-greedy tla-log-library tla-config-directory
;;;;;;  tla-tag-function tla-bookmarks-file-name tla-toggle-three-way-merge
;;;;;;  tla-toggle-use-skip-present-option tla-use-skip-present-option
;;;;;;  tla-three-way-merge tla-commit-fix-missing-space tla-commit-headers-allowed-to-be-empty
;;;;;;  tla-commit-check-log-buffer-functions tla-strict-commits
;;;;;;  tla-changes-recursive tla-automatically-revert-buffers tla-do-not-prompt-for-save
;;;;;;  tla-install-command-help-system tla-highlight tla-patch-executable
;;;;;;  tla-diff-executable tla-tla-executable tla-prefix-key) "xtla-defs"
;;;;;;  "xtla-defs.el" (17003 33542))
;;; Generated autoloads from xtla-defs.el

(eval-and-compile (require (quote easymenu)))

(defmacro tla--do-in-gnu-emacs (&rest body) "\
Execute BODY if in GNU/Emacs." (unless (featurep (quote xemacs)) (\` (progn (\,@ body)))))

(defmacro tla--do-in-xemacs (&rest body) "\
Execute BODY if in XEmacs." (when (featurep (quote xemacs)) (\` (progn (\,@ body)))))

(defvar tla--key-help 63)

(defvar tla--key-diff 61)

(defvar tla--key-show-bookmark 98)

(defvar tla--key-file-diff 100)

(defvar tla--key-tree-lint 108)

(defvar tla--key-logs 76)

(defvar tla--key-ediff 101)

(defvar tla--key-log-entry 97)

(defvar tla--key-inventory 105)

(defvar tla--key-commit 99)

(defvar tla--key-update 117)

(defvar tla--keyvec-help (vector tla--key-help))

(defvar tla--keyvec-ediff (vector tla--key-ediff))

(defvar tla--keyvec-tree-lint (vector tla--key-tree-lint))

(defvar tla--keyvec-logs (vector tla--key-logs))

(defvar tla--keyvec-log-entry (vector tla--key-log-entry))

(defvar tla--keyvec-diff (vector tla--key-diff))

(defvar tla--keyvec-file-diff (vector tla--key-file-diff))

(defvar tla--keyvec-file-diff (vector tla--key-file-diff))

(defvar tla--keyvec-commit (vector tla--key-commit))

(defvar tla--keyvec-update (vector tla--key-update))

(defvar tla--keyvec-inventory (vector tla--key-inventory))

(defvar tla--keyvec-show-bookmark (vector tla--key-show-bookmark))

(defvar tla-global-keymap (let ((map (make-sparse-keymap))) (define-key map [85] (quote tla-undo)) (define-key map [82] (quote tla-redo)) (define-key map tla--keyvec-log-entry (quote tla-add-log-entry)) (define-key map [65] (quote tla-archives)) (define-key map tla--keyvec-file-diff (quote tla-file-diff)) (define-key map tla--keyvec-ediff (quote tla-file-ediff)) (define-key map [111] (quote tla-file-view-original)) (define-key map tla--keyvec-diff (quote tla-changes)) (define-key map tla--keyvec-commit (quote tla-edit-log)) (define-key map [116] (quote tla-tag-insert)) (define-key map tla--keyvec-inventory (quote tla-inventory)) (define-key map [114] (quote tla-tree-revisions)) (define-key map tla--keyvec-logs (quote tla-logs)) (define-key map tla--keyvec-tree-lint (quote tla-tree-lint)) (define-key map tla--keyvec-update (quote tla-update)) (define-key map tla--keyvec-show-bookmark (quote tla-bookmarks)) (define-key map tla--keyvec-help (quote tla-help)) map) "\
Global keymap used by Xtla.")

(define-key ctl-x-4-map [84] (quote tla-add-log-entry))

(defvar tla--name-read-extension-keydefs (quote (([(control r)] . tla-name-read-refresh-cache) ([(meta *)] . tla-name-read-insert-default-archive) ([(meta \.)] . tla-name-read-insert-info-at-point) ([(meta \;)] . tla-name-read-insert-version-associated-with-default-directory) ([(control n)] . tla-name-read-insert-partner-next) ([(control p)] . tla-name-read-insert-partner-previous) ([(control v)] . tla-name-read-insert-bookmark-next) ([(meta v)] . tla-name-read-insert-bookmark-previous) ([(meta ^)] . tla-name-read-insert-ancestor) ([(control h)] . tla-name-read-help) ([(meta \?)] . tla-name-read-inline-help))) "\
Key definitions table for `tla--name-read-minibuf-map'.
The reason these definitions are defined separately from
`tla--name-read-minibuf-map' is that to reuse these definitions
in `tla-name-read-help'. Don't forget to evalute
`tla--name-read-minibuf-map' again after updating this value.")

(defvar tla--name-read-minibuf-map (let ((map (copy-keymap minibuffer-local-completion-map))) (mapc (lambda (pair) (let ((key (car pair)) (func (cdr pair))) (define-key map key func))) tla--name-read-extension-keydefs) (define-key map [menu-bar xtla] (cons "Xtla" (make-sparse-keymap "Xtla"))) (define-key map [menu-bar xtla refresh] (list (quote menu-item) "Refresh Completion Cache" (quote tla-name-read-refresh-cache))) (define-key map [menu-bar xtla ancestor] (list (quote menu-item) "Insert Ancestor" (quote tla-name-read-insert-ancestor) :enable (quote (and (minibufferp) (equal "" (minibuffer-contents)) (member archive (quote (prompt maybe))) (not (eq this-command (quote tla-compute-direct-ancestor))))))) (define-key map [menu-bar xtla default] (list (quote menu-item) "Insert Default Archive" (quote tla-name-read-insert-default-archive) :enable (quote (and (minibufferp) (equal "" (minibuffer-contents)) (member archive (quote (prompt maybe))))))) (define-key map [menu-bar xtla here] (list (quote menu-item) "Insert Thing at Point" (quote tla-name-read-insert-info-at-point) :enable (quote (and (minibufferp) (equal "" (minibuffer-contents)) tla-name-read-insert-info-at-point)))) (define-key map [menu-bar xtla bookmark] (list (quote menu-item) "Insert Version in Bookmark" (quote xtla--name-read-bookmark-menu) :enable (quote (let* ((l (condition-case nil (let ((default-version (tla-tree-version-list default-directory))) (tla-bookmarks-get-partner-versions default-version)) (error nil)))) (and l (< 0 (length l))))))) (define-key map [menu-bar xtla partner] (list (quote menu-item) "Insert Partner Version" (quote xtla--name-read-partner-menu) :enable (quote (let* ((l (condition-case nil (tla-partner-list) (error nil)))) (and l (< 0 (length l))))))) map) "\
Keymap to input a gnuarch revision at the minibuffer.")

(defconst tla-inventory-file-types-manipulators (quote ((83 tla-inventory-display-source tla-inventory-toggle-source 115 "source") (80 tla-inventory-display-precious tla-inventory-toggle-precious 112 "precious") (74 tla-inventory-display-junk tla-inventory-toggle-junk 106 "junk") (66 tla-inventory-display-backup tla-inventory-toggle-backup 98 "backup") (84 tla-inventory-display-tree tla-inventory-toggle-tree 116 "tree root") (85 tla-inventory-display-unrecognized tla-inventory-toggle-unrecognized 117 "unrecognized"))) "\
List of possible file types in inventory.")

(dolist (type-arg tla-inventory-file-types-manipulators) (eval (\` (defcustom (\, (cadr type-arg)) t (\, (concat "Wether " (nth 4 type-arg) " should be printed in inventory")) :group (quote tla-inventory) :type (quote boolean)))))

(easy-menu-add-item (or (tla--do-in-gnu-emacs menu-bar-tools-menu) nil) (or (tla--do-in-xemacs (quote ("Tools"))) nil) (quote ("Xtla" ["Browse Archives" tla-archives t] ["Show Bookmarks" tla-bookmarks t] ["Start New Project" tla-start-project t] "---" "Tree Commands:" ["View Changes" tla-changes t] ["View Inventory" tla-inventory t] ["View Tree Lint" tla-tree-lint t] ["Show Tree Revisions" tla-tree-revisions t] ["Edit Arch Log" tla-edit-log t] "---" "File Commands:" ["Insert Arch Tag" tla-tag-insert t] ["Add Log Entry" tla-add-log-entry t] ["View File Diff" tla-file-diff t] ["View File Ediff" tla-file-ediff t] ["View Original" tla-file-view-original t] ["View Conflicts" tla-view-conflicts t] "---" ("Quick Configuration" ["Three Way Merge" tla-toggle-three-way-merge :style toggle :selected tla-three-way-merge] ["Use --skip-present" tla-toggle-use-skip-present-option :style toggle :selected tla-use-skip-present-option]))) "PCL-CVS")

(defgroup xtla nil "Arch interface for emacs." :group (quote tools) :prefix "tla-")

(defgroup tla-inventory nil "This group contains items used in inventory mode." :group (quote xtla))

(defgroup tla-revisions nil "This group contains items used in revisions mode." :group (quote xtla))

(defgroup tla-file-actions nil "This group contains items manipulating finding, saving and reverting files." :group (quote xtla))

(defgroup tla-bindings nil "This group contains items related to key bindings." :group (quote xtla))

(defgroup tla-faces nil "This group contains faces defined for Xtla." :group (quote xtla))

(defvar tla-prefix-key [(control x) 84] "\
Prefix key for the Xtla commands in the global keymap.")

(global-set-key tla-prefix-key tla-global-keymap)

(defvar tla-tla-executable "tla" "\
*The name of the tla executable.")

(defvar tla-diff-executable "diff" "\
*The name of the diff executable.")

(defvar tla-patch-executable "patch" "\
*The name of the patch executable.")

(defvar tla-highlight t "\
*Use highlighting for tla buffers.")

(defvar tla-install-command-help-system t "\
*Use f1 to display help for the actual function call during minibuffer input.
Note: this functionality is provided for all minibuffer prompts.")

(defvar tla-do-not-prompt-for-save nil "\
*Whether or not xtla will prompt before saving.

If non nil, xtla will not prompt you before saving buffers of the
working local tree.")

(defvar tla-automatically-revert-buffers t "\
*Whether or not xtla will automatically revert buffers.

If non nil, xtla will automatically revert unmodified buffers after an
arch operation modifying the file.")

(defvar tla-changes-recursive t "\
*Whether or not xtla will compute changes recursively.

If non nil, `tla-changes' will be applied recursively to subprojects
of the current tree")

(defvar tla-strict-commits nil "\
*If non-nil, commit operations are invoked with the --strict option.")

(defvar tla-commit-check-log-buffer-functions (quote (tla-commit-check-empty-headers tla-commit-check-empty-line tla-commit-check-missing-space)) "\
*List of functions to check the ++log.. buffer.

Each function is called, from the log buffer, with no argument. It
should raise an error if commit should be canceled.")

(defvar tla-commit-headers-allowed-to-be-empty "^\\(Keywords\\)$" "\
*Headers allowed to be empty in the ++log.. buffer.

This should be a regexp matching the header names. Headers not
matching this regexp should not be empty when committing.")

(defvar tla-commit-fix-missing-space t "\
*Whether or not xtla will add missing spaces after header names.

If non-nil, missing spaces after a space will be inserted
automatically instead of raising an error when committing.")

(defvar tla-three-way-merge t "\
*If non-nil, star-merge operations are invoked with --three-way.")

(defvar tla-use-skip-present-option nil "\
*If non-nil, use --skip-present with commands that allow it.")

(autoload (quote tla-toggle-use-skip-present-option) "xtla-defs" "\
Toggle the value of `tla-use-skip-present-option'." t nil)

(autoload (quote tla-toggle-three-way-merge) "xtla-defs" "\
Toggle the value of `tla-three-way-merge'." t nil)

(defgroup tla-bookmarks nil "xtla bookmarks allows you to save places (archive, category,\nbranch, version) in the archive that you use often. Try M-x\ntla-bookmarks RET to see." :group (quote xtla))

(defvar tla-bookmarks-file-name "bookmarks.el" "\
*File in which xtla bookmarks will be saved.
The bookmark file is stored in the `tla-config-directory'")

(defvar tla-tag-function (quote tla-tag-uuid) "\
Function called to generate the value of the arch-tag.

The function must take no argument, and return a string without a
final newline.")

(defvar tla-config-directory "~/.xtla/" "\
*Directory in which the xtla config files will be stored.")

(defvar tla-log-library "~/.arch-log-library/" "\
*Directory in which the log library will be stored.")

(defvar tla-log-library-greedy t "\
*Whether log files are automatically saved in the log library.

If non-nil, then, whenever xtla needs to access a log file, this file
will be copied to the log library.")

(defvar tla-cache-tla-get-changeset 2 "\
*Cache `tla-get-changeset' calls.
When nil, don't cache.
When a number, cache only if the `tla-get-changeset' call takes
more seconds than the number.
Otherwise don't cache the results.
The cache is kept only in RAM.")

(defvar tla-bookmarks-cleanup-dont-prompt nil "\
*Whether xtla should prompt before cleaning a local tree.

non nil means `tla-bookmarks-cleanup-local-trees' shouldn't prompt
before removing a local-tree")

(defgroup tla-internal nil "This group contains items used mainly for debugging." :group (quote xtla))

(defvar tla-log-commands t "\
*Non nil means log all tla commands in the buffer *tla-log*.")

(defvar tla-log-buffer " *tla-log*" "\
*Name of the buffer in which xtla logs main events.")

(defvar tla-switch-to-buffer-mode (quote pop-to-buffer) "\
*Mode for switching to xtla buffers.
Recommended settings are: 'pop-to-buffer, and 'show-in-other-window
and 'single-window")

(defvar tla-read-project-tree-mode (quote sometimes) "\
*Mode for prompting project tree directories. Possible values are:

- always: When running a tla command requiring a tree, always prompt
  for it.

- sometimes: If a command is ran inside a project tree, the tree root
  is used. Otherwise, prompt.

- never: If a command is run inside a project tree, use the tree root.
  Othwise, raise an error.")

(defvar tla-read-directory-mode (quote sometimes) "\
*How prompting project directories should be done.

Works similarly to `tla-read-project-tree-mode', but this one is used
for commands like `tla-inventory' for which a subdirectory of a
project tree is accepted.")

(defvar tla-switch-to-buffer-first t "\
*Switch to newly created buffer on creation of buffers?

If non-nil, xtla commands implementing this feature will switch to the
newly created buffer when the command is called. Further (potentially
asynchronous) processes are run without modifying your
window-configuration. Otherwise, xtla will switch to the new buffer on
command completion.")

(defvar tla-buffer-quit-mode (quote kill) "\
*How *tla-...* buffer should be killed.
If the value is 'kill, buffers are actually killed. Otherwise, just
burry them.")

(defvar tla-log-insert-last t "\
*If non-nil, insert changelog entries at the end of the log file.")

(defgroup tla-hooks nil "This group contains hooks into xtla." :group (quote xtla))

(defvar tla-commit-done-hook (quote nil) "\
*Hooks run after a successful commit via `tla-commit'.")

(defvar tla-archive-list-mode-hook (quote nil) "\
*Hooks run after switching to `tla-archive-list-mode'.")

(defvar tla-bookmarks-mode-hook (quote nil) "\
*Hooks run after switching to `tla-bookmarks-mode'.")

(defvar tla-branch-list-mode-hook (quote nil) "\
*Hooks run after switching to `tla-branch-list-mode'.")

(defvar tla-cat-log-mode-hook (quote nil) "\
*Hooks run after switching to `tla-cat-log-mode'.")

(defvar tla-category-list-mode-hook (quote nil) "\
*Hooks run after switching to `tla-category-list-mode'.")

(defvar tla-inventory-file-mode-hook (quote nil) "\
*Hooks run after switching to `tla-inventory-file-mode'.")

(defvar tla-inventory-mode-hook (quote nil) "\
*Hooks run after switching to `tla-inventory-mode'.")

(defvar tla-log-edit-mode-hook (quote nil) "\
*Hooks run after switching to `tla-log-edit-mode'.")

(defvar tla-logs-mode-hook (quote nil) "\
*Hooks run after switching to `tla-logs-mode'.")

(defvar tla-revision-list-mode-hook (quote nil) "\
*Hooks run after switching to `tla-revision-list-mode'.")

(defvar tla-version-list-mode-hook (quote nil) "\
*Hooks run after switching to `tla-version-list-mode'.")

(defvar tla-make-branch-hook (quote nil) "\
*Hooks run after making a branch.")

(defvar tla-make-category-hook (quote nil) "\
*Hooks run after making a category.")

(defvar tla-make-version-hook (quote nil) "\
*Hooks run after making a version.")

(defvar tla-make-archive-hook (quote nil) "\
*Hooks run after creating a new archive.")

(defvar tla-name-read-init-hook (quote nil) "\
*Hooks run when the control enters to `tla-name-read'.")

(defvar tla-log-edit-keywords (quote ("bugfix" "docfix" "warnfix" "linting" "newfeature" "merge" "update" "rename" "delete" "newfile")) "\
A list of keywords for the Keywords field of a log message.")

(defvar tla-apply-patch-mapping nil "\
*Tree in which patches should be applied.

An alist of rules to match fully qualified revision names to target
directories.

That variable is used to offer a directory in `tla-gnus-apply-patch'.
Example setting: '(((nil \"xtla\" nil nil nil) \"~/work/tla/xtla\")))")

(defgroup tla-tips nil "\"Tip of the day\" feature for Xtla" :group (quote xtla))

(defvar tla-tips-enabled t "\
*Set this to nil to disable tips.")

(defgroup tla-state nil "Saving Xtlas state between Emacs sessions." :group (quote xtla))

(defvar tla-state-file-name "state.el" "\
*File in which xtla saves state variables between Emacs sessions.
The file is stored in the `tla-config-directory'")

(defvar tla-state-variables-list (quote (tla-tips-number)) "\
*List of variables to store in the state file `tla-state-file-name'.")

(defgroup tla-merge nil "Merging with Xtla." :group (quote xtla))

(defvar tla-version-to-name-function nil "\
*Function returning a name for a version.

If non-nil, it must be a function that is called with the version as
an argument, and must return a string that will be used to instead of
the nickname.

See `tla-merge-summary-line-for-log'.")

(defvar tla-generate-line-function nil "\
*Function generating a string summarizing the merge.

If non-nil, it must be a function that is called with a list like
\((\"Robert\" 167 168 170) (\"Masatake\" 209 213 214 215 217 218)) as
an argument, and must return a string.

See `tla-merge-summary-line-for-log'.")

(defvar tla-format-line-function nil "\
*Function formatting the summary line.

If non-nil, it must be a function that is called with a string as an
argument, and returns another string (typically adding a \"Merges \"
comment in front of it.

See `tla-merge-summary-line-for-log'.")

(defface tla-marked (quote ((((type tty) (class color)) (:foreground "magenta" :weight light)) (((class color) (background light)) (:foreground "magenta")) (((class color) (background dark)) (:foreground "yellow")) (t (:weight bold)))) "Face to highlight a marked entry in xtla buffers" :group (quote tla-faces))

(defface tla-archive-name (quote ((((type tty) (class color)) (:foreground "lightblue" :weight light)) (((class color) (background light)) (:foreground "blue4")) (((class color) (background dark)) (:foreground "lightskyblue1")) (t (:weight bold)))) "Face to highlight xtla archive names." :group (quote tla-faces))

(defface tla-source-archive-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla source archive names." :group (quote tla-faces))

(defface tla-mirror-archive-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla mirror archive names." :group (quote tla-faces))

(defface tla-category-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla category names." :group (quote tla-faces))

(defface tla-branch-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla branch names." :group (quote tla-faces))

(defface tla-version-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla version names." :group (quote tla-faces))

(defface tla-revision-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla revision names." :group (quote tla-faces))

(defface tla-local-directory (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla local directory." :group (quote tla-faces))

(defface tla-buffer (quote ((t (:inherit tla-archive-name)))) "Face to highlight buffer names printed in xtla's buffer." :group (quote tla-faces))

(defface tla-tagging-method (quote ((t (:inherit tla-archive-name)))) "Face to highlight taggine methods." :group (quote tla-faces))

(defface tla-bookmark-name (quote ((t (:inherit tla-archive-name)))) "Face to highlight xtla revision names." :group (quote tla-faces))

(defface tla-id (quote ((t (:inherit tla-keyword)))) "Face to highlight an arch id." :group (quote tla-faces))

(defface tla-separator (quote ((((type tty)) (:underline t :weight bold)) (((background light)) (:underline t :weight bold)) (((background dark)) (:underline t :weight bold)))) "Face to highlight separators." :group (quote tla-faces))

(defface tla-keyword (quote ((t (:inherit font-lock-keyword-face)))) "Face to highlight keywords." :group (quote tla-faces))

(defface tla-comment (quote ((t (:inherit font-lock-comment-face)))) "Face to highlight comments." :group (quote tla-faces))

(defface tla-precious (quote ((t (:inherit font-lock-comment-face)))) "Face to highlight precious entries" :group (quote tla-faces))

(defface tla-unrecognized (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight unrecognized entries" :group (quote tla-faces))

(defface tla-duplicate (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight files with duplicate IDs" :group (quote tla-faces))

(defface tla-source (quote ((t (:inherit font-lock-string-face)))) "Face to highlight source code entries" :group (quote tla-faces))

(defface tla-junk (quote ((t (:inherit font-lock-function-name-face)))) "Face to highlight junk entries" :group (quote tla-faces))

(defface tla-nested-tree (quote ((t (:inherit font-lock-type-face)))) "Face to highlight nested trees" :group (quote tla-faces))

(defface tla-to-add (quote ((t (:inherit font-lock-comment-face)))) "Face to highlight a file that should probably be added to the\narchive" :group (quote tla-faces))

(defface tla-broken-link (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight a broken link" :group (quote tla-faces))

(defface tla-unmerged (quote ((t (:inherit font-lock-keyword-face)))) "Face to highlight unmerged patches" :group (quote tla-faces))

(defface tla-header (quote ((t (:inherit font-lock-function-name-face)))) "Face to highlight header in log mode for example" :group (quote tla-faces))

(defface tla-conflict (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight conflicts" :group (quote tla-faces))

(defface tla-modified (quote ((t (:inherit font-lock-function-name-face)))) "Face to highlight modified files" :group (quote tla-faces))

(defface tla-move (quote ((t (:inherit font-lock-function-name-face)))) "Face to highlight moved files/directory" :group (quote tla-faces))

(defface tla-deleted (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight deleted files" :group (quote tla-faces))

(defface tla-added (quote ((t (:inherit font-lock-warning-face)))) "Face to highlight added files" :group (quote tla-faces))

(defface tla-meta-info (quote ((t (:inherit font-lock-comment-face)))) "Face to highlight files with meta-info changes" :group (quote tla-faces))

(defface tla-messages (quote ((t (:inherit font-lock-function-name-face)))) "Face to highlight messages in tla buffers" :group (quote tla-faces))

(defface tla-highlight (quote ((((class color) (background dark)) (:background "darkblue")) (((class color) (background light)) (:background "gold")))) "Face to use as an alternative to `highlight' face.\nIf there could be more than two highlighted things, the user will confuse.\nIn such case use this face." :group (quote tla-faces))

(defface tla-mark (quote ((((class color) (background dark)) (:foreground "green" :bold t)) (((class color) (background light)) (:foreground "green3" :bold t)) (t (:bold t)))) "Xtla face used to highlight marked file indicator." :group (quote tla-faces))

(add-to-list (quote auto-mode-alist) (quote ("/\\(=tagging-method\\|\\.arch-inventory\\)$" . tla-inventory-file-mode)))

;;;***

;;;### (autoloads (tla-tests-run tla-tests-batch) "xtla-tests" "xtla-tests.el"
;;;;;;  (17003 33535))
;;; Generated autoloads from xtla-tests.el

(eval-when-compile (require (quote cl)))

(autoload (quote tla-tests-batch) "xtla-tests" "\
Run all the available test-cases in batch mode." t nil)

(autoload (quote tla-tests-run) "xtla-tests" "\
Run the testcase TEST.

Switch HOME to the test directory, clear the log buffer, call the
function TEST, and check that the list of tla commands ran by calling
TEST is the same as the one expected, stored in
`tla-tests-command-alist'" t nil)

;;;***

;;;### (autoloads (tla-tips-popup tla-tips-popup-maybe) "xtla-tips"
;;;;;;  "xtla-tips.el" (17003 33535))
;;; Generated autoloads from xtla-tips.el

(autoload (quote tla-tips-popup-maybe) "xtla-tips" "\
Pop up a buffer with a tip if tips are enabled.

see `tla-tips-enabled'." nil nil)

(autoload (quote tla-tips-popup) "xtla-tips" "\
Pop up a buffer with a tip message.

Don't use this function from Xtla. Use `tla-tips-popup-maybe'
instead." t nil)

;;;***

;;;### (autoloads (tla-browse) "xtla-browse" "xtla-browse.el" (17003
;;;;;;  33538))
;;; Generated autoloads from xtla-browse.el

(autoload (quote tla-browse) "xtla-browse" "\
Browse registered archives as trees within one buffer.
You can specify the node should be opened by alist,
INITIAL-OPEN-LIST.  If APPEND is nil, the nodes not in
INITIAL-OPEN-LIST are made closed.  If non-nil, the nodes
already opened are kept open." t nil)

;;;***
(provide 'xtla-autoloads)
