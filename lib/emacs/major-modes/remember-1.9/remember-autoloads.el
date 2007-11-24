;;; remember-autoloads.el --- autoloads for Remember
;;
;;; Code:

;;;### (autoloads (remember-destroy remember-buffer remember-clipboard
;;;;;;  remember-region remember) "remember" "remember.el" (17618
;;;;;;  15288))
;;; Generated autoloads from remember.el

(autoload (quote remember) "remember" "\
Remember an arbitrary piece of data.
With a prefix, uses the region as INITIAL.

\(fn &optional INITIAL)" t nil)

(autoload (quote remember-region) "remember" "\
Remember the data from BEG to END.
If called from within the remember buffer, BEG and END are ignored,
and the entire buffer will be remembered.

This function is meant to be called from the *Remember* buffer.
If you want to remember a region, supply a universal prefix to
`remember' instead. For example: C-u M-x remember.

\(fn &optional BEG END)" t nil)

(autoload (quote remember-clipboard) "remember" "\
Remember the contents of the current clipboard.
Most useful for remembering things from Netscape or other X Windows
application.

\(fn)" t nil)

(autoload (quote remember-buffer) "remember" "\
Remember the contents of the current buffer.

\(fn)" t nil)

(autoload (quote remember-destroy) "remember" "\
Destroy the current *Remember* buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (remember-location remember-url) "remember-bibl"
;;;;;;  "remember-bibl.el" (17618 15313))
;;; Generated autoloads from remember-bibl.el

(autoload (quote remember-url) "remember-bibl" "\
Remember a URL in `bibl-mode' that is being visited with w3.

\(fn)" t nil)

(autoload (quote remember-location) "remember-bibl" "\
Remember a bookmark location in `bibl-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (remember-blosxom) "remember-blosxom" "remember-blosxom.el"
;;;;;;  (17617 34591))
;;; Generated autoloads from remember-blosxom.el

(autoload (quote remember-blosxom) "remember-blosxom" "\
Remember this text to a blosxom story.
This function can be added to `remember-handler-functions'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (remember-emacs-wiki-journal-add-entry-maybe remember-emacs-wiki-journal-add-entry-auto
;;;;;;  remember-emacs-wiki-journal-add-entry) "remember-emacs-wiki-journal"
;;;;;;  "remember-emacs-wiki-journal.el" (17617 34591))
;;; Generated autoloads from remember-emacs-wiki-journal.el

(autoload (quote remember-emacs-wiki-journal-add-entry) "remember-emacs-wiki-journal" "\
Prompt for category and heading and add entry.

\(fn)" nil nil)

(autoload (quote remember-emacs-wiki-journal-add-entry-auto) "remember-emacs-wiki-journal" "\
Add entry where the category is the first word and the heading the
rest of the words on the first line.

\(fn)" nil nil)

(autoload (quote remember-emacs-wiki-journal-add-entry-maybe) "remember-emacs-wiki-journal" "\
Like `remember-emacs-wiki-journal-add-entry-auto' but only adds
entry if the first line matches `emacs-wiki-journal-category-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("read-file-name.el" "remember-diary.el"
;;;;;;  "remember-experimental.el" "remember-planner.el") (17621
;;;;;;  18483 971380))

;;;***

;;;### (autoloads (remember-bbdb-store-in-mailbox) "remember-bbdb"
;;;;;;  "remember-bbdb.el" (17618 15318))
;;; Generated autoloads from remember-bbdb.el

(autoload (quote remember-bbdb-store-in-mailbox) "remember-bbdb" "\
Store remember data as if it were incoming mail.
In which case `remember-mailbox' should be the name of the mailbox.
Each piece of psuedo-mail created will have an `X-Todo-Priority'
field, for the purpose of appropriate splitting.

\(fn)" nil nil)

;;;***

(provide 'remember-autoloads)
;;; remember-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

