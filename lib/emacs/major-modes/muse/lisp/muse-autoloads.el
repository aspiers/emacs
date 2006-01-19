;;; muse-autoloads.el --- autoloads for Muse
;;
;;; Code:

;;;### (autoloads (muse-latex-transform) "muse-convert" "muse-convert.el"
;;;;;;  (17327 26137))
;;; Generated autoloads from muse-convert.el

(autoload (quote muse-latex-transform) "muse-convert" nil t nil)

;;;***

;;;### (autoloads (muse-message-markup) "muse-message" "../experimental/muse-message.el"
;;;;;;  (17194 28038))
;;; Generated autoloads from ../experimental/muse-message.el

(autoload (quote muse-message-markup) "muse-message" "\
Markup a wiki-ish e-mail message as HTML alternative e-mail.
This step is manual by default, to give the author a chance to review
the results and ensure they are appropriate.
If you wish it to be automatic (a risky proposition), just add this
function to `message-send-hook'." t nil)

;;;***

;;;### (autoloads (muse-insert-tag muse-index muse-what-changed muse-previous-reference
;;;;;;  muse-next-reference muse-follow-name-at-point-other-window
;;;;;;  muse-follow-name-at-point muse-browse-result muse-edit-link-at-point
;;;;;;  muse-mode) "muse-mode" "muse-mode.el" (17351 9070))
;;; Generated autoloads from muse-mode.el

(autoload (quote muse-mode) "muse-mode" "\
Muse is an Emacs mode for authoring and publishing documents.
\\{muse-mode-map}" t nil)

(autoload (quote muse-edit-link-at-point) "muse-mode" "\
Edit the current link.
Do not rename the page originally referred to." t nil)

(autoload (quote muse-browse-result) "muse-mode" "\
Visit the current page's published result." t nil)

(autoload (quote muse-follow-name-at-point) "muse-mode" "\
Visit the link at point, or insert a newline if none is found." t nil)

(autoload (quote muse-follow-name-at-point-other-window) "muse-mode" "\
Visit the link at point in other window." t nil)

(autoload (quote muse-next-reference) "muse-mode" "\
Move forward to next Muse link or URL, cycling if necessary." t nil)

(autoload (quote muse-previous-reference) "muse-mode" "\
Move backward to the next Muse link or URL, cycling if necessary.
This function is not entirely accurate, but it's close enough." t nil)

(autoload (quote muse-what-changed) "muse-mode" "\
Show the unsaved changes that have been made to the current file." t nil)

(autoload (quote muse-index) "muse-mode" "\
Display an index of all known Muse pages." t nil)

(autoload (quote muse-insert-tag) "muse-mode" "\
Insert a tag interactively with a blank line after it." t nil)

;;;***

;;;### (autoloads (muse-project-publish muse-project-find-file) "muse-project"
;;;;;;  "muse-project.el" (17351 9070))
;;; Generated autoloads from muse-project.el

(autoload (quote muse-project-find-file) "muse-project" "\
Open the Muse page given by NAME in PROJECT.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the page
will be created if it does not already exist.  Otherwise, the
first directory within the project's fileset is used." t nil)

(autoload (quote muse-project-publish) "muse-project" "\
Publish the pages of PROJECT that need publishing." t nil)

;;;***

;;;### (autoloads (muse-browse-url) "muse-protocols" "muse-protocols.el"
;;;;;;  (17327 26137))
;;; Generated autoloads from muse-protocols.el

(autoload (quote muse-browse-url) "muse-protocols" "\
Handle URL with the function specified in `muse-url-protocols'.
If OTHER-WINDOW is non-nil, open in a different window." t nil)

;;;***

;;;### (autoloads (muse-publish-this-file muse-publish-file) "muse-publish"
;;;;;;  "muse-publish.el" (17352 12537))
;;; Generated autoloads from muse-publish.el

(autoload (quote muse-publish-file) "muse-publish" "\
Publish the given FILE in a particular STYLE to OUTPUT-DIR.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what." t nil)

(autoload (quote muse-publish-this-file) "muse-publish" "\
Publish the page in the current file." t nil)

;;;***

;;;### (autoloads (muse-registry-initialize) "muse-registry" "muse-registry.el"
;;;;;;  (17340 39366))
;;; Generated autoloads from muse-registry.el

(autoload (quote muse-registry-initialize) "muse-registry" "\
Set `muse-registry-alist' from `muse-registry-file'.
If `muse-registry-file' doesn't exist, create it.
If FROM-SCRATCH is non-nil, make the registry from scratch." t nil)

;;;***

;;;### (autoloads (muse-blosxom-new-entry) "muse-blosxom" "muse-blosxom.el"
;;;;;;  (17314 62700))
;;; Generated autoloads from muse-blosxom.el

(autoload (quote muse-blosxom-new-entry) "muse-blosxom" "\
Start a new blog entry with given CATEGORY.
The filename of the blog entry is derived from TITLE.
The page will be initialized with the current date and TITLE." t nil)

;;;***

(provide 'muse-autoloads)
;;; muse-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

