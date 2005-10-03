;;; emacs-wiki.el --- Maintain a local Wiki using Emacs-friendly markup

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley
;; Copyright (C) 2003, 2004 Damien Elmes
;; Copyright (C) 2004 Mark Triggs
;; Copyright (C) 2004 Sacha Chua
;; Copyright (C) 2004 Gary Vaughan
;; Copyright (C) 2004, 2005 Michael Olson
;; Copyright (C) 2004 Yohanes Santoso
;; Copyright (C) 2005 Markus Hoenicka

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain Emacs-friendly Wikis in a local directory
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Wiki is a concept, more than a thing.  It is a way of creating
;; document pages using plain text markup and simplified hyperlinking.
;;
;; By typing a name in MixedCase, a hyperlink is automatically created
;; to the document "MixedCase".  Pressing return on that name will
;; create the file if it doesn't exist, or visit it if it does.
;;
;; The markup used by emacs-wiki is intended to be very friendly to
;; people familiar with Emacs.  Type C-h v emacs-wiki-publishing-markup
;; after this mode is loaded for how to get started.

;; * Startup

;; To begin using emacs-wiki, put this in your .emacs file:
;;
;;   (require 'emacs-wiki)
;;
;; Now you can type M-x emacs-wiki-find-file, give it a WikiName (or
;; just hit return) and start typing!
;;
;; You should also type M-x customize-group, and give the name
;; "emacs-wiki".  Change it to suite your preferences.  Each of the
;; options has its own documentation.

;; * Keystroke summary
;;
;; Here is a summary of keystrokes available in every Wiki buffer:
;;
;;   C-c C-a    jump to an index of all the Wiki pages
;;   C-c C-b    show all pages that reference this page
;;   C-c C-s    search for a word in your Wiki pages
;;   C-c C-f    jump to another Wiki page; prompts for the name
;;   C-c C-l    highlight/refresh the current buffer
;;   C-c C-p    publish any Wiki pages that have changed as HTML
;;   C-c C-r    rename wiki link at point
;;   C-c C-v    change wiki project
;;   C-c C-D    delete wiki link at point (binding will only work on X)
;;   C-c =      diff this page against the last backup version
;;   TAB        move to the next Wiki reference
;;   S-TAB      move to the previous Wiki reference

;; * Using pcomplete

;; If you have pcomplete loaded, you can type M-TAB to complete Wiki
;; names.  Hitting M-TAB twice or more time in succession, will cycle
;; through all of the possibilities.  You can download pcomplete from
;; John Wiegley's website:
;;
;; http://www.newartisans.com/johnw/emacs.html

;; * ChangeLog support

;; If you use a ChangeLog (C-x 4 a) within one of your Wiki
;; directories, it will be used for notifying visitors to your wiki of
;; recent changes.

;; * Changing title or stylesheet

;; For convenience, if you want to change the visible title, or the
;; stylesheet, used by a certain Wiki page during HTML publishing,
;; just put:
;;
;; #title Hello there
;; #style hello.css
;;
;; at the top of the page.

;; * <lisp> tricks

;; <lisp></lisp> tags can be used, not only to evaluate forms for
;; insertion at that point, but to influence the publishing process in
;; many ways.  Here's another way to change a page's stylesheet:
;;
;; <lisp>
;; (ignore
;;   ;; use special.css for this Wiki page
;;   (set (make-variable-buffer-local 'emacs-wiki-style-sheet)
;;        "<link rel=\"stylesheet\" type=\"text/css\" href=\"special.css\" />"))
;; </lisp>
;;
;; The 'ignore' is needed so nothing is inserted where the <lisp> tag
;; occurred.  Also, there should be no blank lines before or after the
;; tag (to avoid empty paragraphs from being created).  The best place
;; to put this would be at the very top or bottom of the page.

;; * Multiple projects

;; Multiple inter-related projects may be defined.  Here is an example:
;;
;; (setq emacs-wiki-projects
;;       '(("WebWiki" .
;;          ((emacs-wiki-directories . ("~/proj/wiki/webpage"))
;;           (emacs-wiki-project-server-prefix . "../wiki/")
;;           (emacs-wiki-publishing-directory
;;            . "~/personal-site/site/wiki")))
;;         ("ProjectsWiki" .
;;          ((emacs-wiki-directories . ("~/proj/wiki/projects"))
;;           (emacs-wiki-project-server-prefix . "../projects/")
;;           (emacs-wiki-publishing-directory
;;            . "~/personal-site/site/projects")))))
;;
;; Whatever is assigned to the `emacs-wiki-projects' variable is
;; respected by all Emacs Wiki projects like Planner and Journal.
;; Multiple Planner and Journal projects are not currently supported
;; "natively", but support could easily be hacked up for them.

;; * Related links

;; To assign a related link with this page, put:
;;
;; #related link1 link2
;;
;; at the top of the page, and then make use of the resulting
;; `emacs-wiki-current-page-related' variable.  For example, put this
;; in your footer to write related links such that they are relative
;; to "../bookmarks/":
;;
;;<lisp>
;;  (when emacs-wiki-current-page-related
;;    (concat
;;     "<strong>Related:</strong>\n"
;;     (mapconcat
;;      (lambda (page)
;;        (concat "<a href=\"../bookmarks/"
;;                page "\">" page "</a>"))
;;      (split-string
;;       emacs-wiki-current-page-related)
;;      "\n")))
;;</lisp>

;; * Sub-lists?

;; There is no inherent support for sub-lists, since I couldn't think
;; of a simple way to do it.  But if you really need them, here's a
;; trick you can use:
;;
;; - Hello
;;   <ul>
;;   <li>There
;;   <li>My friend
;;   </ul>

;; * Previous Maintainers

;; Damien Elmes (arch AT repose DOT cx) maintained Emacs Wiki from
;; June 2003 to January 2004.
;;
;; Mark Triggs (mst AT dishevelled DOT net) maintained Emacs Wiki
;; from January 2004 to March 2004.
;;
;; Sacha Chua (sacha AT free DOT net DOT ph) maintained Emacs Wiki
;; from March 2004 to September 2004.

;; * Contributors

;; Alex Schroeder (alex AT gnu DOT org) is current author of
;; "wiki.el".  His latest version is here:
;; http://www.geocities.com/kensanata/wiki/WikiMode.html
;;
;; Frank Gerhardt (Frank DOT Gerhardt AT web DOT de) is the author of
;; the original wiki-mode.  His latest version is here:
;; http://www.s.netic.de/fg/wiki-mode/wiki.el
;;
;; Thomas Link (t DOT link AT gmx DOT at)
;;
;; Michal Maruska (mmaruska AT tin DOT it) contributed a patch for
;; including #title in the wiki index.  Ideas from it were used, but
;; not the code itself.
;;
;; Junichi Uekawa (dancer AT netfort DOT gr DOT jp) added euc-jp to
;; the list of coding translations.
;;
;; Brent Goodrick (bgoodrick AT ipns DOT com) contributed a small
;; patch that made derived modes such as planner work correctly.
;;
;; Ephrem Christopher Walborn (christopherw AT BonitaBayGroup DOT com)
;; helped to make much of the generated HTML code be XHTML 1.0
;; Transitional compliant.
;;
;; Joe Corneli (jcorneli AT math DOT utexas DOT edu) made a small
;; patch to `emacs-wiki-transform-name'.
;;
;; Xian (email address unknown) contributed a one-line patch that made
;; the S-tab key sequence work properly on Mac OS X.
;;
;; Gary Vaughan (gary AT gnu DOT org) contributed a number of
;; enhancements.  Multi-project support in particular was extensively
;; hacked on by him.
;;
;; John Sullivan (john AT wjsullivan DOT net) contributed a patch that
;; fixes a problem with colorizing URI's that have spaces in them.
;;
;; Yohanes Santoso (ysantoso AT dessyku DOT is-a-geek DOT org)
;; contributed the `emacs-wiki-full-path-ignore-regexp' function.  It
;; is useful for VC directories like CVS or {arch} or .arch-ids, and
;; for various files that happen to be in the wiki directory but are
;; not meant to be published.
;;
;; Pascal Quesseveur (quesseveur AT abaksystemes DOT fr) contributed a
;; patch that causes symbolic links to be resolved in
;; `emacs-wiki-directories-member'.  This is used when deciding
;; whether or not to activate emacs-wiki-mode.
;;
;; Wei-Hao Lin (whlin AT cs DOT cmu DOT edu) found a problem with
;; parenthesis nesting in `emacs-wiki-walk-directories' and submitted
;; a patch.
;;
;; Jay Bromley (jbromley AT gmail DOT com) contributed a 2-line patch
;; to fix a "Saving emacs-wiki-url-protocols: Invalid function: nil."
;; error in `emacs-wiki-url-protocols'.
;;
;; Markus Hoenicka (markus DOT hoenicka AT mhoenicka DOT de) added
;; backlink functionality to emacs-wiki.
;;
;; Frederik Fouvry (fouvry AT CoLi DOT Uni-SB DOT DE) provided a patch
;; for `emacs-wiki-wiki-link-target' that helps deal with edge cases
;; in links like "[[#8]]" (10 changed lines).
;;
;; Jim Ottaway (j DOT ottaway AT lse DOT ac DOT uk) provided a patch
;; (2 lines modified) for `emacs-wiki-wiki-visible-name' that causes
;; attention to be paid to case during matches.
;;
;; Andrea Riciputi (ariciputi AT pito DOT com) gave an initial
;; implementation for tag completion by means of the
;; `emacs-wiki-insert-tag' function.
;;
;; Yamagata Yoriyuki (yoriyuki AT mbg DOT ocn DOT ne DOT jp) helped
;; fix an interwiki link issue in `emacs-wiki-make-link'.

(defvar emacs-wiki-version "2.70"
  "The version of emacs-wiki currently loaded.")

(require 'compile)
(require 'derived)
(require 'font-lock)
(require 'info)

;; load pcomplete if it's available
(load "pcomplete" t t)

(defvar emacs-wiki-under-windows-p
  (memq system-type '(ms-dos windows-nt)))

;;; Options:

(defgroup emacs-wiki nil
  "Options controlling the behavior of Emacs Wiki Mode.
Wiki is a concept, more than a thing.  It is a way of creating
document pages using plain text markup and simplified hyperlinking.

By typing a name in MixedCase, a hyperlink is automatically created
to the document \"MixedCase\".  Pressing return on that name will
create the file if it doesn't exist, or visit it if it does.

The markup used by emacs-wiki is intended to be very friendly to
people familiar with Emacs.  See the documentation for the variable
`emacs-wiki-publishing-markup' for a full description."
  :group 'hypermedia)

(defcustom emacs-wiki-mode-hook
  '(emacs-wiki-use-font-lock)
  "A hook that is run when emacs-wiki mode is entered."
  :type 'hook
  :options '(emacs-wiki-use-font-lock
             emacs-wiki-highlight-buffer
             flyspell-mode
             footnote-mode
             highlight-changes-mode)
  :group 'emacs-wiki)

;;;###autoload
(defcustom emacs-wiki-directories '("~/Wiki")
  "A list of directories where Wiki pages can be found."
  :require 'emacs-wiki
  :type '(repeat :tag "Wiki directories" directory)
  :group 'emacs-wiki)

(defcustom emacs-wiki-default-page "WelcomePage"
  "Name of the default page used by \\[emacs-wiki-find-file].
This is also used to resolve a link to a project that has no Wiki
page specified."
  :type 'string
  :group 'emacs-wiki)

(defcustom emacs-wiki-interwiki-names
  '(("GnuEmacs" . "http://www.gnu.org/software/emacs/emacs.html")
    ("TheEmacsWiki" .
     (lambda (tag)
       (concat "http://www.emacswiki.org/cgi-bin/wiki/"
               (or tag "SiteMap"))))
    ("MeatballWiki" .
     (lambda (tag)
       (concat "http://www.usemod.com/cgi-bin/mb.pl?"
               (or tag "MeatballWiki")))))
  "A table of WikiNames that refer to external entities.
The format of this table is an alist, or series of cons cells.
Each cons cell must be of the form:

  (WIKINAME . STRING-OR-FUNCTION)

The second part of the cons cell may either be a STRING, which in most
cases should be a URL, or a FUNCTION.  If a function, it will be
called with one argument: the tag applied to the Interwiki name, or
nil if no tag was used.  If the cdr was a STRING and a tag is used,
the tag is simply appended.

Here are some examples:

  (\"JohnWiki\" . \"http://alice.dynodns.net/wiki?\")

Referring to [[JohnWiki#EmacsModules]] then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
                       (choice (string :tag "URL") function)))
  :group 'emacs-wiki)

(defcustom emacs-wiki-create-backlinks nil
  "When true, create hierarchical backlinks on top of new Wiki pages."
  :type 'boolean
  :group 'emacs-wiki)

(defun emacs-wiki-count-chars (string char)
  (let ((i 0)
        (l (length string))
        (count 0))
    (while (< i l)
      (if (eq char (aref string i))
          (setq count (1+ count)))
      (setq i (1+ i)))
    count))

(provide 'emacs-wiki)

(require 'emacs-wiki-regexps)

(defcustom emacs-wiki-url-protocols
  '(("info" emacs-wiki-browse-url-info identity)
    ("man" emacs-wiki-browse-url-man identity)
    ("google" emacs-wiki-browse-url-google
     emacs-wiki-resolve-url-google)
    ("http" browse-url identity)
    ("https" browse-url identity)
    ("ftp" browse-url identity)
    ("gopher" browse-url identity)
    ("telnet" browse-url identity)
    ("wais" browse-url identity)
    ("file" browse-url identity)
    ("news" browse-url identity)
    ("snews" browse-url identity)
    ("mailto" browse-url identity))
  "A list of (PROTOCOL BROWSE-FUN MARKUP-FUN) used to match protocols for URLs.
BROWSE-FUN should accept URL as an argument and open the URL in the
current window. MARKUP-FUN should accept URL as an argument and return
the final URL or nil if no URL should be included."
  :type '(alist
          :key-type (string :format "Protocol: %v")
          :value-type (group (function :format "Browse: %v")
                             (function :format "Resolve: %v")))
  :set 'emacs-wiki-set-sym-and-url-regexp
  :group 'emacs-wiki)

(defvar emacs-wiki-url-regexp
  nil
  "A regexp used to match URLs within a Wiki buffer.
Dynamically calculated from `emacs-wiki-url-protocols'.")

(defvar emacs-wiki-url-server-regexp
  nil
  "A regexp used to match server URLs from `emacs-wiki-project-server-prefix'.
Dynamically calculated from `emacs-wiki-url-protocols'.")

(defcustom emacs-wiki-grep-command
  "find %D -type f ! -name '*~' | xargs egrep -n -e \"\\<%W\\>\""
  "The name of the program to use when grepping for backlinks.
The string %D is replaced by `emacs-wiki-directories',
space-separated.  The string %W is replaced with the name of the
Wiki page.

Note: I highly recommend using glimpse to search large Wiki's.
To use glimpse, install and edit a file called .glimpse_exclude
in your home directory.  Put a list of glob patterns in that file
to exclude Emacs backup files, etc.  Then, run the indexer using:

  glimpseindex -o <list of Wiki directories>

Once that's completed, customize this variable to have the
following value:

  glimpse -nyi \"%W\"

Your searches will go much, much faster, especially for very
large Wiki's.  Don't forget to add a user cronjob to update the
index at intervals."
  :type 'string
  :group 'emacs-wiki)

(defun emacs-wiki-edit-link-at-point ()
  "Edit the current link.
Do not rename the wiki page originally referred to."
  (interactive "*")
  (if (emacs-wiki-link-at-point)
      (let* ((old-link (emacs-wiki-match-string-no-properties 1))
             (old-text (emacs-wiki-match-string-no-properties 3))
             (match-start (match-beginning 0))
             (match-end (match-end 0))
             (link (emacs-wiki-make-link
                    (read-string "Link: "
                                 (emacs-wiki-link-unescape old-link))
                    (when old-text
                      (read-string
                       "Text: "
                       (emacs-wiki-link-unescape old-text t))))))
        (delete-region match-start match-end)
        (insert link))
    (error "There is no valid link at point")))

(defvar emacs-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'emacs-wiki-index)
    (define-key map [(control ?c) (control ?f)] 'emacs-wiki-find-file)
    (define-key map [(control ?c) (control ?b)] 'emacs-wiki-backlink)
    (define-key map [(control ?c) (control ?s)] 'emacs-wiki-search)
    (define-key map [(control ?c) (control ?p)] 'emacs-wiki-publish)
    (define-key map [(control ?c) (control ?t)]
      'emacs-wiki-publish-this-page)
    (define-key map [(control ?c) (control ?v)]
      'emacs-wiki-change-project)
    (define-key map [(control ?c) (control ?e)]
      'emacs-wiki-edit-link-at-point)
    (define-key map [(control ?c) (control ?r)]
      'emacs-wiki-rename-link-at-point)
    (define-key map [(control ?c) (control ?D)]
      'emacs-wiki-delete-link-at-point)

    (define-key map [(control ?c) (control ?l)] 'font-lock-mode)

    (define-key map [(control ?c) ?=]
      (lambda ()
        (interactive)
        (diff-backup buffer-file-name)))

    (define-key map [tab] 'emacs-wiki-next-reference)
    (define-key map [(control ?i)] 'emacs-wiki-next-reference)

    (define-key map [(control ?c) tab] 'emacs-wiki-insert-tag)
    (define-key map [(control ?c) (control ?i)]
      'emacs-wiki-insert-tag)

    (define-key map [(shift tab)] 'emacs-wiki-previous-reference)
    (unless (featurep 'xemacs)
      (define-key map [(shift iso-lefttab)]
        'emacs-wiki-previous-reference)
      (define-key map [(shift control ?i)]
        'emacs-wiki-previous-reference))

    (when (featurep 'pcomplete)
      (define-key map [(meta tab)] 'pcomplete)
      (define-key map [(meta control ?i)] 'pcomplete))

    map)
  "Keymap used by emacs-wiki-mode.")

(defvar emacs-wiki-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'emacs-wiki-follow-name-at-point)
    (define-key map [(control ?m)] 'emacs-wiki-follow-name-at-point)
    (define-key map [(shift return)]
      'emacs-wiki-follow-name-at-point-other-window)
    (if (featurep 'xemacs)
        (progn
          (define-key map [(button2)]
            'emacs-wiki-follow-name-at-mouse)
          (define-key map [(shift button2)]
            'emacs-wiki-follow-name-at-mouse-other-window))
      (define-key map [(shift control ?m)]
        'emacs-wiki-follow-name-at-point-other-window)
      (define-key map [mouse-2] 'emacs-wiki-follow-name-at-mouse)
      (define-key map [(shift mouse-2)]
        'emacs-wiki-follow-name-at-mouse-other-window)
      (unless (eq emacs-major-version 21)
        (set-keymap-parent map emacs-wiki-mode-map)))
    map)
  "Local keymap used by emacs-wiki while on a WikiName.")

;; Code:

(defvar emacs-wiki-project nil)

(require 'emacs-wiki-publish)

;;;###autoload
(define-derived-mode emacs-wiki-mode text-mode "Wiki"
  "An Emacs mode for maintaining a local Wiki database.

Wiki is a hypertext and a content management system: Normal users
are encouraged to enhance the hypertext by editing and
refactoring existing wikis and by adding more.  This is made easy
by requiring a certain way of writing the wikis.  It is not as
complicated as a markup language such as HTML.  The general idea
is to write plain ASCII.

Words with mixed case such as ThisOne are WikiNames.  WikiNames
are links you can follow.  If a wiki with that name exists, you
will be taken there.  If such a does not exist, following the
link will create a new wiki for you to fill.  WikiNames for
non-existing wikis are rendered as links with class
\"nonexistent\", and are also displayed in a warning color so
that you can see whether following the link will lead you
anywhere or not.

In order to follow a link, hit RET when point is on the link, or use
mouse-2.

All wikis reside in the `emacs-wiki-directories'.

\\{emacs-wiki-mode-map}"
  ;; because we're not inheriting from normal-mode, we need to
  ;; explicitly run file variables if the user wants to
  (condition-case err
      (hack-local-variables)
    (error (message "File local-variables error: %s"
                    (prin1-to-string err))))
  ;; set the current project
  (emacs-wiki-change-project (or emacs-wiki-project
                                 emacs-wiki-default-project))
  ;; bootstrap the file-alist, if it's not been read in yet
  (emacs-wiki-file-alist)
  ;; if pcomplete is available, set it up!
  (when (featurep 'pcomplete)
    (set (make-variable-buffer-local
          'pcomplete-default-completion-function)
         'emacs-wiki-completions)
    (set (make-variable-buffer-local
          'pcomplete-command-completion-function)
         'emacs-wiki-completions)
    (set (make-variable-buffer-local
          'pcomplete-parse-arguments-function)
         'emacs-wiki-current-word))
  ;; Make fill not split up links
  (when (boundp 'fill-nobreak-predicate)
    (make-local-variable 'fill-nobreak-predicate)
    ;; Work around annoying inconsistency in fill handling
    (if (not (emacs-wiki-extreg-usable-p))
        (setq fill-nobreak-predicate 'emacs-wiki-fill-nobreak-p)
      (add-to-list 'fill-nobreak-predicate
                   'emacs-wiki-fill-nobreak-p)))
  ;; Make adaptive fill work nicely with item lists
  (set (make-local-variable 'adaptive-fill-regexp)
       (concat "[" emacs-wiki-regexp-blank "]*\\(-+["
               emacs-wiki-regexp-blank
               "]*\\|[0-9]+\\.["
               emacs-wiki-regexp-blank "]*\\)*"))
  ;; avoid problems caused by emacs-wiki-url-protocols not getting
  ;; custom-set properly.
  (emacs-wiki-set-sym-and-url-regexp 'emacs-wiki-url-protocols
                                     emacs-wiki-url-protocols)
  ;; update the project settings -- this used to not be necessary, but
  ;; apparently the `customize' handling changed slightly in Emacs.
  (run-hooks 'emacs-wiki-update-project-hook)
  ;; avoid problems caused by the 'intangible' text property with
  ;; paragraph filling.
  (set (make-local-variable 'inhibit-point-motion-hooks) t))

;; Make flyspell not mess with links
(put 'emacs-wiki-mode
     'flyspell-mode-predicate
     'emacs-wiki-flyspell-p)

(defsubst emacs-wiki-page-file (page &optional no-check-p)
  "Return a filename if PAGE exists within the current Wiki."
  (cdr (assoc page (emacs-wiki-file-alist no-check-p))))

;;; Compatibility functions

(defun emacs-wiki-line-end-position (&optional N)
  (if (fboundp 'line-end-position)
      (line-end-position N)
    (save-excursion (end-of-line N) (point))))

(defun emacs-wiki-line-beginning-position (&optional N)
  (if (fboundp 'line-beginning-position)
      (line-beginning-position N)
    (save-excursion (beginning-of-line N) (point))))

(defun emacs-wiki-match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
num specifies which parenthesized expression in the last regexp."
  (if (fboundp 'match-string-no-properties)
      (match-string-no-properties num string)
    (match-string num string)))

(defun emacs-wiki-derived-mode-p (&rest modes)
    "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace
backwards."
  (if (fboundp 'derived-mode-p)
      (derived-mode-p modes)
    ;; PUBLIC: find if the current mode derives from another.
    ;; Taken from GNU Emacs 21 subr.el
    (let ((parent major-mode))
      (while (and (not (memq parent modes))
                  (setq parent (get parent 'derived-mode-parent))))
      parent)))

(defun emacs-wiki-replace-regexp-in-string
  (regexp replacement text &optional substitute)
  "Replace REGEXP with REPLACEMENT in TEXT.
If SUBSTITUTE is non-nil, use `replace-match'-style substitutions."
  ;; This inversion is necessary because old code had been written to
  ;; always literally replace.
  (cond
   ((fboundp 'replace-in-string)
    (let ((case-fold-search nil))
      (replace-in-string text regexp replacement (not substitute))))
   ((fboundp 'replace-regexp-in-string)
    (let ((case-fold-search nil))
      (replace-regexp-in-string regexp replacement text t
                                (not substitute))))
   (t (while (string-match regexp text)
        (setq text (replace-match replacement t
                                  (not substitute) text)))
      text)))

;;; Other functions

(defun emacs-wiki-fill-nobreak-p ()
  "Return nil if we should allow a fill to occur at point.
Otherwise return non-nil.

This is used to keep long extended links from being mangled by
autofill."
  (save-excursion
    (save-match-data
      (and (re-search-backward "\\[\\[\\|\\]\\]"
                               (emacs-wiki-line-beginning-position) t)
           (string= (or (match-string 0) "")
                    "[[")))))

(defun emacs-wiki-flyspell-p ()
  "Return non-nil if we should allow spell-checking to occur at point.
Otherwise return nil.

This is used to keep links from being improperly colorized by flyspell."
  (save-match-data
    (null (emacs-wiki-link-at-point))))

(defun emacs-wiki-directories-member (&optional directories)
  "Return non-nil if the current buffer is in `emacs-wiki-directories'."
  (let ((name (or emacs-wiki-current-file buffer-file-name)))
    (unless (null name)
      (let ((here (file-truename          ; resolve symbolic links
                   (expand-file-name (file-name-directory name))))
            (d (or directories emacs-wiki-directories))
            yes)
        (save-match-data
          (while d
            (let ((dir (file-name-as-directory (if (consp (car d))
                                                   (caar d) (car d)))))
              (if (and (string-match
                        (concat "^" (regexp-quote
                                     (file-truename
                                      (expand-file-name dir))))
                        here)
                       ;; not an ignored file
                       (not (string-match emacs-wiki-file-ignore-regexp name)))
                  (setq yes (car d) d nil)
                (setq d (cdr d))))))
        yes))))

(defun emacs-wiki-maybe (&optional check-only)
  "Maybe turn emacs-wiki mode on for this file."
  (let ((projs emacs-wiki-projects)
        (mode-func 'emacs-wiki-mode)
        project yes)
    (while (and (not yes) projs)
      (let* ((projsyms (cdar projs))
             (pred (assq 'emacs-wiki-predicate projsyms))
             dirs)
        (if pred
            (setq yes (funcall (cdr pred)))
          (setq dirs (assq 'emacs-wiki-directories projsyms))
          (if dirs
              (setq yes (emacs-wiki-directories-member (cdr dirs)))))
        (if yes
            (setq project (caar projs)
                  mode-func (or (cdr (assq 'emacs-wiki-major-mode
                                           projsyms))
                                mode-func))))
      (setq projs (cdr projs)))
    (setq yes (or yes (emacs-wiki-directories-member)))
    (if (and yes (not check-only))
        (let ((emacs-wiki-project project))
          (funcall mode-func)))
    yes))

(add-hook 'find-file-hooks 'emacs-wiki-maybe)

;;; Support WikiName completion using pcomplete

(defun emacs-wiki-completions ()
  "Return a list of possible completions names for this buffer."
  (while (pcomplete-here
          (mapcar 'car (append (emacs-wiki-file-alist)
                               emacs-wiki-interwiki-names)))))

(defun emacs-wiki-current-word ()
  (let ((end (point)))
    (save-excursion
      (save-restriction
        (skip-chars-backward (concat "^["
                                     emacs-wiki-regexp-space
                                     "\\"))
        (narrow-to-region (point) end))
      (pcomplete-parse-buffer-arguments))))

;;; Return an list of known wiki names and the files they represent.

(defsubst emacs-wiki-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun emacs-wiki-page-name (&optional name)
  "Return the canonical form of the Wiki page name.
All this means is that certain extensions, like .gz, are removed."
  (save-match-data
    (unless name
      (setq name (or emacs-wiki-current-file
                     buffer-file-name
                     (concat default-directory (buffer-name)))))
    (if name
        (let ((page (file-name-nondirectory name)))
          (if (string-match emacs-wiki-ignored-extensions-regexp page)
              (replace-match "" t t page)
            page)))))

(defun emacs-wiki-page-title (&optional name)
  "Return the canonical form of the Wiki page name.
All this means is that certain extensions, like .gz, are removed."
  (or emacs-wiki-current-page-title
      (emacs-wiki-prettify-title (emacs-wiki-page-name name))))

(defun emacs-wiki-page-date ()
  "Return this page's publishing date."
  (or emacs-wiki-current-page-date
      (format-time-string
       "%e %B %Y" (nth 5 (file-attributes (or emacs-wiki-current-file
                                              buffer-file-name))))))

(defcustom emacs-wiki-recurse-directories
  nil
  "Determine whether or not to descend recursively into directories.
nil means do not recurse, non-nil means recurse.

Currently recursing into directories does not work well.  It is
much better to explicitly list the directories you need in
`emacs-wiki-directories' or in the proper section of
`emacs-wiki-projects' if you use multiple projects."
  :type 'boolean
  :group 'emacs-wiki)

(defun emacs-wiki-walk-directories (directories)
  "Return a list of subdirectories in DIRECTORIES, excluding hidden dirs.
Directory paths are expanded to avoid duplication.  You can use
this when initializing `emacs-wiki-directories'.

Only return subdirectories if `emacs-wiki-recurse-directories' is
non-nil, since this feature was causing some problems for some
users."
  (if emacs-wiki-recurse-directories
      (let (d)
        (while directories
          (let ((pending-dir (expand-file-name (car directories))))
            (and (file-directory-p (car directories))
                 (not (member pending-dir d))
                 (setq d (append d (list (car directories)))
                       directories
                       (append directories
                               (directory-files
                                (car directories) t "^[^.]" t)))))
          (setq directories (cdr directories)))
        d)
    directories))

(defvar emacs-wiki-file-alist nil)

(defcustom emacs-wiki-refresh-file-alist-p
  nil
"Decide whether the file list cache needs to be refreshed.
If this is a function, the return value decides if the file list
cache needs to be refreshed.
If a variable, the value nil means never refresh. If non-nil,
always refresh.

Normally emacs-wiki will try to keep the file name cache up to date,
but if wiki files are created outside of emacs-wiki you may need to
experiment with this variable.

Using non-nil for this option will slow down updating of pages
considerably.

See `emacs-wiki-refresh-file-alist-maybe'."
  :type '(choice
          (const :tag "Do not automatically refresh" nil)
          (const :tag "Always automatically refresh" t)
          (function :tag "Function"))
  :group 'emacs-wiki)

(defun emacs-wiki-refresh-file-alist-maybe ()
  "Return non-nil if any of the files have been modified.
Suitable for use as `emacs-wiki-refresh-file-alist-p'."
  (let* ((file-alist (assoc emacs-wiki-current-project
                            emacs-wiki-file-alist))
         (d emacs-wiki-directories) last-mod)
    ;; Set `last-mod' to modification time of last modified directory.
    (while d
      (let ((mod-time (nth 5 (file-attributes (car d)))))
        (if (or (null last-mod)
                (and mod-time (emacs-wiki-time-less-p last-mod
                                                      mod-time)))
            (setq last-mod mod-time)))
      (setq d (cdr d)))
    (or (null (cddr file-alist))
        (null last-mod)
        (emacs-wiki-time-less-p (cddr file-alist)
                                last-mod))))

(defun emacs-wiki-refresh-file-alist ()
  "Force a refresh of the file alist."
  (interactive)
  (let* ((file-alist (assoc emacs-wiki-current-project
                            emacs-wiki-file-alist))
         last-mod)
    ;; Store the current `last-mod' against the
    ;; `emacs-wiki-current-project' assoc key in
    ;; `emacs-wiki-file-alist' to timestamp this recalculation of the
    ;; list of files.
    (dolist (dir emacs-wiki-directories)
      (let ((mod-time (nth 5 (file-attributes dir))))
        (if (or (null last-mod)
                (and mod-time (emacs-wiki-time-less-p last-mod
                                                      mod-time)))
            (setq last-mod mod-time))))
    (if file-alist
        (setcdr (cdr file-alist) last-mod)
      (setq file-alist (cons emacs-wiki-current-project
                             (cons nil last-mod))
            emacs-wiki-file-alist (cons file-alist
                                        emacs-wiki-file-alist)))
    ;; Add the current list of subdirectories to
    ;; `emacs-wiki-directories'.  If a new file or directory is added
    ;; to a directory in this list, the modification time of one of
    ;; these directories will change, the `last-mod' checks above will
    ;; notice, and the files and/or directories that triggered the
    ;; change will be added below.
    (setq emacs-wiki-directories
          (emacs-wiki-walk-directories emacs-wiki-directories))
    ;; Recalculate the list of files.
    (save-match-data
      (let* ((dirs emacs-wiki-directories)
             (names (list t))
             (lnames names))
        (while dirs
          (if (file-readable-p (car dirs))
              (let ((files (directory-files (car dirs) t nil t)))
                (while files
                  (unless
                      (or (file-directory-p (car files))
                          (string-match emacs-wiki-file-ignore-regexp
                                        (file-name-nondirectory
                                         (car files)))
                          (string-match
                           emacs-wiki-full-path-ignore-regexp
                           (car files)))
                    (setcdr lnames
                            (cons
                             (cons
                              (emacs-wiki-page-name (car files))
                              (car files)) nil))
                    (setq lnames (cdr lnames)))
                  (setq files (cdr files)))))
          (setq dirs (cdr dirs)))
        (setcar
         (cdr file-alist)
         (cdr names))))))

(defun emacs-wiki-file-alist (&optional no-check-p)
  "Return possible Wiki filenames in `emacs-wiki-directories'."
  (let ((file-list
         (cadr (assoc emacs-wiki-current-project
                      emacs-wiki-file-alist))))
    (when (or (null file-list)
              no-check-p
              (if (functionp emacs-wiki-refresh-file-alist-p)
                  (funcall emacs-wiki-refresh-file-alist-p)
                emacs-wiki-refresh-file-alist-p))
      (emacs-wiki-refresh-file-alist)
      (setq file-list (cadr (assoc emacs-wiki-current-project
                                   emacs-wiki-file-alist))))
    file-list))

(defun emacs-wiki-complete-alist ()
  "Return equivalent of calling (emacs-wiki-file-alist) for all projects."
  (let ((emacs-wiki-current-project "_CompositeFileList")
        (emacs-wiki-directories
         (copy-alist emacs-wiki-directories))
        (projs emacs-wiki-projects))
    (while projs
      (let* ((projsyms (cdar projs))
             (dirs (cdr (assq 'emacs-wiki-directories projsyms))))
        (while dirs
          (add-to-list 'emacs-wiki-directories (car dirs))
          (setq dirs (cdr dirs))))
      (setq projs (cdr projs)))
    (emacs-wiki-file-alist)))

(defmacro emacs-wiki-with-temp-buffer (&rest body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
See also `with-temp-file' and `with-output-to-string'.
Unlike `with-temp-buffer', this will never attempt to save the temp buffer."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *emacs-wiki-temp*")))
       (unwind-protect
           (if debug-on-error
               (with-current-buffer ,temp-buffer
                 ,@body)
             (condition-case err
                 (with-current-buffer ,temp-buffer
                   ,@body)
               (error
                (if (fboundp 'display-warning)
                    (display-warning 'emacs-wiki
                                     (format "%s: Error occurred: %s"
                                             (emacs-wiki-page-name)
                                             err)
                                     (if (featurep 'xemacs)
                                         'warning
                                       :warning))
                  (message "%s: Error occurred: %s"
                           (emacs-wiki-page-name)
                           err)))))
         (when (buffer-live-p ,temp-buffer)
           (with-current-buffer ,temp-buffer
             (set-buffer-modified-p nil))
           (unless debug-on-error (kill-buffer ,temp-buffer)))))))
(put 'emacs-wiki-with-temp-buffer 'lisp-indent-function 0)
(put 'emacs-wiki-with-temp-buffer 'edebug-form-spec '(body))

;;; Utility functions to extract parts of a Wiki name

(defsubst emacs-wiki-wiki-url-p (name)
  "Return non-nil if NAME is a URL."
  (and name
       (save-match-data
         (string-match emacs-wiki-url-regexp name))))

(defun emacs-wiki-wiki-visible-name-1 (base tag)
  "Figure out what should be displayed for a Wiki link.
Either one of BASE or TAG must be specified."
  ;; Split this part out from `emacs-wiki-wiki-visible-name' since it
  ;; was getting too hard to indent properly.
  (if (or (null base)
          (string= "" base))
      (emacs-wiki-page-name tag)
    (concat (if (and emacs-wiki-project-remove-last-word
                     (assoc base emacs-wiki-projects))
                (replace-regexp-in-string
                 (concat "[" emacs-wiki-regexp-upper "]"
                         "[" emacs-wiki-regexp-lower "]+$")
                 "" base)
              base)
            (unless (or (null tag)
                        (string= "" tag))
              (concat ":" tag)))))

(defun emacs-wiki-wiki-visible-name (wiki-name)
  "Return the visible part of a Wiki link.
This only really means something if [[extended][links]] are involved."
  (save-match-data
    (let ((case-fold-search nil)
          (name wiki-name))
      (if (string-match emacs-wiki-extended-link-regexp name)
          (if (match-string 2 name)
              (setq name (match-string 3 name))
            (setq name (match-string 1 name))
            (when (string-match "^mailto:" name)
              (setq name (substring name (match-end 0))))))
      (when (string-match (concat "^" emacs-wiki-name-regexp "$")
                          name)
        (string-match "#" name)
        (let ((base (substring name 0 (match-beginning 0)))
              (tag (substring name (match-end 0))))
          (when (= 0 (match-beginning 0))
            ;; figure out whether name is a base or a tag
            (if (assoc name emacs-wiki-interwiki-names)
                (setq base name)
              (setq tag name)))
          (setq name (emacs-wiki-wiki-visible-name-1 base tag))))
      (emacs-wiki-link-unescape name t))))

(defun emacs-wiki-wiki-tag (wiki-name)
  (save-match-data
    (if (string-match "#" wiki-name)
        (substring wiki-name (match-end 0)))))

;; FIXME: Find a more efficient way to do this.
(defun emacs-wiki-link-escape (text &optional further)
  "Escape dangerous characters in TEXT.

If FURTHER is set to t, which indicates link description, escape
brackets and #.  If FURTHER is unspecified or nil, which
indicates link destination, escape brackets and spaces."
  (when text
    (save-match-data
      (emacs-wiki-replace-regexp-in-string
       "\\[" "%5B"
       (emacs-wiki-replace-regexp-in-string
        "\\]" "%5D"
        (if further
            (emacs-wiki-replace-regexp-in-string
             "#" "%23" text)
          (emacs-wiki-replace-regexp-in-string
           " " "%20" text)))))))

(defun emacs-wiki-link-unescape (text &optional further)
  "Unescape dangerous characters in TEXT.

If FURTHER is set to t, which indicates link description,
unescape brackets and #.  If FURTHER is unspecified or nil, which
indicates link destination, unescape brackets and spaces."
  (when text
    (save-match-data
      (emacs-wiki-replace-regexp-in-string
       "%5B" "["
       (emacs-wiki-replace-regexp-in-string
        "%5D" "]"
        (if further
            (emacs-wiki-replace-regexp-in-string
             "%23" "#" text)
          (emacs-wiki-replace-regexp-in-string
           "%20" " " text)))))))

(defun emacs-wiki-make-link (link &optional name)
  "Return a Wiki link to LINK with NAME as the text."
  (let ((case-fold-search nil))
    (unless name
      (setq name (emacs-wiki-wiki-visible-name link)))
    (when (string-match "^\\[\\[\\([^]]+\\)\\]" link)
      (setq link (match-string 1 link)))
    (when (equal name link)
      (setq name nil))
    (if name
        (if (and (string-match
                  (concat "^\\(?:" emacs-wiki-url-or-name-regexp
                          "\\)$") link)
                 (string= name link))
            name
          (concat "[[" (emacs-wiki-link-escape link)
                  "][" (emacs-wiki-link-escape name t) "]]"))
      ;; No name
      (if (string-match
           (concat "^\\(?:" emacs-wiki-url-or-name-regexp
                   "\\)$") link)
          link
        (concat "[[" (emacs-wiki-link-escape link) "]]")))))

(defun emacs-wiki-wiki-link-target (wiki-name)
  "Return the target of a Wiki link.
This might include anchor tags."
  (save-match-data
    (let ((name wiki-name) lookup)
      (if (string-match "^\\[\\[\\([^]]+\\)\\]" name)
          (setq name (match-string 1 name)))
      (unless (or (not (> (length name) 0))
                  (emacs-wiki-page-file name))
        (if (and emacs-wiki-interwiki-names
                 (string-match "\\`\\([^#]+\\)\\(#\\(.+\\)\\)?\\'" name)
                 (setq lookup (assoc
                                (or (match-string 1 name)
                                    name)
                                emacs-wiki-interwiki-names)))
            (let ((tag (match-string 3 name))
                  (target (cdr lookup)))
              (setq name (if (stringp target)
                             (concat target tag)
                           (funcall target tag))))
          (if (eq (aref name 0) ?#)
              (setq name (concat (emacs-wiki-page-name) name)))))
      (emacs-wiki-link-unescape name))))

(defun emacs-wiki-wiki-base (wiki-name)
  "Find the WikiName or URL mentioned by a Wiki link.
This means without tags, in the case of a WikiName."
  (save-match-data
    (let ((file (emacs-wiki-wiki-link-target wiki-name)))
      (when file
        (if (emacs-wiki-wiki-url-p file)
            file
          (if (string-match "#" file)
              (substring file 0 (match-beginning 0))
            file))))))

;;; Open a Wiki page (with completion)

(defvar emacs-wiki-history-list nil)

(defun emacs-wiki-read-name (file-alist &optional prompt)
  "Read the name of a valid Wiki page from minibuffer, with completion."
  (let* ((default emacs-wiki-default-page)
         (str (completing-read
               (format "%s(default: %s) " (or prompt "Wiki page: ")
                       default)
               file-alist nil nil nil 'emacs-wiki-history-list)))
        (if (or (null str) (= (length str) 0))
            default
          str)))

;;;###autoload
(defun emacs-wiki-find-file (wiki &optional command directory)
  "Open the Emacs Wiki page WIKI by name.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the Wiki
page will be created if it does not already exist."
  (interactive
   (list
    (let ((num (prefix-numeric-value current-prefix-arg)))
       (if (< num 16)
           (let* ((file-alist (if (= num 4)
                                  (emacs-wiki-complete-alist)
                                (emacs-wiki-file-alist)))
                  (name (emacs-wiki-read-name file-alist)))
             (cons name (cdr (assoc name file-alist))))
         (let ((name (read-file-name "Open wiki file: ")))
           (cons name name))))))
  (unless (interactive-p)
    (setq wiki (cons wiki
                     (cdr (assoc wiki (emacs-wiki-file-alist))))))
  ;; At this point, `wiki' is (GIVEN-PAGE FOUND-FILE).
  (if (cdr wiki)
      (let ((buffer (funcall (or command 'find-file) (cdr wiki))))
        (if (= (prefix-numeric-value current-prefix-arg) 16)
            (with-current-buffer buffer
              (set (make-variable-buffer-local 'emacs-wiki-directories)
                   (cons (file-name-directory (cdr wiki))
                         emacs-wiki-directories))
              (set (make-variable-buffer-local 'emacs-wiki-file-alist)
                   nil)))
        buffer)
    (let* ((dirname (or directory
                        (emacs-wiki-maybe t)
                        (car emacs-wiki-directories)))
           (filename (expand-file-name (car wiki) dirname)))
      (unless (file-exists-p dirname)
        (make-directory dirname t))
      (funcall (or command 'find-file) filename))))

;;; Navigate/visit links or URLs.  Use TAB, S-TAB and RET (or mouse-2).

(defun emacs-wiki-next-reference ()
  "Move forward to next Wiki link or URL, cycling if necessary."
  (interactive)
  (let ((case-fold-search nil)
        (cycled 0) pos)
    (save-excursion
      (if (emacs-wiki-link-at-point)
          (goto-char (match-end 0)))
      (save-match-data
        (while (< cycled 2)
          (if (re-search-forward emacs-wiki-url-or-name-regexp nil t)
              (when (get-text-property (match-beginning 0)
                                       'keymap)
                (setq pos (match-beginning 0)
                      cycled 2))
            (goto-char (point-min))
            (setq cycled (1+ cycled))))))
    (if pos
        (goto-char pos))))

(defun emacs-wiki-previous-reference ()
  "Move backward to the next Wiki link or URL, cycling if necessary.
This function is not entirely accurate, but it's close enough."
  (interactive)
  (let ((case-fold-search nil)
        (cycled 0) pos)
    (save-excursion
      (save-match-data
        (while (< cycled 2)
          (if (re-search-backward emacs-wiki-url-or-name-regexp nil t)
              (when (get-text-property (match-beginning 0)
                                       'keymap)
                (setq pos (point)
                      cycled 2))
            (goto-char (point-max))
            (setq cycled (1+ cycled))))))
    (if pos
        (goto-char pos))))

(defun emacs-wiki-browse-url (url &optional other-window)
  "Handle URL with the function specified in `emacs-wiki-url-protocols'.
If OTHER-WINDOW is non-nil, open in a different window."
  (interactive (list (read-string "URL: ")
                     current-prefix-arg))
  (when other-window
    (switch-to-buffer-other-window (current-buffer)))
  (when (string-match emacs-wiki-url-regexp url)
    (let ((entry (assoc (match-string 1 url) emacs-wiki-url-protocols)))
      (when entry
        (funcall (cadr entry) url)))))

(defun emacs-wiki-get-current-backlink ()
  "Return the backlink in the current page or nil if there is none."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at "\\*\\*/.*\\*\\*\n")
        (buffer-substring
         (+ (point-min) 2) (- (match-end 0) 3))))))

(defun emacs-wiki-insert-backlink (backlink &optional parent-links)
  "Insert BACKLINK in the current page.
If PARENT-LINKS is non-nil, insert it before BACKLINK."
  (save-excursion
    (goto-char (point-min))
    (insert "**")
    (when parent-links
      (insert parent-links " "))
    (insert "/ " backlink "**\n\n")))

(defun emacs-wiki-visit-link
  (link-name &optional refresh-buffer other-window)
  "Visit the URL or link named by LINK-NAME.
REFRESH-BUFFER is an optional buffer to refresh on saving the
visited page.  This makes the bad link face in the linking buffer
go away."
  (let ((link (emacs-wiki-wiki-link-target link-name)))
    (if (emacs-wiki-wiki-url-p link)
        (emacs-wiki-browse-url link other-window)
      ;; The name list is current since the last time the buffer was
      ;; highlighted
      (let* ((base (emacs-wiki-wiki-base link-name))
             (file (emacs-wiki-page-file base))
             (tag  (and (not (emacs-wiki-wiki-url-p link))
                        (emacs-wiki-wiki-tag link)))
             (find-file-function (if other-window
                                     'find-file-other-window
                                   'find-file))
             ;; the name of the buffer that contains the link.  check
             ;; whether buffer-name is a WikiName, else make it one
             (parent-name (or (emacs-wiki-page-name)
                              (concat "[[" (buffer-name) "]]")))
             (parent-allowed-backlink-p
              (not (string-match
                    emacs-wiki-exclude-backlink-parent-regexp
                    parent-name)))
             (parent-backlink (and parent-allowed-backlink-p
                                   (emacs-wiki-get-current-backlink)))
             (newbuf (if (null file)
                         (funcall find-file-function
                                  (expand-file-name
                                   base
                                   (file-name-directory
                                    (buffer-file-name))))
                       (funcall find-file-function file)))
             (emacs-wiki-create-backlinks
              (and emacs-wiki-create-backlinks parent-allowed-backlink-p))
             (make-link-p (and emacs-wiki-create-backlinks
                               ;; insert backlink only in new files
                               (null file)
                               (not (string-match
                                     emacs-wiki-exclude-backlink-regexp
                                     (emacs-wiki-page-name
                                      (buffer-file-name newbuf)))))))
        (when tag
          (goto-char (point-min))
          (save-match-data (re-search-forward (concat "^\\.?#" tag) nil t)))
        (when refresh-buffer
          (when make-link-p
            (emacs-wiki-insert-backlink parent-name parent-backlink))
          ;; todo: is with-current-buffer necessary here?
          (with-current-buffer newbuf
            (add-hook 'after-save-hook
                      'emacs-wiki-refresh-buffers-once t t)
            ;; save the new file to avoid another backlink if the
            ;; buffer is visited again before being saved
            (when (or make-link-p
                      (and (null file)
                           (not parent-allowed-backlink-p)))
              ;; set modified status to t: pages with excluded
              ;; parents may not be modified, but we want to save
              ;; anyway
              (set-buffer-modified-p t)
              (save-buffer))))))))

(defun emacs-wiki-refresh-buffers-once ()
  "Refresh all the buffers once from after-save-hook.
Call it first to add it to the hook, then the hook will remove
the function."
  (remove-hook 'after-save-hook 'emacs-wiki-refresh-buffers-once t)
  (emacs-wiki-refresh-buffers))

(defun emacs-wiki-refresh-buffers (&rest args)
  "Rebuild file alist and refresh current project.
Call after creating a page."
  (interactive)
  (emacs-wiki-refresh-file-alist)
  (let ((my-project emacs-wiki-current-project))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal emacs-wiki-current-project my-project)
          (emacs-wiki-highlight-buffer))))))

(defun emacs-wiki-link-at-point (&optional pos)
  "Return non-nil if a URL or Wiki link name is at point."
  (if (or (null pos)
          (and (char-after pos)
               (not (eq (char-syntax (char-after pos)) ? ))))
      (let ((case-fold-search nil)
            (here (or pos (point)))
            there)
        (save-excursion
          (goto-char here)
          ;; There is a bug in `skip-chars-backward' that makes it
          ;; necessary to use (concat "\n" emacs-wiki-regexp-blank)
          ;; rather than just `emacs-wiki-regexp-space'.
          (skip-chars-backward (concat "^['\"<>{}(\n"
                                       emacs-wiki-regexp-blank))
          (setq there (point))
          (if (save-match-data
                (and (re-search-backward "\\[\\[\\|\\]\\]"
                                         (emacs-wiki-line-beginning-position)
                                         t)
                     (string= (or (match-string 0) "")
                              "[[")))
              (and (looking-at emacs-wiki-extended-link-regexp)
                   (<= here (match-end 0)))
            (goto-char there)
            (looking-at emacs-wiki-url-or-name-regexp))))))

(defun emacs-wiki-follow-name-at-point (&optional other-window)
  "Visit the link at point, or insert a newline if none."
  (interactive "P")
  ;; if we're visiting a bad link, pass the current buffer to the
  ;; visiting function so that it can be refreshed on saving the new
  ;; page
  (let (buf)
    (when (eq (get-text-property (point) 'face)
              'emacs-wiki-bad-link-face)
      (setq buf (current-buffer)))
    (if (emacs-wiki-link-at-point)
        (emacs-wiki-visit-link (emacs-wiki-match-string-no-properties 0)
                               buf other-window)
      (error "There is no valid link at point"))))

(defun emacs-wiki-follow-name-at-point-other-window ()
  "Visit the link at point in other window."
  (interactive)
  (emacs-wiki-follow-name-at-point t))

(defun emacs-wiki-follow-name-at-mouse (event &optional other-window)
  "Visit the link at point, or yank text if none."
  (interactive "eN")
  ;; if we're visiting a bad link, pass the current buffer to the
  ;; visiting function so that it can be refreshed on saving the new
  ;; page
  (let ((path)
        (buf))
    (save-excursion
      (cond ((featurep 'xemacs)    ; XEmacs
             (set-buffer (window-buffer (event-window event)))
             (and (event-point event)
                  (goto-char (event-point event))))
            ((fboundp 'posn-window)     ; Emacs
             (set-buffer (window-buffer
                          (posn-window (event-start event))))
             (goto-char (posn-point (event-start event)))))
      (when (emacs-wiki-link-at-point)
        (setq path (match-string 0))
        (when (eq (get-text-property (point) 'face)
                  'emacs-wiki-bad-link-face)
          (setq buf (current-buffer)))))
    (when path
      (emacs-wiki-visit-link path buf other-window))))

(defun emacs-wiki-follow-name-at-mouse-other-window (event)
  "Visit the link at point"
  (interactive "e")
  ;; Throw away the old window position, since other-window will
  ;; change it anyway
  (select-window (car (car (cdr event))))
  (emacs-wiki-follow-name-at-mouse event t))

(defun emacs-wiki-rename-link-file (link-name new-name)
  (when (emacs-wiki-wiki-url-p link-name)
    (error "Can't rename a URL"))
  (let* ((base (emacs-wiki-wiki-base link-name))
         (file (emacs-wiki-page-file base)))
    (if (null file)
        (rename-file base new-name)
      (rename-file file new-name))))

(defun emacs-wiki-rename-link (old-name new-name)
  "Change OLD-NAME to NEW-NAME throughout the current file.
It is assumed that OLD-NAME is a Wiki page that has recently been
renamed on the file system."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\(\\b\\|\\[\\|#\\)" old-name
                      "\\(\\b\\|\\]\\)")
              nil t)
        (replace-match
         (concat "\\1" new-name "\\2"))))))

(defun emacs-wiki-rename-link-at-point ()
  "Rename the link at current point, and the location it points to.
This does not work with URLs, and will preserve a description in
an extended link.

The new name will be applied throughout the current buffer.  Do
not try to move a file outside its current directory with this
function, since it does not (yet) know how to update the Wiki
link accordingly."
  (interactive "*")
  (let (new-name old-name)
    (if (emacs-wiki-link-at-point)
        (progn
          (setq old-name (emacs-wiki-wiki-base
                          (emacs-wiki-match-string-no-properties 0)))
          (setq new-name
                (read-from-minibuffer "Rename file to: " old-name))
          (when (string= old-name new-name)
            (error "Nothing to do"))

          (emacs-wiki-rename-link-file old-name new-name)
          (emacs-wiki-rename-link
           (file-name-nondirectory old-name)
           (file-name-nondirectory new-name))
          ;; make the new link appear in the correct face
          (emacs-wiki-refresh-buffers))
      (error "There is no valid link at point"))))

(defun emacs-wiki-delete-link (link-name)
  "Delete the file which link-name corresponds to"
  (when (emacs-wiki-wiki-url-p link-name)
    (error "Can't rename a URL"))
  (let* ((base (emacs-wiki-wiki-base link-name))
         (file (emacs-wiki-page-file base)))
    (if (null file)
        (delete-file base)
      (delete-file file))))

(defun emacs-wiki-delete-link-at-point ()
  "Delete the link under point, and the location it points to.
This does not work with URL's."
  (interactive "*")
  (let (name)
    (if (emacs-wiki-link-at-point)
        (progn
          (setq name (match-string 0))
          (when (yes-or-no-p
                 (concat "Delete "
                         name "? You can not undo this. "))
            (emacs-wiki-delete-link name)
            (replace-match "" nil t)))
      (error "There is no valid link at point"))))

;;; Find text in Wiki pages, or pages referring to the current page

(defvar emacs-wiki-search-history nil)

(defun emacs-wiki-grep (string &optional grep-command-no-shadow)
  "Grep for STRING in the Wiki directories.
GREP-COMMAND if passed will supplant `emacs-wiki-grep-command'."
  ;; careful - grep-command leaks into compile, so we call it
  ;; -no-shadow instead
  (require 'compile)
  (let ((str (or grep-command-no-shadow emacs-wiki-grep-command))
        (dirs (mapconcat (lambda (dir)
                           (shell-quote-argument
                            (expand-file-name dir)))
                         emacs-wiki-directories " ")))
    (while (string-match "%W" str)
      (setq str (replace-match string t t str)))
    (while (string-match "%D" str)
      (setq str (replace-match dirs t t str)))
    (if (fboundp 'compilation-start)
        (compilation-start str nil (lambda (&rest args) "*search*")
                           grep-regexp-alist)
      (compile-internal str "No more search hits" "search"
                        nil grep-regexp-alist))))

(defun emacs-wiki-search (text)
  "Search for the given TEXT string in the Wiki directories."
  (interactive
   (list (let ((str (concat emacs-wiki-grep-command)) pos)
           (when (string-match "%W" str)
             (setq pos (match-beginning 0))
             (unless (featurep 'xemacs)
               (setq pos (1+ pos)))
             (setq str (replace-match "" t t str)))
           (read-from-minibuffer "Search command: "
                                 (cons str pos) nil nil
                                 'emacs-wiki-search-history))))
  (emacs-wiki-grep nil text))

(defun emacs-wiki-backlink ()
  "Grep for the current pagename in all the Wiki directories."
  (interactive)
  (emacs-wiki-grep (emacs-wiki-page-name)))

;;; Generate an index of all known Wiki pages

(defvar emacs-wiki-index-title-threshold nil
  "*If nil, filenames are always used in the index.
This is faster, but less informative. If a positive integer, only
that many bytes will be scanned for a #title directive.
Otherwise the entire wiki file is scanned for a #title.")

(defun emacs-wiki-get-title-fast (filename)
  "Return the title in FILENAME.
Scan only the first `emacs-wiki-index-title-threshold' bytes."
  (emacs-wiki-with-temp-buffer
    (insert-file-contents filename nil 0
                          (and (integerp emacs-wiki-index-title-threshold)
                               (> emacs-wiki-index-title-threshold 0)
                               emacs-wiki-index-title-threshold))
    (goto-char (point-max))
    (when (re-search-backward "^#title\\s-+\\(.+\\)$" nil t)
      (emacs-wiki-match-string-no-properties 1))))

(defun emacs-wiki-file-alist-with-titles (&optional verbose)
  "Return a list of pages with titles and filenames.
If VERBOSE is non-nil, print status messages."
  (when verbose
    (message "Scanning Emacs-wiki pages for titles..."))
  (let ((len (length (emacs-wiki-file-alist))))
    (mapcar
     (lambda (item)
       (when verbose
         (if (= (% len 20) 0)
             (message "%d pages to go." len))
         (setq len (1- len)))
       (list
        (or
         (emacs-wiki-get-title-fast (cdr item))
         (emacs-wiki-prettify-title (car item))); title
        (car item) ; page
        (cdr item))) ; filename
     (emacs-wiki-file-alist))))

(defun emacs-wiki-index-files-list (&optional verbose)
  "Return a list of files to index.
If VERBOSE is non-nil, print status messages."
  (sort
   (copy-alist
    (if emacs-wiki-index-title-threshold
        (emacs-wiki-file-alist-with-titles verbose)
      (emacs-wiki-file-alist)))
   (function
    (lambda (l r)
      (string-lessp (car l) (car r))))))

(defsubst emacs-wiki-index-file-title (item) (car item))
(defsubst emacs-wiki-index-file-page (item)
  (if emacs-wiki-index-title-threshold
      (car (cdr item))
    (car item)))

(defun emacs-wiki-generate-index
  (&optional as-list exclude-private verbose)
  "Generate an index of all Wiki pages.
If AS-LIST is non-nil, format results as a bulleted list.  If
EXCLUDE-PRIVATE is non-nil, no page satisfying
`emacs-wiki-private-p' is indexed.  If VERBOSE is non-nil, status
messages are printed."
  (let ((emacs-wiki-project emacs-wiki-current-project)
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Wiki Index*")
      (erase-buffer)
      (when emacs-wiki-project
          (emacs-wiki-change-project emacs-wiki-project))
      (let ((files (emacs-wiki-index-files-list verbose)))
        (while files
          (unless (and exclude-private
                       (emacs-wiki-private-p
                        (emacs-wiki-index-file-page (car files))))
            (insert
             (if as-list " - " "")
             (emacs-wiki-make-link
              (emacs-wiki-index-file-page (car files))
              (emacs-wiki-index-file-title (car files)))
             "\n"))
          (setq files (cdr files)))
      (current-buffer)))))

(defun emacs-wiki-index ()
  "Display an index of all known Wiki pages."
  (interactive)
  (let ((emacs-wiki-project emacs-wiki-current-project))
    (message "Generating Wiki index...")
    (pop-to-buffer (emacs-wiki-generate-index t))
    (goto-char (point-min))
    (unless (emacs-wiki-derived-mode-p 'emacs-wiki-mode)
      (emacs-wiki-mode))
    (message "Generating Wiki index...done")))

;; URLs

(defun emacs-wiki-resolve-url-google (url)
  "Return the correct Google search string."
  (when (string-match "^google:/?/?\\(.+\\)" url)
    (concat "http://www.google.com/search?q="
            (match-string 1 url))))

(defun emacs-wiki-browse-url-google (url)
  "If this is a Google URL, jump to it."
  (let ((google-url (emacs-wiki-resolve-url-google url)))
    (when google-url
      (browse-url google-url))))

(defun emacs-wiki-browse-url-info (url)
  "If this in an Info URL, jump to it."
  (require 'info)
  (cond
   ((string-match "^info://\\([^#]+\\)#\\(.+\\)" url)
    (Info-find-node (match-string 1 url)
                    (match-string 2 url)))
   ((string-match "^info://\\([^#]+\\)" url)
    (Info-find-node (match-string 1 url)
                    "Top"))
   ((string-match "^info://(\\([^)]+\\))\\(.+\\)" url)
    (Info-find-node (match-string 1 url) (match-string 2 url)))
   ((string-match "^info://\\(.+\\)" url)
    (Info-find-node (match-string 1 url) "Top"))))

(defun emacs-wiki-browse-url-man (url)
  "If this in a manpage URL, jump to it."
  (cond ((string-match "^man://\\(.+\\):\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url)
                               "(" (match-string 2 url) ")")))
        ((string-match "^man://\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url))))))

(defvar emacs-wiki-tag-history nil
  "List of recently-entered tags; used by `emacs-wiki-insert-tag'.
If you want a tag to start as the default, you may manually set
this variable to a list.")

(defvar emacs-wiki-custom-tags nil
  "Keep track of any new tags entered in `emacs-wiki-insert-tag'.
If there are (X)HTML tags that you use frequently with that
function, you might want to set this manually.")

(defun emacs-wiki-insert-tag ()
  "Insert a tag interactively with a blank line after it."
  (interactive)
  (let ((markup-tag (completing-read
                     (concat "Tag: "
                             (when emacs-wiki-tag-history
                               (concat "(default: "
                                       (car emacs-wiki-tag-history)
                                       ") ")))
                     (mapcar 'list
                             (nconc (mapcar 'car emacs-wiki-markup-tags)
                                    (mapcar 'car emacs-wiki-dangerous-tags)
                                    emacs-wiki-custom-tags))
                     nil nil nil
                     'emacs-wiki-tag-history
                     (car emacs-wiki-tag-history)))
        tag-entry
        (markup-opt ""))
    (when (equal markup-tag "")
      (setq markup-tag (car emacs-wiki-tag-history)))
    ;; Get full tag entry
    (setq tag-entry (or (assoc markup-tag emacs-wiki-markup-tags)
                        (assoc markup-tag emacs-wiki-dangerous-tags)))
    ;; Add to custom list if no entry exists
    (unless tag-entry
      (add-to-list 'emacs-wiki-custom-tags markup-tag))
    ;; Get option
    (when (nth 2 tag-entry)
      (setq markup-opt (read-string "Option: ")))
    (unless (equal markup-opt "")
      (setq markup-opt (concat " " markup-opt)))
    ;; Insert the tag, closing if necessary
    (when markup-tag
      (insert (concat "<" markup-tag markup-opt ">")))
    (when (nth 1 tag-entry)
      (insert (concat "\n\n</" markup-tag ">\n"))
      (forward-line -2))))

;; Load standard modules
(require 'emacs-wiki-colors)
(require 'emacs-wiki-macros)
(require 'emacs-wiki-project)
;;; emacs-wiki.el ends here
