;;; emacs-wiki-publish.el --- Routines that publish an emacs-wiki project.

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley
;; Copyright (C) 2004 Sacha Chua
;; Copyright (C) 2004, 2005 Michael Olson
;; Copyright (C) 2004 Yu Li
;;
;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-publish.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Provide publishing support for emacs-wiki.el
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

;;;_* Commentary

;; This file is the part of the Emacs Wiki project that handles
;; publishing Wiki pages as HTML.

;;;_ + Startup

;; This file is loaded automatically by emacs-wiki.el .  You should
;; not need to manually load it at this time.

;;;_ + Usage

;;;_ + Contributors

;; Zhao Lei (zhaolei AT netarchlab DOT tsinghua DOT edu DOT cn) found
;; a case where an img tag was missing a forward slash on its closing
;; tag.
;;
;; Yamagata Yoriyuki (yoriyuki AT mbg DOT ocn DOT ne DOT jp) suggested
;; a way to keep Japanese characters from being garbled in
;; `emacs-wiki-escape-html-string'.
;;
;; Yu Li (liyu2000 AT hotmail DOT com) provided a fix for
;; `emacs-wiki-escape-html-string' to make it deal with Chinese GBK
;; characters better.
;;
;; Jim Ottaway (j DOT ottaway AT lse DOT ac DOT uk) provided a small
;; patch that fixes a "Wrong type argument: integerp, nil" error in
;; `emacs-wiki-escape-html-string'.  He also fixed a problem with nop
;; tags not being colored properly.
;;
;; Li Daobing (lidaobing AT gmail DOT com) helped the
;; `emacs-wiki-escape-url' function make better decoding decisions (10
;; changed lines).  He also helped fix an XHTML validation problem
;; with alt attributes of IMG tags.
;;
;; Andrew J. Korty (ajk AT iu DOT edu) fixed a case where the current
;; project name is lost during publishing (3 changed lines).
;;
;; Trent Buck (fubarbaz AT bigpond DOT com) got rid of leading
;; newlines between pre tags.  He also suggested replacing ``'' with
;; smart quotes.
;;
;; Zhiqiang Ye (yezq AT mail DOT cbi DOT pku DOT edu DOT cn) suggested
;; appending an 'encoding="..."' fragment to the first line of
;; `emacs-wiki-publishing-header' so that when editing the resulting
;; XHTML file, Emacs would use the proper encoding.
;;
;; Pascal Quesseveur (quesseveur AT abaksystemes DOT fr) provided a
;; patch on 2005-07-18 to fix some recurse problems in
;; `emacs-wiki-publish-index' and `emacs-wiki-link-url'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Publishing (to HTML by default)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(require 'emacs-wiki)
(require 'emacs-wiki-regexps)
(require 'emacs-wiki-menu)

(defgroup emacs-wiki-publish nil
  "Options controlling the behavior of emacs-wiki publishing.
See `emacs-wiki-publish' for more information."
  :group 'emacs-wiki)

(defcustom emacs-wiki-maintainer
  (concat "mailto:webmaster@" (system-name))
  "URL where the maintainer can be reached.

You can alternatively specify a Wiki page name here.  When you
do, clicking on a non-existing link will take the user to that
page."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-index-page "WikiIndex"
  "Title of the Wiki Index page."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-downcase-title-words
  '("the" "and" "at" "on" "of" "for" "in" "an" "a")
  "Strings that should be downcased in a Wiki page title."
  :type '(repeat string)
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-project-remove-last-word nil
  "Specify whether to remove the last word of each project.
A value of non-nil will remove the last word from the project
name when publishing a menu or local interwiki link.

This removal occurs at publish time and only affects the visible
name of an interwiki link to a local project.
`emacs-wiki-menu-make-from-projects' also uses this."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-use-mode-flags nil
  "If non-nil, use file mode flags to determine page permissions.
Otherwise the regexps in `emacs-wiki-private-pages' and
`emacs-wiki-editable-pages' are used."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-relative-links t
  "If non-nil, use relative interwiki links on shared servers.
If two projects share the same host part in their respective
`emacs-wiki-project-server-prefix', then use relative anchor
targets when publishing interwiki links between them."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-private-pages nil
  "A list of regexps to exclude from public view.
This variable only applies if `emacs-wiki-use-mode-flags' is nil."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-editable-pages nil
  "A list of regexps of pages that may be edited via HTTP.
This variable only applies if `emacs-wiki-use-mode-flags' is nil."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-directory "~/WebWiki"
  "Directory where all wikis are published to."
  :type 'directory
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-prefix ""
  "This prefix will be prepended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-suffix ".html"
  "This suffix will be appended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-before-markup-hook nil
  "A hook run in the buffer where markup is done, before it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-after-markup-hook nil
  "A hook run in the buffer where markup is done, after it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publish-function 'emacs-wiki-publish-current
  "The function used to publish a Wiki page."
  :type 'function
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-content-type "text/html"
  "The content type used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-content-coding
  (if (featurep 'mule)
      'detect
    "iso-8859-1")
  "If set to the symbol 'detect, use `emacs-wiki-coding-map' to
try and determine the HTML charset from emacs's coding.

If set to a string, this string will be used to force a
particular charset"
  :type '(choice string symbol)
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-xhtml-inline-css t
  "If non-nil, generate XHTML-compliant output with respect to inline CSS.

There are some tags like '<ul>' that are valid in HTML but not in
XHTML.  If non-nil, this option causes CSS to be used in a way to
emulate the effect of these tags.  If nil, just use the tag and
don't worry about XHTML compatibility."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-xhtml-space-singles t
  "If non-nil, generate XHTML-compliant output with respect to single tags.

In XHTML, tags that have no corresponding tag, like '<br>' must
be represented as '<br />', with the extra space and forward
slash.  This causes the document to not validate properly if you
want HTML 4.0 compliant output.

If non-nil, this option favors the XHTML way of doing things.  If
nil, favor the HTML-friendly markup style."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-charset-default "iso-8859-1"
  "The default HTML meta charset to use if no translation is found in
`emacs-wiki-coding-map'"
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-coding-default 'iso-8859-1
  "The default coding to use for Emacs buffers if no special
characters are found"
  :type 'symbol
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-coding-map
  '((iso-8859-1 "iso-8859-1")
    (iso-2022-jp "iso-2022-jp")
    (utf-8 "utf-8")
    (mule-utf-8 "utf-8")
    (japanese-iso-8bit "euc-jp")
    (chinese-big5 "big5"))
  "An alist mapping emacs coding systems to appropriate HTML charsets.
Use the base name of the coding system (ie, without the -unix)"
  :type '(alist :key-type coding-system :value-type (group string))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-redirect-delay 1
  "The number of seconds to delay before doing a page redirect."
  :type 'integer
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-current-page-title nil
  "Current page title, used instead of buffer name if non-nil.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defvar emacs-wiki-current-page-date nil
  "Current page date stamp, used instead of file modtime if non-nil.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defvar emacs-wiki-current-page-related nil
  "Current page related links.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defcustom emacs-wiki-anchor-on-word nil
  "When true, anchors surround the closest word.
This allows you to select them in a browser (ie, for pasting),
but has the side-effect of marking up headers in multiple colours
if your header style is different to your link style."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-header
  "<?xml version=\"1.0\" encoding=\"<lisp>
 (emacs-wiki-transform-content-type
   (or (and (boundp 'buffer-file-coding-system)
            buffer-file-coding-system)
       emacs-wiki-coding-default))</lisp>\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>(emacs-wiki-page-title)</lisp></title>
    <meta name=\"generator\" content=\"emacs-wiki.el\" />
    <meta http-equiv=\"<lisp>emacs-wiki-meta-http-equiv</lisp>\"
          content=\"<lisp>emacs-wiki-meta-content</lisp>\" />
    <link rel=\"made\" href=\"<lisp>emacs-wiki-maintainer</lisp>\" />
    <link rel=\"home\" href=\"<lisp>(emacs-wiki-published-name
                                     emacs-wiki-default-page)</lisp>\" />
    <link rel=\"index\" href=\"<lisp>(emacs-wiki-published-name
                                      emacs-wiki-index-page)</lisp>\" />
    <lisp>emacs-wiki-style-sheet</lisp>
  </head>
  <body>

    <!-- If you want a menu, uncomment the following lines and
    put (require 'emacs-wiki-menu) in your Emacs setup somewhere.

    <lisp>(when (boundp 'emacs-wiki-menu-factory)
            (funcall emacs-wiki-menu-factory))</lisp>
    -->

    <h1 id=\"top\"><lisp>(emacs-wiki-page-title)</lisp></h1>

    <!-- Page published by Emacs Wiki begins here -->\n"
  "Text to prepend to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-footer
  "
    <!-- Page published by Emacs Wiki ends here -->
    <div class=\"navfoot\">
      <hr />
      <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
        <col width=\"33%\" /><col width=\"34%\" /><col width=\"33%\" />
        <tr>
          <td align=\"left\">
            <lisp>
              (if emacs-wiki-current-file
                  (concat
                   \"<span class=\\\"footdate\\\">Updated: \"
                   (format-time-string emacs-wiki-footer-date-format
                    (nth 5 (file-attributes emacs-wiki-current-file)))
                   (and emacs-wiki-serving-p
                        (emacs-wiki-editable-p (emacs-wiki-page-name))
                        (concat
                         \" / \"
                         (emacs-wiki-link-href
                          (concat \"editwiki?\" (emacs-wiki-page-name))
                          \"Edit\")))
                   \"</span>\"))
            </lisp>
          </td>
          <td align=\"center\">
            <span class=\"foothome\">
              <lisp>
                (concat
                 (and (emacs-wiki-page-file emacs-wiki-default-page t)
                      (not (emacs-wiki-private-p emacs-wiki-default-page))
                      (concat
                       (emacs-wiki-link-href emacs-wiki-default-page \"Home\")
                       \" / \"))
                 (emacs-wiki-link-href emacs-wiki-index-page \"Index\")
                 (and (emacs-wiki-page-file \"ChangeLog\" t)
                      (not (emacs-wiki-private-p \"ChangeLog\"))
                      (concat
                       \" / \"
                       (emacs-wiki-link-href \"ChangeLog\" \"Changes\"))))
              </lisp>
            </span>
          </td>
          <td align=\"right\">
            <lisp>
              (if emacs-wiki-serving-p
                  (concat
                   \"<span class=\\\"footfeed\\\">\"
                   (emacs-wiki-link-href \"searchwiki?get\" \"Search\")
                   (and emacs-wiki-current-file
                        (concat
                         \" / \"
                         (emacs-wiki-link-href
                          (concat \"searchwiki?q=\" (emacs-wiki-page-name))
                          \"Referrers\")))
                   \"</span>\"))
            </lisp>
          </td>
        </tr>
      </table>
    </div>
  </body>
</html>\n"
  "Text to append to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-footer-date-format "%Y-%m-%d"
  "Format of current date for `emacs-wiki-publishing-footer'.
This string must be a valid argument to `format-time-string'."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-style-sheet
  "<style type=\"text/css\">
a.nonexistent {
  background-color: #F5F5F5;          /* white smoke */
  color: red;
  font-weight: bold;
}

a.nonexistent:visited {
  background-color: #F5F5F5;          /* white smoke */
  color: red;
}

body {
  background-color: white;
  color: black;
  margin-left: 5%;
  margin-right: 5%;
  margin-top: 3%;
}

em { font-style: italic; }
strong { font-weight: bold; }

ul { list-style-type: disc }

dl.contents { margin-top: 0; }
dt.contents { margin-bottom: 0; }

p.verse {
  white-space: pre;
  margin-left: 5%;
}

pre {
  white-space: pre;
  font-family: monospace;
  margin-left: 5%;
}

/* Menu properties */

.menu {
  background-color: #F5F5F5;          /* white smoke */
  color: white;
  border-top: 5px solid #D3D3D3;      /* light gray */
  border-bottom: 5px solid #D3D3D3;   /* light gray */
  content: \"Menu\";
  display: inline-table;
  float: right;
  padding: .5em;
}

.menuitem {
  margin: .2em;
  text-align: center;
}

.menuitem a:link, .menuitem a:visited {
  color: #006400;                     /* dark green */
  text-decoration: none;
}

.menuitem a:hover {
  background-color: #D3D3D3;          /* light gray */
}

/* Uncomment this if you want characters to appear before a menu
   entry when hovering the mouse over it.

.menuitem a:hover:before { content:\">> \"}
*/

.menu:before {
  border: 1pt solid;
  color: #BEBEBE;                     /* gray */
  display: block;
  content: \"Menu\";
  text-align: center;
}

#tooltipbox {
  float: right;
  visibility: hidden;
}

#tooltip {
  color: #D3D3D3;                     /* light gray */
}
</style>
"
  "The style sheet used for each wiki page.
This can either be an inline stylesheet, using <style></style> tags,
or an external stylesheet reference using a <link> tag.

Here is an example of using a <link> tag:

  <link rel=\"stylesheet\" type=\"text/css\" href=\"emacs-wiki.css\" />"
  :type 'string
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-publishing-p nil
  "Set to t while Wiki pages are being published.
This can be used by <lisp> tags to know when HTML is being generated.")

(defcustom emacs-wiki-block-groups-regexp
  "\\(h[1-9r]\\|[oud]l\\|table\\|form\\|center\\|blockquote\\|pre\\)[^>]*"
  "This regexp identifies HTML tag which defines their own blocks.
That is, they do not need to be surrounded by <p>."
  :type 'regexp
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-table-attributes
  "class=\"ewiki-table\" border=\"2\" cellpadding=\"5\""
  "The attribute to be used with HTML <table> tags.
Note that since emacs-wiki support direct insertion of HTML tags, you
can easily create any kind of table you want, as long as every line
begins at column 0 (to prevent it from being blockquote'd).  To make
really ANYTHING you want, use this idiom:

  <verbatim>
  <table>
    [... contents of my table, in raw HTML ...]
  </verbatim></table>

It may look strange to have the tags out of sequence, but remember
that verbatim is processed long before table is even seen."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-report-threshhold 100000
  "If a Wiki file is this size or larger, report publishing progress."
  :type 'integer
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-markup
  (list
   ;; change the displayed title or the stylesheet for a given page
   ["^#\\(menu\\|title\\|date\\|style\\|related\\)\\s-+\\(.+\\)\n+" 0
    emacs-wiki-markup-initial-directives]

   ;; process markup tags, in order to get the <verbatim>s out of the
   ;; way
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags]

   `[,(concat "&\\([-_#" emacs-wiki-regexp-alnum "]+\\);")
     0 emacs-wiki-markup-entity]

   [emacs-wiki-url-or-name-regexp 0 emacs-wiki-mark-noemphasis]

   ;; emphasized or literal text
   `[,(concat "\\(^\\|[-["
            emacs-wiki-regexp-space
            "<('`\"]\\)\\(=[^="
            emacs-wiki-regexp-space
            "]\\|_[^_"
            emacs-wiki-regexp-space
            "]\\|\\*\\{1,3\\}[^*"
            emacs-wiki-regexp-space
            "]\\)")
    2 emacs-wiki-markup-word]

   ;; headings, outline-mode style
   ["^\\(\\*\\{1,5\\}\\)\\s-+" 0 emacs-wiki-markup-heading]

   ;; define anchor points
   ["^#\\(\\S-+\\)\\s-*" 0 emacs-wiki-markup-anchor]

   ;; horizontal rule, or section separator
   `["^----+" 0 ,(if emacs-wiki-xhtml-space-singles
                     "<hr />"
                   "<hr>")]

   ;; expand macros
   [emacs-wiki-macro-regexp 0 emacs-wiki-macro-expand]

   ;; don't require newlines between numbered and unnumbered lists.
   ;; This must come before paragraphs are calculated, so that any
   ;; extra newlines added will be condensed.
   ["^\\s-*\\(-\\s-\\|[0-9]+\\.\\s-\\)" 1 "\n\\1"]

   ;; the beginning of the buffer begins the first paragraph
   ["\\`\n*" 0 "<p>\n"]
   ;; plain paragraph separator
   `[,(concat "\n\\(["
              emacs-wiki-regexp-blank
              "]*\n\\)+")
     0 "\n\n</p>\n\n<p>\n"]

   ;; simple table markup. use | to separate cells, || to separate
   ;; header elements, and ||| for footer elements
   ["^\\s-*\\(\\([^|\n]+\\(|+\\)\\s-*\\)+\\)\\([^|\n]+\\)?$"
    1 emacs-wiki-markup-table]

   ;; unnumbered List items begin with a -.  numbered list items begin
   ;; with number and a period.  definition lists have a leading term
   ;; separated from the body with ::.  centered paragraphs begin with
   ;; at least six columns of whitespace; any other whitespace at the
   ;; beginning indicates a blockquote.  The reason all of these rules
   ;; are handled here, is so that blockquote detection doesn't
   ;; interfere with indented list members.
   `[,(concat "^\\(\\s-*\\(-\\|[0-9]+\\.\\|\\(.+\\)["
              emacs-wiki-regexp-blank
              "]+::\n?\\)\\)?\\(["
              emacs-wiki-regexp-blank
              "]+\\)")
     4 emacs-wiki-markup-list-or-paragraph]

   ;; "verse" text is indicated the same way as a quoted e-mail
   ;; response: "> text", where text may contain initial whitespace
   ;; (see below).
   ["<p>\\s-+> \\(\\([^\n]\n?\\)+\\)\\(\\s-*</p>\\)?" 0
    emacs-wiki-markup-verse]

   ;; join together the parts of a list
   ["</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" 0 ""]

   ;; join together the parts of a table
   (vector
    (concat "</tbody>\\s-*"
            "</table>\\s-*" "\\(</p>\\s-*<p>\\s-*\\)?" "<table[^>]*>\\s-*"
            "<tbody>\\s-*") 0 "")
   ["</table>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<table[^>]*>\\s-*" 0 ""]

   ;; fixup paragraph delimiters
   (vector
    (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp ">\\)")
    0 "\\1")
   (vector (concat "\\(</?" emacs-wiki-block-groups-regexp
                   ">\\)\\s-*\\(</p>\\)") 3 "\\1")

   ;; terminate open paragraph at the end of the buffer
   ["<p>\\s-*\\'" 0 ""]
   ;; make sure to close any open text (paragraphs)
   `[,(concat "\\([^>"
              emacs-wiki-regexp-space
              "]\\)\\s-*\\'")
     0 "\\1\n</p>"]

   ;; replace WikiLinks in the buffer (links to other pages)
   ;; <nop> before a WikiName guards it from being replaced
   ;; '''' can be used to add suffixes, such as WikiName''''s
   [emacs-wiki-url-or-name-regexp 0 emacs-wiki-markup-link]
   ["''''" 0 ""]

   ;; footnotes section is separated by a horizontal rule in HTML
   `["^\\(\\* \\)?Footnotes:?\\s-*" 0
     ,(concat "</p>"
              (if emacs-wiki-xhtml-space-singles
                  "<hr />"
                "<hr>")
              "\n<p>\n")]

   ;; footnote definition/reference (def if at beginning of line)
   ["\\[\\([1-9][0-9]*\\)\\]" 0 emacs-wiki-markup-footnote]

   ;; bare email addresses
   (vector
    (concat
     "\\([^-+:.@/" emacs-wiki-regexp-alnum "]\\)"
     "\\([-+._" emacs-wiki-regexp-alnum "]+@\\([-_"
     emacs-wiki-regexp-alnum "]+\\.\\)+[" emacs-wiki-regexp-alnum
     "]+\\)\\([^-\"" emacs-wiki-regexp-alnum "]\\)")
    0
    "\\1<a href=\"mailto:\\2\">\\2</a>\\4")

   ;; use smart quotes
   ["\\(``\\)" 0 "&ldquo;"]
   ["\\(''\\)" 0 "&rdquo;"]

   ;; insert the default publishing header
   (function
    (lambda ()
      (if (and (not (string= emacs-wiki-publishing-header ""))
               (file-readable-p emacs-wiki-publishing-header))
          (insert-file-contents-literally emacs-wiki-publishing-header)
        (insert emacs-wiki-publishing-header))))

   ;; insert the default publishing footer
   (function
    (lambda ()
      (goto-char (point-max))
      (if (and (not (string= emacs-wiki-publishing-footer ""))
               (file-readable-p emacs-wiki-publishing-footer))
          (insert-file-contents-literally emacs-wiki-publishing-footer)
        (insert emacs-wiki-publishing-footer))))

   ;; process any remaining markup tags
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags])
  "List of markup rules to apply when publishing a Wiki page.
Each member of the list is either a function, or a vector of the form:

  [REGEXP/SYMBOL TEXT-BEGIN-GROUP REPLACEMENT-TEXT/FUNCTION/SYMBOL].

REGEXP is a regular expression, or symbol whose value is a regular
expression, which is searched for using `re-search-forward'.
TEXT-BEGIN-GROUP is the matching group within that regexp which
denotes the beginning of the actual text to be marked up.
REPLACEMENT-TEXT is a string that will be passed to `replace-match'.
If it is not a string, but a function, it will be called to determine
what the replacement text should be (it must return a string).  If it
is a symbol, the value of that symbol should be a string.

The replacements are done in order, one rule at a time.  Writing the
regular expressions can be a tricky business.  Note that case is never
ignored.  `case-fold-search' is always be bound to nil while
processing the markup rules.

Here is a description of the default markup rules:

Headings

 * First level
 ** Second level
 *** Third level

 Note that the first level is actually indicated using H2, so that
 it doesn't appear at the same level as the page heading (which
 conceptually titles the section of that Wiki page).

Horizontal rules

----

Emphasis

 *emphasis*
 **strong emphasis**
 ***very strong emphasis***
 _underlined text_
 =verbatim=

 <verbatim>This tag should be used for larger blocks of
 text</verbatim>.

Footnotes

  A reference[1], which is just a number in square brackets,
  constitutes a footnote reference.

  Footnotes:

  [1]  Footnotes are defined by the same number in brackets
       occurring at the beginning of a line.  Use footnote-mode's C-c
       ! a command, to very easily insert footnotes while typing.  Use
       C-x C-x to return to the point of insertion.

Paragraphs

  One or more blank lines separates paragraphs.

Centered paragraphs and quotations

  A line that begins with six or more columns of whitespace (made up
  of tabs or spaces) indicates a centered paragraph.  I assume this
  because it's expected you will use M-s to center the line, which
  usually adds a lot of whitespace before it.

  If a line begins with some whitespace, but less than six columns, it
  indicates a quoted paragraph.

Poetic verse

  Poetry requires that whitespace be preserved, without resorting to
  the monospace typical of <pre>.  For this, the following special
  markup exists, which is reminiscent of e-mail quotations:

    > A line of Emacs verse;
    > forgive its being so terse.

  You can also use the <verse> tag, if you prefer:

    <verse>
    A line of Emacs verse;
    forgive its being so terse.
    </verse>

Literal paragraphs

  Use the HTML tags <pre></pre> to insert a paragraph and preserve
  whitespace.  If you're inserting a block of code, you will almost
  always want to use <verbatim></verbatim> *within* the <pre> tags.
  The shorcut for doing this is to use the <example> tag:

    <example>
    Some literal text or code here.
    </example>

Lists

  - bullet list

  1. Enumerated list

  Term :: A definition list

  Blank lines between list elements are optional, but required between
  members of a definition list.

Tables

  There are two forms of table markup supported.  If Takaaki Ota's
  table.el package is available, then simply create your tables using
  his package, and they will be rendered into the appropriate HTML.
  You need to (require 'emacs-wiki-table) for this functionality.

  If table.el is not available, then only very simple table markup is
  supported.  The attributes of the table are kept in
  `emacs-wiki-table-attributes'.  The syntax is:

    Double bars || Separate header fields
    Single bars | Separate body fields
    Here are more | body fields
    Triple bars ||| Separate footer fields

  Other paragraph markup applies to both styles, meaning that if six
  or more columns of whitespace precedes the first line of the table,
  it will be centered, and if any whitespace at all precedes first
  line, it will occur in a blockquote.

Anchors and tagged links

  #example If you begin a line with \"#anchor\" -- where anchor
  can be any word that doesn't contain whitespace -- it defines an
  anchor at that point into the document.  This anchor text is not
  displayed.

  You can reference an anchored point in another page (or even in the
  current page) using WikiName#anchor.  The #anchor will never be
  displayed in HTML, whether at the point of definition or reference,
  but it will cause browsers to jump to that point in the document.

Redirecting to another page or URL

  Sometimes you may wish to redirect someone to another page.  To do
  this, put:

    <redirect url=\"http://somewhereelse.com\" />

  at the top of the page.  If the <redirect> tag specifies content,
  this will be used as the redirection message, rather than the
  default.

  The numbers of seconds to delay is defined by
  `emacs-wiki-redirect-delay', which defaults to 2 seconds.  The page
  shown will also contain a link to click on, for browsing which do
  not support automatic refreshing.

URLs

  A regular URL is given as a link.  If it's an image URL, it will
  be inlined using an IMG tag.

Embedded lisp

  <lisp>(concat \"This form gets\" \"inserted\")</lisp>

Special handling of WikiNames

  If you need to add a plural at the end of a WikiName, separate it
  with four single quotes (WikiName''''s) or make it an explicit
  link ([[WikiName]]s).

  To prevent a link name (of any type) from being treated as such,
  surround it with =equals= (to display it in monotype), or prefix it
  with the tag <nop> to escape it from WikiName markup.

Special Wiki links

  Besides the normal WikiName type links, emacs-wiki also supports
  extended links:

    [[link text][optional link description]]

  An extended link is always a link, no matter how it looks.  This
  means you can use any file in your `emacs-wiki-directories' as a
  Wiki file.  If you provide an optional description, that's what will
  be shown instead of the link text.  This is very useful for
  providing textual description of URLs.

  See the documentation to emacs-wiki-image-regexp for how to inline
  files and images.

InterWiki names

  There are times when you will want to constantly reference pages on
  another website.  Rather than repeating the URL ad nauseum, you can
  define an InterWiki name.  This is a set of WikiNames to URL
  correlations, that support textual substitution using #anchor names
  (which are appended to the URL).  For example, MeatballWiki is
  defined in the variable `emacs-wiki-interwiki-names'.  It means you
  can reference the page \"MeatBall\" on MeatballWiki using this
  syntax:

    MeatballWiki#MeatBall

  In the resulting HTML, the link is simply shown as
  \"MeatballWiki:MeatBall\"."
  :type '(repeat
          (choice
           (vector :tag "Markup rule"
                   (choice regexp symbol)
                   integer
                   (choice string function symbol))
           function))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-changelog-markup
  (list
   ["&" 0 "&amp;"]
   ["<" 0 "&lt;"]
   [">" 0 "&gt;"]

   ["^\\(\\S-+\\)\\s-+\\(.+\\)" 0 emacs-wiki-markup-changelog-section]

   ;; emphasized or literal text
   `[,(concat "\\(^\\|[-["
              emacs-wiki-regexp-space
              "<('`\"]\\)\\(=[^="
              emacs-wiki-regexp-space
              "]\\|_[^_"
              emacs-wiki-regexp-space
              "]\\|\\*+[^*"
              emacs-wiki-regexp-space
              "]\\)")
     2 emacs-wiki-markup-word]

   ;; headings, outline-mode style
   ["^\\*\\s-+\\(.+\\)$" 0 "<h2>\\1</h2>"]

   ;; escape the 'file' entries, in case they are extended wiki links
   `[,(concat "^["
              emacs-wiki-regexp-blank
              "]+\\* \\([^:(]+\\)\\(["
              emacs-wiki-regexp-blank
              "]+(\\|:\\)")
     0 emacs-wiki-changelog-escape-files]

   ;; don't require newlines between unnumbered lists.
   ["^\\s-*\\(\\*\\)" 1 "\n\\1"]

   ;; the beginning of the buffer begins the first paragraph
   ["\\`\n*" 0 "<p>\n"]
   ;; plain paragraph separator
   `[,(concat "\n\\(["
              emacs-wiki-regexp-blank
              "]*\n\\)+")
     0 "\n\n</p>\n\n<p>\n"]

   ;; unnumbered List items begin with a -.  numbered list items
   ;; begin with number and a period.  definition lists have a
   ;; leading term separated from the body with ::.  centered
   ;; paragraphs begin with at least six columns of whitespace; any
   ;; other whitespace at the beginning indicates a blockquote.  The
   ;; reason all of these rules are handled here, is so that
   ;; blockquote detection doesn't interfere with indented list
   ;; members.
   `[,(concat "^\\(\\s-*\\(\\*\\)\\)?\\(["
              emacs-wiki-regexp-blank
              "]+\\)\\(\\([^\n]\n?\\)+\\)")
     3 "<ul>\n<li>\n\\4</li></ul>\n"]

   ;; join together the parts of a list
   ["</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" 0 ""]

   ;; fixup paragraph delimiters
   (vector
    (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp
            ">\\)") 0 "\\1")
   (vector (concat "\\(</?" emacs-wiki-block-groups-regexp
                   ">\\)\\s-*\\(</p>\\)") 3 "\\1")

   ;; terminate open paragraph at the end of the buffer
   ["<p>\\s-*\\'" 0 ""]
   ;; make sure to close any open text (paragraphs)
   `[,(concat "\\([^>"
              emacs-wiki-regexp-space
              "]\\)\\s-*\\'")
     0 "\\1\n</p>"]

   ;; bare email addresses
   (vector
    (concat
     "\\([^-+:.@/" emacs-wiki-regexp-alnum "]\\)"
     "\\([-+._" emacs-wiki-regexp-alnum "]+@\\([-_"
     emacs-wiki-regexp-alnum "]+\\.\\)+[" emacs-wiki-regexp-alnum
     "]+\\)\\([^-\"" emacs-wiki-regexp-alnum "]\\)")
    0
    "\\1<a href=\"mailto:\\2\">\\2</a>\\4")

   ;; replace WikiLinks in the buffer (links to other pages)
   [emacs-wiki-url-or-name-regexp 0 emacs-wiki-markup-link]
   ["''''" 0 ""]

   ;; insert the default publishing header
   (function
    (lambda ()
      (if (and (not (string= emacs-wiki-publishing-header ""))
               (file-readable-p emacs-wiki-publishing-header))
          (insert-file-contents-literally emacs-wiki-publishing-header)
        (insert emacs-wiki-publishing-header))))

   ;; insert the default publishing footer
   (function
    (lambda ()
      (goto-char (point-max))
      (if (and (not (string= emacs-wiki-publishing-footer ""))
               (file-readable-p emacs-wiki-publishing-footer))
          (insert-file-contents-literally emacs-wiki-publishing-footer)
        (insert emacs-wiki-publishing-footer))))

   ;; process any custom markup tags
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags])
  "List of markup rules for publishing ChangeLog files.
These are used when the wiki page's name is ChangeLog."
  :type '(repeat
          (choice
           (vector :tag "Markup rule"
                   (choice regexp symbol)
                   integer
                   (choice string function symbol))
           function))
  :group 'emacs-wiki-publish)

(defun emacs-wiki-transform-content-type (content-type)
  "Using `emacs-wiki-coding-map', try and resolve an emacs coding
system to an associated HTML coding system.

If no match is found, `emacs-wiki-charset-default' is used instead."
  (let ((match (when (fboundp 'coding-system-base)
                 (assoc (coding-system-base content-type)
                        emacs-wiki-coding-map))))
    (if match
        (cadr match)
      emacs-wiki-charset-default)))

(defun emacs-wiki-private-p (name)
  "Return non-nil if NAME is a private page, and shouldn't be published."
  (if name
      (if emacs-wiki-use-mode-flags
          (let* ((page-file (emacs-wiki-page-file name))
                 (filename (and page-file (file-truename page-file))))
            (if filename
                (or (eq ?-
                        (aref (nth 8 (file-attributes
                                      (file-name-directory filename)))
                              7))
                    (eq ?-
                        (aref (nth 8 (file-attributes filename))
                              7)))))
        (let ((private-pages emacs-wiki-private-pages) private)
          (while private-pages
            (if (string-match (car private-pages) name)
                (setq private t private-pages nil)
              (setq private-pages (cdr private-pages))))
          private))))

(defcustom emacs-wiki-publishing-transforms nil
  "A list of cons cells mapping regexps to replacements, which is
applied when generating the published name from the wiki file
name.  The replacements run in order so you can chain them
together.

An example is how I publish the emacs-wiki documentation. The
emacs-wiki homepage is in a file called EmacsWiki.  With the
following settings I can publish directly to my webserver via
tramp (the first rule catches 'WikiMarkup' for instance):

\(setq emacs-wiki-publishing-directory \"/webserver:/var/www/\")
\(setq emacs-wiki-publishing-transforms
      '((\".*Wiki.*\" . \"emacs/wiki/\\\&\")
         (\"EmacsWiki\\|WelcomePage\" . \"index\")))

Then when trying to publish a page EmacsWiki:

\(emacs-wiki-published-file \"EmacsWiki\")

You get:

\"/webserver:/var/www/emacs/wiki/index.html\""
  :type '(repeat
          (cons
           (regexp :tag "String to match")
           (string :tag "Replacement string")))
  :group 'emacs-wiki-publish)

(defsubst emacs-wiki-transform-name (name)
  "Transform NAME as per `emacs-wiki-publishing-transforms',
returning NAME."
  (save-match-data
    (dolist (elt emacs-wiki-publishing-transforms)
      (let ((reg (car elt))
            (rep (cdr elt)))
        (setq name (emacs-wiki-replace-regexp-in-string
                    reg rep name t)))))
  name)

(defsubst emacs-wiki-published-name (name &optional current)
  "Return the externally visible NAME for a wiki page.
The `emacs-wiki-publishing-transforms' are used.

If CURRENT is provided, convert NAME to be relative to it."
  (when current
    ;; Make NAME relative to CURRENT path
    (setq name (file-relative-name
                (emacs-wiki-transform-name
                 (or (emacs-wiki-page-file name) name))
                (file-name-directory
                 (emacs-wiki-transform-name current)))))
  ;; Check to see if NAME is a path and transform it nicely, getting
  ;; rid of the problem with names like "~user"
  (when (file-name-absolute-p name)
    (setq name (expand-file-name name)))
  ;; Perform other transforms on NAME
  (emacs-wiki-transform-name
   (concat (if emacs-wiki-serving-p
               (unless (string-match "\\?" name) "wiki?")
             emacs-wiki-publishing-file-prefix)
           name
           (if emacs-wiki-serving-p
               (if emacs-wiki-current-project
                   (concat "&project=" emacs-wiki-current-project))
             (unless (string-match emacs-wiki-image-regexp name)
               emacs-wiki-publishing-file-suffix)))))

(defsubst emacs-wiki-published-file (&optional file)
  "Return the filename of the published file.
Since this is based on the published-name, it will be filtered
through `emacs-wiki-publishing-transforms'"
  (expand-file-name
   (emacs-wiki-published-name (emacs-wiki-page-name file))
   (let ((d emacs-wiki-directories)
         (alist (emacs-wiki-file-alist))
         found)
     (while d
       (let* ((dir (file-name-as-directory (if (consp (car d))
                                               (caar d) (car d))))
              (dir-re (concat "^" (regexp-quote
                                   (expand-file-name dir))))
              (buffer-file (or (cdr (assoc file alist))
                               emacs-wiki-current-file
                               buffer-file-name))
              (buffer-directory
               (file-name-directory
                (or buffer-file emacs-wiki-publishing-directory))))
         (if (string-match dir-re buffer-directory)
             (setq found (substring buffer-directory (match-end 0))
                   d nil)
           (setq d (cdr d)))))
     (if found
         (concat (file-name-as-directory
                  emacs-wiki-publishing-directory)
                 found)
       emacs-wiki-publishing-directory))))

(defun emacs-wiki-editable-p (name)
  "Return non-nil if NAME is a page that may be publically edited."
  nil) ;; unless you load emacs-wiki-httpd.el, no pages can be edited

(defun emacs-wiki-visit-published-file (&optional arg)
  "Visit the current wiki page's published result."
  (interactive "P")
  (if arg
      (find-file-other-window (emacs-wiki-published-file))
    (run-hook-with-args-until-success
     'emacs-wiki-browse-url-functions
     (concat "file:" (emacs-wiki-published-file)))))

(defun emacs-wiki-dired-publish ()
  "Publish all marked files in a dired buffer."
  (interactive)
  (emacs-wiki-publish-files (dired-get-marked-files) t))

(defun emacs-wiki-prettify-title (title)
  "Prettify the given TITLE."
  (save-match-data
    (let ((case-fold-search nil))
      (while (string-match (concat "\\(["
                                   emacs-wiki-regexp-upper
                                   emacs-wiki-regexp-lower "]\\)\\(["
                                   emacs-wiki-regexp-upper "0-9]\\)")
                           title)
        (setq title (replace-match "\\1 \\2" t nil title)))
      (let* ((words (split-string title))
             (w (cdr words)))
        (while w
          (if (member (downcase (car w))
                      emacs-wiki-downcase-title-words)
              (setcar w (downcase (car w))))
          (setq w (cdr w)))
        (mapconcat 'identity words " ")))))

(defun emacs-wiki-publish-index ()
  "Publish an index of the Wiki pages.
This function can be added to `emacs-wiki-after-wiki-publish-hook'."
  (interactive)
  ;; cd to the directory containing the Wiki home page before creating
  ;; the index
  (let (default-directory)
    (when (emacs-wiki-page-file emacs-wiki-default-page)
      (cd (file-name-directory
           (emacs-wiki-page-file emacs-wiki-default-page))))
    (with-current-buffer (emacs-wiki-generate-index t t t)
      (let ((emacs-wiki-current-file
             (emacs-wiki-published-file emacs-wiki-index-page)))
        (message "Marking up index...")
        (emacs-wiki-replace-markup emacs-wiki-index-page)
        (let ((backup-inhibited t))
          (write-file (emacs-wiki-published-file emacs-wiki-index-page)))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))))))

(defun emacs-wiki-publish (&optional arg)
  "Publish all wikis that need publishing.
If the published wiki already exists, it is only overwritten if
the wiki is newer than the published copy.

When given the optional argument ARG, all wikis are rewritten, no
matter how recent they are.  The index file is rewritten no
matter what."
  (interactive "P")
  ;; prompt to save any emacs-wiki buffers
  (save-some-buffers nil (lambda ()
                           (derived-mode-p 'emacs-wiki-mode)))
  (let ((emacs-wiki-project emacs-wiki-current-project))
    (emacs-wiki-refresh-file-alist)
    ;; don't run ispell for every buffer we enter
    (remove-hook 'text-mode-hook 'flyspell-mode t)
    (let* ((names (emacs-wiki-file-alist))
           (files (list t))
           (lfiles files)
           ;; disable hooks
           (emacs-wiki-mode-hook nil))
      (while names
        (setcdr lfiles (cons (cdar names) nil))
        (setq lfiles (cdr lfiles)
              names (cdr names)))
      (if (emacs-wiki-publish-files (cdr files) arg)
          (progn
            (run-hooks 'emacs-wiki-after-wiki-publish-hook)
            (message "All Wiki pages%s have been published."
                     (if emacs-wiki-current-project
                         (concat " for project "
                                 emacs-wiki-current-project)
                       "")))
        (message "No Wiki pages%s need publishing at this time."
                 (if emacs-wiki-current-project
                     (concat " in project "
                             emacs-wiki-current-project)
                   ""))))))

(defun emacs-wiki-publish-this-page ()
  "Force publication of the current page."
  (interactive)
  (emacs-wiki-publish-files (list buffer-file-name) t))

(defvar emacs-wiki-after-file-publish-hook nil
  "Hook run after every file is published.")
(defvar emacs-wiki-after-wiki-publish-hook '(emacs-wiki-publish-index)
  "Hook run after the files have been published through
`emacs-wiki-publish'.")

(defun emacs-wiki-write-buffer (output-path)
  (let ((backup-inhibited t)
        (buffer-file-coding-system
         (when (boundp 'buffer-file-coding-system)
           buffer-file-coding-system))
        (find-file-literally t))
    (when (eq buffer-file-coding-system 'undecided-unix)
      ;; make it agree with the default charset
      (setq buffer-file-coding-system
            emacs-wiki-coding-default))
    (write-file output-path)))

(defun emacs-wiki-publish-current (file output-path)
  (emacs-wiki-with-temp-buffer
    (let ((emacs-wiki-publishing-p t)
          (emacs-wiki-current-file file))
      (insert-file-contents file)
      (cd (file-name-directory file))
      (emacs-wiki-maybe)
      ;; We have to set the current file again since it gets erased in
      ;; `emacs-wiki-mode-maybe'.
      (setq emacs-wiki-current-file file)
      (setq buffer-file-name nil)
      (unless (string-match emacs-wiki-image-regexp
                            (emacs-wiki-page-name))
        (emacs-wiki-replace-markup))
      (emacs-wiki-write-buffer output-path))))

(defun emacs-wiki-publish-files (files force)
  "Publish all files in list FILES.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (let ((emacs-wiki-publishing-p t)
        published-some file page published)
    (while files
      (setq file (car files)
            files (cdr files)
            page (emacs-wiki-page-name file)
            published (emacs-wiki-published-file page))
      ;; ensure the publishing location is available
      (let ((publishing-directory (file-name-directory published)))
        (unless (file-exists-p publishing-directory)
          (message "Creating publishing subdirectory %s"
                   publishing-directory)
          (make-directory publishing-directory 'parents)))
      (when (and (not (emacs-wiki-private-p page))
               (or force (file-newer-than-file-p file published)))
        (let ((project emacs-wiki-current-project))
          (with-emacs-wiki-project project
            (funcall emacs-wiki-publish-function file published)))
        (run-hook-with-args 'emacs-wiki-after-file-publish-hook file)
        (setq published-some t)))
    published-some))

(defvar emacs-wiki-serving-p nil
  "Non-nil when emacs-wiki is serving a wiki page directly.")

(defun emacs-wiki-escape-html-specials (&optional end)
  (save-match-data
    (while (and (or (not end) (< (point) end))
                (re-search-forward "[<>&\"]" end t))
      (cond
       ((eq (char-before) ?\")
        (delete-char -1)
        (insert "&quot;"))
       ((eq (char-before) ?\<)
        (delete-char -1)
        (insert "&lt;"))
       ((eq (char-before) ?\>)
        (delete-char -1)
        (insert "&gt;"))
       ((eq (char-before) ?\&)
        (delete-char -1)
        (insert "&amp;"))))))

(defcustom emacs-wiki-publish-url-coding-system 'iso-8859-1
  "*Default coding system used to encode url strings"
  :group 'emacs-wiki-publish
  :type '(coding-system :size 0))

;; Copied from w3m-url-encode-string (w3m.el)
(defun emacs-wiki-escape-url (str &optional coding)
  "Hexify dangerous characters in STR.
If CODING is used, use that coding system."
  (save-match-data
    (apply (function concat)
           (mapcar
            (lambda (ch)
              (cond
               ((eq ch ?\n)             ; newline
                "%0D%0A")
               ;; xxx?
               ((string-match (concat "[-_:/." emacs-wiki-regexp-alnum
                                      "]") (char-to-string ch))
                (char-to-string ch))    ; printable
               ((char-equal ch ?\x20)   ; space
                "%20")
               ((char-equal ch ?\&)
                "&amp;")
               ((char-equal ch ?\<)
                "&lt;")
               ((char-equal ch ?\>)
                "&gt;")
               (t
                (format "%%%02x" ch)))) ; escape
            ;; Coerce a string to a list of chars.
            (append (if (fboundp 'encode-coding-string)
                        (encode-coding-string
                         str
                         (or coding
                             emacs-wiki-publish-url-coding-system
                             'iso-8859-1))
                      ;; Do not transform if function not defined
                      str)
                    nil)))))

;; we currently only do this on links. this means a stray '&' in an
;; emacs-wiki document risks being misinterpreted when being
;; published, but this is the price we pay to be able to inline HTML
;; content without special tags.
(defun emacs-wiki-escape-html-string (str)
  "Convert to character entities any non alphanumeric characters
outside of a few punctuation symbols, that risk being
misinterpreted if not escaped"
  (when str
    (let (pos code len char)
      (save-match-data
        (while (and (setq pos (string-match
                               (concat "[^-"
                                       emacs-wiki-regexp-alnum
                                       "/:._=@\\?~#]")
                               str pos))
                    (setq char (aref str pos)))
          ;; Work around XEmacs differentiation of char and int
          (setq code (int-to-string
                      (cond ((fboundp 'char-to-ucs)
                             (char-to-ucs char))
                            ((fboundp 'char-to-int)
                             (char-to-int char))
                            (t char))))
          (if (or (not (boundp 'char-charset))
                  (string= (char-charset char) "ascii"))
              (setq len (length code)
                    str (concat (substring str 0 pos)
                                "&#" code ";"
                                (when (< pos (length str))
                                  (substring str (1+ pos) nil)))
                    pos (+ 3 len pos))
            (cond ((string= (string (char-syntax char)) "w")
                   (setq pos (+ 1 pos)))
                  ((string= (string (char-syntax char)) "_")
                   (setq pos (+ 1 pos)))
                  ((string= (string (char-syntax char)) "(")
                   (setq pos (+ 1 pos)))
                  ((string= (string (char-syntax char)) ")")
                   (setq pos (+ 1 pos)))
                  (t
                   (setq len (length code)
                         str (concat (substring str 0 pos)
                                     "&#" code ";"
                                     (when (< pos (length str))
                                       (substring str (1+ pos) nil)))
                         pos (+ 3 len pos))))))
        str))))

(eval-when-compile
  (defvar emacs-wiki-meta-content))

;; Markup-related functions

(defun emacs-wiki-replace-markup (&optional title)
  "Replace markup according to `emacs-wiki-publishing-markup'."
  (let* ((emacs-wiki-meta-http-equiv emacs-wiki-meta-http-equiv)
         (emacs-wiki-current-page-title title)
         emacs-wiki-current-page-date
         (emacs-wiki-publishing-p t)
         (case-fold-search nil)
         (inhibit-read-only t)
         (rules (if (string= (emacs-wiki-page-name) "ChangeLog")
                    emacs-wiki-changelog-markup
                  emacs-wiki-publishing-markup))
         (limit (* (length rules) (point-max)))
         (verbose (and emacs-wiki-report-threshhold
                       (> (point-max) emacs-wiki-report-threshhold)))
         (base 0)
         (emacs-wiki-meta-content
          (concat emacs-wiki-meta-content-type "; charset="
                  (if (stringp emacs-wiki-meta-content-coding)
                      emacs-wiki-meta-content-coding
                    (emacs-wiki-transform-content-type
                     (or (and (boundp 'buffer-file-coding-system)
                              buffer-file-coding-system)
                         emacs-wiki-coding-default))))))
    (run-hooks 'emacs-wiki-before-markup-hook)
    (while rules
      (goto-char (point-min))
      (if (functionp (car rules))
          (funcall (car rules))
        (let ((regexp (aref (car rules) 0))
              (group (aref (car rules) 1))
              (replacement (aref (car rules) 2))
              last-pos pos)
          (if (symbolp regexp)
              (setq regexp (symbol-value regexp)))
          (if verbose
              (message "Publishing %s...%d%%"
                       (emacs-wiki-page-name)
                       (* (/ (float (+ (point) base)) limit) 100)))
          (while (and regexp
                      (setq pos (re-search-forward regexp nil t)))
            (if verbose
                (message "Publishing %s...%d%%"
                         (emacs-wiki-page-name)
                         (* (/ (float (+ (point) base)) limit) 100)))
            (unless (get-text-property (match-beginning group)
                                       'read-only)
              (let ((text (cond
                           ((functionp replacement)
                            (funcall replacement))
                           ((symbolp replacement)
                            (symbol-value replacement))
                           (t replacement))))
                (when text
                  (condition-case nil
                      (replace-match text t)
                    (error
                     (replace-match "[FIXME: invalid characters]"
                                    t))))))
                (if (and last-pos (= pos last-pos))
                    (if (eobp)
                        (setq regexp nil)
                      (forward-char 1)))
                (setq last-pos pos))))
          (setq rules (cdr rules)
                base (+ base (point-max))))
        (run-hooks 'emacs-wiki-after-markup-hook)
        (if verbose
            (message "Publishing %s...done" (emacs-wiki-page-name)))))

(defun emacs-wiki-custom-tags (&optional highlight-p)
  (let ((tag-info
         (or (assoc (match-string 1) emacs-wiki-markup-tags)
             (assoc (match-string 1) emacs-wiki-dangerous-tags))))
    (when (and tag-info (or (not highlight-p)
                            (nth 3 tag-info)))
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            (beg (point)) end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match
                         (concat "\\([^"
                                 emacs-wiki-regexp-space
                                 "=]+\\)\\(=\"\\([^\"]+\\)\"\\)?")
                         attrstr))
              (let ((attr
                     (cons (downcase
                            (emacs-wiki-match-string-no-properties 1 attrstr))
                           (emacs-wiki-match-string-no-properties 3 attrstr))))
                (setq attrstr (replace-match "" t t attrstr))
                (if attrs
                    (nconc attrs (list attr))
                  (setq attrs (list attr)))))))
        (if (and (cadr tag-info) (not closed-tag))
            (if (search-forward (concat "</" (car tag-info) ">")
                                nil t)
                (unless highlight-p
                  (delete-region (match-beginning 0) (point)))
              (setq tag-info nil)))
        (when tag-info
          (setq end (point-marker))
          (unless highlight-p
            (delete-region start beg)
            (goto-char start))
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (if (nth 3 tag-info)
                (nconc args (list highlight-p)))
            (apply (nth 4 tag-info) args))))))
  nil)

(defun emacs-wiki-markup-initial-directives ()
  (cond
   ((string= (match-string 1) "menu")
    (make-local-variable 'emacs-wiki-menu-current)
    (setq emacs-wiki-menu-definition
          (car (read-from-string (match-string 2))))
    (emacs-wiki-menu-make-from-list))
   ((string= (match-string 1) "title")
    (set (make-local-variable 'emacs-wiki-current-page-title)
         (match-string 2)))
   ((string= (match-string 1) "date")
    (set (make-local-variable 'emacs-wiki-current-page-date)
         (match-string 2)))
   ((string= (match-string 1) "style")
    (set (make-local-variable 'emacs-wiki-style-sheet)
          (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                  (match-string 2)
                  (if emacs-wiki-xhtml-space-singles
                      "\" />"
                    "\">"))))
   ((string= (match-string 1) "related")
    (set (make-local-variable 'emacs-wiki-current-page-related)
         (match-string 2))))
  "")

(defalias 'emacs-wiki-markup-custom-tags 'emacs-wiki-custom-tags)

(defun emacs-wiki-highlight-title ()
  (add-text-properties (+ 7 (match-beginning 0))
                       (emacs-wiki-line-end-position)
                       '(face emacs-wiki-header-1)))

;; This should be the very last tag highlighted.
(defun emacs-wiki-example-tag (beg end highlight-p)
  (if highlight-p
      (progn
        (emacs-wiki-unhighlight-region beg end)
        (emacs-wiki-multiline-maybe beg end)
        (add-text-properties beg end '(face emacs-wiki-verbatim-face)))
    (insert "<pre class=\"example\">")
    (while (equal (char-to-string (char-after)) "\n")
      (delete-char 1))                  ; delete leading newline(s).
    (emacs-wiki-escape-html-specials end)
    (when (< (point) end)
      (goto-char end))
    (insert "</pre>")
    (add-text-properties beg (point) '(rear-nonsticky (read-only)
                                                      read-only t))))

(defun emacs-wiki-verbatim-tag (beg end highlight-p)
  (if highlight-p
      (progn
        (emacs-wiki-unhighlight-region beg end)
        (emacs-wiki-multiline-maybe beg end)
        (add-text-properties beg end '(face emacs-wiki-verbatim-face)))
    (emacs-wiki-escape-html-specials end)
    (add-text-properties beg end '(rear-nonsticky (read-only)
                                                  read-only t))))

(defun emacs-wiki-nowiki-tag (beg end highlight-p)
  (if highlight-p
      (goto-char end)
    (add-text-properties
     beg end '(read-nonsticky (read-only) read-only t))))

(defun emacs-wiki-verse-tag (beg end)
  (save-excursion
    (while (< (point) end)
      (unless (eq (char-after) ?\n)
        (insert "> "))
      (forward-line))))

(defvar emacs-wiki-numbered-counter 1)
(make-variable-buffer-local 'emacs-wiki-numbered-counter)

(defun emacs-wiki-numbered-tag (beg end)
  (save-excursion
    (goto-char beg)
    (setq end (copy-marker (1- end)))
    (insert "<table cellspacing=\"8\">")
    (insert (format
             (concat "<tr><td valign=\"top\"><strong>%d</strong></td>"
                     "<td><p><a id=\"%d\""
                     (if emacs-wiki-xhtml-space-singles
                         " />"
                       ">"))
             emacs-wiki-numbered-counter emacs-wiki-numbered-counter))
    (setq emacs-wiki-numbered-counter
          (1+ emacs-wiki-numbered-counter))
    (save-match-data
      (while (and (< (point) end)
                  (re-search-forward "^$" end t))
        (replace-match (format "</p>
</td>
</tr><tr><td valign=\"top\"><strong>%d</strong></td><td>
<p id=\"%d\">" emacs-wiki-numbered-counter
                     emacs-wiki-numbered-counter))
        (setq emacs-wiki-numbered-counter
              (1+ emacs-wiki-numbered-counter))))
    (goto-char end)
    (insert "</p>
</td></tr></table>")))

(defun emacs-wiki-redirect-tag (beg end attrs)
  (let ((link (cdr (assoc "url" attrs))))
    (when link
      (setq emacs-wiki-meta-http-equiv "Refresh"
            emacs-wiki-meta-content
            (concat (or (cdr (assoc "delay" attrs))
                        (int-to-string emacs-wiki-redirect-delay))
                    ";\nURL=" (emacs-wiki-link-url link)))
      (if (= beg end)
          (insert "You should momentarily be redirected to [["
                  link "]].")
        (goto-char end))
      (delete-region (point) (point-max)))))

(defun emacs-wiki-nop-tag (beg end highlight-p)
  (when (<= (- end beg) 5)
    (if highlight-p
        (add-text-properties beg end
                             '(invisible emacs-wiki intangible t)))
    (when (looking-at emacs-wiki-name-regexp)
      (goto-char (match-end 0))
      (unless highlight-p
        (add-text-properties
         beg (match-end 0) '(rear-nonsticky
                             (read-only) read-only t))))))

(defun emacs-wiki-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward emacs-wiki-regexp-space)
  (if (looking-at "<\\([^ />]+\\)>")
      (let ((tag (match-string 1)))
        (goto-char (match-end 0))
        (insert "<a id=\"" anchor "\">")
        (when emacs-wiki-anchor-on-word
          (or (and (search-forward (format "</%s>" tag)
                                   (emacs-wiki-line-end-position) t)
                   (goto-char (match-beginning 0)))
              (forward-word 1)))
        (insert "</a>"))
    (insert "<a id=\"" anchor "\">")
    (when emacs-wiki-anchor-on-word
      (forward-word 1))
    (insert "</a>")))

(defun emacs-wiki-contents-tag (beg end attrs)
  (let ((max-depth (let ((depth (cdr (assoc "depth" attrs))))
                     (or (and depth (string-to-number depth)) 2)))
        (index 1)
        base contents l)
    (save-excursion
      (save-match-data
        (catch 'done
          (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)" nil t)
            (setq l (length (match-string 1)))
            (if (null base)
                (setq base l)
              (if (< l base)
                  (throw 'done t)))
            (when (<= l max-depth)
              (setq contents
                    (cons (cons l (emacs-wiki-wiki-visible-name
                                   (emacs-wiki-match-string-no-properties 2)))
                          contents))
              (goto-char (match-beginning 2))
              (emacs-wiki-insert-anchor
               (concat "sec" (int-to-string index)))
              (setq index (1+ index)))))))
    (setq index 1 contents (reverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (insert "<dl class=\"contents\">\n")
      (while contents
        (insert "<dt class=\"contents\">\n")
        (insert "<a href=\"#sec" (int-to-string index) "\">"
                (cdar contents)
                "</a>\n")
        (setq index (1+ index))
        (insert "</dt>\n")
        (setq depth (caar contents)
              contents (cdr contents))
        (if contents
            (cond
             ((< (caar contents) depth)
              (let ((idx (caar contents)))
                (while (< idx depth)
                  (insert "</dl>\n</dd>\n")
                  (setq sub-open (1- sub-open)
                        idx (1+ idx)))))
             ;; can't jump more than one ahead
             ((> (caar contents) depth)
              (insert "<dd>\n<dl class=\"contents\">\n")
              (setq sub-open (1+ sub-open))))))
      (while (> sub-open 0)
        (insert "</dl>\n</dd>\n")
        (setq sub-open (1- sub-open)))
      (insert "</dl>\n")
      (put-text-property p (point) 'read-only t))))

(defun emacs-wiki-comment-tag (beg end)
  (delete-region beg end))

(defun emacs-wiki-lisp-tag (beg end highlight-p)
  (if highlight-p
      (progn
        (emacs-wiki-unhighlight-region beg end)
        (add-text-properties
         beg end
         (list 'font-lock-multiline t
               'display (emacs-wiki-eval-lisp
                         (buffer-substring-no-properties (+ beg 6)
                                                         (- end 7)))
               'intangible t)))
    (save-excursion
      (let ((str (emacs-wiki-eval-lisp
                  (prog1
                      (buffer-substring-no-properties beg end)
                    (delete-region beg end)))))
        (set-text-properties 0 (length str) nil str)
        (insert str)))))

(defcustom emacs-wiki-command-default-file nil
  "If non-nil, this default program to use with <command> tags.
If nil, Eshell is used, since it works on all platforms."
  :type '(choice file (const :tag "Use Eshell" nil))
  :group 'emacs-wiki-publish)

;; Since XEmacs does not have a `make-temp-file' function, use the
;; recommended hack in the XEmacs manual to make a temporary file.
(if (fboundp 'make-temp-file)
    (defalias 'emacs-wiki-make-temp-file 'make-temp-file)
  (defun emacs-wiki-make-temp-file (prefix)
    (make-temp-name (expand-file-name prefix (temp-directory)))))

(defun emacs-wiki-command-tag (beg end attrs &optional highlight-p pre-tags)
  (if highlight-p
      (goto-char end)
    (while (looking-at "\\s-*$")
      (forward-line))
    (let ((interp (or (cdr (assoc "file" attrs))
                      emacs-wiki-command-default-file)))
      (if (null interp)
          (eshell-command
           (prog1
               (buffer-substring-no-properties (point) end)
             (delete-region beg end)) t)
        (let ((file (emacs-wiki-make-temp-file "ewiki")))
          (unwind-protect
              (let ((args (save-match-data (split-string interp))))
                (write-region (point) end file)
                (delete-region beg end)
                (if pre-tags
                    (insert "<pre>\n"))
                (apply 'call-process (car args) file t nil (cdr args))
                (while (eq (char-syntax (char-before)) ? )
                  (backward-char))
                (add-text-properties
                 beg (point)
                 '(rear-nonsticky (read-only) read-only t))
                (if pre-tags
                    (insert "</pre>\n")))
            (if (file-exists-p file)
                (delete-file file))))))))

(defcustom emacs-wiki-c-to-html
  (if (or (featurep 'executable)
          (load "executable" t t))
      (concat (executable-find "c2html") " -c -s"))
  "Program to use to convert <c-source> tag text to HTML."
  :type 'string
  :group 'emacs-wiki-publish)

(defun emacs-wiki-c-source-tag (beg end attrs highlight-p)
  (if highlight-p
      (goto-char end)
    (if emacs-wiki-c-to-html
        (let ((c-to-html emacs-wiki-c-to-html))
          (if (assoc "numbered" attrs)
              (setq c-to-html (concat c-to-html " -n")))
          (emacs-wiki-command-tag beg end
                                  (list (cons "file" c-to-html))))
      (insert "<pre>")
      (emacs-wiki-escape-html-specials end)
      (goto-char end)
      (add-text-properties beg (point)
                           '(rear-nonsticky (read-only) read-only t))
      (insert "</pre>"))))

(defun emacs-wiki-python-tag (beg end attrs highlight-p)
  (emacs-wiki-command-tag
   beg end (list (cons "file" (executable-find "python")))
   highlight-p (unless (assoc "raw" attrs) t)))

(defun emacs-wiki-perl-tag (beg end attrs highlight-p)
  (emacs-wiki-command-tag
   beg end (list (cons "file" (executable-find "perl")))
   highlight-p (unless (assoc "raw" attrs) t)))

(defun emacs-wiki-insert-xbel-bookmarks (bmarks folder)
  "Insert a set of XBEL bookmarks as an HTML list."
  (require 'xml-parse)
  (when (fboundp 'xml-tag-name)
    (while bmarks
      (let ((bookmark (car bmarks)))
        (cond
         ((equal (xml-tag-name bookmark) "folder")
          (let ((title (cadr (xml-tag-child bookmark "title"))))
            (unless folder
              (insert "<li>" title "\n<ul>\n"))
            (emacs-wiki-insert-xbel-bookmarks
             (xml-tag-children bookmark)
             (if (equal folder title)
                 nil
               folder))
            (unless folder
              (insert "</ul>\n"))))
         ((equal (xml-tag-name bookmark) "bookmark")
          (unless folder
            (insert "<li><a href=\""
                    (xml-tag-attr bookmark "href") "\">"
                    (cadr (xml-tag-child bookmark "title"))
                    "</a>\n")))))
      (setq bmarks (cdr bmarks)))))

(defcustom emacs-wiki-xbel-bin-directory "/usr/bin"
  "Directory where the xbel parsing utilities reside."
  :type 'directory
  :group 'emacs-wiki-publish)

(defun emacs-wiki-include-tag (beg end attrs)
  "Include the named file at the current location during publishing.

<include file=\"...\">

Includes the named file at the current location during
publishing. Files are marked up according to the emacs-wiki rules
except for inserting header and footer. If you want no markup to
be performed, either add <example>..</example> inside the source
file or use

<include file=\"...\" markup=\"nil\">

The markup attribute controls how this section is marked up. If
non-nil, it should be the name of a function to call after
inserting the file with the buffer narrowed to the section
inserted. Note that no further marking-up will be performed on
this region."
  (let ((filename (expand-file-name (cdr (assoc "file" attrs))))
        (markup (cdr (assoc "markup" attrs)))
        (emacs-wiki-publishing-header "")
        (emacs-wiki-publishing-footer ""))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (insert-file-contents filename)
        (if markup
            (let ((markup-function (read markup)))
              (when markup-function
                (funcall markup-function))
          (emacs-wiki-replace-markup))
        (add-text-properties
         (point-min)
         (point-max)
         '(rear-nonsticky (read-only) read-only t)))))))

(defun emacs-wiki-bookmarks-tag (beg end attrs)
  (require 'xml-parse)
  (let ((filename (expand-file-name (cdr (assoc "file" attrs))))
        (type (cdr (assoc "type" attrs)))
        (folder (cdr (assoc "folder" attrs)))
        buffer)
    (when filename
      (cond
       (type
        (setq buffer (get-buffer-create " *xbel_parse*"))
        (with-current-buffer buffer
          (erase-buffer)
          (call-process
           (format "%s/%s_parse"
                   (directory-file-name emacs-wiki-xbel-bin-directory)
                   type)
           nil t nil filename)))
       (t
        (setq buffer (find-file-noselect filename))))
      (insert "<ul>\n")
      (emacs-wiki-insert-xbel-bookmarks
       (with-current-buffer buffer
         (goto-char (point-min))
         ;; XBEL format
         (save-match-data
           (when (re-search-forward "<!DOCTYPE\\s-+xbel" nil t)
             (goto-char (match-beginning 0))
             ;; the `cdr' is to skip the "title" child
             (cdr (xml-tag-children (read-xml))))))
       folder)
      (insert "</ul>\n")
      (kill-buffer buffer)))
  (while (eq (char-syntax (char-before)) ? )
    (backward-char))
  (add-text-properties beg (point)
                       '(rear-nonsticky (read-only) read-only t)))

(defun emacs-wiki-link-url (wiki-link)
  "Resolve the given WIKI-LINK into its ultimate URL form."
  (let ((link (emacs-wiki-wiki-link-target wiki-link)))
     (save-match-data
        (cond
         ((emacs-wiki-wiki-url-p link)
          (emacs-wiki-escape-url link))
         ((or (string-match emacs-wiki-image-regexp link)
              (string-match emacs-wiki-file-regexp link))
          link)
         ((assoc (emacs-wiki-wiki-base link)
                 (emacs-wiki-file-alist))
          (if (string-match "#" link)
              (concat
               (emacs-wiki-escape-url
                (emacs-wiki-published-name
                 (substring link 0 (match-beginning 0))
                 (emacs-wiki-page-name)))
               "#"
               (substring link (match-end 0)))
            (emacs-wiki-escape-url
             (emacs-wiki-published-name
              link (emacs-wiki-page-name)))))))))

(defsubst emacs-wiki-link-href (url name)
  "Return an href string for URL and NAME."
  (concat "<a href=\""
          (emacs-wiki-link-url url) "\">" name "</a>"))

(defcustom emacs-wiki-markup-nonexistent-link t
  "Non-nil means transform a nonexistent link into a mailto link."
  :group 'emacs-wiki
  :type 'boolean)

(defun emacs-wiki-markup-link ()
  "Resolve the matched wiki-link into its ultimate <a href> form.
Images used the <img> tag."
  ;; avoid marking up urls that appear to be inside existing HTML
  (save-match-data
    (when (and (not (memq (char-after (point)) '(?\" ?\>)))
               (not (memq (char-before (match-beginning 0)) '(?\" ?\/))))
      (let* (string
             (wiki-link (match-string 0))
             (url (emacs-wiki-link-url wiki-link))
             (text (emacs-wiki-wiki-visible-name wiki-link))
             (name text))
        (when (and url (string-match emacs-wiki-url-regexp url))
          (let ((protocol (assoc (match-string 1 url)
                                 emacs-wiki-url-protocols)))
            (when protocol
              (setq url
                    (if (functionp (elt protocol 2))
                        (save-match-data
                          (funcall (elt protocol 2) url))
                      (elt protocol 2))))))
        ;; Replace protocols in the name, too
        (when (and name (string-match emacs-wiki-url-regexp name))
          (let ((protocol (assoc (match-string 1 name)
                                 emacs-wiki-url-protocols)))
            (when protocol
              (setq name
                    (replace-match
                     (or
                      (if (functionp (elt protocol 2))
                          (save-match-data
                            (funcall (elt protocol 2) (match-string 0 name)))
                        (elt protocol 2))
                      "")
                     t t name)))))
        (when name
          (setq name
                (if (string-match emacs-wiki-image-regexp name)
                    (if (string-match "^\\([^ ]+\\)\\s-+\\(.+\\)" name)
                        ;; [[image][image2 caption]]
                        (format (concat
                                 "<img src=\"%s\" alt=\"%s\""
                                 (if emacs-wiki-xhtml-space-singles
                                     " />"
                                   ">"))
                                (match-string 1 name)
                                (emacs-wiki-escape-html-string
                                 (match-string 2 name)))
                      ;; [[image][image2]]
                      (concat "<img src=\"" name "\" alt=\"\""
                              (if emacs-wiki-xhtml-space-singles
                                  " />"
                                ">")))
                  ;; [[image][caption]]
                  (if (and url (string-match emacs-wiki-image-regexp url))
                      (format (concat "<img src=\"%s\" alt=\"%s\""
                                      (if emacs-wiki-xhtml-space-singles
                                          " />"
                                        ">"))
                              url (emacs-wiki-escape-html-string name))
                    ;; [[link][caption]]
                    (emacs-wiki-escape-html-string name)))))
        (setq string
              (if (null url)
                  (if (and emacs-wiki-serving-p
                           (emacs-wiki-editable-p
                            (emacs-wiki-wiki-base wiki-link)))
                      (format
                       "<a class=\"nonexistent\" href=\"editwiki?%s\">%s</a>"
                       (emacs-wiki-wiki-base wiki-link) name)
                    (if (and emacs-wiki-maintainer
                             emacs-wiki-markup-nonexistent-link)
                        (format "<a class=\"nonexistent\" href=\"%s\">%s</a>"
                                emacs-wiki-maintainer name)
                      name))
                (if (string= url "")
                    name
                  (format "<a href=\"%s\">%s</a>"
                          url name))))
        (when (not (string= string ""))
          (let ((beg (point)))
            (insert string)
            (add-text-properties beg (point)
                                 '(rear-nonsticky (read-only)
                                        read-only t))))
        ""))))

(defun emacs-wiki-mark-noemphasis (&optional beg end)
  (unless beg (setq beg (match-beginning 0)))
  (unless end (setq end (match-end 0)))
  (add-text-properties beg end '(noemphasis t))
  nil)

(defun emacs-wiki-markup-word ()
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc)
    (unless (get-text-property beg 'read-only)
      (cond
       ((string= leader "_")
        (setq open-tag (if emacs-wiki-xhtml-inline-css
                           "<span style=\"text-decoration: underline;\">"
                         "<u>")
              close-tag (if emacs-wiki-xhtml-inline-css
                            "</span>"
                          "</u>")))
       ((string= leader "=")
        (setq open-tag "<code>" close-tag "</code>")
        (setq mark-read-only t))
       (t
        (let ((l (length leader)))
          (cond
           ((= l 1) (setq open-tag "<em>" close-tag "</em>"))
           ((= l 2) (setq open-tag "<strong>" close-tag "</strong>"))
           ((= l 3) (setq open-tag "<strong><em>"
                          close-tag "</em></strong>"))))))
      (if (and (not (get-text-property beg 'noemphasis))
               (setq loc (search-forward leader nil t))
               (not (eq (char-syntax (char-after loc)) ?w))
               (not (eq (char-syntax (char-before (point))) ?\ ))
               (not (get-text-property (point) 'noemphasis)))
          (progn
            (replace-match "")
            (insert close-tag)
            (save-excursion
              (goto-char beg)
              (delete-region beg end)
              (insert open-tag))
            (if mark-read-only
                (add-text-properties beg (point)
                                     '(rear-nonsticky (read-only) read-only
                                                      t))))
        (backward-char))
      nil)))

(defun emacs-wiki-markup-anchor ()
  (save-match-data
    (emacs-wiki-insert-anchor (match-string 1)))
  "")

(defcustom emacs-wiki-entity-table
  '(("#7779" . "s")
    ("#7717" . "h")
    ("#7789" . "t")
    ("#7716" . "H")
    ("#7826" . "Z"))
  "Substitutions to use for HTML entities which are not fully
supported by all browsers -- in other words, we are pre-empting the
entity mechanism and providing our own textual equivalent.  For
Unicode browsers, this is usually unnecessary."
  :type 'sexp
  :group 'emacs-wiki)

(defun emacs-wiki-markup-entity ()
  (or (cdr (assoc (match-string 1)
                  emacs-wiki-entity-table))
      (concat "&" (match-string 1) ";")))

(defsubst emacs-wiki-surround-text (beg-tag end-tag move-func)
  (insert beg-tag)
  (funcall move-func)
  (insert end-tag))                     ; returns nil for us

(defun emacs-wiki-markup-heading ()
  (let ((len (1+ (length (match-string 1)))))
    (emacs-wiki-surround-text (format "<h%d>" len) (format "</h%d>" len)
                              'end-of-line)
    ""))

(defun emacs-wiki-markup-footnote ()
  (if (/= (emacs-wiki-line-beginning-position) (match-beginning 0))
      "<sup><a id=\"fnr.\\1\" href=\"#fn.\\1\">\\1</a></sup>"
    (prog1
        "<sup>[<a id=\"fn.\\1\" href=\"#fnr.\\1\">\\1</a>]</sup>"
      (save-excursion
        (save-match-data
          (let* ((beg (goto-char (match-end 0)))
                 (end (and (search-forward "\n\n" nil t)
                           (prog1
                               (copy-marker (match-beginning 0))
                             (goto-char beg)))))
            (while (re-search-forward
                    (concat "^["
                            emacs-wiki-regexp-blank
                            "]+\\([^\n]\\)")
                    end t)
              (replace-match "\\1" t))))))))

(defsubst emacs-wiki-forward-paragraph ()
  (save-match-data
    (and (re-search-forward "^\\s-*$" nil t)
         (match-beginning 0))))

(defun emacs-wiki-markup-list-or-paragraph ()
  "Markup a list entry or quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (if (null (match-string 2))
      (let ((ws (match-string 4)))
        (when (save-excursion
                (forward-line)
                (or (eolp)
                    (looking-at "\\S-")))
          (let (block-start block-end)
            (delete-region (match-beginning 4) (match-end 4))
            (if (< (string-width ws) 6)
                (setq block-start "<blockquote>\n<p>"
                      block-end "</p>\n</blockquote>")
              (if emacs-wiki-xhtml-inline-css
                  (setq block-start "<span style=\"text-align: center;\">"
                        block-end "</span>")
                (setq block-start "<center>\n<p>"
                      block-end "</p>\n</center>")))
            (emacs-wiki-surround-text (format "%s\n%s" block-start ws)
                                      (format "\n%s\n" block-end)
                                      'emacs-wiki-forward-paragraph))))
    (let ((str (match-string 2)))
      (cond
       ((and (eq (aref str 0) ?-))
        (delete-region (match-beginning 0) (match-end 0))
        (emacs-wiki-surround-text
         "<ul>\n<li>" "</li>\n</ul>\n"
         (function
          (lambda ()
            (and (re-search-forward "^\\s-*\\(-\\s-\\|$\\)" nil t)
                 (goto-char (match-beginning 0)))))))
       ((and (>= (aref str 0) ?0)
             (<= (aref str 0) ?9))
        (delete-region (match-beginning 0) (match-end 0))
        (emacs-wiki-surround-text
         "<ol>\n<li>" "</li>\n</ol>\n"
         (function
          (lambda ()
            (and (re-search-forward "^\\s-*\\([0-9]+\\.\\s-\\|$\\)" nil t)
                 (goto-char (match-beginning 0)))))))
       (t
        (goto-char (match-beginning 0))
        (insert "<dl>\n<dt>")
        (save-match-data
          (when (re-search-forward
                 (concat "["
                         emacs-wiki-regexp-space
                         "]+::["
                         emacs-wiki-regexp-space
                         "]+")
                 nil t)
            (replace-match "</dt>\n<dd>\n")))
        (emacs-wiki-forward-paragraph)
        (insert "</dd>\n</dl>\n"))))))

(defun emacs-wiki-markup-table ()
  "Mark up tables normally."
  (let* ((str (save-match-data
                (emacs-wiki-replace-regexp-in-string
                 " *|+i'l *$" ""
                 (match-string 1))))
         (fields
          (append (save-match-data
                    (remove "" (split-string
                                str
                                (concat "["
                                        emacs-wiki-regexp-blank
                                        "]*|+["
                                        emacs-wiki-regexp-blank
                                        "]*"))))
                  (list (match-string 4))))
         (len (length (match-string 3)))
         (row (cond ((= len 1) "tbody")
                    ((= len 2) "thead")
                    ((= len 3) "tfoot")))
         (col (cond ((= len 1) "td")
                    ((= len 2) "th")
                    ((= len 3) "td"))))
    (concat "<table " emacs-wiki-table-attributes ">\n"
            "<" row ">\n" "<tr>\n<" col ">"
            (mapconcat 'identity fields (format "</%s><%s>" col col))
            "</" col ">\n" "</tr>\n" "</" row ">\n"
            "</table>\n")))

(defun emacs-wiki-markup-verse ()
  (save-match-data
    (let* ((lines (split-string (match-string 1) "\n"))
           (l lines))
      (while l
        (if (and (> (length (car l)) 2)
                 (string-match "\\`\\s-*> " (car l)))
            (setcar l (substring (car l) (match-end 0))))
        (setq l (cdr l)))
      (concat "<pre class=\"verse\">"
              (mapconcat 'identity lines "\n") "</pre>"))))

(defcustom emacs-wiki-pretty-changelogs nil
  "If non-nil, markup ChangeLog buffers using pretty tables.
This rule requires that a GIF file called \"onepixel.gif\" be in your
publication tree.  Here is a uuencoded version of such a file:

begin 644 onepixel.gif
M1TE&.#EA`0`!`*$``````/___________R'Y!`'__P$`+``````!``$```(\"
$3`$`.P``
`
end"
  :type 'boolean
  :group 'emacs-wiki-publish)

(defun emacs-wiki-changelog-escape-files ()
  (replace-match "[[\\1]]" t nil nil 1))

(defun emacs-wiki-markup-changelog-section ()
  (if (not emacs-wiki-pretty-changelogs)
      "* \\1 \\2"
    (let ((email (match-string 2))
          (date (match-string 1)))
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (while (eolp)
        (kill-line 1))
      (insert (format "  <table class=\"changelog table parent\"
         width=\"100%%\" border=\"0\" cellspacing=\"1\"
         cellpadding=\"2\">
    <tr>
      <td style=\"background: #000 url(onepixel.gif);\">
        <table width=\"100%%\" border=\"0\"
               cellpadding=\"5\" cellspacing=\"0\">
          <tr>
            <td class=\"changelog table author\" style=\"color: navy;
                background: #b0c4de url(onepixel.gif); text-align: left;\">
              <b>%s</b>
            </td>
            <td class=\"changelog table email\" style=\"color: #2f4f4f;
                font-size: 90%%; background: #b0c4de url(onepixel.gif);
                text-align: right; vertical-align: bottom;\">
               %s
            </td>
          </tr>
          <tr>
            <td class=\"changelog table changes\" style=\"color: #000;
                background: #fffff0 url(onepixel.gif);\" colspan=\"2\">
" email date))
      (add-text-properties (match-beginning 0) (point)
                           '(read-only t rear-nonsticky (read-only))))
    (if (re-search-forward "^[0-9]" nil t)
        (goto-char (1- (match-beginning 0)))
      (goto-char (point-max))
      (while (eq (char-before (1- (point))) ?\n)
        (delete-char -1)))
    (let ((here (1- (point))))
      (insert "
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </table>")
      (add-text-properties here (point)
                           '(read-only t rear-nonsticky (read-only)))
      nil)))

(provide 'emacs-wiki-publish)
;;; emacs-wiki-publish.el ends here
