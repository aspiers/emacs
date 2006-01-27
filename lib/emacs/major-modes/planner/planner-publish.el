;;; planner-publish.el --- planner-specific publishing

;; Copyright (C) 2005, 2006 Peter K. Lee
;; Parts copyright (C) 2005 Chris McMahan
;; Parts copyright (C) 2005, 2006 Free Software Foundation, Inc.
;; Parts copyright (C) 2005 Dale P. Smith

;; Author: Peter K. Lee <saint@ c o r e n o v a .com>
;; Keywords: planner publish
;; Timestamp: 20 Jul 2005 10:05:29
;; X-URL: http://www.corenova.com/...

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Introduction

;; Muse Styles for Planner: planner-xml, planner-html, planner-xhtml, etc.

;; Handles publishing of planner files.  Works with Muse to generate
;; flexible markup.

;;; History:

;; 2005-07-15 (0.1) : creation date
;; 2005-07-20 (0.2) : first public release
;; 2005-07-21 (0.3) : added planner-html-style-sheet customize option
;; 2005-08-09 : added to Planner, see ChangeLog for further changes

;;; TODO:

;; add support for various PLANNER specific sections such as Diary,
;; Accomplishments, Timeclock, etc.

;;; Contributors:

;; Chris McMahan (cmcmahan AT one.net) helped notes to publish correctly.

;; Jim Ottaway fixed several bugs.

;; David Smith fixed a few bugs.

;; Dale Smith implemented a new version of the "notes" tag and
;; provided several patches.

(require 'planner)

(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html) ;;; allow derive style from "html" and "xhtml"
(require 'muse-xml)  ;;; allow derive style from "xml"

(defgroup planner-publish nil
  "Options controlling the behavior of PLANNER publishing.
See `planner-publish' for more information."
  :group 'planner)

(defcustom planner-publish-markup-regexps
  '((1275 "^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oXDCP]\\)\\s-*\\(.+\\)" 0 task)
    (1280 "^\\.#[0-9]+\\s-*" 0 note)
    (3200 planner-date-regexp 0 link))
  "List of markup rules for publishing PLANNER.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-html)

(defcustom planner-publish-markup-functions
  '((task . planner-publish-markup-task)
    (note . planner-publish-markup-note))
    "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'planner-publish)

(defcustom planner-publish-markup-tags
  '(("nested-section" t nil planner-publish-nested-section-tag)
    ("title" t nil planner-publish-title-tag)
    ("content" t nil planner-publish-content-tag)
    ("tasks-section" t nil planner-publish-tasks-section-tag)
    ("notes-section" t nil planner-publish-notes-section-tag)
    ("notes"   nil nil planner-publish-notes-tag)
    ("past-notes" nil t planner-publish-past-notes-tag)
    ("task"    t t   planner-publish-task-tag)
    ("note"    t t   planner-publish-note-tag))
  "A list of tag specifications, for specially marking up PLANNER.
See `muse-publish-markup-tags' for more information."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       function))
  :group 'planner-publish)

;;;_ + XML specific customizations

(defcustom planner-xml-markup-strings
  '((planner-begin-nested-section . "<section>")
    (planner-end-nested-section   . "</section>")
    (planner-begin-title         . "<title>")
    (planner-end-title           . "</title>")
    (planner-begin-content       . "")
    (planner-end-content         . "")
    (planner-begin-body          . "")
    (planner-end-body            . "")
    (planner-begin-task-section  . "<tasks>")
    (planner-end-task-section    . "</tasks>")
    (planner-begin-task-body     . "")
    (planner-end-task-body       . "")
    (planner-begin-note-section  . "<notes>")
    (planner-end-note-section    . "</notes>")
    (planner-begin-task   . "<task status=\"%s\" priority=\"%s\">")
    (planner-end-task     . "</task>")
    (planner-begin-note   . "<note number=\"%s\">")
    (planner-end-note     . "</note>")
    (planner-begin-note-details . "<details><timestamp>%s</timestamp>")
    (planner-end-note-details . "</details>")
    (planner-begin-note-link . "<references>")
    (planner-end-note-link . "</references>")
    (planner-begin-note-categories . "<categories>")
    (planner-end-note-categories . "</categories>"))
  "Strings used for marking up text as XML.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles.

If a markup rule is not found here, `muse-xml-markup-strings' is
searched."
  :type '(alist :key-type symbol :value-type string)
  :group 'planner-publish)

(defcustom planner-xml-header
  "<?xml version=\"1.0\" encoding=\"<lisp>(muse-xml-encoding)</lisp>\"?>
<PLANNER>
  <pageinfo>
	<title><lisp>(muse-publishing-directive \"title\")</lisp></title>
	<author><lisp>(muse-publishing-directive \"author\")</lisp></author>
	<maintainer><lisp>(muse-style-element :maintainer)</lisp></maintainer>
    <pubdate><lisp>(muse-publishing-directive \"date\")</lisp></pubdate>
  </pageinfo>
<!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing PLANNER XML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-xml-footer "
<!-- Page published by Emacs Muse ends here -->
</PLANNER>\n"
  "Footer used for publishing PLANNER XML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

;;;_ + HTML specific customizations

(defcustom planner-html-markup-strings
  '((planner-begin-nested-section . "<div class=\"section\">")
    (planner-end-nested-section   . "</div>")
    (planner-begin-title         . "<div class=\"title\">")
    (planner-end-title           . "</div>")
    (planner-begin-content       . "<div class=\"content\">")
    (planner-end-content         . "</div>")
    (planner-begin-body          . "<div class=\"body\">")
    (planner-end-body            . "</div>")
    (planner-begin-task-section  . "<div id=\"tasks\" class=\"section\">")
    (planner-end-task-section    . "</div>")
    (planner-begin-task-body     . "<ul class=\"body\">")
    (planner-end-task-body       . "</ul>")
    (planner-begin-note-section  . "<div id=\"notes\" class=\"section\">")
    (planner-end-note-section    . "</div>")
    (planner-begin-task   . "<li class=\"task\"><span class=\"%s\"><span class=\"%s\">%s</span>")
    (planner-end-task     . "</span></li>")
    (planner-begin-note   . "<div class=\"note\"><a name=\"%s\"></a><span class=\"anchor\">%s</span>")
    (planner-end-note     . "</div>")
    (planner-begin-note-details . "<div class=\"details\"><span class=\"timestamp\">%s</span>")
    (planner-end-note-details . "</div>")
    (planner-begin-note-link . " <span class=\"link\">")
    (planner-end-note-link . "</span>")
    (planner-begin-note-categories . " <span class=\"categories\">")
    (planner-end-note-categories . "</span>\n"))
  "Strings used for marking up text as HTML.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles.

If a markup rule is not found here, `muse-html-markup-strings' is
searched."
  :type '(alist :key-type symbol :value-type string)
  :group 'planner-publish)

(defcustom planner-html-style-sheet
    "<style type=\"text/css\">
body {
  background: white; color: black;
  margin-left: 3%; margin-right: 3%;
}

p { margin-top: 3px; margin-bottom: 3px; }
p.verse { margin-left: 3% }

h1,h2,h3,h4,h5 { margin:0; padding:0; }

h1 { padding: 10px; margin-bottom: 10px; }

table.muse-table { margin: 0; font-size: 11px;
                   border-collapse: collapse;
                   background: #e2effa;
                   border: 1px solid #aadeed; }

table.muse-table tbody td { border: 1px solid #ccdeed; }

.example { margin-left: 5px; padding: 3px;
           background: #fffffc;
           border: 1px solid #ccdeed; }

/* enabled with planner-sectionalize.el */
.section { margin: 0; padding: 10px;
           margin-bottom: 15px;
           font-size: 12px; }

.section .section { margin: 0; margin-left: 5px;
                    font-size: 11px; }

.title { margin: 0; padding; 0 }

.section .title { font-size: 14px; }
.section .section .title { font-size: 12px; }
.section .section .section .title { font-size: 11px; }

/* Tasks section */
.task .A { color: red }
.task .B { color: green }
.task .C { color: navy }
.task .done      { color: gray; text-decoration: line-through; }
.task .cancelled { color: gray; text-decoration: italic; }

</style>"
  "Store your stylesheet definitions here.  The provided default
is for reference only.  You definitely want to customize this for
your particular needs & wants.  This is used in
`planner-html-header' and `planner-xhtml-header'.  Refer to
`muse-html-style-sheet' for details on usage.  You may simply
override the above by specifying an explicit link to a CSS file."
  :type 'string
  :group 'planner-publish)

(defcustom planner-html-header
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html>
  <head>
    <title><lisp>
  (concat (muse-publishing-directive \"title\")
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat \" (by \" author \")\"))))</lisp></title>
    <meta name=\"generator\" content=\"muse.el\">
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\">
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
        (when maintainer
          (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\">\")))
    </lisp>
    <lisp>planner-html-style-sheet</lisp>
  </head>
  <body>
    <div id=\"content\">
      <h1><span><lisp>
    (concat (muse-publishing-directive \"title\")
            (let ((author (muse-publishing-directive \"author\")))
              (if (not (string= author (user-full-name)))
                  (concat \" (by \" author \")\"))))</lisp></span></h1>
      <div id=\"inner-header\">
        <lisp>planner-html-inner-header</lisp>
      </div>
      <div id=\"muse-sections\">
      <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing PLANNER HTML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-html-footer "
<!-- Page published by Emacs Muse ends here -->
      </div>
      <div id=\"inner-footer\">
        <lisp>planner-html-inner-footer</lisp>
      </div>
    </div>
  </body>
</html>\n"
  "Footer used for publishing PLANNER HTML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-xhtml-header
  "<?xml version=\"1.0\" encoding=\"<lisp>
  (muse-html-encoding)</lisp>\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>
  (concat (muse-publishing-directive \"title\")
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat \" (by \" author \")\"))))</lisp></title>
    <meta name=\"generator\" content=\"muse.el\" />
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\" />
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
        (when maintainer
          (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\" />\")))
    </lisp>
    <lisp>planner-html-style-sheet</lisp>
  </head>
  <body>
    <div id=\"content\">
      <h1><span><lisp>
    (concat (muse-publishing-directive \"title\")
            (let ((author (muse-publishing-directive \"author\")))
              (if (not (string= author (user-full-name)))
                  (concat \" (by \" author \")\"))))</lisp></span></h1>
      <div id=\"inner-header\">
        <lisp>planner-html-inner-header</lisp>
      </div>
      <div id=\"muse-sections\">
      <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing PLANNER XHTML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-xhtml-footer "
<!-- Page published by Emacs Muse ends here -->
      </div>
      <div id=\"inner-footer\">
        <lisp>planner-html-inner-footer</lisp>
      </div>
    </div>
  </body>
</html>\n"
  "Footer used for publishing PLANNER XHTML files.
This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-html-inner-header ""
  "Extra header section that can be embedded w/in existing
`planner-html-header'.  This may be text or a filename."
  :type 'string
  :group 'planner-publish)

(defcustom planner-html-inner-footer ""
  "Extra footer section that can be embedded w/in existing
`planner-html-footer'.  This may be text or a filename."
  :type 'string
  :group 'planner-publish)

;;;_ + Publishing hooks

(defun planner-publish-prepare-buffer ()
  "Return nil to allow hook to continue"
  (planner-sectionalize-page)
  nil)

;;;_ + Markup

(defun planner-publish-markup-task ()
  "Replace tasks with XML representation of task data."
  (save-restriction
    (narrow-to-region
     (planner-line-beginning-position)
     (planner-line-end-position))
    (muse-publish-escape-specials (point-min) (point-max))
    (let ((info (planner-current-task-info)))
      (delete-region (point-min) (point-max))
      (forward-line 1)
      (insert
       (format (concat "<task id=\"%s\" priority=\"%s\" status=\"%s\""
                       " link=\"%s\" plan=\"%s\" date=\"%s\">")
               (or (planner-task-number info) "")
               (or (planner-task-priority info) "")
               (or (planner-publish-task-status-expand
                    (planner-task-status info)) "")
               (or (planner-task-link-text info) "")
               (or (planner-task-plan info) "")
               (or (planner-task-date info) ""))
       (planner-task-description info)  ; mark this area read only
       "</task>"))))

(defun planner-publish-markup-note ()
  "Replace note with XML representation of note data.  Borrowed
  heavily from Sacha's personal configs."
  (save-restriction
    (narrow-to-region
     (save-excursion (beginning-of-line) (point))
     (or (save-excursion
           (and (re-search-forward "^\\(\\.#\\|* \\|</notes-section>\\)" nil t)
                (match-beginning 0)))
         (point-max)))
    (let ((info (planner-current-note-info t)))
      (delete-region (point-min) (point-max))
      (insert (format (concat "<note anchor=\"%s\" timestamp=\"%s\""
                              " link=\"%s\" categories=\"%s\">")
                      (planner-note-anchor info)
                      (or (planner-note-timestamp info) "")
                      (or (planner-note-link info) "")
                      (or (planner-note-link-text info) ""))
              "<title>" (planner-note-title info) "</title>\n"
              "<content>\n" (planner-note-body info) "\n\n</content>\n"
              "</note>\n"))))


;;;_ + Tags

(defun planner-insert-markup (&rest args)
  (if (fboundp 'muse-insert-markup)
      (apply 'muse-insert-markup args)
    (apply 'insert args)))

(defun planner-publish-nested-section-tag (beg end)
  "Generated by `sectionalize', the nested section tag now takes
  in TITLE and LEVEL attributes.  Do not get this confused with
  MUSE specific `section', `subsection', etc. tags.  The MUSE
  specific sections are more like title/heading tags than this
  nested section block version."
  (save-excursion
    (goto-char beg)
    (planner-insert-markup (muse-markup-text 'planner-begin-nested-section))
    (goto-char end)
    (planner-insert-markup (muse-markup-text 'planner-end-nested-section))))

(defun planner-publish-title-tag (beg end)
  (save-excursion
    (goto-char beg)
    (planner-insert-markup (muse-markup-text 'planner-begin-title))
    (goto-char end)
    (planner-insert-markup (muse-markup-text 'planner-end-title))))

(defun planner-publish-content-tag (beg end)
  (save-excursion
    (goto-char end)
    (planner-insert-markup (muse-markup-text 'planner-end-content))
    (goto-char beg)
    (planner-insert-markup (muse-markup-text 'planner-begin-content))))

(defun planner-publish-tasks-section-tag (beg end)
  (save-excursion
    (goto-char beg)
    (planner-insert-markup (muse-markup-text 'planner-begin-task-section))
    (forward-line 1)
    (planner-insert-markup (muse-markup-text 'planner-begin-task-body))
    (goto-char end)
    (planner-insert-markup (muse-markup-text 'planner-end-task-body))
    (planner-insert-markup (muse-markup-text 'planner-end-task-section))))

(defun planner-publish-task-tag (beg end attrs)
  (save-excursion
    (let ((number   (cdr (assoc "id" attrs)))
          (status   (cdr (assoc "status" attrs)))
          (priority (cdr (assoc "priority" attrs)))
          (link     (cdr (assoc "link" attrs)))
          (plan     (cdr (assoc "plan" attrs)))
          (date     (cdr (assoc "date" attrs))))
      (goto-char beg)
      (planner-insert-markup
       (muse-markup-text 'planner-begin-task
                         status
                         priority
                         (concat priority number " "
                                 (planner-publish-task-status-collapse status)
                                 " ")))
      (goto-char end)
      (when link
        (insert " (" (planner-make-link link) ")"))
      (planner-insert-markup (muse-markup-text 'planner-end-task)))))

(defun planner-publish-notes-section-tag (beg end)
  "Replace the region BEG to END with the notes for this page."
  (save-excursion
    (planner-insert-markup (muse-markup-text 'planner-begin-note-section))
    (forward-line 1)
    (planner-insert-markup (muse-markup-text 'planner-begin-body))
    (goto-char end)
    (planner-insert-markup (muse-markup-text 'planner-end-body))
    (planner-insert-markup (muse-markup-text 'planner-end-note-section))))

(defun planner-publish-notes-tag (beg end)
  "Replace the region BEG to END with an index of the notes for this page."
  (delete-region beg end)
  (insert "\n")
  (mapcar
   (lambda (item)
     (insert (format " - [[%s%s][%s]]\n"
                     (planner-page-name)
                     (car item)
                     (planner-remove-links (cdr item)))))
   (save-excursion
     (find-file muse-publishing-current-file)
     (planner-notes-get-headlines)))
  (insert "\n"))

(defun planner-publish-past-notes-tag (beg end attrs)
  "Replace the region BEG to END with an index of past notes.
If ATTRS is non-nil, it is an alist containing values for
DIRECTORY and START."
  (let ((files (save-excursion
                 (find-file muse-publishing-current-file)
                 (planner-get-day-pages nil nil t)))
        (earliest (cdr (assoc "start" attrs))))
    (while files
      (when (or (null earliest)
                (not (string-lessp (caar files) earliest)))
        (let ((title-lines (list t)))
          (with-temp-buffer
            (insert-file-contents (cdar files))
            (while (re-search-forward "^\\.#\\([0-9]+\\)\\s-+\\(.+\\)" nil t)
              (nconc title-lines (list (cons (match-string 1)
                                             (match-string 2))))))
          (setq title-lines (cdr title-lines))
          (when title-lines
            (insert (planner-make-link (planner-page-name (caar files)))
                    " ::\n")
            (planner-insert-markup "  <dl class=\"contents\">\n")
            (while title-lines
              (planner-insert-markup "  <dt class=\"contents\">")
              (insert (format "[[%s#%s][%s]]"
                              (planner-page-name (caar files))
                              (caar title-lines) (cdar title-lines)))
              (planner-insert-markup "</dt>\n")
              (setq title-lines (cdr title-lines)))
            (planner-insert-markup "  </dl>\n\n"))))
      (setq files (cdr files)))))

(defun planner-publish-note-tag (beg end attrs)
  (save-excursion
    (let ((anchor     (or (cdr (assoc "anchor" attrs)) ""))
          (timestamp  (or (cdr (assoc "timestamp" attrs)) ""))
          (link       (or (cdr (assoc "link" attrs)) ""))
          (categories (or (cdr (assoc "categories" attrs)) "")))

      (setq categories "") ; categories broken for now
      (goto-char beg)
      (planner-insert-markup (muse-markup-text 'planner-begin-note
                                               anchor
                                               (concat "#" anchor)))
      (goto-char end)
      (planner-insert-markup (muse-markup-text 'planner-begin-note-details
                                               timestamp)
                             (muse-markup-text 'planner-begin-note-link))
      (insert link)
      (planner-insert-markup (muse-markup-text 'planner-end-note-link)
                             (muse-markup-text 'planner-begin-note-categories))
      (insert categories)
      (planner-insert-markup (muse-markup-text 'planner-end-note-categories)
                             (muse-markup-text 'planner-end-note-details))
      (planner-insert-markup (muse-markup-text 'planner-end-note)))))

;;;_ + helper routine

(defun planner-publish-task-status-expand (status)
  (cond
   ((string= status "_") "open")
   ((string= status "o") "in-progress")
   ((string= status "D") "delegated")
   ((string= status "P") "pending")
   ((string= status "X") "done")
   ((string= status "C") "cancelled")
   (t "unknown")))

(defun planner-publish-task-status-collapse (status)
  (cond
   ((string= status "open")        "_")
   ((string= status "in-progress") "o")
   ((string= status "delegated")   "D")
   ((string= status "pending")     "P")
   ((string= status "done")        "X")
   ((string= status "cancelled")   "C")
   (t "?")))

(defvar planner-sectionalize-delimiter "*"
  "The delimiter used to sectionalize.")

(defun planner-sectionalize-page ()
  "A wrapper around `sectionalize' that calls it on the
  entire page.  Uses the `planner-sectionalize-delimiter'
  variable value.  Should not have to call directly.  Should be a
  part of before-publish-hook."
  (interactive)
  (let ((delim planner-sectionalize-delimiter))
    (save-excursion
      (goto-char (point-min))
      (sectionalize delim)
      t)))

(defvar sectionalize-markup-tagname
  '(("* Tasks" . "tasks-section")
    ("* Notes" . "notes-section")))

(defun sectionalize-markup-tagname (text)
  "A routine that checks `sectionalize-markup-tagname' for tagname."
  (let ((tagname (cdr (assoc text sectionalize-markup-tagname))))
    (if tagname
        tagname
        "nested-section")))

(defun sectionalize (delim &optional n)
  "A routine that envelops regions of the buffer based on areas
bound by the DELIM character.

optional parameter N is used *internally* to denote the current
recursion depth."
  (unless n (setq n 0))
  (let ((regexp (concat "^\\(\\" delim "+\\)\\s-+")))
    (while (and regexp (re-search-forward regexp nil t))
      (let ((depth (length (match-string 1)))
            (title (buffer-substring (match-end 0) (point-at-eol)))
            (tagname (sectionalize-markup-tagname
                      (buffer-substring (match-beginning 0) (point-at-eol)))))
        (cond ((> depth n)
               (delete-region (match-beginning 0) (point-at-eol))
               (when (not (string= title ""))
                 (insert (format "<%s level=\"%s\"><title>%s</title>"
                                 tagname depth title))
                 (sectionalize delim depth)
                 (insert (format "</%s>\n" tagname))))
              (t (setq regexp nil)
                 (goto-char (match-beginning 0))))))
      (if regexp (goto-char (point-max)))))

;;;_ + Planner Style Definitions

(unless (assoc "planner-xml" muse-publishing-styles)
  (muse-derive-style "planner-xml" "xml"
                     :regexps   'planner-publish-markup-regexps
                     :functions 'planner-publish-markup-functions
                     :tags      'planner-publish-markup-tags
                     :strings   'planner-xml-markup-strings
                     :before    'planner-publish-prepare-buffer
                     :header    'planner-xml-header
                     :footer    'planner-xml-footer)
  (muse-derive-style "planner-html" "html"
                     :regexps   'planner-publish-markup-regexps
                     :functions 'planner-publish-markup-functions
                     :tags      'planner-publish-markup-tags
                     :strings   'planner-html-markup-strings
                     :before    'planner-publish-prepare-buffer
                     :header    'planner-html-header
                     :footer    'planner-html-footer)
  (muse-derive-style "planner-xhtml" "xhtml"
                     :regexps   'planner-publish-markup-regexps
                     :functions 'planner-publish-markup-functions
                     :tags      'planner-publish-markup-tags
                     :strings   'planner-html-markup-strings
                     :before    'planner-publish-prepare-buffer
                     :header    'planner-xhtml-header
                     :footer    'planner-xhtml-footer))

(provide 'planner-publish)

;;; planner-publish.el ends here

