;;; emacs-wiki-menu.el --- Generate menus for Emacs Wiki projects

;; Copyright (C) 2004 Mark Tommasi
;; Copyright (C) 2004,2005 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-menu.el
;; Keywords: hypermedia
;; Author: Mark Tommasi (tommasi AT univ-lille3 DOT fr)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Generate menus for Emacs Wiki projects
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

;; This file is the part of the Emacs Wiki project that allows you to
;; generate menus.

;;;_ + Startup

;; To generate menus for your projects, you will need to put this in
;; your .emacs file:
;;
;;   (require 'emacs-wiki-menu)
;;
;; You will have to include the following in your stylesheet:
;;
;;   <lisp>(funcall emacs-wiki-menu-factory)</lisp>
;;
;; In order to make menu entries look right, you should take a look at
;; the /* Menu properties */ section of `emacs-wiki-style-sheet' and
;; add those entries to your style sheet.

;;;_ + Usage

;; Once you have gone through all of the procedures in the Startup
;; section, emacs-wiki-menu.el will generate menus that contain an
;; alphabetized listing of everything in `emacs-wiki-projects'.
;;
;; Of course, other options are available.  If you change
;; `emacs-wiki-menu-factory', you will have access to several other
;; ways of generating menus.  Here are the different possible settings
;; for that option.
;;
;;     `emacs-wiki-menu-fixed': The text in `emacs-wiki-menu-default'
;;        will be used to generate the menu.
;;
;;     `emacs-wiki-menu-no-menu': No menu will be made.
;;
;;     `emacs-wiki-menu-make-from-projects': Create menu items from
;;        the project lists.
;;
;;     `emacs-wiki-menu-make-from-file': Create menu from a file.
;;
;;     `emacs-wiki-menu-make-from-list': Define
;;        `emacs-wiki-menu-definition', which is a list of 3 tuples of
;;        the form: text, url, tooltip, and this function will
;;        generate a menu based on that list.

;;;_ + Contributors

;; Christopher San Diego (toppy AT toppy DOT port5 DOT com) provided a
;; patch that makes `emacs-wiki-menu-make-from-list' use a template
;; instead of hard-coding what the generated menu items should look
;; like.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Menu Generation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki)

(defgroup emacs-wiki-menu nil
  "Options controlling the behavior of emacs-wiki menu generation."
  :group 'emacs-wiki)

(defcustom emacs-wiki-menu-factory 'emacs-wiki-menu-make-from-projects
  "Choose which method to use in producing the menu.

 `emacs-wiki-menu-fixed'

    The text in `emacs-wiki-menu-default' will be used to
    generate the menu.

 `emacs-wiki-menu-no-menu'

    No menu will be made.

 `emacs-wiki-menu-make-from-projects'

    Create menu items from the project lists.

 `emacs-wiki-menu-make-from-file'

    Create menu from a file.

 `emacs-wiki-menu-make-from-list'

    Define `emacs-wiki-menu-definition', which is a list of 3
    tuples of the form: text, url, tooltip, and this function
    will generate a menu based on that list."
  :type 'function
  :group 'emacs-wiki-menu)

(defcustom emacs-wiki-menu-filename ".menu-emacs-wiki"
  "Location of file to obtain a menu from.

This is relative to the current project's directory.
If this file does not exist, a menu will be generated."
  :type 'string
  :group 'emacs-wiki-menu)

(defcustom emacs-wiki-menu-definition nil
  "List of 3 tuples of the form: text, url, tooltip.
This is used if you have set `emacs-wiki-menu-factory' to
'emacs-wiki-menu-make-from-list."
  :type 'string
  :group 'emacs-wiki-menu)

(defcustom emacs-wiki-menu-make-from-list-format
  "  <div class=\"menuitem\">
  <a href=\"%url%\" onmouseout=\"hide()\"
onmouseover=\"showToolTip('%tooltip%')\">%text%</a>
  </div>\n"

"The format to be used for each menu item produced by
`emacs-wiki-menu-make-from-list'.

%text%, %url% and %tooltip% will be replaced by their respective values
from each of the tuples in `emacs-wiki-menu-definition'."
  :type 'string
  :group 'emacs-wiki-menu)

(defcustom emacs-wiki-menu-top
"<script type=\"text/javascript\">
function showToolTip(machaine) {
  if (machaine != \"\")   {
    document.getElementById('tooltip').innerHTML     = machaine;
    document.getElementById('tooltipbox').style.visibility = 'visible';
  }
}
function hide() {
  document.getElementById('tooltip').innerHTML     = '';
  document.getElementById('tooltipbox').style.visibility = 'hidden';
}
</script>
<div class=\"menu\">
"
  "HTML code to insert before any menu items.

This code is inserted when `emacs-wiki-menu-factory' is set to
`emacs-wiki-make-menu-from-projects' or the inline #menu
directive is used."
  :type 'string
  :group 'emacs-wiki-menu)

(defcustom emacs-wiki-menu-bottom
  "
</div><!-- menu ends here -->
<div id=\"tooltipbox\">
<div id=\"tooltip\"></div>
</div>"
  "HTML code to append to the menu items.

This code is inserted when `emacs-wiki-menu-factory' is set to
`emacs-wiki-menu-from-projects' or the inline #menu directive is
used."
  :type 'string
  :group 'emacs-wiki-menu)

(defvar emacs-wiki-menu-current nil
  "Used to see if a menu has already been generated.")

(defcustom emacs-wiki-menu-default
  "<div class=\"menu\">
       <div class=\"menuitem\"><a href=\"/\">Home</a></div>
    </div>\n"
  "List of menuitems.

Each menu item is 3 elements: list text, url, and tooltip."
  :type 'string
  :group 'emacs-wiki-menu)

(defun emacs-wiki-menu-fixed ()
  "The text in `emacs-wiki-menu-default' will be used to generate
the menu."
  (or emacs-wiki-menu-current emacs-wiki-menu-default))

(defun emacs-wiki-menu-no-menu ()
  "Do not make a menu."
  (or emacs-wiki-menu-current "\n<!-- No menu -->\n"))

(defun emacs-wiki-menu-make-from-projects ()
  "Build a menu according to the list of projects."
  (unless emacs-wiki-menu-current
    (let ((projects ;; get sorted list of project names
           (sort
            (mapcar 'car emacs-wiki-projects)
            'string<)))
      (concat
       emacs-wiki-menu-top
       (mapconcat
        (lambda (project)
          (let ((url (emacs-wiki-project-homepage project)))
            (if url
                (concat
                 "<div class=\"menuitem\">\n  <a href=\""
                 url "\">"
                 (if emacs-wiki-project-remove-last-word
                     (replace-regexp-in-string
                      (concat "[" emacs-wiki-regexp-upper
                              "][" emacs-wiki-regexp-lower
                              "]+$")
                      "" project)
                   project)
                 "</a>\n</div>\n")
              "")))
        projects "\n")
       emacs-wiki-menu-bottom))))

(defun emacs-wiki-menu-make-from-file ()
  "Build a menu according to the contents of a file in the same
form as described in `emacs-wiki-menu-make-from-list'."
  (unless emacs-wiki-menu-current
    (setq emacs-wiki-menu-definition
     (let ((buffer (find-file-noselect emacs-wiki-menu-filename))
           resu)
       (and buffer
            (save-excursion
              (save-match-data
                (set-buffer buffer)
                (setq buffer-read-only t)
                (goto-char (point-min))
                (and (re-search-forward "^#\\(menu \\)\\(.*\\)$"
                                        nil t)
                     (setq resu (match-string 2)))
                (kill-buffer buffer)))
            (car (read-from-string resu)))))
    (emacs-wiki-menu-make-from-list)))

(defun emacs-wiki-menu-make-from-list ()
  "Generate a menu from a list of 3 tuples.
The tuples must be of the form: text, url, tooltip.  The menu
will be read from `emacs-wiki-menu-definition'."
  (when (and (not emacs-wiki-menu-current)
             emacs-wiki-menu-definition)
    (concat
     emacs-wiki-menu-top
     (mapconcat
      (lambda (entry)
        (replace-regexp-in-string
         "%text%" (car entry)
         (replace-regexp-in-string
          "%url%" (nth 1 entry)
          (replace-regexp-in-string
           "%tooltip%" (nth 2 entry)
           emacs-wiki-menu-make-from-list-format))))
      emacs-wiki-menu-definition "\n")
     emacs-wiki-menu-bottom)))

(provide 'emacs-wiki-menu)
;;; emacs-wiki-menu.el ends here
