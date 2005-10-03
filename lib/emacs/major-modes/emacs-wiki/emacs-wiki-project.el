;;; emacs-wiki-project.el --- Provide multi-project support for emacs-wiki.

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley
;; Copyright (C) 2004 Sacha Chua
;; Copyright (C) 2004 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-project.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Provide multiple project support for emacs-wiki.el
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
;; multiple project support.

;;;_ + Startup

;; This file is loaded automatically by emacs-wiki.el .  You should
;; not need to manually load it at this time.

;;;_ + Usage

;;;_ + Contributors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for multile Emacs Wiki projects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki)
(require 'emacs-wiki-publish)
(require 'emacs-wiki-regexps)

(defgroup emacs-wiki-project nil
  "Options controlling multi-project behavior in emacs-wiki."
  :group 'emacs-wiki)

(defcustom emacs-wiki-default-project "DefaultProject"
  "Project to use by default when switching projects.
This is used by \\[emacs-wiki-change-project] to determine a
default project to switch to."
  :type 'string
  :group 'emacs-wiki-project)

(defvar emacs-wiki-current-project nil)
(defvar emacs-wiki-predicate nil)
(defvar emacs-wiki-major-mode nil)
(defvar emacs-wiki-project-server-prefix nil)

(defvar emacs-wiki-current-file nil
  "File currently being published.
This is usually set by code called by `emacs-wiki-publish-current'.
It should never be changed globally.")

(defcustom emacs-wiki-show-project-name-p t
  "When true, display the current project name in the mode-line"
  :type 'boolean
  :group 'emacs-wiki-project)

(defun emacs-wiki-update-project-interwikis ()
  (let ((projs emacs-wiki-projects))
    (while projs
      (add-to-list
       'emacs-wiki-interwiki-names
       `(,(caar projs)
         . (lambda (tag)
             (emacs-wiki-project-interwiki-link ,(caar projs) tag))))
      (setq projs (cdr projs)))))

(defcustom emacs-wiki-update-project-hook
  '(emacs-wiki-update-project-interwikis)
  "A hook called whenever `emacs-wiki-projects' is modified.
By default, this hook is used to update the Interwiki table so
that it contains links to each project name."
  :type 'hook
  :group 'emacs-wiki-project)

(defcustom emacs-wiki-projects nil
  "A list of project-specific Emacs-Wiki variable settings.
Each entry is a cons cell, of the form (PROJECT VARS).  Projects
are useful for maintaining separate wikis that vary in some way.

You can change between projects with
\\[emacs-wiki-change-project], by default bound to C-c C-v.  When
you use \\[emacs-wiki-find-file] to find a new file, emacs-wiki
will attempt to detect which project it is part of by finding the
first project where emacs-wiki-directories contains that file.

VARS is an alist of symbol to value mappings, to be used locally
in all emacs-wiki buffers associated with that PROJECT.

You may also set the variable `emacs-wiki-predicate' in this
alist, which should be a function to determine whether or not the
project pertains to a certain buffer.  It will be called within
the buffer in question.  The default predicate checks whether the
file exists within `emacs-wiki-directories' for that project.

The variable `emacs-wiki-major-mode' can be used to determine the
major mode for a specific emacs-wiki buffer, in case you have
developed a customized major-mode derived from `emacs-wiki-mode'.

The variable `emacs-wiki-project-server-prefix' is prepended to
the Interwiki URL, whenever an Interwiki reference to another
project is made.  For example, if you had two projects, A and B,
and in A you made a reference to B by typing B#WikiPage, A needs
to know what directory or server to prepend to the WikiPage.html
href.  If this variable is not set, it is assumed that both A and
B publish to the same location.

If any variable is not customized specifically for a project, the
global value is used."
  :type '(alist
          :key-type (string :tag "Project name")
          :value-type (alist
                       :key-type (symbol :tag "Variable")
                       :value-type (sexp :tag "Value")))
  :set (function
        (lambda (sym val)
          (set sym val)
          (run-hooks 'emacs-wiki-update-project-hook)))
  :group 'emacs-wiki-project)

(defmacro with-emacs-wiki-project (project &rest body)
  "Evaluate as part of PROJECT the given BODY forms."
  `(emacs-wiki-with-temp-buffer
     (emacs-wiki-change-project ,project)
     ,@body))

(put 'with-emacs-wiki-project 'lisp-indent-function 1)
(put 'with-emacs-wiki-project 'edebug-form-spec '(body))

(defun emacs-wiki-change-project (project)
  "Change wiki projects.

When called interactively, load the welcome page of the selected
project in a new buffer. If no project is selected, the default
project as specified in `emacs-wiki-default-project' will be
used.

Note that if the welcome page does not exist for the target
project, it is not clear which of wiki directory should be used
if there are multiple directories defined for that project.

When called from a lisp program, update the current buffer's
project to PROJECT."
  (interactive (list (completing-read "Switch to project: "
                                      emacs-wiki-projects
                                      nil t nil)))
  (when (string= "" project)
    (setq project emacs-wiki-default-project))
  (let ((projsyms (cdr (assoc project emacs-wiki-projects)))
        sym)
    (while projsyms
      (setq sym (caar projsyms))
      (unless (memq sym '(emacs-wiki-predicate emacs-wiki-major-mode))
        (let ((custom-set (or (get sym 'custom-set) 'set))
              (var (if (eq (get sym 'custom-type) 'hook)
                       (make-local-hook sym)
                     (make-local-variable sym))))
          (if custom-set
              (funcall custom-set var (cdar projsyms)))))
      (setq projsyms (cdr projsyms))))

  (let ((current emacs-wiki-current-project))
    (if (and (interactive-p)
             (not (string= current project)))
        ;; when changing projects interactively, jump to the welcome
        ;; page of the new project, and don't clobber the existing
        ;; buffer
        (with-emacs-wiki-project project
          (emacs-wiki-find-file emacs-wiki-default-page))
      ;; change to the new project, and update modeline if appropriate
      (set (make-local-variable 'emacs-wiki-current-project) project)
      (when emacs-wiki-show-project-name-p
        (setq mode-name (concat "Wiki[" project "]"))))))

(defun emacs-wiki-relative-link-maybe (dest src)
  "Return the relative link for DEST based on SRC."
  (when (and dest src)
    (let ((dest-host
           (and (string-match emacs-wiki-url-server-regexp dest)
                (match-string 3 dest)))
          (src-host
           (and (string-match emacs-wiki-url-server-regexp src)
                (match-string 3 src))))
      (and dest-host src-host (string= dest-host src-host)
           (file-relative-name dest src)))))

(defun emacs-wiki-project-interwiki-link (project tag)
  "Return the interwiki link for TAG based on PROJECT."
  (with-emacs-wiki-project project
    (if emacs-wiki-publishing-p
        (let* ((tag (or tag emacs-wiki-default-page))
               (page-publishing-directory
                (file-name-directory
                 (concat emacs-wiki-project-server-prefix
                         (emacs-wiki-link-url
                          (emacs-wiki-page-name tag)))))
               (url (emacs-wiki-link-url tag)))
          (cond
           ;; bad link, no prefix will be added
           ((null url) "")
           ;; relative link
           ((string-match "\\`\\.\\./" url) url)
           ;; try and convert to a relative link
           ((and emacs-wiki-relative-links
                 ;; without catching extended links by mistake
                 (not (string-match
                       emacs-wiki-extended-link-regexp
                       url))
                 (emacs-wiki-relative-link-maybe
                  (concat emacs-wiki-project-server-prefix url)
                  page-publishing-directory)))
           ;; use the server prefix
           ((concat emacs-wiki-project-server-prefix url))))
      (or (emacs-wiki-page-file (or tag emacs-wiki-default-page))
          ;; doesn't yet exist, so we don't qualify the name, causing
          ;; it to be rendered as a bad link
          tag))))

(defun emacs-wiki-project-homepage (&optional project)
  "Return the home page of PROJECT.

If nil is used for PROJECT, use the current project to determine
the home page."
  (emacs-wiki-project-interwiki-link project nil))

(provide 'emacs-wiki-project)
;;; emacs-wiki-project.el ends here
