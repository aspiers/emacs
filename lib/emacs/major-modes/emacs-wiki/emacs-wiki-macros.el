;;; emacs-wiki-menu.el --- Allow the use of macros in emacs-wiki projects

;; Copyright (C) 2004 Mark Tommasi
;; Copyright (C) 2004 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-macros.el
;; Keywords: hypermedia
;; Author: Mark Tommasi (tommasi AT univ-lille3 DOT fr)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Allow use of macros in emacs-wiki projects
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
;; use macros.

;;;_ + Startup

;; Macro functionality is automatically loaded from emacs-wiki.el, so
;; you don't need to do anything special to enable them.  This may
;; possibly change in the future.

;;;_ + Usage

;; You can specify macros with an association list:
;;
;; (setq emacs-wiki-macro-alist
;;       '(("mt"
;;          . "<a
;;             href=\"http://www.grappa.univ-lille3.fr/~tommasi\">Marc
;;             Tommasi</a>")
;;         ("ap"
;;          . "<a
;;             href=\"http://www.cril-univ-artois.fr/~parrain\">Anne
;;             Parrain</a>")))
;;
;; Use the syntax %macro% in project pages to denote a macro.
;;
;; For example, %mt% is expanded into
;;
;; "<a
;; href=\"http://www.grappa.univ-lille3.fr/~tommasi">Marc
;; Tommasi</a>"
;;
;; during the publication process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-macros nil
  "Options controlling the behavior of emacs-wiki macros generation."
  :group 'emacs-wiki)

(defcustom emacs-wiki-macro-alist nil
  "An association name url."
  :type 'alist
  :group 'emacs-wiki-macros)

(defcustom emacs-wiki-macro-regexp "%\\(\\w+\\)%"
  "Macro regexp."
  :type 'regexp
  :group 'emacs-wiki-macros)

(defun emacs-wiki-macro-expand ()
  (save-excursion
    (save-match-data
      (cdr (assoc (match-string 1)
                  emacs-wiki-macro-alist)))))

(provide 'emacs-wiki-macros)
;;; emacs-wiki-macros.el ends here
