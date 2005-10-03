;;; emacs-wiki-regexps.el --- Define regexps used by emacs-wiki.

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley
;; Copyright (C) 2003, 2004 Damien Elmes
;; Copyright (C) 2004 Mark Triggs
;; Copyright (C) 2004 Sacha Chua
;; Copyright (C) 2004 Gary Vaughan
;; Copyright (C) 2004, 2005 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-regexps.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Define regexps used by emacs-wiki
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

;; This file is the part of the Emacs Wiki project that describes
;; regexps that are used throughout the project.

;;;_ + Startup

;; This file is loaded automatically by emacs-wiki.el .  You should
;; not need to manually load it at this time.

;;;_ + Usage

;;;_ + Contributors

;; Ephrem Christopher Walborn (christopherw AT BonitaBayGroup DOT com)
;; helped to make much of the generated HTML code be XHTML 1.0
;; Transitional compliant.
;;
;; Gary Vaughan (gary AT gnu DOT org) contributed a number of
;; enhancements.  Multi-project support in particular was extensively
;; hacked on by him.
;;
;; John Sullivan (john AT wjsullivan DOT net) contributed a patch that
;; fixes a problem with colorizing URI's that have spaces in them.
;;
;; Yohanes Santoso (ysantoso AT dessyku DOT is-a-geek DOT org)
;; contributed the `emacs-wiki-full-path-ignore-regexp' option.  It is
;; useful for VC directories like CVS or {arch} or .arch-ids, and for
;; various files that happen to be in the wiki directory but are not
;; meant to be published.

(require 'emacs-wiki)

(defun emacs-wiki-extreg-usable-p ()
  "Return non-nil if extended character classes can be used,
nil otherwise.

This is only used when deciding the initial values of the
emacs-wiki-regexp options."
  (save-match-data
    (string-match "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"
                  emacs-version)
    (cond ((featurep 'xemacs) nil)             ; unusable on XEmacs
          ((> emacs-major-version 21) t)       ; usable if > 21
          ((< emacs-major-version 21) nil)
          ((< emacs-minor-version 3) nil)
          ;; don't use if version is of format 21.x
          ((null (match-string 1 emacs-version)) nil)
          ;; only trust 21.3.50 or higher
          ((>= (string-to-number (match-string 1 emacs-version)) 50) t)
          (t nil))))

(defgroup emacs-wiki-regexp nil
  "Options relating to regular expressions as used in publishing
and syntax highlighting."
  :group 'emacs-wiki)

(defvar emacs-wiki-url-or-name-regexp nil
  "Matches either a Wiki link or a URL.
This variable is auto-generated.")

(defvar emacs-wiki-url-or-name-regexp-group-count nil
  "Matches either a Wiki link or a URL.
This variable is auto-generated.")

(defcustom emacs-wiki-extended-link-regexp
  "\\[\\[\\([^][\t\n]+\\)\\]\\(\\[\\([^][\n]+\\)\\]\\)?\\]"
  "Regexp used to match [[extended][links]]."
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-image-regexp
  "\\.\\(eps\\|gif\\|jp\\(e?g\\)\\|p\\(bm\\|ng\\)\\|tiff\\|x\\([bp]m\\)\\)\\'"
  "A link matching this regexp will be published inline as an image.
Remember that it must be matched as a link first - so use either
\[[CamelCaps]] or include a leading slash - [[./text]].

An example:

  [[./wife.jpg][A picture of my wife]]

If you omit the description, the alt tag of the resulting HTML buffer will be
the name of the file."
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-file-regexp
  "[/?]\\|\\.\\(html?\\|pdf\\|el\\|zip\\|txt\\|tar\\)\\(\\.\\(gz\\|bz2\\)\\)?\\'"
  "A link matching this regexp will be regarded as a link to a file.
Remember that it must be matched as a link first - so use either
\[[CamelCaps]] or include a leading slash - [[./text]]"
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-file-ignore-regexp
  "\\`\\(\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\'"
  "A regexp matching files to be ignored in Wiki directories."
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-full-path-ignore-regexp
  "/?\\(?:[{}.]arch\\(?:[{}]\\|-ids\\)\\)\\|CVS/?"
  "A regexp matching the file's full path to be ignored in Wiki
directories.

Contrast to `emacs-wiki-file-ignore-regexp' which compares only
the files' basenames"
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-ignored-extensions-regexp
  "\\.\\(bz2\\|gz\\|[Zz]\\)\\'"
  "A regexp of extensions to omit from the ending of Wiki page name."
  :type 'string
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-regexp-blank
  (if (emacs-wiki-extreg-usable-p)
      "[:blank:]"
    " \t")
  "Regexp to use in place of \"[:blank:]\".
This should be something that matches spaces and tabs.

It is like a regexp, but should be embeddable inside brackets.
emacs-wiki will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:blank:]" " \t")
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-regexp-space
  (if (emacs-wiki-extreg-usable-p)
      "[:space:]"
    " \t\n")
  "Regexp to use in place of \"[:space:]\".
This should be something that matches spaces, tabs, and newlines.

It is like a regexp, but should be embeddable inside brackets.
emacs-wiki will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:space:]" " \t\n")
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-regexp-alnum
  (if (emacs-wiki-extreg-usable-p)
      "[:alnum:]"
    "A-Za-z0-9")
  "Regexp to use in place of \"[:alnum:]\".
This should be something that matches all letters and numbers.

It is like a regexp, but should be embeddable inside brackets.
emacs-wiki will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:alnum:]" "A-Za-z0-9")
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-regexp-lower
  (if (emacs-wiki-extreg-usable-p)
      "[:lower:]"
    "a-z")
  "Regexp to use in place of \"[:lower:]\".
This should match all lowercase characters.

It is like a regexp, but should be embeddable inside brackets.
emacs-wiki will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:lower:]" "a-z")
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-regexp-upper
  (if (emacs-wiki-extreg-usable-p)
      "[:upper:]"
    "A-Z")
  "Regexp to use in place of \"[:upper:]\".
This should match all uppercase characters.

It is like a regexp, but should be embeddable inside brackets.
emacs-wiki will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:upper:]" "A-Z")
  :group 'emacs-wiki-regexp)

(defun emacs-wiki-set-sym-and-url-regexp (sym value)
  (when (eq sym 'emacs-wiki-url-protocols)
    (setq emacs-wiki-url-protocols value)
    (setq emacs-wiki-url-regexp
          (concat "\\<\\("
                  (mapconcat 'car emacs-wiki-url-protocols "\\|")
                  "\\):"
                  "[^]["
                  emacs-wiki-regexp-space
                  "\"'()<>^`{}]*[^]["
                  emacs-wiki-regexp-space
                  "\"'()<>^`{}.,;]+")
          emacs-wiki-url-server-regexp
          (concat "\\<\\("
                  (mapconcat 'car emacs-wiki-url-protocols "\\|")
                  "\\):"
                  "\\([^:@]+@[^:]+\\)?"
                  "\\([^]["
                  emacs-wiki-regexp-space
                  "\"'()<>^`{},;/]+\\)"
                  "\\(/.*\\|$\\)")))
  (setq emacs-wiki-url-or-name-regexp
        (concat "\\("
                (if (eq sym 'emacs-wiki-name-regexp)
                    value
                  emacs-wiki-name-regexp) "\\|"
                  (if (eq sym 'emacs-wiki-name-regexp)
                      (if (boundp 'emacs-wiki-url-regexp)
                          emacs-wiki-url-regexp
                        "")
                    emacs-wiki-url-regexp) "\\)")
        emacs-wiki-url-or-name-regexp-group-count
        (- (emacs-wiki-count-chars
            emacs-wiki-url-or-name-regexp ?\() 2))
  (set sym value))

(defcustom emacs-wiki-name-regexp
  (concat emacs-wiki-extended-link-regexp "\\|"
          "\\<[" emacs-wiki-regexp-upper "][" emacs-wiki-regexp-lower
          "]+\\([" emacs-wiki-regexp-upper "]["
          emacs-wiki-regexp-lower "]+\\)+\\(#["
          emacs-wiki-regexp-alnum "]+\\)?")
  "Regexp used to match WikiNames."
  :type 'regexp
  :set 'emacs-wiki-set-sym-and-url-regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-tag-regexp
  (concat "<\\([^/"
          emacs-wiki-regexp-space
          "][^"
          emacs-wiki-regexp-space
          "</>]*\\)\\(\\s-+[^<>]+[^</>]\\)?\\(/\\)?>")
  "A regexp used to find XML-style tags within a buffer when publishing.
Group 1 should be the tag name, group 2 the properties, and group
3 the optional immediate ending slash."
  :type 'regexp
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-exclude-backlink-parent-regexp
  "\\`\\'"                              ; null match
;;  "^[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]$"
  "A regexp of buffer names used to exclude their children from
receiving backlinks."
  :type 'string
  :group 'emacs-wiki-regexp)

(defcustom emacs-wiki-exclude-backlink-regexp
  "^[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]$"
  "A regexp of buffer names used to exclude them from receiving backlinks."
  :type 'string
  :group 'emacs-wiki-regexp)

(provide 'emacs-wiki-regexps)
;;; emacs-wiki-regexps.el ends here
