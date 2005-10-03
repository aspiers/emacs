;;; emacs-wiki-table.el --- Publish fancy tables

;; Copyright (C) 2001, 2002, 2003 John Wiegley

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-table.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT member DOT fsf DOT org)
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

;; To get fancy table markup, add
;;
;;    (require 'emacs-wiki-table)
;;
;; to your .emacs. In your planner source files, you can now
;; make tables that look like this:
;;
;; +------------------------------------------------------------------+
;; |                    A table header                                |
;; +-------------------------------+----------------------------------+
;; |           Column 1            |           Column 2               |
;; +-------------------------------+----------------------------------+
;; |Some text here                 |More text here, even wrapping to  |
;; |                               |the next line                     |
;; +-------------------------------+----------------------------------+
;; |Some text here                 |More text here, even wrapping to  |
;; |                               |the next line                     |
;; +-------------------------------+----------------------------------+
;;
;; See table.el for more information.

(require 'table)
(require 'emacs-wiki)

;;; Code:

(defun emacs-wiki-table-markup-fancy-table ()
  "Mark up tables using the `table' package."
  (let ((leader (match-string 1))
        (begin (copy-marker (match-beginning 0)))
        table end)
    (goto-char (match-end 0))
    (setq table
          (with-current-buffer (table-generate-source 'html)
            (prog1
                (buffer-string)
              (kill-buffer (current-buffer)))))
    (goto-char begin)
    (if (re-search-backward (concat "<p>["
                                    emacs-wiki-regexp-space
                                    "]+") nil t)
        (replace-match (if (>= (string-width leader) 6)
                           (if emacs-wiki-xhtml-inline-css
                               "<span style=\"text-align:center;\">\n"
                             "<center>\n")
                         (if (> (length leader) 0)
                             "<blockquote>\n"
                           ""))))
    (delete-region begin (re-search-forward "-+\\+\\s-*[\r\n]+\\s-*$"
                                            nil t))
    (insert table)
    (setq end (point-marker))
    (goto-char begin)
    (while (< (point) end)
      (if (looking-at "^\\s-+")
          (replace-match ""))
      (forward-line))
    (goto-char end)
    (if (re-search-forward (concat "["
                                   emacs-wiki-regexp-space
                                   "]+</p>") nil t)
        (replace-match (if (>= (string-width leader) 6)
                           (if emacs-wiki-xhtml-inline-css
                               "\n</span>"
                             "\n</center>")
                         (if (> (length leader) 0)
                             "\n</blockquote>"
                           ""))))
    (set-match-data (list begin begin begin begin))
    nil))

(add-hook 'emacs-wiki-mode-hook 'table-recognize)

(add-to-list 'emacs-wiki-publishing-markup
   `[,(concat "^\\(\\s-*\\)\\(\\+[-+]+\\+["
              emacs-wiki-regexp-space
              "]+|\\)")
     1 emacs-wiki-table-markup-fancy-table])

(provide 'emacs-wiki-table)

;;; emacs-wiki-table.el ends here
