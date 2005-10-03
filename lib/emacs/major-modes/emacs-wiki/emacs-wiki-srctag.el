;;; emacs-wiki-srctag.el --- Make emacs-wiki publish fontified example codes.

;; Copyright (C) 2004, 2005 Sun Jiyang

;; Emacs Lisp Archive Entry
;; Filename:      emacs-wiki-srctag.el
;; Version:       1.0
;; Date:          2004-07-28
;; Keywords:      wiki, extension
;; Author:        Sun Jiyang (sunyijiang AT gmail DOT com)
;; Description:   Add `<src>' tag to emacs-wiki to make fontified codes.
;; URL:           http://emacs.mysmth.org
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is NOT part of GNU Emacs.

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

;; When we use <example> tags in emacs-wiki, the codes inside the tags turn
;; out plain text with no syntax highlighting as they are in Emacs. this
;; package introduces a new tag <src> into emacs-wiki. With this tag you can
;; get your codes fontified with different major-modes when wiki is published.

;; This package is based on emacs-wiki.el and htmlize.el, you need to have
;; these two packages installed on your Emacs.

;;; Usage:

;; (require 'emacs-wiki-srctag)
;; ;;example: add perl support
;; (add-to-list 'emacs-wiki-src-tag-modes-alist
;;              '("perl" . perl-mode))

;; Then in your wiki files, you can use the <src type="what"> tag:

;; <src type="c">
;; main ()
;; {
;;    printf ("hello world!");
;; }
;; </src>

;; <src type="perl">
;; foreach (@CodeList)
;; {print "<a target=\"code\" href=$_".".html".">$_</a><br>\n";}
;; </src>

;; Code's type and the major-mode used to fontify it are associated by
;; variable `emacs-wiki-src-tag-modes-alist'. You can add your types as the
;; example shows above. By default, it supports c, c++, elisp and verilog
;; codes. That means you can use <src type="c/cpp/elisp/vlog"> by default.

;; When published, codes are highlighted using CSS styles, so you have to add
;; styles into your css file to view your colored codes. Also you need to
;; check the variable `emacs-wiki-style-sheet' first. I prefer to use an
;; external css file, and here is my setting:

;; (setq emacs-wiki-style-sheet
;;       "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">")

;; See the "Source tag" section in the `examples/default.css' file for
;; some reasonable defaults.

;; You can replace them with your favorite colors. If you find some part of
;; codes are not fontified when you view the html output, it's most likely
;; that you do not have the corresponding css style defined in your css
;; file. View the html file to find out them and have them defined in your css
;; file, just like the example shows above.

;;; Contributors

;; Trent Buck (fubarbaz AT bigpond DOT com) provided a small patch to
;; make <pre> tags use the "source" class, which allows for easier CSS
;; customization.  He also got rid of leading newlines between pre tags.

;;; Code:
(require 'htmlize)
(require 'emacs-wiki)

(defvar emacs-wiki-src-tag-modes-alist
  '(("c"     . c-mode)
    ("cpp"   . c++-mode)
    ("elisp" . emacs-lisp-mode)
    ("vlog"  . verilog-mode))
  "types <==> modes")

(add-to-list 'emacs-wiki-markup-tags
             '("src" t   t   t   emacs-wiki-src-tag))

(defun emacs-wiki-src-tag (beg-arg end-arg attrs highlight-p)
  (if highlight-p
      (progn
        (emacs-wiki-multiline-maybe beg-arg end-arg)
        (goto-char end-arg))
    (let* ((mode (or (cdr (assoc "type" attrs)) "nil"))
           (mode-func (or (cdr (assoc mode emacs-wiki-src-tag-modes-alist))
                          'fundamental-mode))
           (tbuf (get-buffer-create "*ewiki-temp*"))
           (rbuf (get-buffer-create "*ewiki-html*"))
           (beg  (min beg-arg end-arg))
           (end  (max beg-arg end-arg)))
      (copy-to-register 16 beg end)
      (kill-region beg end)
      (goto-char beg)
      (with-current-buffer rbuf
        (erase-buffer))
      (with-current-buffer tbuf
        (insert-register 16)
        (funcall mode-func)
        (font-lock-mode 1)
        (emacs-wiki-htmlize-buffer rbuf)
        (kill-buffer tbuf))
      (insert-buffer rbuf)
      (kill-buffer rbuf)
      (search-forward "</pre>")
      (add-text-properties beg (point) '(rear-nonsticky (read-only)
                                                        read-only t)))))

;; this is from htmlize.el, modified a little.
(defun emacs-wiki-htmlize-buffer (obuf)
  (save-excursion
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (let* ((buffer-faces (htmlize-faces-in-buffer))
           (face-map     (htmlize-make-face-map
                          (adjoin 'default buffer-faces)))
           (htmlbuf      obuf)
           (name         (or emacs-wiki-current-file (buffer-file-name)))
           (title        (if name
                             (file-name-nondirectory name)
                           (buffer-name))))
      (with-current-buffer htmlbuf
        (buffer-disable-undo))
      (let (next-change text face-list fstruct-list)
        (goto-char (point-min))
        (while (not (eobp))
          (setq next-change (htmlize-next-change (point) 'face))
          (setq face-list (htmlize-faces-at-point)
                fstruct-list (delq nil (mapcar (lambda (f)
                                                 (gethash f face-map))
                                               face-list)))
          (setq text (htmlize-buffer-substring-no-invisible
                      (point) next-change))
          (setq text (htmlize-untabify text (current-column)))
          (setq text (htmlize-protect-string text))
          (when (> (length text) 0)
            (htmlize-css-insert-text text fstruct-list htmlbuf))
          (goto-char next-change)))
      (with-current-buffer htmlbuf
        (insert "</pre>\n")
        (when htmlize-generate-hyperlinks
          (htmlize-make-hyperlinks))
        (goto-char (point-min))
        (insert "<pre class=\"source\">")
        (while (equal (char-to-string (char-after)) "\n")
          (delete-char 1))              ; delete leading newline(s).
        (when htmlize-html-major-mode
          (funcall htmlize-html-major-mode))
        (run-hooks 'htmlize-after-hook)
        (buffer-enable-undo))
      htmlbuf)))

(provide 'emacs-wiki-srctag)

;;; emacs-wiki-srctag.el ends here
