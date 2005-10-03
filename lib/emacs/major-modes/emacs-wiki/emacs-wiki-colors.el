;;; emacs-wiki-colors.el --- Routines for emacs-wiki that work with
;;; faces and colors.

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley
;; Copyright (C) 2004 Sacha Chua
;; Copyright (C) 2004, 2005 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-colors.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Provide face support for emacs-wiki.el
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
;; faces and link coloring, and other visual features.

;;;_ + Startup

;; This file is loaded automatically by emacs-wiki.el .  You should
;; not need to manually load it at this time.

;;;_ + Usage

;;;_ + Contributors

;; Lan Yufeng (nlany DOT web AT gmail DOT com) found an error where
;; headings were being given the wrong face, contributing a patch to
;; fix this.
;;
;; Anselm Levskaya (levskaya AT gmail DOT com) supplied the initial
;; version of the `emacs-wiki-make-file-glyph' function, which allows
;; XEmacs users to have inlined images.
;;
;; Jim Ottaway (j DOT ottaway AT lse DOT ac DOT uk) contributed a
;; patch (11 lines changed) that makes invisible text stay invisible
;; when using outline-minor-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Colors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki)
(require 'emacs-wiki-regexps)
(require 'emacs-wiki-project)
(require 'font-lock)

(defgroup emacs-wiki-highlight nil
  "Options controlling the behavior of Emacs Wiki highlighting.
See `emacs-wiki-highlight-buffer' for more information."
  :group 'emacs-wiki)

(defcustom emacs-wiki-autogen-headings t
  "Specify whether the heading faces should be auto-generated.
The default is to scale them.  Choosing 'outline will copy the
colors from the outline-mode headings.  If you want to customize
each of the headings individually, set this to nil."
  :type '(choice (const :tag "Default (scaled) headings" t)
                 (const :tag "Use outline-mode headings" outline)
                 (const :tag "Don't touch the headings" nil))
  :group 'emacs-wiki-highlight)

(defvar emacs-wiki-outline-faces-list
  (if (facep 'outline-1)
      '(outline-1 outline-2 outline-3 outline-4
                  outline-5 outline-6)
    ;; These are supposed to be equivalent in coloring
    '(font-lock-function-name-face
      font-lock-variable-name-face
      font-lock-keyword-face
      font-lock-builtin-face
      font-lock-comment-face
      font-lock-constant-face))
  "Outline faces to use when assigning emacs-wiki header faces.")

(defun emacs-wiki-make-faces ()
  (let ((faces '(1 2 3 4 5)) num newsym)
    (while faces
      (setq num (car faces))
      (setq newsym (intern (concat "emacs-wiki-header-"
                                   (int-to-string num))))
      (cond
       ((null emacs-wiki-autogen-headings)
        (make-empty-face newsym))
       ((featurep 'xemacs)
        (if (eq emacs-wiki-autogen-headings 'outline)
            (copy-face (nth (1- (car faces))
                            emacs-wiki-outline-faces-list)
                       newsym)
          (eval `(defface ,newsym
                   '((t (:size
                         ,(nth (1- num)
                               '("24pt" "18pt" "14pt" "12pt" "11pt"))
                         :bold t)))
                   "emacs-wiki header face"
                   :group 'emacs-wiki-highlight))))
       ((< emacs-major-version 21)
        (if (eq emacs-wiki-autogen-headings 'outline)
            (copy-face (nth (1- (car faces))
                            emacs-wiki-outline-faces-list)
                       newsym)
          (copy-face 'default newsym)))
       ((eq emacs-wiki-autogen-headings 'outline)
        (eval `(defface ,newsym
                 '((t (:inherit
                       ,(nth (1- (car faces))
                             emacs-wiki-outline-faces-list))))
                 "emacs-wiki header face"
                 :group 'emacs-wiki-highlight)))
       (t
        (eval `(defface ,newsym
                 '((t (:height ,(1+ (* 0.1 (- 4 num)))
                               :inherit variable-pitch
                               :weight bold)))
                 "emacs-wiki header face"
                 :group 'emacs-wiki-highlight))))
      (setq faces (cdr faces)))))

(defface emacs-wiki-link-face
  '((((class color) (background light))
     (:foreground "blue" :underline "blue" :bold t))
    (((class color) (background dark))
     (:foreground "cyan" :underline "cyan" :bold t))
    (t (:bold t)))
  "Face for Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defface emacs-wiki-bad-link-face
  '((((class color) (background light))
     (:foreground "red" :underline "red" :bold t))
    (((class color) (background dark))
     (:foreground "coral" :underline "coral" :bold t))
    (t (:bold t)))
  "Face for bad Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defface emacs-wiki-verbatim-face
  '((((class color) (background light))
     (:foreground "slate gray"))
    (((class color) (background dark))
     (:foreground "gray")))
  "Face for verbatim text."
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-highlight-buffer-hook nil
  "A hook run after a region is highlighted.
Each function receives three arguments: BEG END VERBOSE.

BEG and END mark the range being highlighted, and VERBOSE
specifies whether progress messages should be displayed to the
user."
  :type 'hook
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-before-highlight-buffer-hook nil
  "A hook run before a region is highlighted.
Each function receives three arguments: BEG END VERBOSE.

BEG and END mark the range being highlighted, and VERBOSE specifies
whether progress messages should be displayed to the user."
  :type 'hook
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-inline-images (or (fboundp 'create-image)
                                        (fboundp 'make-glyph))
  "If non-nil, inline locally available images within Wiki pages."
  :type 'boolean
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-inline-relative-to 'emacs-wiki-publishing-directory
  "The name of a symbol which records the location relative to
where images should be found.

The default assumes that when editing, the images can be found in
the publishing directory.  Another sensible default is
`default-directory', which will try and find the images relative
to the local page.

You can use this to store images in wikidir/images, and maintain
a parallel copy on the remote host."
  :type 'symbol
  :group 'emacs-wiki)

(defcustom emacs-wiki-markup-tags
  '(("example" t nil t emacs-wiki-example-tag)
    ("verbatim" t nil t emacs-wiki-verbatim-tag)
    ("nowiki" t nil t emacs-wiki-nowiki-tag)
    ("verse" t nil nil emacs-wiki-verse-tag)
    ("numbered" t nil nil emacs-wiki-numbered-tag)
    ("nop" nil nil t emacs-wiki-nop-tag)
    ("contents" nil t nil emacs-wiki-contents-tag)
    ("include" nil t nil emacs-wiki-include-tag)
    ("c-source" t t t emacs-wiki-c-source-tag)
    ("comment" t nil nil emacs-wiki-comment-tag))
  "A list of tag specifications, for specially marking up Wiki text.
XML-style tags are the best way to add custom markup to Emacs
Wiki.  This is easily accomplished by customizing this list of
markup tags.

For each entry, the name of the tag is given, whether it expects
a closing tag and/or an optional set of attributes, if the
handler function can also highlight the tag, and a function that
performs whatever action is desired within the delimited region.

The tags themselves are deleted during publishing, although not
during highlighting, before the function is called.  The function
is called with three arguments, the beginning and end of the
region surrounded by the tags (including the tags themselves, in
the case of highlighting).  The third argument indicates whether
the purpose of the call is to highlight the region, or mark it up
for publishing.  If properties are allowed, they are passed as a
fourth argument in the form of an alist.  The `end' argument to
the function is always a marker.

Point is always at the beginning of the region within the tags,
when the function is called.  Wherever point is when the function
finishes is where tag markup/highlighting will resume.

These tag rules are processed once at the beginning of markup,
and once at the end, to catch any tags which may have been
inserted in-between.  For highlighting, they are processed as
they occur, in the order they occur, once per text region.

Here is a summary of the default tags.  This includes the
dangerous tags listed in `emacs-wiki-dangerous-tags', which may
not be used by outsiders.

 verbatim

   Protects against highlighting and wiki interpretation, and
   escapes any characters which have special meaning to the
   publishing format. For HTML, this means characters like '<'
   are escaped as HTML entities.

 example

   Like verbatim, but typesets in HTML using the <pre> tag, with
   class=example, so whitespace formatting is preserved.

 nowiki

   Inhibits wiki markup, but does not do any escaping to the
   underlying publishing medium. Useful for embedding HTML, PHP,
   etc.

 verse

   Typesets like a normal paragraph, but without word-wrapping.
   That is, whitespace is preserved.

 redirect

   Using the \"url\" attribute, you can specify that a page
   should redirect to another page.  The remaining contents of
   the page will not be published.  The optional \"delay\"
   attribute specifies how long to wait before redirecting.

 nop

   When placed before a WikiLink, it will prevent that WikiLink
   from being treated as such.  Good for names like DocBook.

 contents

   Produces a compact table of contents for any section heading
   at the same level or lower than the next section header
   encountered.  Optional \"depth\" attribute specifies how deep
   the table of contents should go.

 lisp

   Evaluate the region as a Lisp form, and displays the result.
   When highlighting, the `display' text property is used,
   preserving the underlying text.  Turn off font-lock mode if
   you wish to edit it.

 command

   Pass the region to a command interpretor and insert the
   result, guarding it from any further expansion.  Optional
   \"file\" attribute specifies the shell or interpretor to use.
   If none is given, and `emacs-wiki-command-tag-file' has not
   been configured, Eshell is used.

 python, perl

   Pass the region to the Python or Perl language interpretor,
   and insert the result.

 c-source

   Markup the region as C or C++ source code, using the c2html
   program, if available.  Optional boolean attribute
   \"numbered\" will cause source lines to be numbered.

   Note: If c2html is not available, the region will be converted
   to HTML friendly text (i.e., <> turns into &lt;&gt;), and
   placed in a <pre> block.  In this case, line numbering is not
   available.

 bookmarks

   Insert bookmarks at the location of the tag from the given
   bookmarks file.  Required attribute \"file\" specifies which
   file to read from, and the optional attribute \"type\" may be
   one of: adr (for Opera), lynx, msie, ns, xbel or xmlproc.  The
   default type is \"xbel\".  The optional attribute \"folder\"
   may be used to specify which folder (and its children) should
   be inserted.

   Note that xml-parse.el version 1.5 (available from my website)
   and the xbel-utils package (available at least to Debian
   users) is required for this feature to work."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-dangerous-tags
  '(("redirect" t t nil emacs-wiki-redirect-tag)
    ("lisp" t nil t emacs-wiki-lisp-tag)
    ("include" nil t nil emacs-wiki-include-tag)
    ("command" t t t emacs-wiki-command-tag)
    ("python" t t t emacs-wiki-python-tag)
    ("perl" t t t emacs-wiki-perl-tag)
    ("bookmarks" nil t nil emacs-wiki-bookmarks-tag))
  "A list of tag specifications, for specially marking up Wiki text.
These tags are dangerous -- meaning represent a gaping security hole
-- and therefore are not available to outsiders who happen to edit a
Wiki page."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :group 'emacs-wiki-highlight)

(defvar emacs-wiki-highlight-regexp nil)
(defvar emacs-wiki-highlight-vector nil)

(defun emacs-wiki-configure-highlighting (sym val)
  (setq emacs-wiki-highlight-regexp
        (concat "\\(" (mapconcat (function
                                  (lambda (rule)
                                    (if (symbolp (car rule))
                                        (symbol-value (car rule))
                                      (car rule)))) val "\\|") "\\)")
        emacs-wiki-highlight-vector (make-vector 128 nil))
  (let ((rules val))
    (while rules
      (if (eq (cadr (car rules)) t)
          (let ((i 0) (l 128))
            (while (< i l)
              (unless (aref emacs-wiki-highlight-vector i)
                (aset emacs-wiki-highlight-vector i
                      (nth 2 (car rules))))
              (setq i (1+ i))))
        (aset emacs-wiki-highlight-vector (cadr (car rules))
              (nth 2 (car rules))))
      (setq rules (cdr rules))))
  (set sym val))

(defsubst emacs-wiki-highlight-ok-context-p (beg end str)
  "Ensures whitespace or punctuation comes before the position
BEG, and after the string STR.

A search-forward is done for STR, bounding by END, and the
position of the end of the match is returned if in the correct
context."
  (save-excursion
    (save-match-data
      (let ((len (length str))
            (punctuation "\"(). "))
        (and
         (setq end (search-forward str end t))
         ;; post end, want eob or whitespace/punctuation
         (or (> (skip-syntax-forward punctuation (1+ end)) 0)
             (eq nil (char-after end)))
         (goto-char (- end len))
         ;; pre end, no whitespace
         (eq (skip-syntax-backward " " (- end len 1)) 0)
         (goto-char (+ beg len))
         ;; post beg, no whitespace
         (eq (skip-syntax-forward " " (+ beg len 1)) 0)
         (or (backward-char len) t) ;; doesn't return anything useful
         ;; pre beg, want sob or whitespace/punctuation
         (or (< (skip-syntax-backward punctuation (1- beg)) 0)
             (eq nil (char-before beg)))
         end)))))

(eval-when-compile
  (defvar end))

(defun emacs-wiki-multiline-maybe (beg end &optional predicate)
  "If region between BED and END is a multi-line region, and the
optional PREDICATE is true, font lock the current region as
multi-line.

PREDICATE is called with the excursion saved."
  (when (and (save-excursion
               (goto-char beg)
               (forward-line 1)
               (> end (point)))
             (or (not predicate)
                 (save-excursion (funcall predicate beg end))))
    (add-text-properties beg end '(font-lock-multiline t))
    t))

(defun emacs-wiki-highlight-emphasized ()
  ;; Here we need to check four different points - the start and end
  ;; of the leading *s, and the start and end of the trailing *s.  We
  ;; allow the outsides to be surrounded by whitespace or punctuation,
  ;; but no word characters, and the insides must not be surrounded by
  ;; whitespace or punctuation.  Thus the following are valid:
  ;;
  ;; " *foo bar* "
  ;; "**foo**,"
  ;; and the following is invalid:
  ;; "** testing **"
  (let* ((beg (match-beginning 0))
         (e1 (match-end 0))
         (leader (- e1 beg))
         b2 e2 multiline)
    (unless (eq (get-text-property beg 'invisible) 'emacs-wiki)
      ;; check if it's a header
      (if (eq (char-after e1) ?\ )
          (when (or (= beg (point-min))
                    (eq (char-before beg) ?\n))
            (add-text-properties
             (emacs-wiki-line-beginning-position)
             (emacs-wiki-line-end-position)
             (list 'face (intern (concat "emacs-wiki-header-"
                                         (int-to-string leader))))))
        ;; beginning of line or space or symbol
        (when (or (= beg (point-min))
                  (eq (char-syntax (char-before beg)) ?\ )
                  (memq (char-before beg)
                        '(?\- ?\[ ?\< ?\( ?\' ?\` ?\" ?\n)))
          (save-excursion
            (skip-chars-forward "^*<>\n" end)
            (when (eq (char-after) ?\n)
              (setq multiline t)
              (skip-chars-forward "^*<>" end))
            (setq b2 (point))
            (skip-chars-forward "*" end)
            (setq e2 (point))
            ;; Abort if space exists just before end
            ;; or bad leader
            ;; or no '*' at end
            (unless (or (> leader 5)
                        (not (eq leader (- e2 b2)))
                        (eq (char-syntax (char-before b2)) ?\ )
                        (not (eq (char-after b2) ?*))
                        (eq (char-syntax (char-after (1+ b2))) ?w))
              (add-text-properties beg e1 '(invisible emacs-wiki))
              (add-text-properties
               e1 b2 (list 'face (cond ((= leader 1) 'italic)
                                       ((= leader 2) 'bold)
                                       ((= leader 3) 'bold-italic))))
              (add-text-properties b2 e2 '(invisible emacs-wiki))
              (when multiline
                (add-text-properties
                 beg e2 '(font-lock-multiline t))))))))))

(defun emacs-wiki-highlight-underlined ()
  (let ((start (match-beginning 0))
        multiline)
    (unless (eq (get-text-property start 'invisible) 'emacs-wiki)
      ;; beginning of line or space or symbol
      (when (or (= start (point-min))
                (eq (char-syntax (char-before start)) ?\ )
                (memq (char-before start)
                      '(?\- ?\[ ?\< ?\( ?\' ?\` ?\" ?\n)))
        (save-excursion
          (skip-chars-forward "^_<>\n" end)
          (when (eq (char-after) ?\n)
            (setq multiline t)
            (skip-chars-forward "^_<>" end))
          ;; Abort if space exists just before end
          ;; or no '_' at end
          (unless (or (eq (char-syntax (char-before (point))) ?\ )
                      (not (eq (char-after (point)) ?_))
                      (eq (char-syntax (char-after (1+ (point)))) ?w))
            (add-text-properties start (1+ start) '(invisible emacs-wiki))
            (add-text-properties (1+ start) (point) '(face underline))
            (add-text-properties (point)
                                 (min (1+ (point)) (point-max))
                                 '(invisible emacs-wiki))
            (when multiline
              (add-text-properties
               start (min (1+ (point)) (point-max))
               '(font-lock-multiline t)))))))))

(defun emacs-wiki-highlight-verbatim ()
  (let ((start (match-beginning 0))
        multiline)
    (unless (eq (get-text-property start 'invisible) 'emacs-wiki)
      ;; beginning of line or space or symbol
      (when (or (= start (point-min))
                (eq (char-syntax (char-before start)) ?\ )
                (memq (char-before start)
                      '(?\- ?\[ ?\< ?\( ?\' ?\` ?\" ?\n)))
        (save-excursion
          (skip-chars-forward "^=\n" end)
          (when (eq (char-after) ?\n)
            (setq multiline t)
            (skip-chars-forward "^=" end))
          ;; Abort if space exists just before end
          ;; or no '=' at end
          ;; or word constituent follows
          (unless (or (eq (char-syntax (char-before (point))) ?\ )
                      (not (eq (char-after (point)) ?=))
                      (eq (char-syntax (char-after (1+ (point)))) ?w))
            (add-text-properties start (1+ start) '(invisible emacs-wiki))
            (add-text-properties (1+ start) (point)
                                 '(face emacs-wiki-verbatim-face))
            (add-text-properties (point)
                                 (min (1+ (point)) (point-max))
                                 '(invisible emacs-wiki))
            (when multiline
              (add-text-properties
               start (min (1+ (point)) (point-max))
               '(font-lock-multiline t)))))))))

(defcustom emacs-wiki-highlight-markup
  `(;; make emphasized text appear emphasized
    ("\\*\\{1,5\\}"
     ?* emacs-wiki-highlight-emphasized)

    ;; make underlined text appear underlined
    (,(concat "_[^"
              emacs-wiki-regexp-blank
              "_]")
     ?_ emacs-wiki-highlight-underlined)

    ;; make quadruple quotes invisible
    ("''''" ?\'
     ,(function
       (lambda ()
         (add-text-properties (match-beginning 0) (match-end 0)
                              '(invisible emacs-wiki intangible t)))))

    ("^#title " ?\# emacs-wiki-highlight-title)

    (emacs-wiki-extended-link-regexp ?\[ emacs-wiki-highlight-extended-link)

    ;; render in teletype and suppress further parsing
    (,(concat "=[^"
              emacs-wiki-regexp-blank
              "=]")
     ?= emacs-wiki-highlight-verbatim)

    ;; highlight any markup tags encountered
    (emacs-wiki-tag-regexp ?\< emacs-wiki-highlight-custom-tags)

    (emacs-wiki-name-regexp t emacs-wiki-highlight-wikilink))
  "Expressions to highlight an Emacs Wiki buffer.
These are arranged in a rather special fashion, so as to be as
quick as possible.

Each element of the list is itself a list, of the form:

  (LOCATE-REGEXP TEST-CHAR MATCH-FUNCTION)

LOCATE-REGEXP is a partial regexp, and should be the smallest
possible regexp to differentiate this rule from other rules.  It
may also be a symbol containing such a regexp.  The buffer region
is scanned only once, and LOCATE-REGEXP indicates where the
scanner should stop to look for highlighting possibilities.

TEST-CHAR is a char or t.  The character should match the
beginning text matched by LOCATE-REGEXP.  These chars are used to
build a vector for fast MATCH-FUNCTION calling.

MATCH-FUNCTION is the function called when a region has been
identified.  It is responsible for adding the appropriate text
properties to change the appearance of the buffer.

This markup is used to modify the appearance of the original text
to make it look more like the published HTML would look (like
making some markup text invisible, inlining images, etc).

font-lock is used to apply the markup rules, so that they can
happen on a deferred basis.  They are not always accurate, but
you can use \\[font-lock-fontifty-block] near the point of error
to force fontification in that area.

Lastly, none of the regexp should contain grouping elements that
will affect the match data results."
  :type '(repeat
          (list :tag "Highlight rule"
                (choice (regexp :tag "Locate regexp")
                        (symbol :tag "Regexp symbol"))
                (choice (character :tag "Confirm character")
                        (const :tag "Default rule" t))
                function))
  :set 'emacs-wiki-configure-highlighting
  :group 'emacs-wiki-highlight)

;; XEmacs users don't have `font-lock-multiline'.
(unless (boundp 'font-lock-multiline)
  (defvar font-lock-multiline nil))

;; XEmacs does not have this function, so we need to define it.
;; Ripped from subr.el in Emacs CVS.
(defun emacs-wiki-add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (if (fboundp 'add-to-invisibility-spec)
      (add-to-invisibility-spec arg)
    (if (eq buffer-invisibility-spec t)
        (setq buffer-invisibility-spec (list t)))
    (setq buffer-invisibility-spec
          (cons arg buffer-invisibility-spec))))

(defun emacs-wiki-use-font-lock ()
  (emacs-wiki-add-to-invisibility-spec 'emacs-wiki)
  (set (make-local-variable 'font-lock-multiline) 'undecided)
  (set (make-local-variable 'font-lock-defaults)
       `(nil t nil nil 'beginning-of-line
         (font-lock-fontify-region-function
          . emacs-wiki-highlight-region)
         (font-lock-unfontify-region-function
          . emacs-wiki-unhighlight-region)))
  (set (make-local-variable 'font-lock-fontify-region-function)
       'emacs-wiki-highlight-region)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'emacs-wiki-unhighlight-region)
  (emacs-wiki-make-faces)
  (font-lock-mode t))

(defun emacs-wiki-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (condition-case err
        (let ((object (eval (read form))))
          (cond
           ((stringp object) object)
           ((and (listp object)
                 (not (eq object nil)))
            (let ((string (pp-to-string object)))
              (substring string 0 (1- (length string)))))
           ((numberp object)
            (number-to-string object))
           ((eq object nil) "")
           (t (pp-to-string object))))
      (error
       (if (fboundp 'display-warning)
           (display-warning 'emacs-wiki
                            (format "%s/%s: Error evaluating %s: %s"
                                    emacs-wiki-current-project
                                    (emacs-wiki-page-name)
                                    form
                                    err)
                            (if (featurep 'xemacs)
                                'warning
                              :warning))
         (message "%s/%s: Error evaluating %s: %s"
                  emacs-wiki-current-project
                  (emacs-wiki-page-name)
                  form
                  err))
       "<!--INVALID LISP CODE-->"))))

(defun emacs-wiki-highlight-custom-tags ()
  ;; Remove the match-data related to the url-or-name-regexp, which is
  ;; part of emacs-wiki-highlight-regexp.  All in the name of speed.
  (let ((end (match-end 0)))
    (goto-char (match-beginning 0))
    (looking-at emacs-wiki-tag-regexp)
    (emacs-wiki-custom-tags t)
    (unless (> (point) end)
      (goto-char end))))

(defun emacs-wiki-highlight-buffer ()
  "Re-highlight the entire Wiki buffer."
  (interactive)
  (emacs-wiki-highlight-region (point-min) (point-max) t))

(defun emacs-wiki-highlight-region (beg end &optional verbose)
  "Apply highlighting according to `emacs-wiki-highlight-markup'.
Note that this function should NOT change the buffer, nor should
any of the functions listed in `emacs-wiki-highlight-markup'."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (save-excursion
          (save-restriction
            (widen)
            ;; check to see if we should expand the beg/end area for
            ;; proper multiline matches
            (when (and font-lock-multiline
                       (> beg (point-min))
                       (get-text-property (1- beg) 'font-lock-multiline))
              ;; We are just after or in a multiline match.
              (setq beg (or (previous-single-property-change
                             beg 'font-lock-multiline)
                            (point-min)))
              (goto-char beg)
              (setq beg (emacs-wiki-line-beginning-position)))
            (when font-lock-multiline
              (setq end (or (text-property-any end (point-max)
                                               'font-lock-multiline nil)
                            (point-max))))
            (goto-char end)
            (setq end (emacs-wiki-line-beginning-position 2))
            ;; Undo any fontification in the area.
            (font-lock-unfontify-region beg end)
            (run-hook-with-args 'emacs-wiki-before-highlight-buffer-hook
                                beg end verbose)
            ;; And apply fontification based on `emacs-wiki-highlight-markup'
            (let ((len (float (- end beg)))
                  (case-fold-search nil)
                  markup-func)
              (goto-char beg)
              (while (and (< (point) end)
                          (re-search-forward emacs-wiki-highlight-regexp
                                             end t))
                (if verbose
                    (message "Highlighting buffer...%d%%"
                             (* (/ (float (- (point) beg)) len) 100)))
                (setq markup-func
                      (aref emacs-wiki-highlight-vector
                            (char-after (match-beginning 0))))
                (when markup-func (funcall markup-func)))
              (run-hook-with-args 'emacs-wiki-highlight-buffer-hook
                                  beg end verbose)
              (if verbose (message "Highlighting buffer...done")))))
      (set-buffer-modified-p modified-p))))

(defun emacs-wiki-unhighlight-region (begin end &optional verbose)
  "Remove all visual highlights in the buffer (except font-lock)."
  ;; this messes up japanese-egg input methods, so check for it
  ;; before removing text properties
  (unless (and (boundp 'current-input-method)
               current-input-method
               (save-match-data
                 (string-match "-egg-" current-input-method)))
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          (modified-p (buffer-modified-p))
          deactivate-mark)
      (unwind-protect
          (remove-text-properties
           begin end '(face nil font-lock-multiline nil
                            invisible nil intangible nil
                            display nil))
        (set-buffer-modified-p modified-p)))))

(defvar emacs-wiki-keymap-property
  (if (or (featurep 'xemacs)
          (>= emacs-major-version 21))
      'keymap
    'local-map))

(defsubst emacs-wiki-link-properties (help-str &optional face point)
  (append (if face
              (list 'face face 'rear-nonsticky t
                    emacs-wiki-keymap-property emacs-wiki-local-map)
            (list 'invisible 'emacs-wiki 'intangible t 'rear-nonsticky t
                  emacs-wiki-keymap-property emacs-wiki-local-map))
          (list 'mouse-face 'highlight
                'help-echo help-str
                emacs-wiki-keymap-property emacs-wiki-local-map)))

(defun emacs-wiki-link-face (link-name)
  "Return the type of LINK-NAME as a face symbol.
This will be either a normal link or a bad-link face."
  (let ((base (emacs-wiki-wiki-base link-name)))
    (if (or (not base)
            (emacs-wiki-page-file base)
            (save-match-data
              (string-match (concat "\\(/\\|\\`["
                                    emacs-wiki-regexp-lower
                                    "]\\{3,6\\}:\\)") base)))
        'emacs-wiki-link-face
      'emacs-wiki-bad-link-face)))

(defun emacs-wiki-highlight-extended-link ()
  ;; remove flyspell overlays
  (when (fboundp 'flyspell-unhighlight-at)
    (let ((cur (match-beginning 0)))
      (while (> (match-end 0) cur)
        (flyspell-unhighlight-at cur)
        (setq cur (1+ cur)))))
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at emacs-wiki-extended-link-regexp))
  (let ((start (match-beginning 0))
        (link (emacs-wiki-match-string-no-properties 1))
        (desc (emacs-wiki-match-string-no-properties 3)))
    (cond
     ((and emacs-wiki-inline-images
           desc
           (not (emacs-wiki-wiki-url-p desc))
           (save-match-data (string-match emacs-wiki-image-regexp
                                          desc)))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               desc desc))
     ((and emacs-wiki-inline-images
           link
           (not (emacs-wiki-wiki-url-p link))
           (save-match-data (string-match emacs-wiki-image-regexp
                                          link)))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               link desc))
     ((and emacs-wiki-inline-images
           (save-match-data (string-match emacs-wiki-image-regexp
                                          (match-string 0))))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               (match-string 0)))
     (link
      (let* ((props (emacs-wiki-link-properties
                     link (emacs-wiki-link-face link) start))
             (invis-props (append props (emacs-wiki-link-properties
                                         link nil start))))
            (if desc
                (progn
                  ;; we put the normal face properties on the
                  ;; invisible portion too, since emacs sometimes will
                  ;; position the cursor on an intangible character
                  (add-text-properties (match-beginning 0)
                                       (match-beginning 3)
                                       invis-props)
                  (add-text-properties (match-beginning 3)
                                       (match-end 3)
                                       props)
                    (let ((escaped (save-match-data
                                     (emacs-wiki-link-unescape link t))))
                      (unless (string= escaped link)
                        (add-text-properties
                         (match-beginning 3)
                         (match-end 3)
                         (list 'display
                               escaped
                               'intangible t))))
                    (add-text-properties (match-end 3) (match-end 0)
                                         invis-props))
              (add-text-properties (match-beginning 0)
                                   (match-beginning 1)
                                   invis-props)
              (add-text-properties (match-beginning 1) (match-end 0)
                                   props)
              (add-text-properties (match-end 1) (match-end 0)
                                   invis-props))))
     (t
      (goto-char (match-end 0))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       (emacs-wiki-link-properties
        (emacs-wiki-match-string-no-properties 0)
        (emacs-wiki-link-face (match-string 0))
        start))
      (goto-char (match-end 0))))))

(defun emacs-wiki-highlight-wikilink ()
  "Color WikiNames."
  ;; remove flyspell overlays
  (when (fboundp 'flyspell-unhighlight-at)
    (let ((cur (match-beginning 0)))
      (while (> (match-end 0) cur)
        (flyspell-unhighlight-at cur)
        (setq cur (1+ cur)))))
  (unless (or (eq (get-text-property (match-beginning 0) 'invisible)
                  'emacs-wiki)
              (eq (char-before (match-beginning 0)) ?\")
              (eq (char-after (match-end 0)) ?\"))
    (let ((link (emacs-wiki-match-string-no-properties 1))
          (face (emacs-wiki-link-face (match-string 1))))
      (when face
        (add-text-properties (match-beginning 1) (match-end 0)
                             (emacs-wiki-link-properties
                              (emacs-wiki-match-string-no-properties 1)
                              face))))))

(defun emacs-wiki-make-file-glyph (filename)
  "Given a file name, return a newly-created image glyph.
This is a hack for supporting inline images in Xemacs."
  (let ((case-fold-search nil))
    ;; Scan filename to determine image type
    (when (fboundp 'make-glyph)
      (save-match-data
        (cond ((string-match "jpe?g" filename)
               (make-glyph (vector 'jpeg :file filename) 'buffer))
              ((string-match "gif" filename)
               (make-glyph (vector 'gif :file filename) 'buffer))
              ((string-match "png" filename)
               (make-glyph (vector 'png :file filename) 'buffer)))))))

(defun emacs-wiki-inline-image (beg end url &optional desc)
  "Inline locally available images."
  (let ((filename
         (save-match-data
           (cond
            ((string-match "\\`file:\\(.+\\)" url)
             (match-string 1 url))
            ((string-match "/" url)
             (expand-file-name url (symbol-value
                                    emacs-wiki-inline-relative-to))))))
        glyph)
    (when (and filename
               (file-readable-p filename))
      (if (fboundp 'create-image)
          ;; Use the much nicer `create-image'
          (add-text-properties
           beg end (list 'display (create-image filename nil nil
                                                :ascent 'center)
                         'keymap emacs-wiki-local-map
                         'help-echo (or desc url)))
        ;; If we get a valid glyph, use it
        (and (setq glyph (emacs-wiki-make-file-glyph filename))
             (add-text-properties
              beg end (list 'invisible 'emacs-wiki
                            'end-glyph glyph
                            'help-echo (or desc url))))))))

(provide 'emacs-wiki-colors)
;;; emacs-wiki-colors.el ends here
