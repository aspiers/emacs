;;; muse-publish.el --- base publishing implementation

;; Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.

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

;;; Contributors:

;; Yann Hodique (yann DOT hodique AT gmail DOT com) fixed an
;; unnecessary URL description transform in `muse-publish-url'.
;;
;; Peter K. Lee (saint AT corenova DOT com) provided the
;; `muse-style-elements-list' function.
;;
;; Jim Ottaway (j DOT ottaway AT lse DOT ac DOT uk) provided an
;; implementation for nested lists.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-regexps)

(defgroup muse-publish nil
  "Options controlling the general behavior of Muse publishing."
  :group 'muse)

(defcustom muse-before-publish-hook nil
  "A hook run in the buffer to be published, before it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-after-publish-hook nil
  "A hook run in the buffer to be published, after it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-publish-url-transforms
  '(muse-resolve-url)
  "A list of functions used to prepare URLs for publication.
Each is passed the URL.  The transformed URL should be returned."
  :type 'hook
  :options '(muse-resolve-url)
  :group 'muse-publish)

(defcustom muse-publish-desc-transforms nil
  "A list of functions used to prepare URL desciptions for publication.
Each is passed the description.  The modified description should
be returned."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-publish-comments-p nil
  "If nil, remove comments before publishing.
If non-nil, publish comments using the markup of the current style."
  :type 'boolean
  :group 'muse-publish)

(defcustom muse-publish-report-threshhold 100000
  "If a file is this size or larger, report publishing progress."
  :type 'integer
  :group 'muse-publish)

(defcustom muse-publish-markup-regexps
  `(;; Remove leading and trailing whitespace from the file
    (1000 "\\(\\`\n+\\|\n+\\'\\)" 0 "")

    ;; Remove trailing whitespace from all lines
    (1100 ,(concat "[" muse-regexp-blank "]+$") 0 "")

    ;; Handle any leading #directives
    (1200 "\\`#\\([a-zA-Z-]+\\)\\s-+\\(.+\\)\n+" 0 directive)

    ;; markup tags
    (1300 muse-tag-regexp 0 tag)

    ;; commented lines
    (1350 "^;\\s-+\\(.+\\)" 0 comment)

    ;; prevent emphasis characters in explicit links from being marked
    (1400 muse-explicit-link-regexp 0 muse-publish-mark-noemphasis)

    ;; define anchor points
    (1500 "^\\(\\W*\\)#\\(\\S-+\\)\\s-*" 0 anchor)

    ;; emphasized or literal text
    (1600 ,(concat "\\(^\\|[-[" muse-regexp-blank
                   "<('`\"\n]\\)\\(=[^=" muse-regexp-blank
                   "\n]\\|_[^_" muse-regexp-blank
                   "\n]\\|\\*+[^*" muse-regexp-blank
                   "\n]\\)")
          2 word)

    ;; headings, outline-mode style
    (1700 "^\\(\\*+\\)\\s-+" 0 heading)

    ;; ellipses
    (1800 "\\.\\.\\.\\." 0 enddots)
    (1850 "\\.\\.\\." 0 dots)

    ;; horizontal rule, or section separator
    (1900 "^----+" 0 rule)

    ;; non-breaking space
    (1950 "~~" 0 no-break-space)

    ;; beginning of footnotes section
    (2000 "^Footnotes:?\\s-*" 0 fn-sep)
    ;; footnote definition/reference (def if at beginning of line)
    (2100 "\\[\\([1-9][0-9]*\\)\\]" 0 footnote)

    ;; unnumbered List items begin with a -.  numbered list items
    ;; begin with number and a period.  definition lists have a
    ;; leading term separated from the body with ::.  centered
    ;; paragraphs begin with at least six columns of whitespace; any
    ;; other whitespace at the beginning indicates a blockquote.  The
    ;; reason all of these rules are handled here, is so that
    ;; blockquote detection doesn't interfere with indented list
    ;; members.

    (2200 ,(format muse-list-item-regexp (concat "[" muse-regexp-blank "]+"))
          1 list)

    (2300 ,(concat "^\\(\\(?:.+?\\)[" muse-regexp-blank "]+::\n?\\)")
          0 list)

    (2400 ,(concat "^\\([" muse-regexp-blank "]+\\).+") 0 quote)

    (2500 ,(concat "\\(^\\|[" muse-regexp-blank "]+\\)--\\($\\|["
                   muse-regexp-blank "]+\\)")
          0 emdash)

    ;; "verse" text is indicated the same way as a quoted e-mail
    ;; response: "> text", where text may contain initial whitespace
    ;; (see below).
    (2600 ,(concat "^[" muse-regexp-blank "]*> ") 0 verse)

    ;; simple table markup is supported, nothing fancy.  use | to
    ;; separate cells, || to separate header cells, and ||| for footer
    ;; cells
    (2700 ,(concat "\\(" muse-table-line-regexp "\n"
                   "\\(" muse-regexp-blank "*\n\\)?\\)+")
          0 table)

    ;; replace links in the buffer (links to other pages)
    (2900 muse-explicit-link-regexp 0 link)

    ;; bare URLs
    (3000 muse-url-regexp  0 url)

    ;; bare email addresses
    (3500
     "\\([^[]\\)[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+" 0 email)
    )
  "List of markup rules for publishing a page with Muse.
The rules given in this variable are invoked first, followed by
whatever rules are specified by the current style.

Each member of the list is either a function, or a list of the form:

  (REGEXP/SYMBOL TEXT-BEGIN-GROUP REPLACEMENT-TEXT/FUNCTION/SYMBOL)

REGEXP is a regular expression, or symbol whose value is a regular
expression, which is searched for using `re-search-forward'.
TEXT-BEGIN-GROUP is the matching group within that regexp which
denotes the beginning of the actual text to be marked up.
REPLACEMENT-TEXT is a string that will be passed to `replace-match'.
If it is not a string, but a function, it will be called to determine
what the replacement text should be (it must return a string).  If it
is a symbol, the value of that symbol should be a string.

The replacements are done in order, one rule at a time.  Writing
the regular expressions can be a tricky business.  Note that case
is never ignored.  `case-fold-search' is always bound to nil
while processing the markup rules."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-publish)

(defcustom muse-publish-markup-functions
  '((directive . muse-publish-markup-directive)
    (comment   . muse-publish-markup-comment)
    (anchor    . muse-publish-markup-anchor)
    (tag       . muse-publish-markup-tag)
    (word      . muse-publish-markup-word)
    (emdash    . muse-publish-markup-emdash)
    (enddots   . muse-publish-markup-enddots)
    (dots      . muse-publish-markup-dots)
    (rule      . muse-publish-markup-rule)
    (no-break-space . muse-publish-markup-no-break-space)
    (heading   . muse-publish-markup-heading)
    (footnote  . muse-publish-markup-footnote)
    (fn-sep    . muse-publish-markup-fn-sep)
    (list      . muse-publish-markup-list)
    (quote     . muse-publish-markup-quote)
    (verse     . muse-publish-markup-verse)
    (table     . muse-publish-markup-table)
    (email     . muse-publish-markup-email)
    (link      . muse-publish-markup-link)
    (url       . muse-publish-markup-url))
  "An alist of style types to custom functions for that kind of text.

Each member of the list is of the form:

  (SYMBOL FUNCTION)

SYMBOL describes the type of text to associate with this rule.
`muse-publish-markup-regexps' maps regexps to these symbols.

FUNCTION is the function to use to mark up this kind of rule if
no suitable function is found through the :functions tag of the
current style."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-publish)

(defcustom muse-publish-markup-tags
  '(("contents" nil t   muse-publish-contents-tag)
    ("verse"    t   nil muse-publish-verse-tag)
    ("example"  t   nil muse-publish-example-tag)
    ("code"     t   nil muse-publish-code-tag)
    ("quote"    t   nil muse-publish-quote-tag)
    ("literal"  t   nil muse-publish-mark-read-only)
    ("verbatim" t   nil muse-publish-verbatim-tag)
    ("lisp"     t   nil muse-publish-lisp-tag)
    ("class"    t   t   muse-publish-class-tag)
    ("command"  t   t   muse-publish-command-tag)
    ("comment"  t   nil muse-publish-comment-tag))
  "A list of tag specifications, for specially marking up text.
XML-style tags are the best way to add custom markup to Muse.
This is easily accomplished by customizing this list of markup tags.

For each entry, the name of the tag is given, whether it expects
a closing tag and/or an optional set of attributes, and a
function that performs whatever action is desired within the
delimited region.

The tags themselves are deleted during publishing, before the
function is called.  The function is called with three arguments,
the beginning and end of the region surrounded by the tags.  If
properties are allowed, they are passed as a third argument in
the form of an alist.  The `end' argument to the function is
always a marker.

Point is always at the beginning of the region within the tags, when
the function is called.  Wherever point is when the function finishes
is where tag markup will resume.

These tag rules are processed once at the beginning of markup, and
once at the end, to catch any tags which may have been inserted
in-between."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       function))
  :group 'muse-publish)

(defcustom muse-publish-markup-specials nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-publish)

(defvar muse-publishing-p nil
  "Set to t while a page is being published.")
(defvar muse-batch-publishing-p nil
  "Set to t while a page is being batch published.")
(defvar muse-publishing-styles nil
  "The publishing styles that Muse recognizes.
This is automatically generated when loading publishing styles.")
(defvar muse-publishing-current-file nil
  "The file that is currently being published.")
(defvar muse-publishing-current-output-path nil
  "The path where the current file will be published to.")
(defvar muse-publishing-current-style nil
  "The style of the file that is currently being published.")
(defvar muse-publishing-directives nil
  "An alist of publishing directives from the top of a file.")
(defvar muse-publish-generate-contents nil
  "Non-nil if a table of contents should be generated.
If non-nil, it is a cons cell specifying (MARKER . DEPTH), to
tell where the <contents> was seen, and to what depth the
contents were requested.")
(defvar muse-publishing-last-position nil
  "Last position of the point when publishing.
This is used to make sure that publishing doesn't get stalled.")

;; Functions for handling style information

(defsubst muse-style (&optional style)
  "Resolve the given STYLE into a Muse style, if it is a string."
  (if (null style)
      muse-publishing-current-style
    (if (stringp style)
        (assoc style muse-publishing-styles)
      (muse-assert (consp style))
      style)))

(defun muse-define-style (name &rest elements)
  (let ((entry (assoc name muse-publishing-styles)))
    (if entry
        (error "There is already a style named %s." name)
      (setq muse-publishing-styles
            (cons (append (list name) elements)
                  muse-publishing-styles)))))

(defun muse-derive-style (new-name base-name &rest elements)
  (let ((entry (assoc new-name muse-publishing-styles)))
    (if entry
        (error "There is already a style named %s." new-name)
      (apply 'muse-define-style new-name
             (append elements (list :base base-name))))))

(defsubst muse-get-keyword (keyword list &optional direct)
  (let ((value (cadr (memq keyword list))))
    (if (and (not direct) (symbolp value))
        (symbol-value value)
      value)))

(defun muse-style-elements-list (elem &optional style)
  "Return a list all references to ELEM in STYLE, including base styles.
If STYLE is not specified, use current style."
  (let (base elements)
    (while style
      (setq style (muse-style style))
      (setq elements (append elements
                             (muse-get-keyword elem style)))
      (setq style (muse-get-keyword :base style)))
    elements))

(defsubst muse-style-element (elem &optional style direct)
  "Search for ELEM in STYLE, including base styles.
If STYLE is not specified, use current style."
  (setq style (muse-style style))
  (let ((value (muse-get-keyword elem style direct)))
    (if value
        value
      (let ((base (muse-get-keyword :base style)))
        (if base
            (muse-style-element elem base direct))))))

(defun muse-find-markup-element (keyword ident style)
  (let ((def (assq ident (muse-style-element keyword style))))
    (if def
        (cdr def)
      (let ((base (muse-style-element :base style)))
        (if base
            (muse-find-markup-element keyword ident base))))))

(defun muse-markup-text (ident &rest args)
  "Insert ARGS into the text markup associated with IDENT.
If the markup text has sections like %N%, this will be replaced
with the N-1th argument in ARGS.  After that, `format' is applied
to the text with ARGS as parameters."
  (let ((text (muse-find-markup-element :strings ident (muse-style))))
    (if (and text args)
        (progn
          (let (start repl-text)
            (while (setq start (string-match "%\\([1-9][0-9]*\\)%" text start))
              ;; escape '%' in the argument text, since we will be
              ;; using format on it
              (setq repl-text (muse-replace-regexp-in-string
                               "%" "%%"
                               (nth (1- (string-to-number
                                         (match-string 1 text))) args)
                               t t)
                    start (+ start (length repl-text))
                    text (replace-match repl-text t t text))))
          (apply 'format text args))
      (or text ""))))

(defun muse-insert-markup (&rest args)
  (let ((beg (point)))
    (apply 'insert args)
    (muse-publish-mark-read-only beg (point))))

(defun muse-find-markup-tag (keyword tagname style)
  (let ((def (assoc tagname (muse-style-element keyword style))))
    (or def
        (let ((base (muse-style-element :base style)))
          (if base
              (muse-find-markup-tag keyword tagname base))))))

(defsubst muse-markup-tag-info (tagname &rest args)
  (let ((tag-info (muse-find-markup-tag :tags tagname (muse-style))))
    (or tag-info
        (assoc (match-string 1) muse-publish-markup-tags))))

(defsubst muse-markup-function (category)
  (let ((func (muse-find-markup-element :functions category (muse-style))))
    (or func
        (cdr (assq category muse-publish-markup-functions)))))

;; Publishing routines

(defun muse-publish-markup (name rules)
  (let* ((case-fold-search nil)
         (inhibit-read-only t)
         (limit (* (length rules) (point-max)))
         (verbose (and muse-publish-report-threshhold
                       (> (point-max) muse-publish-report-threshhold)))
         (base 0))
    (while rules
      (goto-char (point-min))
      (let ((regexp (nth 1 (car rules)))
            (group (nth 2 (car rules)))
            (repl (nth 3 (car rules)))
            pos)
        (setq muse-publishing-last-position nil)
        (if (symbolp regexp)
            (setq regexp (symbol-value regexp)))
        (if (and verbose (not muse-batch-publishing-p))
            (message "Publishing %s...%d%%" name
                     (* (/ (float (+ (point) base)) limit) 100)))
        (while (and regexp (setq pos (re-search-forward regexp nil t)))
          (if (and verbose (not muse-batch-publishing-p))
              (message "Publishing %s...%d%%" name
                       (* (/ (float (+ (point) base)) limit) 100)))
          (unless (and (> (- (match-end 0) (match-beginning 0)) 0)
                       (get-text-property (match-beginning group) 'read-only))
            (let* (func
                   (text (cond
                          ((and (symbolp repl)
                                (setq func (muse-markup-function repl)))
                           (funcall func))
                          ((functionp repl)
                           (funcall repl))
                          ((symbolp repl)
                           (symbol-value repl))
                          (t repl))))
              (if (stringp text)
                  (replace-match text t))))
          (if (and muse-publishing-last-position
                   (= pos muse-publishing-last-position))
              (if (eobp)
                  (setq regexp nil)
                (forward-char 1)))
          (setq muse-publishing-last-position pos)))
      (setq rules (cdr rules)
            base (+ base (point-max))))
    (if (and verbose (not muse-batch-publishing-p))
        (message "Publishing %s...done" name))))

(defun muse-insert-file-or-string (file-or-string &optional title)
  (let ((beg (point)) end)
    (if (and (not (string-equal file-or-string ""))
             (file-readable-p file-or-string))
        (setq end (+ beg (cadr (insert-file-contents file-or-string))))
      (insert file-or-string)
      (setq end (point)))
    (save-restriction
      (narrow-to-region beg end)
      (muse-publish-markup (or title "")
                           '((100 "<\\(lisp\\)>" 0
                              muse-publish-markup-tag))))))

(defun muse-style-run-hooks (keyword style &rest args)
  (let (handled)
    (while (and style (not handled))
      (setq style (muse-style style))
      (let ((func (muse-get-keyword keyword style t)))
        (if func
            (if (apply func args)
                (setq handled t))))
      (unless handled
        (setq style (muse-style-element :base style))))
    handled))

(defun muse-publish-markup-region (beg end title style)
  "Apply the given STYLE's markup rules to the given region."
  (save-restriction
    (narrow-to-region beg end)
    (muse-style-run-hooks :before style)
    (muse-publish-markup
     title
     (sort (copy-alist (append muse-publish-markup-regexps
                               (muse-style-elements-list :regexps style)))
           (function
            (lambda (l r)
              (< (car l) (car r))))))
    (muse-style-run-hooks :before-end style)
    (muse-publish-escape-specials (point-min) (point-max) nil 'document)))

(defun muse-publish-markup-buffer (title style)
  "Apply the given STYLE's markup rules to the current buffer."
  (setq style (muse-style style))
  (let ((style-header (muse-style-element :header style))
        (style-footer (muse-style-element :footer style))
        (muse-publishing-current-style style)
        (muse-publishing-directives
         (list (cons "title" title)
               (cons "author" (user-full-name))
               (cons "date" (format-time-string
                             "%B %e, %Y"
                             (nth 5 (file-attributes
                                     muse-publishing-current-file))))))
        (muse-publishing-p t)
        (inhibit-read-only t))
    (run-hooks 'muse-before-publish-hook)
    (muse-publish-markup-region (point-min) (point-max) title style)
    (goto-char (point-min))
    (when style-header
      (muse-insert-file-or-string style-header title))
    (goto-char (point-max))
    (when style-footer
      (muse-insert-file-or-string style-footer title))
    (muse-style-run-hooks :after style)
    (run-hooks 'muse-after-publish-hook)))

(defun muse-publish-markup-string (string &optional style)
  "Markup STRING using the given STYLE's markup rules."
  (setq style (muse-style style))
  (muse-with-temp-buffer
    (insert string)
    (let ((muse-publishing-current-style style)
          (muse-publishing-p t))
      (muse-publish-markup "*string*" (muse-style-element :rules style)))
    (buffer-string)))

;; Commands for publishing files

(defsubst muse-publish-get-style ()
  (if (= 1 (length muse-publishing-styles))
      (car muse-publishing-styles)
    (assoc (completing-read "Publish with style: "
                            muse-publishing-styles nil t)
           muse-publishing-styles)))

(defsubst muse-publish-get-output-dir (style)
  (let ((default-directory (or (muse-style-element :path style)
                               default-directory)))
    (muse-read-directory-name "Publish to directory: " nil default-directory)))

(defsubst muse-publish-get-info ()
  (let ((style (muse-publish-get-style)))
    (list style (muse-publish-get-output-dir style)
          current-prefix-arg)))

(defsubst muse-publish-output-name (&optional file style)
  (setq style (muse-style style))
  (concat (muse-style-element :prefix style)
          (muse-page-name file)
          (muse-style-element :suffix style)))

(defsubst muse-publish-output-file (file &optional output-dir style)
  (setq style (muse-style style))
  (if output-dir
      (expand-file-name (muse-publish-output-name file style) output-dir)
    (concat (file-name-directory file)
            (muse-publish-output-name file style))))

(defsubst muse-publish-link-name (&optional file style)
  (setq style (muse-style style))
  (concat (muse-style-element :prefix style)
          (muse-page-name file)
          (or (muse-style-element :link-suffix style)
              (muse-style-element :suffix style))))

(defsubst muse-publish-link-file (file &optional output-dir style)
  (setq style (muse-style style))
  (if output-dir
      (expand-file-name (muse-publish-link-name file style) output-dir)
    (concat (file-name-directory file)
            (muse-publish-link-name file style))))

;;;###autoload
(defun muse-publish-file (file style &optional output-dir force)
  "Publish the given FILE in a particular STYLE to OUTPUT-DIR.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (interactive (cons (read-file-name "Publish file: ")
                     (muse-publish-get-info)))
  (setq style (muse-style style))
  (let* ((output-path (muse-publish-output-file file output-dir style))
         (output-suffix (muse-style-element :osuffix style))
         (muse-publishing-current-file file)
         (muse-publishing-current-output-path output-path)
         (target (if output-suffix
                     (concat (file-name-sans-extension output-path)
                             output-suffix)
                   output-path)))
    (when (or force (file-newer-than-file-p file target))
      (if (and muse-publish-report-threshhold
               (> (nth 7 (file-attributes file))
                  muse-publish-report-threshhold))
          (message "Publishing %s ..." file))
      (muse-with-temp-buffer
        (insert-file-contents file)
        (muse-publish-markup-buffer (muse-page-name file) style)
        (let ((backup-inhibited t))
          (write-file output-path))
        (muse-style-run-hooks :final style file output-path target))
      t)))

;;;###autoload
(defun muse-publish-this-file (style output-dir &optional force)
  "Publish the page in the current file."
  (interactive (muse-publish-get-info))
  (unless (muse-publish-file buffer-file-name style output-dir force)
    (message (concat "The published version is up-to-date; use"
                     " C-u C-c C-t to force an update."))))

(defun muse-batch-publish-files ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        style-name style output-dir)
    (setq style-name (car command-line-args-left)
          style (muse-style style-name)
          command-line-args-left (cdr command-line-args-left)
          output-dir (car command-line-args-left)
          output-dir
          (if (string-match "\\`--output-dir=" output-dir)
              (prog1
                  (substring output-dir (match-end 0))
                (setq command-line-args-left (cdr command-line-args-left)))))
    (unless style
      (error "There is no style '%s' defined." style-name))
    (dolist (file command-line-args-left)
      (muse-publish-file file style output-dir t))))

;; Default publishing rules

(defun muse-publish-section-close (depth)
  "Seach forward for the closing tag of given DEPTH."
  (let (not-end)
    (save-excursion
      (while (and (setq not-end (re-search-forward
                                 (concat "^\\*\\{1," (number-to-string depth)
                                         "\\}\\s-+")
                                 nil t))
                  (get-text-property (match-beginning 0) 'read-only)))
      (if not-end
          (forward-line 0)
        (goto-char (point-max)))
      (cond ((not (eq (char-before) ?\n))
             (insert "\n\n"))
            ((not (eq (char-before (1- (point))) ?\n))
             (insert "\n")))
      (muse-insert-markup (muse-markup-text 'section-close depth))
      (insert "\n"))))

(defun muse-publish-markup-directive (&optional name value)
  (unless name (setq name (match-string 1)))
  (unless value (setq value (match-string 2)))
  (let ((elem (assoc name muse-publishing-directives)))
    (if elem
        (setcdr elem value)
      (setq muse-publishing-directives
            (cons (cons name value)
                  muse-publishing-directives))))
  ;; Make sure we don't ever try to move the point forward (past the
  ;; beginning of buffer) while we're still searching for directives.
  (setq muse-publishing-last-position nil)
  (delete-region (match-beginning 0) (match-end 0)))

(defun muse-publish-markup-anchor ()
  (unless (get-text-property (match-end 1) 'noemphasis)
    (let ((text (muse-markup-text 'anchor (match-string 2))))
      (unless (string= text "")
        (save-match-data
          (skip-chars-forward (concat muse-regexp-blank "\n"))
          (muse-insert-markup text)))
      (match-string 1))))

(defun muse-publish-markup-comment ()
  (if (null muse-publish-comments-p)
      ""
    (goto-char (match-end 0))
    (muse-insert-markup (muse-markup-text 'comment-end))
    (muse-publish-mark-read-only (match-beginning 1) (match-end 1))
    (delete-region (match-beginning 0) (match-beginning 1))
    (goto-char (match-beginning 0))
    (muse-insert-markup (muse-markup-text 'comment-begin))))

(defun muse-publish-markup-tag ()
  (let ((tag-info (muse-markup-tag-info (match-string 1))))
    (when (and tag-info
               (not (get-text-property (match-beginning 0) 'read-only)))
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            (beg (point))
            end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match (concat "\\([^"
                                              muse-regexp-blank
                                              "=\n]+\\)\\(=\"\\"
                                              "([^\"]+\\)\"\\)?")
                                      attrstr))
              (let ((attr (cons (downcase
                                 (muse-match-string-no-properties 1 attrstr))
                                (muse-match-string-no-properties 3 attrstr))))
                (setq attrstr (replace-match "" t t attrstr))
                (if attrs
                    (nconc attrs (list attr))
                  (setq attrs (list attr)))))))
        (if (and (cadr tag-info) (not closed-tag))
            (if (search-forward (concat "</" (car tag-info) ">") nil t)
                (delete-region (match-beginning 0) (point))
              (setq tag-info nil)))
        (when tag-info
          (setq end (point-marker))
          (delete-region start beg)
          (goto-char start)
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (apply (nth 3 tag-info) args))))))
  nil)

(defun muse-publish-escape-specials (beg end &optional ignore-read-only context)
  "Escape specials from BEG to END using style-specific :specials.
If IGNORE-READ-ONLY is non-nil, ignore the read-only property.
CONTEXT is used to figure out what kind of specials to escape.

The following contexts exist in Muse.
'underline  _underlined text_
'literal    =monospaced text= or <code>region</code> (monospaced, escaped)
'emphasis   *emphasized text*
'email      email@example.com
'url        http://example.com
'url-desc   [[...][description of an extended link]]
'example    <example>region</example> (monospaced, block context, escaped)
'verbatim   <verbatim>region</verbatim> (escaped)
'document   normal text"
  (let ((specials (muse-style-element :specials nil t)))
    (cond ((functionp specials)
           (setq specials (funcall specials context)))
          ((symbolp specials)
           (setq specials (symbol-value specials))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (if (and (not ignore-read-only)
                   (get-text-property (point) 'read-only))
              (goto-char (or (next-single-property-change (point) 'read-only)
                             (point-max)))
            (let ((repl (or (assoc (char-after) specials)
                            (assoc (char-after)
                                   muse-publish-markup-specials))))
              (if (null repl)
                  (forward-char 1)
                (delete-char 1)
                (insert-before-markers (cdr repl))))))))))

(defun muse-publish-markup-word ()
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc context)
    (cond
     ((string= leader "_")
      (setq context 'underline
            open-tag (muse-markup-text 'begin-underline)
            close-tag (muse-markup-text 'end-underline)))
     ((string= leader "=")
      (setq context 'literal
            open-tag (muse-markup-text 'begin-literal)
            close-tag (muse-markup-text 'end-literal))
      (setq mark-read-only t))
     (t
      (let ((l (length leader)))
        (setq context 'emphasis)
        (cond
         ((= l 1) (setq open-tag (muse-markup-text 'begin-emph)
                        close-tag (muse-markup-text 'end-emph)))
         ((= l 2) (setq open-tag (muse-markup-text 'begin-more-emph)
                        close-tag (muse-markup-text 'end-more-emph)))
         ((= l 3) (setq open-tag (muse-markup-text 'begin-most-emph)
                        close-tag (muse-markup-text 'end-most-emph)))
         (t (setq context nil))))))
    (if (and context
             (not (get-text-property beg 'noemphasis))
             (setq loc (search-forward leader nil t))
             (or (eobp) (not (eq (char-syntax (char-after loc)) ?w)))
             (not (eq (char-syntax (char-before (point))) ?\ ))
             (not (get-text-property (point) 'noemphasis)))
        (progn
          (replace-match "")
          (delete-region beg end)
          (setq end (point-marker))
          (muse-insert-markup close-tag)
          (save-excursion
            (goto-char beg)
            (muse-insert-markup open-tag)
            (setq beg (point)))
          (when mark-read-only
            (muse-publish-escape-specials beg end t context)
            (muse-publish-mark-read-only beg end)))
      (backward-char))
    nil))

(defun muse-publish-markup-emdash ()
  (delete-region (match-beginning 0) (match-end 0))
  (muse-insert-markup (muse-markup-text 'emdash))
  (if (eq (char-after) ?\<)
      (insert ?\n)))

(defun muse-publish-markup-enddots ()
  (unless (get-text-property (match-beginning 0) 'noemphasis)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'enddots))))

(defun muse-publish-markup-dots ()
  (unless (get-text-property (match-beginning 0) 'noemphasis)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'dots))))

(defun muse-publish-markup-rule ()
  (unless (get-text-property (match-beginning 0) 'noemphasis)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'rule))))

(defun muse-publish-markup-no-break-space ()
  (unless (get-text-property (match-beginning 0) 'noemphasis)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'no-break-space))))

(defun muse-publish-markup-heading ()
  (let* ((len (length (match-string 1)))
         (start (muse-markup-text
                 (cond ((= len 1) 'section)
                       ((= len 2) 'subsection)
                       ((= len 3) 'subsubsection)
                       (t 'section-other))
                 len))
         (end   (muse-markup-text
                 (cond ((= len 1) 'section-end)
                       ((= len 2) 'subsection-end)
                       ((= len 3) 'subsubsection-end)
                       (t 'section-other-end))
                 len)))
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup start)
    (end-of-line)
    (when end
      (muse-insert-markup end))
    (muse-publish-section-close len)))

(defvar muse-publish-footnotes nil)

(defun muse-publish-markup-footnote ()
  "Scan ahead and snarf up the footnote body"
  (cond
   ((get-text-property (match-beginning 0) 'noemphasis)
    nil)
   ((= (muse-line-beginning-position) (match-beginning 0))
    "")
   (t
    (let ((footnote (save-match-data
                      (string-to-number (match-string 1))))
          footnotemark)
      (delete-region (match-beginning 0) (match-end 0))
      (save-excursion
        (when (re-search-forward (format "^\\[%d\\]\\s-+" footnote) nil t)
          (let* ((start (match-beginning 0))
                 (beg (goto-char (match-end 0)))
                 (end (save-excursion
                        (if (search-forward "\n\n" nil t)
                            (copy-marker (match-beginning 0))
                          (goto-char (point-max))
                          (skip-chars-backward "\n")
                          (point-marker)))))
            (while (re-search-forward
                    (concat "^[" muse-regexp-blank "]+\\([^\n]\\)")
                    end t)
              (replace-match "\\1" t))
            (let ((footnotemark-cmd (muse-markup-text 'footnotemark))
                  (footnotemark-end-cmd (muse-markup-text 'footnotemark-end)))
              (if (string= "" footnotemark-cmd)
                  (setq footnotemark
                        (concat (muse-markup-text 'footnote)
                                (buffer-substring-no-properties beg end)
                                (muse-markup-text 'footnote-end)))
                (setq footnotemark (format footnotemark-cmd footnote
                                           footnotemark-end-cmd))
                (unless muse-publish-footnotes
                  (set (make-local-variable 'muse-publish-footnotes)
                       (make-vector 256 nil)))
                (unless (aref muse-publish-footnotes footnote)
                  (setq footnotemark
                        (concat
                         footnotemark
                         (concat (format (muse-markup-text 'footnotetext)
                                         footnote)
                                 (buffer-substring-no-properties beg end)
                                 (muse-markup-text 'footnotetext-end))))
                  (aset muse-publish-footnotes footnote footnotemark))))
            (goto-char end)
            (skip-chars-forward "\n")
            (delete-region start (point)))))
      (muse-insert-markup (or footnotemark footnote))))))

(defun muse-publish-markup-fn-sep ()
  (delete-region (match-beginning 0) (match-end 0))
  (muse-insert-markup (muse-markup-text 'fn-sep)))

(defun muse-insert-markup-end-list (&rest args)
  (let ((beg (point)))
    (apply 'insert args)
    (add-text-properties beg (point) '(end-list t))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-surround-dl (indent post-indent)
  (let* ((beg-item (muse-markup-text 'begin-dl-item))
         (end-item (muse-markup-text 'end-dl-item))
         (beg-ddt (muse-markup-text 'begin-ddt))
         (end-ddt (muse-markup-text 'end-ddt))
         (beg-dde (muse-markup-text 'begin-dde))
         (end-dde (muse-markup-text 'end-dde))
         (move-term `(lambda ()
                        (while (and (muse-forward-list-item 'dl ,indent)
                                    (null (match-string 3))))))
         (move-entry `(lambda ()
                        (muse-forward-list-item 'dl ,indent t)))
         (continue t)
         beg search-p)
    (while continue
      (muse-insert-markup beg-item)
      (when (looking-at muse-dl-term-regexp)
        (setq beg (point))
        (goto-char (match-end 1))
        (delete-region (point) (match-end 0))
        (muse-insert-markup end-ddt)
        (if (eq (char-after) ?\n)
            (setq search-p t)
          (setq search-p nil)
          (insert ?\n))
        (goto-char beg)
        (delete-region (point) (match-beginning 1))
        (muse-insert-markup beg-ddt))
      (setq beg (point)
            continue (funcall move-term))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char (point-min))
        (when (or (null search-p) (funcall move-entry))
          (muse-publish-surround-text beg-dde end-dde move-entry
                                      indent post-indent))
        (goto-char (point-max))
        (skip-chars-backward (concat muse-regexp-blank "\n"))
        (muse-insert-markup end-item)
        (when continue
          (goto-char (point-max)))))))

(defun muse-publish-surround-text (beg-tag end-tag move-func &optional indent post-indent)
  (unless indent
    (setq indent (concat "[" muse-regexp-blank "]+")))
  (when post-indent
    (setq indent (concat indent " \\{0," (number-to-string post-indent)
                         "\\}")))
  (let ((continue t)
        (list-item (format muse-list-item-regexp
                           (concat "[" muse-regexp-blank "]+")))
        beg)
    (while continue
      (muse-insert-markup beg-tag)
      (setq beg (point)
            continue (funcall move-func))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when (and (not (looking-at list-item))
                     (looking-at indent))
            (replace-match ""))
          (forward-line 1))
        (skip-chars-backward (concat muse-regexp-blank "\n"))
        (muse-insert-markup-end-list end-tag)
        (when continue
          (goto-char (point-max)))))))

(defun muse-list-item-type (str)
"Determine the type of list given STR.
Returns either 'ul, 'ol, or 'dl."
  (cond ((string= str "")
         nil)
        ((= (aref str 0) ?-) 'ul)
        ((save-match-data
           (string-match "\\`[0-9]+\\." str))
         'ol)
        (t 'dl)))

(defsubst muse-forward-paragraph (&optional pattern)
  (forward-line 1)
  (if (re-search-forward (if pattern
                             (concat "^\\(?:" pattern "\\|\n\\|\\'\\)")
                           "^\\s-*\\(\n\\|\\'\\)") nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun muse-forward-list-item (type indent &optional entry-p)
  "Move forward to the next item of TYPE.
Return non-nil if successful, nil otherwise.
The beginning indentation is given by INDENT.

If TYPE is 'dl and ENTRY-P is non-nil, seach ahead by dl entries.
Otherwise if TYPE is 'dl and ENTRY-P is nil, search ahead by dl
terms."
  (let ((list-item (if (eq type 'dl)
                       (if entry-p
                           muse-dl-entry-regexp
                         (format muse-list-item-regexp
                                 (concat "\\(" muse-dl-entry-regexp "\\|"
                                         indent "\\)")))
                     (format muse-list-item-regexp indent)))
        (empty-line (concat "^[" muse-regexp-blank "]*\n"))
        (next-list-end (or (next-single-property-change (point) 'end-list)
                           (point-max))))
    (muse-forward-paragraph (concat "\\(?:" empty-line "\\)?"
                                    "\\(" list-item "\\)"))
    (cond ((> (point) next-list-end)
           (goto-char next-list-end)
           nil)
          ((and (eq type 'dl)
                entry-p)
           (if (match-beginning 1)
               (progn
                 (goto-char (match-beginning 1))
                 (replace-match "" t t nil 1)
                 t)
             nil))
          ((eq type 'dl)
           (if (string= (match-string 2) indent)
               t
             (if (match-beginning 3)
                 (progn
                   (goto-char (match-beginning 1))
                   (eq 'dl (muse-list-item-type (match-string 3))))
               nil)))
          ((and (match-string 2)
                (eq type (muse-list-item-type (match-string 2))))
           (replace-match "" t t nil 2)
           (goto-char (match-beginning 1)))
          (t
           (when (match-beginning 1)
             (goto-char (match-beginning 1)))
           nil))))

(defun muse-publish-markup-list ()
  "Markup a list entry.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (let* ((str (match-string 1))
         (type (muse-list-item-type str))
         (indent (buffer-substring (muse-line-beginning-position)
                                   (match-beginning 1)))
         (post-indent (length str))
         (last (match-beginning 0)))
    (cond
     ((eq type 'ul)
      (unless (eq (char-after (match-end 1)) ?-)
        (delete-region (match-beginning 0) (match-end 0))
        (muse-insert-markup (muse-markup-text 'begin-uli))
        (save-excursion
          (muse-publish-surround-text
           (muse-markup-text 'begin-uli-item)
           (muse-markup-text 'end-uli-item)
           `(lambda ()
              (muse-forward-list-item 'ul ,indent))
           indent post-indent)
          (muse-insert-markup (muse-markup-text 'end-uli)))
        (forward-line 1)))
     ((eq type 'ol)
      (delete-region (match-beginning 0) (match-end 0))
      (muse-insert-markup (muse-markup-text 'begin-oli))
      (save-excursion
        (muse-publish-surround-text
         (muse-markup-text 'begin-oli-item)
         (muse-markup-text 'end-oli-item)
         `(lambda ()
            (muse-forward-list-item 'ol ,indent))
         indent post-indent)
        (muse-insert-markup (muse-markup-text 'end-oli)))
      (forward-line 1))
     (t
      (goto-char (match-beginning 0))
      (muse-insert-markup (muse-markup-text 'begin-dl))
      (muse-publish-surround-dl indent post-indent)
      (muse-insert-markup (muse-markup-text 'end-dl)))))
  nil)

(defun muse-publish-markup-quote ()
  "Markup a quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (let* ((ws (match-string 1))
         (centered (>= (string-width ws) 6))
         (begin-elem (if centered 'begin-center 'begin-quote-item))
         (end-elem (if centered 'end-center 'end-quote-item)))
    (replace-match "" t t nil 1)
    (unless centered
      (muse-insert-markup (muse-markup-text 'begin-quote)))
    (muse-publish-surround-text (muse-markup-text begin-elem)
                                (muse-markup-text end-elem)
                                (function (lambda ()
                                            (muse-forward-paragraph)
                                            nil)))
    (unless centered
      (muse-insert-markup (muse-markup-text 'end-quote)))))

(defun muse-publish-markup-leading-space (markup-space multiple)
  (let (count)
    (when (and markup-space
               (>= (setq count (skip-chars-forward " ")) 0))
      (delete-region (muse-line-beginning-position) (point))
      (while (> count 0)
        (muse-insert-markup markup-space)
        (setq count (- count multiple))))))

(defun muse-publish-markup-verse ()
  (let ((leader (match-string 0)))
    (goto-char (match-beginning 0))
    (muse-insert-markup (muse-markup-text 'begin-verse))
    (while (looking-at leader)
      (replace-match "")
      (muse-publish-markup-leading-space (muse-markup-text 'verse-space) 2)
      (let ((beg (point)))
        (end-of-line)
        (cond
         ((bolp)
          (let ((text (muse-markup-text 'empty-verse-line)))
            (when text (muse-insert-markup text))))
         ((save-excursion
            (save-match-data
              (forward-line 1)
              (or (looking-at (concat leader "["
                                      muse-regexp-blank
                                      "]*$"))
                  (not (looking-at leader)))))
          (let ((begin-text (muse-markup-text 'begin-last-stanza-line))
                (end-text (muse-markup-text 'end-last-stanza-line)))
            (when end-text (muse-insert-markup end-text))
            (goto-char beg)
            (when begin-text (muse-insert-markup begin-text))
            (end-of-line)))
         (t
          (let ((begin-text (muse-markup-text 'begin-verse-line))
                (end-text (muse-markup-text 'end-verse-line)))
            (when end-text (muse-insert-markup end-text))
            (goto-char beg)
            (when begin-text (muse-insert-markup begin-text))
            (end-of-line))))
        (forward-line 1))))
  (muse-insert-markup (muse-markup-text 'end-verse) ?\n))

(defun muse-publish-table-fields (beg end)
  "Parse given region as a table, returning a cons cell.
The car is the length of the longest row.

The cdr is a list of the fields of the table, with the first
element indicating the type of the row:
  1: body, 2: header, 3: footer.

The existing region will be removed."
  (let ((longest 0)
        (left 0)
        fields field-list)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (= left 0)
        (when (looking-at muse-table-line-regexp)
          (setq fields (cons (length (match-string 1))
                             (split-string (match-string 0)
                                           muse-table-field-regexp))
                field-list (cons fields field-list)
                longest (max (length fields) longest)))
        (setq left (forward-line 1))))
    (delete-region beg end)
    (if (= longest 0)
        (cons 0 nil)
      (cons (1- longest) (nreverse field-list)))))

(defun muse-publish-markup-table ()
  "Style does not support tables.")

(defun muse-publish-escape-specials-in-string (string &optional context)
  "Escape specials in STRING using style-specific :specials.
CONTEXT is used to figure out what kind of specials to escape.

See the documentation of the `muse-publish-escape-specials'
function for the list of available contexts."
  (unless string
    (setq string ""))
  (let ((specials (muse-style-element :specials nil t)))
    (cond ((functionp specials)
           (setq specials (funcall specials context)))
          ((symbolp specials)
           (setq specials (symbol-value specials))))
    (if (functionp specials)
        (funcall specials string)
      (apply (function concat)
             (mapcar
              (lambda (ch)
                (let (repl)
                  (setq repl (or (assoc ch specials)
                                 (assoc ch muse-publish-markup-specials)))
                  (if (null repl)
                      (char-to-string ch)
                    (cdr repl))))
              (append string nil))))))

(defun muse-publish-markup-email ()
  (let* ((beg (match-end 1))
         (addr (buffer-substring-no-properties beg (match-end 0))))
    (setq addr (muse-publish-escape-specials-in-string addr 'email))
    (goto-char beg)
    (delete-region beg (match-end 0))
    (if (or (eq (char-before (match-beginning 0)) ?\")
            (eq (char-after (match-end 0)) ?\"))
        (insert addr)
      (insert (format (muse-markup-text 'email-addr) addr addr)))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-classify-url (target)
  "Transform anchors and get published name, if TARGET is a page.
The return value is a cons cell.  The car is the type of link,
the cadr is the page name, and the cddr is the anchor."
  (save-match-data
    (cond ((or (null target) (string= target ""))
           nil)
          ((string-match muse-url-regexp target)
           (cons 'url (cons target nil)))
          ((string-match muse-image-regexp target)
           (cons 'image (cons target nil)))
          ((string-match muse-file-regexp target)
           (cons 'file (cons target nil)))
          ((string-match "#" target)
           (if (eq (aref target 0) ?\#)
              (cons 'anchor-ref (cons nil (substring target 1)))
             (cons 'link-and-anchor
                   (cons (muse-publish-link-name
                          (substring target 0 (match-beginning 0)))
                         (substring target (match-end 0))))))
          (t
           (cons 'link (cons (muse-publish-link-name target) nil))))))

(defun muse-publish-url (url &optional desc explicit)
  "Resolve a URL into its final <a href> form."
  (let ((orig-url url)
        type anchor)
    (dolist (transform muse-publish-url-transforms)
      (setq url (save-match-data (when url (funcall transform url explicit)))))
    (dolist (transform muse-publish-desc-transforms)
      (setq desc (save-match-data
                   (when desc (funcall transform desc explicit)))))
    (when desc
      (setq desc (muse-publish-escape-specials-in-string desc 'url-desc)))
    (setq orig-url
          (muse-publish-escape-specials-in-string orig-url 'url-desc))
    (let ((target (muse-publish-classify-url url)))
      (setq type (car target)
            url (muse-publish-escape-specials-in-string (cadr target) 'url)
            anchor (muse-publish-escape-specials-in-string
                    (cddr target) 'url)))
    (cond ((eq type 'anchor-ref)
           (muse-markup-text 'anchor-ref anchor (or desc orig-url)))
          ((string= url "")
           desc)
          ((eq type 'image)
           (if desc
               (muse-markup-text 'image-with-desc url desc)
             (muse-markup-text 'image-link url)))
          ((eq type 'link-and-anchor)
           (muse-markup-text 'link-and-anchor url anchor
                             (or desc orig-url)))
          ((and desc (string-match muse-image-regexp desc))
           (muse-markup-text 'url-with-image url desc))
          ((eq type 'link)
           (muse-markup-text 'link url (or desc orig-url)))
          (t
           (muse-markup-text 'url url (or desc orig-url))))))

(defun muse-publish-insert-url (url &optional desc explicit)
  "Resolve a URL into its final <a href> form."
  (delete-region (match-beginning 0) (match-end 0))
  (let ((text (muse-publish-url url desc explicit)))
    (when text
      (muse-insert-markup text))))

(defun muse-publish-markup-link ()
  (let (desc explicit link)
    (setq explicit (save-match-data
                     (if (string-match muse-explicit-link-regexp
                                       (match-string 0))
                         t nil)))
    (setq desc (if explicit (match-string 2) (match-string 0)))
    (setq link (if explicit
                   (muse-handle-explicit-link (match-string 1))
                 (muse-handle-implicit-link (match-string 0))))
    (when (and link
               (or explicit
                   (not (or (eq (char-before (match-beginning 0)) ?\")
                            (eq (char-after (match-end 0)) ?\")))))
      (muse-publish-insert-url link desc explicit))))

(defun muse-publish-markup-url ()
  (unless (or (eq (char-before (match-beginning 0)) ?\")
              (eq (char-after (match-end 0)) ?\"))
    (muse-publish-insert-url (match-string 0))))

;; Default publishing tags

(defun muse-publish-contents-tag (beg end attrs)
  (set (make-local-variable 'muse-publish-generate-contents)
       (cons (copy-marker (point) t)
             (let ((depth (cdr (assoc "depth" attrs))))
               (or (and depth (string-to-number depth)) 2)))))

(defun muse-publish-verse-tag (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (eq ?\  (char-syntax (char-after)))
        (delete-char 1))
      (while (< (point) (point-max))
        (insert "> ")
        (forward-line))
      (if (eq ?\  (char-syntax (char-before)))
          (delete-char -1)))))

(defun muse-publish-mark-read-only (beg end)
  (add-text-properties beg end '(rear-nonsticky (read-only) read-only t))
  nil)

(defun muse-publish-mark-noemphasis (&optional beg end)
  (unless beg (setq beg (match-beginning 0)))
  (unless end (setq end (match-end 0)))
  (add-text-properties beg end '(noemphasis t))
  nil)

(defun muse-publish-quote-tag (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (muse-insert-markup (muse-markup-text 'begin-quote))
      (muse-publish-surround-text (muse-markup-text 'begin-quote-item)
                                  (muse-markup-text 'end-quote-item)
                                  (function (lambda ()
                                              (muse-forward-paragraph)
                                              (goto-char (match-end 0))
                                              (< (point) (point-max)))))
      (muse-insert-markup (muse-markup-text 'end-quote)))))

(defun muse-publish-code-tag (beg end)
  (muse-publish-escape-specials beg end nil 'literal)
  (goto-char beg)
  (insert (muse-markup-text 'begin-literal))
  (goto-char end)
  (insert (muse-markup-text 'end-literal))
  (muse-publish-mark-read-only beg (point)))

(defun muse-publish-example-tag (beg end)
  (muse-publish-escape-specials beg end nil 'example)
  (goto-char beg)
  (insert (muse-markup-text 'begin-example))
  (goto-char end)
  (insert (muse-markup-text 'end-example))
  (muse-publish-mark-read-only beg (point)))

(defun muse-publish-verbatim-tag (beg end)
  (muse-publish-escape-specials beg end nil 'verbatim)
  (muse-publish-mark-read-only beg end))

(defalias 'muse-publish-class-tag 'ignore)

(defun muse-publish-lisp-tag (beg end)
  (save-excursion
    (let ((str (muse-eval-lisp
                (prog1
                    (concat "(progn "
                            (buffer-substring-no-properties beg end)
                            ")")
                  (delete-region beg end)))))
      (set-text-properties 0 (length str) nil str)
      (insert str))))

(defun muse-publish-command-tag (beg end attrs)
  (while (looking-at "\\s-*$")
    (forward-line))
  (let ((interp (cdr (assoc "interp" attrs))))
    (if (null interp)
        (shell-command
         (prog1
             (buffer-substring-no-properties (point) end)
           (delete-region beg end)) t)
      (shell-command-on-region beg end interp t t))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-comment-tag (beg end)
  (if (null muse-publish-comments-p)
      (delete-region beg end)
    (goto-char end)
    (muse-insert-markup (muse-markup-text 'comment-end))
    (muse-publish-mark-read-only beg end)
    (goto-char beg)
    (muse-insert-markup (muse-markup-text 'comment-begin))))

;; Miscellaneous helper functions

(defsubst muse-publishing-directive (name)
  (cdr (assoc name muse-publishing-directives)))

(defun muse-publish-strip-tags (string)
  "Remove all tags from the string."
  (while (string-match "<.*?>" string)
    (setq string (replace-match "" nil t string)))
  string)

(defun muse-publish-markup-type (category default-func)
  (let ((rule (muse-find-markup-element :overrides category (muse-style))))
    (funcall (or rule default-func))))

(defun muse-published-buffer-contents (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((beg (and (search-forward "Emacs Muse begins here")
                    (muse-line-end-position)))
          (end (and (search-forward "Emacs Muse ends here")
                    (muse-line-beginning-position))))
      (buffer-substring-no-properties beg end))))

(defun muse-published-contents (file)
  (when (file-readable-p file)
    (muse-with-temp-buffer
      (insert-file-contents file)
      (muse-published-buffer-contents (current-buffer)))))

(defun muse-publish-transform-output
  (file temp-file output-path name gen-func &rest cleanup-exts)
  "Transform the given TEMP-FILE into the OUTPUT-PATH, using GEN-FUNC."
  (setq file (muse-page-name file))
  (message "Generating %s output for %s..." name file)
  (if (not (funcall gen-func temp-file output-path))
      (message "Generating %s from %s...failed" name file)
    (message "Generating %s output for %s...done" name file)
    (muse-delete-file-if-exists temp-file)
    (dolist (ext cleanup-exts)
      (muse-delete-file-if-exists
       (expand-file-name (concat file ext)
                         (file-name-directory output-path))))
    (message "Wrote %s" output-path)))

(defun muse-publish-read-only (string)
  (let ((end (1- (length string))))
    (add-text-properties 0 end
                         '(rear-nonsticky (read-only) read-only t)
                         string)
    string))

(provide 'muse-publish)

;;; muse-publish.el ends here