;;; erin.el --- Emacs editing mode for TWiki pages

;; Copyright (C) 2007-2009 Neil Van Dyke.  This is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.  This is distributed in the hope that it
;; will be useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.  See the GNU General
;; Public License for more details.  You should have received a copy of the GNU
;; General Public License along with Emacs; see the file `COPYING'.  If not,
;; write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author: Neil Van Dyke <neil AT neilvandyke DOT org>
;; Web: http://www.neilvandyke.org/erin-twiki-emacs/
;; CVS: $Id$
;; Keywords: html hypermedia hypertext text twiki wiki major-mode

;;; COMMENTARY:

;; `erin.el' is an Emacs editing mode for the Wiki markup of TWiki.  The
;; current version of `erin.el' mostly just does semi-WYSIWYG fontifying
;; and heading renumbering.
;;
;; To install, put file `erin.el' into one of the directories that's listed in
;; your Emacs `load-path' variable, (optionally) byte-compile `erin.el', and
;; add the following expression to your `.emacs' file:
;;
;;     (require 'erin)
;;
;; Files named with the extension `.twiki' will be opened in `erin-mode'
;; automatically.  You can see a sampler of fontified TWiki markup with the key
;; sequence: M-x erin-sampler RET
;;
;; Headings can be renumbered with the `erin-renumber-headings' command (key
;; sequence C-c C-r).  The number of pluses in the TWiki markup denotes the
;; level of the heading.  Headings with TWiki `!!' markup after the pluses are
;; unnumbered.  For an example of unnumbered and numbered headings:
;;
;;     ---+!! My Important Wiki Paper
;;     ---+ 1 Introduction
;;     ---+ 2 Theory
;;     ---++ 2.1 Pretty Pictures View of Theory
;;     ---++ 2.2 Cryptic Equations View of Theory
;;     ---+++ 2.2.1 Integral Symbol
;;     ---+ 3 Practice
;;
;; Just as TWiki is not named after Twiki, `erin.el' is not named after Twiki's
;; arguably more attractive co-star, Erin Gray.

;;; HISTORY:

;; [Version 0.6, 2009-08-19] Removed the erroneous setting of
;; `fill-paragraph-function'.  Thanks to Michael Shields for reporting.
;;
;; [Version 0.5, 2009-08-13] Note that I am no longer working on this package,
;; since I am no longer using Twiki.  Changed license to GPL3.  Made the
;; variable reference regexp not search past newline.  Corrected docstring for
;; `erin-mode'.  Comment changes.  Included some commented-out partial
;; implementation of paragraph filling and infrastructure for paragraph info.
;;
;; [Version 0.4, 2007-02-28] New command `erin-renumber-headings'.  List markup
;; is now highlighted to arbitrary depths, rather than only 6 levels.  Fixed
;; bug in lower-case ordered-lists, introduced in 0.3.
;;
;; [Version 0.3, 2007-02-20] Default face colors are now reasonable on
;; dark-background (e.g., reverse-video) Emacs.  In a double-square-brackets
;; link with no alternate text and a WikiWord qualified with a Wiki name, mark
;; the Wiki name as part of the link text (underlined, by default).  In ordered
;; lists, now only supports "a", "A", "i", and "I", rather than all letters.
;; Comment changes.
;;
;; [Version 0.2, 2007-02-13] Fixed bug in trying to match newline before a
;; huggable.  Made huggers more liberal about contents (e.g., "*bold:*" now
;; works).  Made huggers permit some punctuation characters immediately
;; following.
;;
;; [Version 0.1, 2007-02-13] First release.  It's already very useful, even in
;; its primitive state, and I have an immediate need for a released version.

;;; CODE:

(require 'font-lock)

(defgroup erin nil
  "*erin.el editing mode for TWiki pages"
  :group 'hypermedia
  :group 'wp)

(defcustom erin-mode-hook nil
  "*Hooks run when `erin-mode' is turned on."
  :type  'hook
  :group 'erin)

(defconst erin-address-background  "#ddddff")
(defconst erin-fixed-background    "#ffdd00")
(defconst erin-variable-background "#40ff40")

(defconst erin-prominent-markup-foreground-light "red4")
(defconst erin-prominent-markup-foreground-dark  "red1")

(defface erin-markup-face
  '((t (:bold t :foreground "gray50")))
  "Face used for TWiki markup."
  :group 'erin)

(defface erin-bullet-face
  `((((class color) (background dark))
     (:bold t :foreground ,erin-prominent-markup-foreground-dark))
    (t
     (:bold t :foreground ,erin-prominent-markup-foreground-light)))
  "Face used for TWiki bullets."
  :group 'erin)

(defface erin-deemph-markup-face
  ;; TODO: Make a dark-background variant of this.
  '((t (:bold nil :underline nil :slant normal :foreground "gray75")))
  "Face used for de-emphasized TWiki markup."
  :group 'erin)

(defface erin-prominent-markup-face
  `((((class color) (background dark))
     (:bold t :foreground ,erin-prominent-markup-foreground-dark))
    (t
     (:bold t :foreground ,erin-prominent-markup-foreground-light)))
  "Face used for prominent TWiki markup."
  :group 'erin)

(defface erin-separator-face
  ;; TODO: inherit from erin-prominent-markup-face
  `((((class color) (background dark))
     (:bold t :foreground ,erin-prominent-markup-foreground-dark))
    (t
     (:bold t :foreground ,erin-prominent-markup-foreground-light)))
  "Face used for TWiki separator markup."
  :group 'erin)

(defface erin-bold-face
  '((t (:bold t)))
  "Face used for TWiki boldface."
  :group 'erin)

(defface erin-italic-face
  '((t (:slant italic)))
  "Face used for TWiki italic."
  :group 'erin)

(defface erin-bold-italic-face
  '((t (:bold t :slant italic)))
  "Face used for TWiki bold-italic."
  :group 'erin)

(defface erin-fixed-face
  `((t (:foreground "black" :background ,erin-fixed-background)))
  "Face used for TWiki fixed."
  :group 'erin)

(defface erin-bold-fixed-face
  `((t (:bold t :foreground "black" :background ,erin-fixed-background)))
  "Face used for TWiki bold-fixed."
  :group 'erin)

(defface erin-heading-1-face
  '((t (:bold t :family "Helvetica" :height 2.48832)))
  "Face used for TWiki level 1 headings."
  :group 'erin)

(defface erin-heading-2-face
  '((t (:bold t :family "Helvetica" :height 2.0736)))
  "Face used for TWiki level 2 headings."
  :group 'erin)

(defface erin-heading-3-face
  '((t (:bold t :family "Helvetica" :height 1.728)))
  "Face used for TWiki level 3 headings."
  :group 'erin)

(defface erin-heading-4-face
  '((t (:bold t :family "Helvetica" :height 1.44)))
  "Face used for TWiki level 4 headings."
  :group 'erin)

(defface erin-heading-5-face
  '((t (:bold t :family "Helvetica" :height 1.2)))
  "Face used for TWiki level 5 headings."
  :group 'erin)

(defface erin-heading-6-face
  '((t (:bold t :family "Helvetica" :height 1.0)))
  "Face used for TWiki level 6 headings."
  :group 'erin)

(defface erin-variable-face
  `((t (:bold t :foreground "black"
              :background ,erin-variable-background)))
  "Face used for TWiki variable references."
  :group 'erin)

(defface erin-variable-parameters-face
  `((t (:bold nil :foreground "black"
              :background ,erin-variable-background)))
  "Face used for parameters of TWiki variable references."
  :group 'erin)

(defface erin-address-link-face
  `((t (:underline t :foreground "blue"
                   :background ,erin-address-background)))
  "Face used for TWiki address (WikiWord, URL, etc.) links."
  :group 'erin)

(defface erin-address-nonlink-face
  `((t (:bold nil :slant normal :underline nil :foreground "black"
              :background ,erin-address-background)))
  "Face used for TWiki address (WikiWord, URL, etc.) non-links."
  :group 'erin)

(defface erin-nonaddress-link-face
  '((t (:underline t :foreground "blue")))
  "Face used for TWiki non-address (WikiWord, URL, etc.) links."
  :group 'erin)

(defface erin-normal-face
  '((t ()))
  "Do not modify this."
  ;; :group 'erin
  )

(defvar erin-mode-syntax-table
  (let ((x (make-syntax-table)))
    ;; Punctuation Characters (Note: This is necessary for bold markup regexps
    ;; and such to detect non-word-boundaries.)

    (modify-syntax-entry ?!  "." x)
    (modify-syntax-entry ?*  "." x)
    (modify-syntax-entry ?,  "." x)
    (modify-syntax-entry ?.  "." x)
    (modify-syntax-entry ?:  "." x)
    (modify-syntax-entry ?:  "." x)
    (modify-syntax-entry ?=  "." x)
    (modify-syntax-entry ??  "." x)
    (modify-syntax-entry ?\( "." x)
    (modify-syntax-entry ?\) "." x)
    (modify-syntax-entry ?\; "." x)
    (modify-syntax-entry ?\[ "." x)
    (modify-syntax-entry ?\] "." x)
    (modify-syntax-entry ?_  "." x)
    
    x))

(defvar erin-mode-map
  (let ((x (make-sparse-keymap)))
    ;; Note: It's the year 2007, Buck Rogers has already left the planet, and
    ;; we can bind a flow-control character to something.
    (define-key x "\C-c\C-r" 'erin-renumber-headings)
    (define-key x "\C-c\C-s" 'erin-sampler)
    x))

(defvar erin-font-lock-keywords
  ;; Let's see how much we can do with font-lock regexps before we decide to
  ;; rewrite the whole thing to do a proper parse.
  (let* ((wikiword "[A-Z][a-z]+[A-Z][A-Za-z0-9]+")
         (possibly-qualified-wikiword
          (concat "\\(\\(?:[A-Z][A-Za-z]*\\.\\)?\\)\\(" wikiword "\\)"))
         ;; Note: possibly-qualified-wikiword introduces 2 items of match
         ;; data.  The first might be zero-length, but will always be non-nil.
         (no-leading-nonwhitespace  "\\(?:[ \t\n]\\|\\`\\|^\\)")
         ;; TODO: We probably need to make this not consume space, otherwise
         ;; "*bold* *bold*" doesn't work.  "*bold* _italic_" does work,
         ;; however, perhaps because it's different rules.
         (no-trailing-nonwhitespaceother "\\(?:[][:,.!?() \t\n]\\|\\'\\|$\\)")
         (make-huggable (lambda (exclude)
                          (concat "\\("
                                  "[^" exclude " \t\r\n]"
                                  "\\(?:"
                                  "[^" exclude "\r\n]"
                                  "*"
                                  "[^" exclude " \t\r\n]"
                                  "\\)?"
                                  "\\)"
                                  ;;"\\(?:\\>\\)"
                                  ))))
    `(
      ;; "%BR%"
      ("%BR%" 0 'erin-prominent-markup-face)

      ;; Variable references. (TODO: We should properly parse the quoted
      ;; strings in the attributes.)
      ("\\(%\\)\\([A-Za-z]+\\)\\(\\(?:{[^}\r\n]*}\\)?\\)\\(%\\)"
       (1 'erin-deemph-markup-face)
       (2 'erin-variable-face)
       (3 'erin-variable-parameters-face)
       (4 'erin-deemph-markup-face))

      ;; TODO: Is "!!" an escape?  If so, we should put it here, but we need
      ;; to make sure that the "!!" in headings still works.

      ;; Escaped WikiWord.
      (,(concat "\\(!\\)\\(" possibly-qualified-wikiword "\\)\\b")
       (1 'erin-deemph-markup-face)
       (2 'erin-normal-face))

      ;; Escaped double open-brackets.  (TODO: The double-bracket matching is
      ;; still matching on this later; it's just not overriding the face on the
      ;; open-brackets, but it *is* setting faces on everything else.)
      ("\\(!\\)\\(\\[\\[\\)"
       (1 'erin-deemph-markup-face)
       (2 'erin-normal-face))

      ;; Anchor.
      (,(concat "^\\(#\\)" possibly-qualified-wikiword)
       (1 'erin-prominent-markup-face)
       (2 'erin-address-nonlink-face)
       (3 'erin-address-nonlink-face))

      ;; Headings. (Note: This is inefficient, but there's no good regexp way
      ;; to say "use this face if we have one plus, and this other face if we
      ;; have two pluses.  Done in reverse order, to consume all leading
      ;; pluses, to permit a heading that starts with a plus (after "!!" and/or
      ;; whitespace).)
      ,@(reverse
         (let ((regexp-rest '("\\(?:!!\\)?\\)\\(?:[ \t]*\\)\\([^\n]*\\)")))
           (mapcar (function
                    (lambda (face)
                      (setq regexp-rest (cons "\\+" regexp-rest))
                      `(,(apply  'concat "^\\(-\\{3,\\}" regexp-rest)
                        (1 'erin-deemph-markup-face)
                        (2 (quote ,face)))))
                   '(erin-heading-1-face
                     erin-heading-2-face
                     erin-heading-3-face
                     erin-heading-4-face
                     erin-heading-5-face
                     erin-heading-6-face))))

      ;; Separator.
      ("^\\(---+\\)[ \t\r]*$" 1 'erin-separator-face)

      ;; Unordered list item.  (TODO: Make this a bullet character.)
      ("^\\(?:   \\)\\{1,\\}\\(\\*\\)[ \t]" 1 'erin-bullet-face)

      ;; Ordered list item.
      (,(concat
         "^\\(?:   \\)\\{1,\\}"
         "\\(\\(?:[1-9]+\\|[AaIi]\\)\\.\\)"
         ;;"\\(1\\.\\)"
         "[ \t]")
       1 'erin-prominent-markup-face)

      ;; Description list item.
      ("^\\(?:   \\)\\{1,\\}\\(\\$\\)[ \t]+[^:\r\n]*\\(:\\)[ \t]"
       (1 'erin-prominent-markup-face)
       (2 'erin-prominent-markup-face))

      ;; Square-brackets link with WikiWord.
      (,(concat "\\(\\[\\[\\)"
                possibly-qualified-wikiword
                "\\(\\]\\]\\)")
       (1 'erin-deemph-markup-face)
       (2 'erin-address-link-face)
       (3 'erin-address-link-face)
       (4 'erin-deemph-markup-face))

      ;; Square-brackets link with non-WikiWord (fontify entire thing as
      ;; WikiWord link).
      (,(concat "\\(\\[\\[\\)"
                "\\([^][\r\n]*\\)"
                "\\(\\]\\]\\)")
       (1 'erin-deemph-markup-face)
       (2 'erin-address-link-face)
       (3 'erin-deemph-markup-face))

      ;; Square-brackets-alternate link.
      (,(concat "\\(\\[\\[\\)"          ; =1
                "\\([^]\r\n]*\\)"       ; =2
                "\\(\\]\\[\\)"          ; =3
                "\\("                   ; <4
                "\\(?:"                 ; <a
                "[^][\r\n]+"
                "\\|"                   ; |a
                "\\][^]\r\n]"
                "\\|"                   ; |a
                "\\["
                "\\)"                   ; >a
                "+"
                "\\)"                   ; >4
                "\\(\\]\\]\\)"          ; =5
                )
       (1 'erin-deemph-markup-face)
       (2 'erin-address-nonlink-face)
       (3 'erin-deemph-markup-face)
       (4 'erin-nonaddress-link-face)
       (5 'erin-deemph-markup-face))

      ;; Plain WikiWord link.
      (,(concat "\\b" possibly-qualified-wikiword "\\b")
       (1 'erin-address-nonlink-face)
       (2 'erin-address-link-face))

      ;; Bold.
      (,(concat no-leading-nonwhitespace
                "\\(\\*\\)"
                (funcall make-huggable "*")
                "\\(\\*\\)"
                no-trailing-nonwhitespaceother)
       (1 'erin-deemph-markup-face)
       (2 'erin-bold-face)
       (3 'erin-deemph-markup-face))

      ;; Bold-Italic.
      (,(concat no-leading-nonwhitespace
                "\\(__\\)"
                (funcall make-huggable "_")
                "\\(__\\)"
                no-trailing-nonwhitespaceother)
       (1 'erin-deemph-markup-face)
       (2 'erin-bold-italic-face)
       (3 'erin-deemph-markup-face))

      ;; Italic.
      (,(concat no-leading-nonwhitespace
                "\\(_\\)"
                (funcall make-huggable "_")
                "\\(_\\)"
                no-trailing-nonwhitespaceother)
       (1 'erin-deemph-markup-face)
       (2 'erin-italic-face)
       (3 'erin-deemph-markup-face))

      ;; Bold-Fixed.
      (,(concat no-leading-nonwhitespace
                "\\(==\\)"
                (funcall make-huggable "=")
                "\\(==\\)"
                no-trailing-nonwhitespaceother)
       (1 'erin-deemph-markup-face)
       (2 'erin-bold-fixed-face)
       (3 'erin-deemph-markup-face))

      ;; Fixed.
      (,(concat no-leading-nonwhitespace
                "\\(=\\)"
                (funcall make-huggable "=")
                "\\(=\\)"
                no-trailing-nonwhitespaceother)
       (1 'erin-deemph-markup-face)
       (2 'erin-fixed-face)
       (3 'erin-deemph-markup-face))

      ;; TODO: Fontify tag contents correctly.  The tags like "<b>" will use
      ;; the erin-deemph-markup-face, and ones like "<noautolink>" will use the
      ;; erin-prominent-markup-face.

      ;; HTML/XML-like tag. (Note: Presently, we don't support attributes.)
      ("</?[a-zA-Z][a-zA-Z0-9]*[ \t]*/?>" 0 'erin-prominent-markup-face)

      )))

(defun erin-indent-line-function (&rest args)
  ;; TODO:
  (apply 'indent-relative-maybe args))

(defmacro erin-setq-local (sym val)
  (or (symbolp sym) (signal 'wrong-type-argument `(symbolp ,sym)))
  `(progn (make-local-variable (quote ,sym))
          (setq ,sym ,val)))

(defun erin-mode ()
  "Major mode for editing TWiki pages.

\\{erin-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'erin-mode)
  (setq mode-name "Erin-TWiki")
  (use-local-map erin-mode-map)
  (set-syntax-table erin-mode-syntax-table)
  ;; (erin-setq-local fill-paragraph-function 'erin-fill-paragraph)
  (erin-setq-local font-lock-beginning-of-syntax-function 'beginning-of-line)
  (erin-setq-local font-lock-defaults '(erin-font-lock-keywords t nil nil nil))
  (erin-setq-local indent-line-function 'erin-indent-line-function)
  (erin-setq-local indent-tabs-mode nil)
  (run-hooks 'erin-mode-hook))

(defun erin-sampler ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*erin-TWiki-sampler*"))
    (buffer-disable-undo)
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (insert
     "This is a sampler of TWiki markup as displayed by erin-mode.\n"
     "Things labeled \"(TODO)\" are displayed incorrectly.\n\n"
     "WikiWord\n"
     "WIKI.WikiWord\n"
     "NONWikiWord\n"
     "WIKI.NONWikiWord\n"
     "[[WikiWord]]\n"
     "[[WIKI.WikiWord]]\n"
     "[[NONWikiWord]]\n"
     "[[WIKI.NONWikiWord]]\n"
     "[[http://host/]]\n"
     "[[WikiWord][link text]]\n"
     "[[WIKI.WikiWord][link text]]\n"
     "[[NONWikiWord][link text]]\n"
     "[[http://host/][link text]]\n"
     "[[WikiWord#AnchorName]]\n"
     "#AnchorName Text\n"
     "!WikiWord\n"
     "![[NONWikiWord]]   (TODO)\n"
     "\n"
     "*bold*          normal *bold words*   here*not*bold\n"
     "_italic_        normal _italic words_ here_not_italic\n"
     "=fixed=         normal =fixed words=  here=not=fixed\n"
     "__bold italic__ normal                here__not__bolditalic\n"
     "==bold fixed==  normal                here==not==boldfixed\n"
     "\n"
     "normal *x* normal  *x:* normal *x*: normal\n"
     "\n"
     "%TOC% %X% %T% %VARIABLE%\n"
     "%VARIABLE{ parameter=\"value\" }%\n"
     "%BR%\n"
     "\n"
     "------------------------------\n"
     "---+ First-Level Heading\n"
     "---++ Second-Level Heading\n"
     "---+++ Third-Level Heading\n"
     "---++++ Fourth-Level Heading\n"
     "---+++++ Fifth-Level Heading\n"
     "---++++++ Sixth-Level Heading\n"
     "  * Non-bullet\n"
     "   * First-level bullet\n"
     "      * Second-level bullet\n"
     "         * Third-level bullet\n"
     "            * Fourth-level bullet\n"
     "               * Fifth-level bullet\n"
     "                  * Sixth-level bullet\n"
     "  1. Non-numbered\n"
     "   1. First-level\n"
     "      1. Second-level\n"
     "         1. Third-level\n"
     "            1. Fourth-level\n"
     "               1. Fifth-level\n"
     "                  1. Sixth-level\n"
     "   111. Multi-digit\n"
     "   A. Capital letters\n"
     "   a. Lowercase letters\n"
     "   I. Roman capitals\n"
     "   i. Roman lowercase\n"
     "   $ name: First-level\n"
     "      $ name: Second-level\n"
     "         $ name: Third-level\n"
     "            $ name: Fourth-level\n"
     "               $ name: Fifth-level\n"
     "                  $ name: Sixth-level\n"
     "\n"
     "<verbatim> WikiWord *bold* </verbatim>   (TODO)\n"
     "<foo>some text</foo>   (TODO)\n"
     "<b>some text</b>   (TODO)\n"
     "<i>some text</i>   (TODO)\n"
     "<emph>some text</emph>   (TODO)\n"
     "<code>some text</code>   (TODO)\n"
     "<noautolink> WikiWord </noautolink>   (TODO)\n"
     "<a href=\"http://host/\">link text</a>   (TODO)\n"
     "\n"
     "|  *One*   |   *Two*   |  *Three*  |   (TODO)\n"
     "| A        | B         | C         |   (TODO)\n"
     "| Two-Column          || Two-Row   |   (TODO)\n"
     "| X        | Y         |^|   (TODO)\n")
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (erin-mode)
    (pop-to-buffer (current-buffer))))

(defun erin-rv-sampler ()
  (interactive)
  (start-process
   "erin-rv-sampler" nil "emacs" "-rv" "--eval"
   "(progn (require 'erin) (erin-sampler) (delete-other-windows))"))

(defconst erin-renumber-headings-re
  (concat
   "^-\\{3,\\}"
   "\\(\\++\\)"                         ; =1 pluses
   "\\(!!\\)?"                          ; =2 bangs
   "\\("                                ; <3 replace
   "[ \t]*"
   "\\("                                ; <4 number
   "[0-9]+\\(?:\\.[0-9]+\\)*"
   "\\)?"                               ; >4
   "\\.?"
   "[ \t]*"
   "\\)"                                ; >3
   ;;"\\([^\r\n]*\\)"
   ))

(defun erin-renumber-headings ()
  (interactive)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((last-number (list 0)))
        (while (re-search-forward erin-renumber-headings-re nil t)
          ;; Go to the beginning of line, so that we don't accidentally advance
          ;; a line by replacing an existing number string with something
          ;; shorter.
          (beginning-of-line)
          ;; Replace existing replaceable part.
          (replace-match
           ;; Do we have a double-bang?
           (if (match-beginning 2)
               ;; Double-bang, so make sure it doesn't have a number, and
               ;; otherwise normalize the whitespace before the text.
               " "
             ;; No double-bang, so make sure it has the correct number.  Do
             ;; this in as anti-Scheme fashion as possible.
             (concat
              " " 
              (let ((level (- (match-end 1) (match-beginning 1)))
                    (probe last-number))
                (while (> level 0)
                  (setq level (1- level))
                  (if (= 0 level)
                      (progn (setcar probe (1+ (car probe)))
                             (setcdr probe nil))
                    (setq probe
                          (or (cdr probe)
                              (setcdr probe (cons 0 nil))))))
                (mapconcat 'number-to-string last-number "."))
              " "))
           t t nil 3)
          ;; Advance to next line.
          (forward-line))))))

;; Filling:

;; PARAINFO and PARATYPE
;;
;; PARAINFO Is a list of the format:
;;
;;     (PARATYPE START FIRSTLINEP { EXTRA }* )
;;
;; START is the point that is the start of the paragraph.
;;
;; FIRSTLINEP is a boolean value for whether or not PT is in the first line
;; of the paragraph.
;;
;; PARATYPE is one of the symbols from the table below.  The contents of the
;; EXTRA value depends on PARATYPE.
;;
;; PARATYPE               EXTRA elements
;; ---------------------  ---------------------------------
;; blank-line
;; description-list-item  (<item-depth> <item-label-string>)
;; heading                <heading-level>
;; normal
;; ordered-list-item      (<item-depth> <item-number-string>)
;; separator
;; table-row
;; unordered-list-item    (<item-depth>)

;; (defmacro erin-make-parainfo (paratype start firstlinep extra)
;;   `(vector ,paratype ,start ,firstlinep ,extra))
;; 
;; (defmacro erin-get-parainfo-paratype   (parainfo) `(aref ,parainfo 0))
;; (defmacro erin-get-parainfo-start      (parainfo) `(aref ,parainfo 1))
;; (defmacro erin-get-parainfo-firstlinep (parainfo) `(aref ,parainfo 2))
;; (defmacro erin-get-parainfo-extras     (parainfo) `(aref ,parainfo 3))
;; 
;; (defmacro erin-set-parainfo-paratype   (parainfo x) `(aset ,parainfo 0 ,x))
;; (defmacro erin-set-parainfo-start      (parainfo x) `(aset ,parainfo 1 ,x))
;; (defmacro erin-set-parainfo-firstlinep (parainfo x) `(aset ,parainfo 2 ,x))
;; (defmacro erin-set-parainfo-extras     (parainfo x) `(aset ,parainfo 3 ,x))
;; 
;; ;; TODO: Change parainfo to an array.
;; 
;; ;; (defun erin-find-line-para-something-from-point (want-parainfo
;; ;;                                                  default-paratype
;; ;;                                                  previous-backslash-presence)
;; ;;   "This does the bulk of the work for `erin-initial-parainfo' !!!"
;; 
;; ;;   '!!!)
;; 
;; ;; (defun erin-find-next-para (!!!)
;; ;;   '!!!)
;; 
;; (defun erin-initial-parainfo (pt)
;;   (save-excursion
;;     (save-match-data
;;       ;; Note: This is a helper function for erin-paragraph-start-info.  It
;;       ;; works from point and changes point, as well as stomps on match data.
;;       (let ((parainfo             nil)
;;             (last-normal-parainfo nil)
;;             (firstlinep           t))
;;         ;; Loop until we've set `parainfo', which should be a list beginning
;;         ;; with paragraph type symbol and followed by any type-specific
;;         ;; elements.
;;         (while (not parainfo)
;;           ;; Make sure we're at the beginning of line.
;;           (beginning-of-line)
;;           ;; Does the previous line end in a backslash?
;;           ;;
;;           ;; TODO: In current versions of TWiki, does backslash really continue
;;           ;; all paragraphs, only table rows, or some other subset?
;;           (if (save-excursion
;;                 (condition-case err
;;                     ;; Note: This won't work with CR-LF newlines.
;;                     (progn (backward-char 2)
;;                            (looking-at "\\\\"))
;;                   (beginning-of-buffer nil)
;;                   (end-of-buffer       nil)))
;;               ;; Preceding line ends in a backslash, so go to previous line for
;;               ;; the next iteration.
;;               (forward-line -1)
;;             ;; Preceding line doesn't end in a backslash, determine the
;;             ;; paragraph type from the current line, and leave point at first
;;             ;; character of paragraph.
;;             (cond
;;              ;; Table row.
;;              ((looking-at "|")
;;               (setq parainfo
;;                     (erin-make-parainfo 'table-row (point) firstlinep nil)))
;;              ;; Blank line.
;;              ((looking-at "[ \t]*$")
;;               ;; TODO: Maybe we should find the first blank line.
;;               (setq parainfo (or last-normal-parainfo
;;                                  (erin-make-parainfo 'blank-line
;;                                                      (point) 
;;                                                      firstlinep
;;                                                      nil))))
;;              ;; Separator or heading.
;;              ((looking-at "-\\{3,\\}\\(?:\\(\\++\\)\\|\\(?:[ \t\r]*$\\)\\)")
;;               (setq parainfo
;;                     (or last-normal-parainfo
;;                         (if (match-beginning 1)
;;                             (erin-make-parainfo 'heading
;;                                   (point)
;;                                   firstlinep
;;                                   (- (match-end 1) (match-beginning 1)))
;;                           (erin-make-parainfo 'separator
;;                                               (point)
;;                                               firstlinep
;;                                               nil)))))
;;              ;; List item?
;;              ((looking-at "\\(   \\)+\\([0-9]+\\|[AaIi]\\)\\.")
;;               ;; TODO: !!! Add unordered-list and description-item to this
;;               ;; regexp. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;               (let ((item-depth (/ (- (match-end 1) (point)) 3)))
;;                 (setq parainfo
;;                       (erin-make-parainfo 'ordered-list-item
;;                                           (point)
;;                                           firstlinep
;;                                           (list
;;                                            item-depth
;;                                            (match-string-no-properties 2))))))
;;              ;; Nothing else matched, so we might be in a `normal', so set
;;              ;; `last-normal-parainfo' to normal and either use that as the
;;              ;; parainfo (if we're at the start of buffer) or move point to
;;              ;; previous line and iterate.
;;              (t (setq last-normal-parainfo
;;                       (erin-make-parainfo 'normal (point) firstlinep nil))
;;                 (if (bobp)
;;                     (setq parainfo last-normal-parainfo)
;;                   (forward-line -1)))))
;;           ;; Set `first-line-p' to nil, in case we're advancing a line and
;;           ;; iterating.
;;           (setq firstlinep nil))
;;         ;; Done iterating, so return the parainfo.
;;         parainfo))))
;; 
;; ;; TODO: !!!!!!!!!!! add END field to parainfo, and populate for the one-liner
;; ;; partypes as we scan them.
;; 
;; (defun erin-parainfo (pt)
;;   (let ((parainfo (erin-initial-parainfo pt)))
;;     ;; TODO: !!! Also find the end of the paragraph, and set it in
;;     ;; parainfo.
;;     parainfo))
;; 
;; (defun erin-fill-paragraph (arg)
;;   (interactive (list nil))
;; 
;;   (let* ((parainfo (erin-parainfo (point)))
;;          (paratype (erin-get-parainfo-paratype parainfo)))
;; 
;;     ;; (cond
;; ;;      ((eq paratype 'normal)
;; ;; ;;!!!!!!!!!!!!!!!!
;; ;;       )
;;      
;; ;;     ;; TODO: Implement other types.
;;     '!!!
;; 
;;     (message "DEBUG: %S" parainfo)))

;; Auto-Modes:

(add-to-list 'auto-mode-alist (cons "\\.twiki\\'" 'erin-mode))

;; TODO: Make a mode menu.
  
;; TODO: Maybe add folding support.

;; TODO: Maybe add section navigation menu support.

;; TODO: Add commands for moving by section (as determined by headings).
;; Commands like forward-page might be involved.  Alternatively, or in the
;; interim, we could try binding `page-delimeter'.

;; TODO: See "fontification-functions" for things for which font-lock rules
;; prove inadequate.

;; TODO: Magic TAB key increments indent level of bullets, etc.

;; TODO: Magic backspace and delete keys decrement indent level of bullets,
;; etc.

;; TODO: Maybe permit "=" hugger to be bordered externally by parens.

;; TODO: Command to renumber headings.

;; TODO: Requested by Zach Beane: "default to auto-fill mode"

;; TODO: Requested by Zach Beane: "fill bulleted paragraphs correctly; right
;; now it treats the " *" as a fill prefix and wraps all bullet text together;
;; would be nice if bullets were individually filled."

;; TODO: Requested by Zach Beane: "would be super extra nice if there was a
;; function to extract the canonical TWiki page name under the point, allowing
;; for all the syntactic variations"

;; TODO: Add command to add "<!-- -*- Erin -*- -->" cookie at top of buffer.
;; But maybe wait to see whether it can be TWiki instead of Erin.

;; TODO: In renumbering, don't match headings like "1A" as numbers.

;; TODO: Document that renumbering is for specs and the like, for which there's
;; reason to make all one Wiki page.

;; TODO: Change colors: code to black on lightgreen, variables to black on
;; orange.

(provide 'erin)

;;; erin.el ends here
