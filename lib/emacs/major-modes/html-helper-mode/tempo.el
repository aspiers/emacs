;;; tempo.el --- Flexible template insertion
;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: David K}gedal <davidk@lysator.liu.se >
;; Created: 16 Feb 1994
;; Version: 1.2
;; Keywords: extensions, languages, tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; tempo.el|David K�gedal|davidk@lysator.liu.se|
;; Flexible template creation for major and minor modes|
;; 2-Mar-94|$Id$|

;;; Commentary:

;; This file provides a simple way to define powerful templates, or
;; macros, if you wish. It is mainly intended for, but not limited to,
;; other programmers to be used for creating shortcuts for editing
;; certain kind of documents. It was originally written to be used by
;; a HTML editing mode written by Nelson Minar <nelson@reed.edu>, and
;; his html-helper-mode.el is probably the best example of how to use
;; this program.

;; A template is defined as a list of items to be inserted in the
;; current buffer at point. Some of the items can be simple strings,
;; while other can control formatting or define special points of
;; interest in the inserted text.

;; If a template defines a "point of interest" that point is inserted
;; in a buffer-local list of "points of interest" that the user can
;; jump between with the commands `tempo-backward-mark' and
;; `tempo-forward-mark'. If the template definer provides a prompt for
;; the point, and the variable `tempo-interactive' is non-nil, the
;; user will be prompted for a string to be inserted in the buffer,
;; using the minibuffer.

;; The template can also define one point to be replaced with the
;; current region if the template command is called with a prefix (or
;; a non-nil argument).

;; More flexible templates can be created by including lisp symbols,
;; which will be evaluated as variables, or lists, which will will be
;; evaluated as lisp expressions.

;; See the documentation for tempo-define-template for the different
;; items that can be used to define a tempo template.

;; One of the more powerful features of tempo templates are automatic
;; completion. With every template can be assigned a special tag that
;; should be recognized by `tempo-complete-tag' and expanded to the
;; complete template. By default the tags are added to a global list
;; of template tags, and are matched against the last word before
;; point. But if you assign your tags to a specific list, you can also
;; specify another method for matching text in the buffer against the
;; tags. In the HTML mode, for instance, the tags are matched against
;; the text between the last `<' and point.

;; When defining a template named `foo', a symbol named
;; `tempo-template-foo' will be created whose value as a variable will
;; be the template definition, and its function value will be an
;; interactive function that inserts the template at the point.

;; The latest tempo.el distribution can be fetched from
;; ftp.lysator.liu.se in the directory /pub/emacs

;;; Known bugs:

;; If the 'o is the first element in a template, strange things can
;; happen when the template is inserted at the beginning of a
;; line. This is due to strange behaviour in open-line. But it should
;; be easily avoided.

;; The 'o tag is also a problem when including the region. This will
;; be looked into.

;; Clicking mouse-2 in the completion buffer gives strange results.

;; There is a bug in some emacs versions that prevents completion from
;; working. If it doesn't work for you, send me a note indicating your
;; emacs version and your problems.

;;; Code:

;; (provide 'tempo)

;;; User options

(defvar tempo-interactive nil
  "*Prompt user for strings in templates.
If this variable is non-nil, `tempo-insert' prompts the
user for text to insert in the templates")

(defvar tempo-insert-region nil
  "*Automatically insert current region when there is a `r' in the template
If this variable is NIL, `r' elements will be treated just like `p'
elements, unless the template function is given a prefix (or a non-nil
argument). If this variable is non-NIL, the behaviour is reversed.

In Transient Mark mode, this option is unused.")

(defvar tempo-show-completion-buffer t
  "*If non-NIL, show a buffer with possible completions, when only
a partial completion can be found")

(defvar tempo-leave-completion-buffer nil
  "*If NIL, a completion buffer generated by \\[tempo-complete-tag]
disappears at the next keypress; otherwise, it remains forever.")

;;; Internal variables

(defvar tempo-insert-string-functions nil
  "List of functions to run when inserting a string.
Each function is called with a single arg, STRING."  )

(defvar tempo-tags nil
  "An association list with tags and corresponding templates")

(defvar tempo-local-tags '((tempo-tags . nil))
  "A list of locally installed tag completion lists.
It is a association list where the car of every element is a symbol
whose varable value is a template list. The cdr part, if non-nil, is a
function or a regexp that defines the string to match. See the
documentation for the function `tempo-complete-tag' for more info.

`tempo-tags' is always in the last position in this list.")

(defvar tempo-collection nil
  "A collection of all the tags defined for the current buffer.")

(defvar tempo-dirty-collection t
  "Indicates if the tag collection needs to be rebuilt.")

(defvar tempo-marks nil
  "A list of marks to jump to with `\\[tempo-forward-mark]' and `\\[tempo-backward-mark]'.")

(defvar tempo-match-finder "\\b\\([^\\b]+\\)\\="
  "The regexp or function used to find the string to match against tags.

If `tempo-match-finder is a string, it should contain a regular
expression with at least one \\( \\) pair. When searching for tags,
`tempo-complete-tag' calls `re-search-backward' with this string, and
the string between the first \\( and \\) is used for matching against
each string in the tag list. If one is found, the whole text between
the first \\( and the point is replaced with the inserted template.

You will probably want to include \\ \= at the end of the regexp to
make sure that the string is matched only against text adjacent to the
point.

If `tempo-match-finder' is a symbol, it should be a function that
returns a pair of the form (STRING . POS), where STRING is the string
used for matching and POS is the buffer position after which text
should be replaced with a template.")

(defvar tempo-user-elements nil
  "Element handlers for user-defined elements.
A list of symbols which are bound to functions that take one argument.
This function should return somthing to be sent to `tempo-insert' if
it recognizes the argument, and NIL otherwise")

(defvar tempo-named-insertions nil
  "Temporary storage for named insertions")

(defvar tempo-region-start (make-marker)
  "Region start when inserting around the region")

(defvar tempo-region-stop (make-marker)
  "Region stop when inserting around the region")

;; Make some variables local to every buffer

(make-variable-buffer-local 'tempo-marks)
(make-variable-buffer-local 'tempo-local-tags)
(make-variable-buffer-local 'tempo-match-finder)
(make-variable-buffer-local 'tempo-collection)
(make-variable-buffer-local 'tempo-dirty-collection)

;;; Functions

;;
;; tempo-define-template

(defun tempo-define-template (name elements &optional tag documentation taglist)
  "Define a template.
This function creates a template variable `tempo-template-NAME' and an
interactive function `tempo-template-NAME' that inserts the template
at the point.  The created function is returned.

NAME is a string that contains the name of the template, ELEMENTS is a
list of elements in the template, TAG is the tag used for completion,
DOCUMENTATION is the documentation string for the insertion command
created, and TAGLIST (a symbol) is the tag list that TAG (if provided)
should be added to).  If TAGLIST is nil and TAG is non-nil, TAG is
added to `tempo-tags'

The elements in ELEMENTS can be of several types:

 - A string. It is sent to the hooks in `tempo-insert-string-functions',
   and the result is inserted.
 - The symbol 'p. This position is saved in `tempo-marks'.
 - The symbol 'r. If `tempo-insert' is called with ON-REGION non-nil
   the current region is placed here. Otherwise it works like 'p.
 - (p PROMPT <NAME>) If `tempo-interactive' is non-nil, the user is
   prompted in the minbuffer with PROMPT for a string to be inserted.
   If the optional parameter NAME is non-nil, the text is saved for
   later insertion with the `s' tag.
   If `tempo-interactive' is nil, it works like 'p.
 - (r PROMPT) like the previous, but if `tempo-interactive' is nil
   and `tempo-insert' is called with ON-REGION non-nil, the current
   region is placed here. This usually happens when you call the
   template function with a prefix argument.
 - (s NAME) Inserts text previously read with the (p ..) construct.
   Finds the insertion saved under NAME and inserts it. Acts like 'p
   if tempo-interactive is nil.
 - '& If there is only whitespace between the line start and point,
   nothing happens. Otherwise a newline is inserted.
 - '% If there is only whitespace between point and end-of-line
   nothing happens. Otherwise a newline is inserted.
 - 'n inserts a newline.
 - '> The line is indented using `indent-according-to-mode'. Note that
   you often should place this item after the text you want on the
   line.
 - 'n> Inserts a newline and indents line.
 - 'o Like '% but leaves the point before the newline.
 - nil. It is ignored.
 - Anything else. It is evaluated and the result is parsed again."

  (let* ((template-name (intern (concat "tempo-template-"
				       name)))
	 (command-name template-name))
    (set template-name elements)
    (fset command-name (list 'lambda (list '&optional 'arg)
			     (or documentation 
				 (concat "Insert a " name "."))
			     (list 'interactive "*P")
			     (list 'tempo-insert-template (list 'quote
								template-name)
				   (list 'if 'tempo-insert-region
					 (list 'not 'arg) 'arg))))
    (and tag
	 (tempo-add-tag tag template-name taglist))
    command-name))

;;;
;;; tempo-insert-template

(defun tempo-insert-template (template on-region)
  "Insert a template.
TEMPLATE is the template to be inserted.  If ON-REGION is non-nil the
`r' elements are replaced with the current region. In Transient Mark
mode, ON-REGION is ignored and assumed true if the region is active."
  (if (and (boundp 'transient-mark-mode)
	   transient-mark-mode
	   mark-active)
      (setq on-region t))
  (and on-region
       (set-marker tempo-region-start (min (mark) (point)))
       (set-marker tempo-region-stop (max (mark) (point))))
  (if on-region
      (goto-char tempo-region-start))
  (save-excursion
    (tempo-insert-mark (point-marker))
    (mapcar (function (lambda (elt)
			(tempo-insert elt on-region)))
	    (symbol-value template))
    (tempo-insert-mark (point-marker)))
  (tempo-forward-mark)
  (tempo-forget-insertions)
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (deactivate-mark)))

;;;
;;; tempo-insert

(defun tempo-insert (element on-region)
  "Insert a template element.
Insert one element from a template. If ON-REGION is non-nil the `r'
elements are replaced with the current region.

See documentation for `tempo-define-template' for the kind of elements
possible."
  (cond ((stringp element) (tempo-process-and-insert-string element))
	((and (consp element) (eq (car element) 'p))
	 (tempo-insert-prompt (cdr element)))
	((and (consp element) (eq (car element) 'r))
	 (if on-region
	     (goto-char tempo-region-stop)
	   (tempo-insert-prompt (cdr element))))
	((and (consp element) (eq (car element) 's))
	 (if tempo-interactive
	     (tempo-insert-named (cdr element))
	   (tempo-insert-mark (point-marker))))
	((and (consp element) (eq (car element) 'l))
	 (mapcar (function (lambda (elt) (tempo-insert elt on-region)))
		 (cdr element)))
	((eq element 'p) (tempo-insert-mark (point-marker)))
	((eq element 'r) (if on-region
			     (goto-char tempo-region-stop)
			   (tempo-insert-mark (point-marker))))
	((eq element 'r>) (if on-region
			      (progn
				(goto-char tempo-region-stop)
				(indent-region (mark) (point) nil))
			    (tempo-insert-mark (point-marker))))
	((eq element '>) (indent-according-to-mode))
	((eq element '&) (if (not (or (= (current-column) 0)
				      (save-excursion
					(re-search-backward
					 "^\\s-*\\=" nil t))))
			     (insert "\n")))
	((eq element '%) (if (not (or (eolp)
				      (save-excursion
					(re-search-forward
					 "\\=\\s-*$" nil t))))
			     (insert "\n")))
	((eq element 'n) (insert "\n"))
	((eq element 'n>) (insert "\n") (indent-according-to-mode))
	;; Bug: If the 'o is the first element in a template, strange
	;; things can happen when the template is inserted at the
	;; beginning of a line.
	((eq element 'o) (if (not (or on-region
				      (eolp)
				      (save-excursion
					(re-search-forward
					 "\\=\\s-*$" nil t))))
			     (open-line 1)))
	((null element))
	(t (tempo-insert (or (tempo-is-user-element element)
			     (eval element))
			 on-region))))

;;;
;;; tempo-insert-prompt

(defun tempo-insert-prompt (prompt)
  "Prompt for a text string and insert it in the current buffer.
If the variable `tempo-interactive' is non-nil the user is prompted
for a string in the minibuffer, which is then inserted in the current
buffer. If `tempo-interactive' is nil, the current point is placed on
`tempo-mark'.

PROMPT is the prompt string or a list containing the prompt string and
a name to save the inserted text under."
  (if tempo-interactive
      (let ((prompt-string (if (listp prompt)
			       (car prompt)
			     prompt))
	    (save-name (and (listp prompt) (nth 1 prompt)))
	    inserted-text)

	(progn
	  (setq inserted-text (read-string prompt-string))
	  (insert inserted-text)
	  (if save-name
	      (tempo-remember-insertion save-name inserted-text))))
    (tempo-insert-mark (point-marker))))

;;;
;;; tempo-is-user-element

(defun tempo-is-user-element (element)
  "Tries all the user-defined element handlers in
`tempo-user-elements'"
  ;; Sigh... I need (some list)
  (catch 'found
    (mapcar (function (lambda (handler)
			(let ((result (funcall handler element)))
			  (if result (throw 'found result)))))
	    tempo-user-elements)
    (throw 'found nil)))

;;;
;;; tempo-remember-insertion

(defun tempo-remember-insertion (save-name string)
  "Save the text in STRING under the name SAVE-NAME for later retrieval."
  (setq tempo-named-insertions (cons (cons save-name string)
				     tempo-named-insertions)))

;;;
;;; tempo-forget-insertions

(defun tempo-forget-insertions ()
  "Forget all the saved named insertions."
  (setq tempo-named-insertions nil))

;;;
;;; tempo-insert-named

(defun tempo-insert-named (elt)
  "Insert the previous insertion saved under a named specified in ELT.
The name is in the car of ELT."
  (let* ((name (car elt))
	 (insertion (cdr (assq name tempo-named-insertions))))
    (if insertion
	(insert insertion)
      (error "Named insertion not found"))))

;;;
;;; tempo-process-and-insert-string

(defun tempo-process-and-insert-string (string)
  "Insert a string from a template.
Run a string through the preprocessors in `tempo-insert-string-functions'
and insert the results."
  (cond ((null tempo-insert-string-functions)
	 nil)
	((symbolp tempo-insert-string-functions)
	 (setq string
	       (apply tempo-insert-string-functions (list string))))
	((listp tempo-insert-string-functions)
	 (mapcar (function (lambda (fn)
			     (setq string (apply fn string))))
		 tempo-insert-string-functions))
	(t
	 (error "Bogus value in tempo-insert-string-functions: %s"
		tempo-insert-string-functions)))
  (insert string))

;;;
;;; tempo-insert-mark

(defun tempo-insert-mark (mark)
  "Insert a mark `tempo-marks' while keeping it sorted"
  (cond ((null tempo-marks) (setq tempo-marks (list mark)))
	((< mark (car tempo-marks)) (setq tempo-marks (cons mark tempo-marks)))
	(t (let ((lp tempo-marks))
	     (while (and (cdr lp)
			 (<= (car (cdr lp)) mark))
	       (setq lp (cdr lp)))
	     (if (not (= mark (car lp)))
		 (setcdr lp (cons mark (cdr lp))))))))
	  
;;;
;;; tempo-forward-mark

(defun tempo-forward-mark ()
  "Jump to the next mark in `tempo-forward-mark-list'."
  (interactive)
  (let ((next-mark (catch 'found
		     (mapcar
		      (function
		       (lambda (mark)
			 (if (< (point) mark)
			     (throw 'found mark))))
		      tempo-marks)
		     ;; return nil if not found
		     nil)))
    (if next-mark
	(goto-char next-mark))))

;;;
;;; tempo-backward-mark

(defun tempo-backward-mark ()
  "Jump to the previous mark in `tempo-back-mark-list'."
  (interactive)
  (let ((prev-mark (catch 'found
		     (let (last)
		       (mapcar
			(function
			 (lambda (mark)
			   (if (<= (point) mark)
			       (throw 'found last))
			   (setq last mark)))
			tempo-marks)
		       last))))
    (if prev-mark
	(goto-char prev-mark))))
	
;;;
;;; tempo-add-tag

(defun tempo-add-tag (tag template &optional tag-list)
  "Add a template tag.
Add the TAG, that should complete to TEMPLATE to the list in TAG-LIST,
or to `tempo-tags' if TAG-LIST is nil."

  (interactive "sTag: \nCTemplate: ")
  (if (null tag-list)
      (setq tag-list 'tempo-tags))
  (if (not (assoc tag (symbol-value tag-list)))
      (set tag-list (cons (cons tag template) (symbol-value tag-list))))
  (tempo-invalidate-collection))

;;;
;;; tempo-use-tag-list

(defun tempo-use-tag-list (tag-list &optional completion-function)
  "Install TAG-LIST to be used for template completion in the current buffer.
TAG-LIST is a symbol whose variable value is a tag list created with
`tempo-add-tag'.

COMPLETION-FUNCTION is an obsolete option for specifyingis an optional
function or string that is used by `\\[tempo-complete-tag]' to find a
string to match the tag against. It has the same definition as the
variable `tempo-match-finder'. In this version, supplying a
COMPLETION-FUNCTION just sets `tempo-match-finder' locally."
  (let ((old (assq tag-list tempo-local-tags)))
    (if old
	(setcdr old completion-function)
      (setq tempo-local-tags (cons (cons tag-list completion-function)
				   tempo-local-tags))))
  (if completion-function
      (setq tempo-match-finder completion-function))
  (tempo-invalidate-collection))

;;;
;;; tempo-invalidate-collection

(defun tempo-invalidate-collection ()
  "Marks the tag collection as obsolete.
Whenever it is needed again it will be rebuilt."
  (setq tempo-dirty-collection t))

;;;
;;; tempo-build-collection

(defvar tempo-dirty)

(defun tempo-build-collection ()
  "Build a collection of all the tags and return it.
If `tempo-dirty-collection' is NIL, the old collection is reused."
  (setq tempo-dirty nil)
  (or (and (not tempo-dirty-collection)
	   tempo-collection)
      (setq tempo-collection
	    (apply (function append)
		   (mapcar (function (lambda (tag-list)
					; If the format for
					; tempo-local-tags changes,
					; change this
				       (eval (car tag-list))))
			   tempo-local-tags)))))

;;;
;;; tempo-find-match-string

(defun tempo-find-match-string (finder)
  "Find a string to be matched against a tag list.
FINDER is a function or a string. Returns (STRING . POS)."
  (cond ((stringp finder)
	 (save-excursion
	   (or (re-search-backward finder nil t)
	       0))
	 (cons (buffer-substring (match-beginning 1)
				 (match-end 1)) ; This seems to be a
						; bug in emacs
	       (match-beginning 1)))
	(t
	 (funcall finder))))

;;;
;;; tempo-complete-tag

(defun tempo-complete-tag (&optional silent)
  "Look for a tag and expand it.
All the tags in the tag lists in `tempo-local-tags' (this includes
`tempo-tags') are searched for a match for the text before the point.
The way the string to match for is determined can be altered with the
variable `tempo-match-finder'

If a single match is found, the corresponding template is expanded in
place of the matching string.

If a partial completion or no match at all is found, and SILENT is
non-NIL, the function will give a signal.

If a partial completion is found and `tempo-show-completion-buffer' is
non-NIL, a buffer containing possible completions is displayed."

  ;; This function may look like a hack, but this is how I want it to
  ;; work.
  (interactive "*")
  (let* ((collection (tempo-build-collection))
	 (match-info (tempo-find-match-string tempo-match-finder))
	 (match-string (car match-info))
	 (match-start (cdr match-info))
	 (exact (assoc match-string collection))
	 (compl (or (car exact)
		    (try-completion match-string collection))))
    (if compl (delete-region match-start (point)))
    (cond ((null compl) (or silent (ding)))
	  ((eq compl t) (tempo-insert-template
			 (cdr (assoc match-string
				     collection))
			 nil))
	  (t (if (setq exact (assoc compl collection))
		 (tempo-insert-template (cdr exact) nil)
	       (insert compl)
	       (or silent (ding))
	       (if tempo-show-completion-buffer
		   (tempo-display-completions match-string
					      collection)))))))


;;;
;;; tempo-display-completions

(defun tempo-display-completions (string tag-list)
  "Show a buffer containing possible completions for STRING."
  (if tempo-leave-completion-buffer
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions string tag-list)))
    (save-window-excursion
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (all-completions string tag-list)))
      (sit-for 32767))))

(provide 'tempo)

;;; tempo.el ends here
