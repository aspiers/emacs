;;; allout.el --- extensive outline mode for use alone and with other modes

;; Copyright (C) 1992, 1993, 1994, 2001, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

;; Author: Ken Manheimer <ken dot manheimer at gmail dot com>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail dot com>
;; Created: Dec 1991 - first release to usenet
;; Version: 2.2
;; Keywords: outlines wp languages

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allout outline minor mode provides extensive outline formatting and
;; and manipulation beyond standard emacs outline mode.  Some features:
;;
;;  - Classic outline-mode topic-oriented navigation and exposure adjustment
;;  - Topic-oriented editing including coherent topic and subtopic
;;    creation, promotion, demotion, cut/paste across depths, etc.
;;  - Incremental search with dynamic exposure and reconcealment of text
;;  - Customizable bullet format - enables programming-language specific
;;    outlining, for code-folding editing.  (Allout code itself is to try it;
;;    formatted as an outline - do ESC-x eval-current-buffer in allout.el; but
;;    emacs local file variables need to be enabled when the
;;    file was visited - see `enable-local-variables'.)
;;  - Configurable per-file initial exposure settings
;;  - Symmetric-key and key-pair topic encryption, plus symmetric passphrase
;;    mnemonic support, with verification against an established passphrase
;;    (using a stashed encrypted dummy string) and user-supplied hint
;;    maintenance.  (See allout-toggle-current-subtree-encryption docstring.)
;;  - Automatic topic-number maintenance
;;  - "Hot-spot" operation, for single-keystroke maneuvering and
;;    exposure control (see the allout-mode docstring)
;;  - Easy rendering of exposed portions into numbered, latex, indented, etc
;;    outline styles
;;  - Careful attention to whitespace - enabling blank lines between items
;;    and maintenance of hanging indentation (in paragraph auto-fill and
;;    across topic promotion and demotion) of topic bodies consistent with
;;    indentation of their topic header.
;;
;; and more.
;;
;; See the `allout-mode' function's docstring for an introduction to the
;; mode.  The development version and helpful notes are available at
;; http://myriadicity.net/Sundry/EmacsAllout .
;;
;; The outline menubar additions provide quick reference to many of
;; the features, and see the docstring of the variable `allout-init'
;; for instructions on priming your emacs session for automatic
;; activation of allout-mode.
;;
;; See the docstring of the variables `allout-layout' and
;; `allout-auto-activation' for details on automatic activation of
;; `allout-mode' as a minor mode.  (It has changed since allout
;; 3.x, for those of you that depend on the old method.)
;;
;; Note - the lines beginning with `;;;_' are outline topic headers.
;;        Just `ESC-x eval-current-buffer' to give it a whirl.

;; ken manheimer (ken dot manheimer at gmail dot com)

;;; Code:

;;;_* Dependency autoloads
(require 'overlay)
(eval-when-compile (progn (require 'pgg)
                          (require 'pgg-gpg)
                          (require 'overlay)
			  ))
(autoload 'pgg-gpg-symmetric-key-p "pgg-gpg"
  "True if decoded armor MESSAGE-KEYS has symmetric encryption indicator.")

;;;_* USER CUSTOMIZATION VARIABLES:

;;;_ > defgroup allout
(defgroup allout nil
  "Extensive outline mode for use alone and with other modes."
  :prefix "allout-"
  :group 'outlines)

;;;_ + Layout, Mode, and Topic Header Configuration

;;;_  = allout-auto-activation
(defcustom allout-auto-activation nil
  "*Regulates auto-activation modality of allout outlines - see `allout-init'.

Setq-default by `allout-init' to regulate whether or not allout
outline mode is automatically activated when the buffer-specific
variable `allout-layout' is non-nil, and whether or not the layout
dictated by `allout-layout' should be imposed on mode activation.

With value t, auto-mode-activation and auto-layout are enabled.
\(This also depends on `allout-find-file-hook' being installed in
`find-file-hook', which is also done by `allout-init'.)

With value `ask', auto-mode-activation is enabled, and endorsement for
performing auto-layout is asked of the user each time.

With value `activate', only auto-mode-activation is enabled,
auto-layout is not.

With value nil, neither auto-mode-activation nor auto-layout are
enabled.

See the docstring for `allout-init' for the proper interface to
this variable."
  :type '(choice (const :tag "On" t)
                (const :tag "Ask about layout" "ask")
                (const :tag "Mode only" "activate")
                (const :tag "Off" nil))
  :group 'allout)
;;;_  = allout-layout
(defvar allout-layout nil
  "*Layout specification and provisional mode trigger for allout outlines.

Buffer-specific.

A list value specifies a default layout for the current buffer, to be
applied upon activation of `allout-mode'.  Any non-nil value will
automatically trigger `allout-mode' \(provided `allout-init' has been called
to enable this behavior).

See the docstring for `allout-init' for details on setting up for
auto-mode-activation, and for `allout-expose-topic' for the format of
the layout specification.

You can associate a particular outline layout with a file by setting
this var via the file's local variables.  For example, the following
lines at the bottom of an Emacs Lisp file:

;;;Local variables:
;;;allout-layout: \(0 : -1 -1 0)
;;;End:

will, modulo the above-mentioned conditions, cause the mode to be
activated when the file is visited, followed by the equivalent of
`\(allout-expose-topic 0 : -1 -1 0)'.  \(This is the layout used for
the allout.el source file.)

Also, allout's mode-specific provisions will make topic prefixes default
to the comment-start string, if any, of the language of the file.  This
is modulo the setting of `allout-use-mode-specific-leader', which see.")
(make-variable-buffer-local 'allout-layout)
;;;_  = allout-show-bodies
(defcustom allout-show-bodies nil
  "*If non-nil, show entire body when exposing a topic, rather than
just the header."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-show-bodies)

;;;_  = allout-header-prefix
(defcustom allout-header-prefix "."
  "*Leading string which helps distinguish topic headers.

Outline topic header lines are identified by a leading topic
header prefix, which mostly have the value of this var at their front.
\(Level 1 topics are exceptions.  They consist of only a single
character, which is typically set to the `allout-primary-bullet'.  Many
outlines start at level 2 to avoid this discrepancy."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-header-prefix)
;;;_  = allout-primary-bullet
(defcustom allout-primary-bullet "*"
  "Bullet used for top-level outline topics.

Outline topic header lines are identified by a leading topic header
prefix, which is concluded by bullets that includes the value of this
var and the respective allout-*-bullets-string vars.

The value of an asterisk (`*') provides for backwards compatibility
with the original Emacs outline mode.  See `allout-plain-bullets-string'
and `allout-distinctive-bullets-string' for the range of available
bullets."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-primary-bullet)
;;;_  = allout-plain-bullets-string
(defcustom allout-plain-bullets-string ".,"
  "*The bullets normally used in outline topic prefixes.

See `allout-distinctive-bullets-string' for the other kind of
bullets.

DO NOT include the close-square-bracket, `]', as a bullet.

Outline mode has to be reactivated in order for changes to the value
of this var to take effect."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-plain-bullets-string)
;;;_  = allout-distinctive-bullets-string
(defcustom allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
  "*Persistent outline header bullets used to distinguish special topics.

These bullets are used to distinguish topics from the run-of-the-mill
ones.  They are not used in the standard topic headers created by
the topic-opening, shifting, and rebulleting \(eg, on topic shift,
topic paste, blanket rebulleting) routines, but are offered among the
choices for rebulleting.  They are not altered by the above automatic
rebulleting, so they can be used to characterize topics, eg:

 `?' question topics
 `\(' parenthetic comment \(with a matching close paren inside)
 `[' meta-note \(with a matching close ] inside)
 `\"' a quotation
 `=' value settings
 `~' \"more or less\"
 `^' see above

 ... for example.  (`#' typically has a special meaning to the software,
according to the value of `allout-numbered-bullet'.)

See `allout-plain-bullets-string' for the selection of
alternating bullets.

You must run `set-allout-regexp' in order for outline mode to
reconcile to changes of this value.

DO NOT include the close-square-bracket, `]', on either of the bullet
strings."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-distinctive-bullets-string)

;;;_  = allout-use-mode-specific-leader
(defcustom allout-use-mode-specific-leader t
  "*When non-nil, use mode-specific topic-header prefixes.

Allout outline mode will use the mode-specific `allout-mode-leaders'
and/or comment-start string, if any, to lead the topic prefix string,
so topic headers look like comments in the programming language.

String values are used as they stand.

Value t means to first check for assoc value in `allout-mode-leaders'
alist, then use comment-start string, if any, then use default \(`.').
\(See note about use of comment-start strings, below.)

Set to the symbol for either of `allout-mode-leaders' or
`comment-start' to use only one of them, respectively.

Value nil means to always use the default \(`.').

comment-start strings that do not end in spaces are tripled, and an
`_' underscore is tacked on the end, to distinguish them from regular
comment strings.  comment-start strings that do end in spaces are not
tripled, but an underscore is substituted for the space. [This
presumes that the space is for appearance, not comment syntax.  You
can use `allout-mode-leaders' to override this behavior, when
incorrect.]"
  :type '(choice (const t) (const nil) string
		 (const allout-mode-leaders)
		 (const comment-start))
  :group 'allout)
;;;_  = allout-mode-leaders
(defvar allout-mode-leaders '()
  "Specific allout-prefix leading strings per major modes.

Entries will be used instead or in lieu of mode-specific
comment-start strings.  See also `allout-use-mode-specific-leader'.

If you're constructing a string that will comment-out outline
structuring so it can be included in program code, append an extra
character, like an \"_\" underscore, to distinguish the lead string
from regular comments that start at bol.")

;;;_  = allout-old-style-prefixes
(defcustom allout-old-style-prefixes nil
  "*When non-nil, use only old-and-crusty `outline-mode' `*' topic prefixes.

Non-nil restricts the topic creation and modification
functions to asterix-padded prefixes, so they look exactly
like the original Emacs-outline style prefixes.

Whatever the setting of this variable, both old and new style prefixes
are always respected by the topic maneuvering functions."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-old-style-prefixes)
;;;_  = allout-stylish-prefixes - alternating bullets
(defcustom allout-stylish-prefixes t
  "*Do fancy stuff with topic prefix bullets according to level, etc.

Non-nil enables topic creation, modification, and repositioning
functions to vary the topic bullet char (the char that marks the topic
depth) just preceding the start of the topic text) according to level.
Otherwise, only asterisks (`*') and distinctive bullets are used.

This is how an outline can look (but sans indentation) with stylish
prefixes:

    * Top level
    .* A topic
    . + One level 3 subtopic
    .  . One level 4 subtopic
    .  . A second 4 subtopic
    . + Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

This would be an outline with stylish prefixes inhibited (but the
numbered and other distinctive bullets retained):

    * Top level
    .* A topic
    . * One level 3 subtopic
    .  * One level 4 subtopic
    .  * A second 4 subtopic
    . * Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

Stylish and constant prefixes (as well as old-style prefixes) are
always respected by the topic maneuvering functions, regardless of
this variable setting.

The setting of this var is not relevant when `allout-old-style-prefixes'
is non-nil."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-stylish-prefixes)

;;;_  = allout-numbered-bullet
(defcustom allout-numbered-bullet "#"
  "*String designating bullet of topics that have auto-numbering; nil for none.

Topics having this bullet have automatic maintenance of a sibling
sequence-number tacked on, just after the bullet.  Conventionally set
to \"#\", you can set it to a bullet of your choice.  A nil value
disables numbering maintenance."
  :type '(choice (const nil) string)
  :group 'allout)
(make-variable-buffer-local 'allout-numbered-bullet)
;;;_  = allout-file-xref-bullet
(defcustom allout-file-xref-bullet "@"
  "*Bullet signifying file cross-references, for `allout-resolve-xref'.

Set this var to the bullet you want to use for file cross-references."
  :type '(choice (const nil) string)
  :group 'allout)
;;;_  = allout-presentation-padding
(defcustom allout-presentation-padding 2
  "*Presentation-format white-space padding factor, for greater indent."
  :type 'integer
  :group 'allout)

(make-variable-buffer-local 'allout-presentation-padding)

;;;_  = allout-abbreviate-flattened-numbering
(defcustom allout-abbreviate-flattened-numbering nil
  "*If non-nil, `allout-flatten-exposed-to-buffer' abbreviates topic
numbers to minimal amount with some context.  Otherwise, entire
numbers are always used."
  :type 'boolean
  :group 'allout)

;;;_ + LaTeX formatting
;;;_  - allout-number-pages
(defcustom allout-number-pages nil
  "*Non-nil turns on page numbering for LaTeX formatting of an outline."
  :type 'boolean
  :group 'allout)
;;;_  - allout-label-style
(defcustom allout-label-style "\\large\\bf"
  "*Font and size of labels for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-head-line-style
(defcustom allout-head-line-style "\\large\\sl "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-body-line-style
(defcustom allout-body-line-style " "
  "*Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title-style
(defcustom allout-title-style "\\Large\\bf"
  "*Font and size of titles for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title
(defcustom allout-title '(or buffer-file-name (buffer-name))
  "*Expression to be evaluated to determine the title for LaTeX
formatted copy."
  :type 'sexp
  :group 'allout)
;;;_  - allout-line-skip
(defcustom allout-line-skip ".05cm"
  "*Space between lines for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-indent
(defcustom allout-indent ".3cm"
  "*LaTeX formatted depth-indent spacing."
  :type 'string
  :group 'allout)

;;;_ + Topic encryption
;;;_  = allout-topic-encryption-bullet
(defcustom allout-topic-encryption-bullet "~"
  "*Bullet signifying encryption of the entry's body."
  :type '(choice (const nil) string)
  :group 'allout)
;;;_  = allout-passphrase-verifier-handling
(defcustom allout-passphrase-verifier-handling t
  "*Enable use of symmetric encryption passphrase verifier if non-nil.

See the docstring for the `allout-enable-file-variable-adjustment'
variable for details about allout ajustment of file variables."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-passphrase-verifier-handling)
;;;_  = allout-passphrase-hint-handling
(defcustom allout-passphrase-hint-handling 'always
  "*Dictate outline encryption passphrase reminder handling:

 always - always show reminder when prompting
 needed - show reminder on passphrase entry failure
 disabled - never present or adjust reminder

See the docstring for the `allout-enable-file-variable-adjustment'
variable for details about allout ajustment of file variables."
  :type '(choice (const always)
                 (const needed)
                 (const disabled))
  :group 'allout)
(make-variable-buffer-local 'allout-passphrase-hint-handling)
;;;_  = allout-encrypt-unencrypted-on-saves
(defcustom allout-encrypt-unencrypted-on-saves t
  "*When saving, should topics pending encryption be encrypted?

The idea is to prevent file-system exposure of any un-encrypted stuff, and
mostly covers both deliberate file writes and auto-saves.

 - Yes: encrypt all topics pending encryption, even if it's the one
        currently being edited.  \(In that case, the currently edited topic
        will be automatically decrypted before any user interaction, so they
        can continue editing but the copy on the file system will be
        encrypted.)
        Auto-saves will use the \"All except current topic\" mode if this
        one is selected, to avoid practical difficulties - see below.
 - All except current topic: skip the topic currently being edited, even if
       it's pending encryption.  This may expose the current topic on the
       file sytem, but avoids the nuisance of prompts for the encryption
       passphrase in the middle of editing for, eg, autosaves.
       This mode is used for auto-saves for both this option and \"Yes\".
 - No: leave it to the user to encrypt any unencrypted topics.

For practical reasons, auto-saves always use the 'except-current policy
when auto-encryption is enabled.  \(Otherwise, spurious passphrase prompts
and unavoidable timing collisions are too disruptive.)  If security for a
file requires that even the current topic is never auto-saved in the clear,
disable auto-saves for that file."

  :type '(choice (const :tag "Yes" t)
                 (const :tag "All except current topic" except-current)
                 (const :tag "No" nil))
  :group 'allout)
(make-variable-buffer-local 'allout-encrypt-unencrypted-on-saves)

;;;_ + Miscellaneous customization

;;;_  = allout-command-prefix
(defcustom allout-command-prefix "\C-c "
  "*Key sequence to be used as prefix for outline mode command key bindings.

Default is '\C-c<space>'; just '\C-c' is more short-and-sweet, if you're
willing to let allout use a bunch of \C-c keybindings."
  :type 'string
  :group 'allout)

;;;_  = allout-keybindings-list
;;; You have to reactivate allout-mode - `(allout-mode t)' - to
;;; institute changes to this var.
(defvar allout-keybindings-list ()
  "*List of `allout-mode' key / function bindings, for `allout-mode-map'.

String or vector key will be prefaced with `allout-command-prefix',
unless optional third, non-nil element is present.")
(setq allout-keybindings-list
      '(
                                        ; Motion commands:
        ("\C-n" allout-next-visible-heading)
        ("\C-p" allout-previous-visible-heading)
        ("\C-u" allout-up-current-level)
        ("\C-f" allout-forward-current-level)
        ("\C-b" allout-backward-current-level)
        ("\C-a" allout-beginning-of-current-entry)
        ("\C-e" allout-end-of-entry)
                                        ; Exposure commands:
        ("\C-i" allout-show-children)
        ("\C-s" allout-show-current-subtree)
        ("\C-h" allout-hide-current-subtree)
        ("h" allout-hide-current-subtree)
        ("\C-o" allout-show-current-entry)
        ("!" allout-show-all)
        ("x" allout-toggle-current-subtree-encryption)
                                        ; Alteration commands:
        (" " allout-open-sibtopic)
        ("." allout-open-subtopic)
        ("," allout-open-supertopic)
        ("'" allout-shift-in)
        (">" allout-shift-in)
        ("<" allout-shift-out)
        ("\C-m" allout-rebullet-topic)
        ("*" allout-rebullet-current-heading)
        ("#" allout-number-siblings)
        ("\C-k" allout-kill-line t)
        ("\C-y" allout-yank t)
        ("\M-y" allout-yank-pop t)
        ("\C-k" allout-kill-topic)
                                        ; Miscellaneous commands:
	;([?\C-\ ] allout-mark-topic)
        ("@" allout-resolve-xref)
        ("=c" allout-copy-exposed-to-buffer)
        ("=i" allout-indented-exposed-to-buffer)
	("=t" allout-latexify-exposed)
	("=p" allout-flatten-exposed-to-buffer)))

;;;_  = allout-use-hanging-indents
(defcustom allout-use-hanging-indents t
  "*If non-nil, topic body text auto-indent defaults to indent of the header.
Ie, it is indented to be just past the header prefix.  This is
relevant mostly for use with indented-text-mode, or other situations
where auto-fill occurs."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-use-hanging-indents)

;;;_  = allout-reindent-bodies
(defcustom allout-reindent-bodies (if allout-use-hanging-indents
				    'text)
  "*Non-nil enables auto-adjust of topic body hanging indent with depth shifts.

When active, topic body lines that are indented even with or beyond
their topic header are reindented to correspond with depth shifts of
the header.

A value of t enables reindent in non-programming-code buffers, ie
those that do not have the variable `comment-start' set.  A value of
`force' enables reindent whether or not `comment-start' is set."
  :type '(choice (const nil) (const t) (const text) (const force))
  :group 'allout)

(make-variable-buffer-local 'allout-reindent-bodies)

;;;_  = allout-enable-file-variable-adjustment
(defcustom allout-enable-file-variable-adjustment t
  "*If non-nil, some allout outline actions edit Emacs local file var text.

This can range from changes to existing entries, addition of new ones,
and creation of a new local variables section when necessary.

Emacs file variables adjustments are also inhibited if `enable-local-variables'
is nil.

Operations potentially causing edits include allout encryption routines.
For details, see `allout-toggle-current-subtree-encryption's docstring."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-enable-file-variable-adjustment)

;;;_* CODE - no user customizations below.

;;;_ #1 Internal Outline Formatting and Configuration
;;;_  : Version
;;;_   = allout-version
(defvar allout-version "2.2"
  "Version of currently loaded outline package.  \(allout.el)")
;;;_   > allout-version
(defun allout-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Allout Outline Mode v " allout-version)))
    (if here (insert msg))
    (message "%s" msg)
    msg))
;;;_  : Mode activation (defined here because it's referenced early)
;;;_   = allout-mode
(defvar allout-mode nil "Allout outline mode minor-mode flag.")
(make-variable-buffer-local 'allout-mode)
;;;_  : Topic header format
;;;_   = allout-regexp
(defvar allout-regexp ""
  "*Regular expression to match the beginning of a heading line.

Any line whose beginning matches this regexp is considered a
heading.  This var is set according to the user configuration vars
by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-regexp)
;;;_   = allout-bullets-string
(defvar allout-bullets-string ""
  "A string dictating the valid set of outline topic bullets.

This var should *not* be set by the user - it is set by `set-allout-regexp',
and is produced from the elements of `allout-plain-bullets-string'
and `allout-distinctive-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string)
;;;_   = allout-bullets-string-len
(defvar allout-bullets-string-len 0
  "Length of current buffers' `allout-plain-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string-len)
;;;_   = allout-line-boundary-regexp
(defvar allout-line-boundary-regexp ()
  "`allout-regexp' with outline style beginning-of-line anchor.

This is properly set when `allout-regexp' is produced by
`set-allout-regexp', so that (match-beginning 2) and (match-end
2) delimit the prefix.")
(make-variable-buffer-local 'allout-line-boundary-regexp)
;;;_   = allout-bob-regexp
(defvar allout-bob-regexp ()
  "Like `allout-line-boundary-regexp', for headers at beginning of buffer.
\(match-beginning 2) and \(match-end 2) delimit the prefix.")
(make-variable-buffer-local 'allout-bob-regexp)
;;;_   = allout-header-subtraction
(defvar allout-header-subtraction (1- (length allout-header-prefix))
  "Allout-header prefix length to subtract when computing topic depth.")
(make-variable-buffer-local 'allout-header-subtraction)
;;;_   = allout-plain-bullets-string-len
(defvar allout-plain-bullets-string-len (length allout-plain-bullets-string)
  "Length of `allout-plain-bullets-string', updated by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-plain-bullets-string-len)


;;;_   X allout-reset-header-lead (header-lead)
(defun allout-reset-header-lead (header-lead)
  "*Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  (setq allout-header-prefix header-lead)
  (setq allout-header-subtraction (1- (length allout-header-prefix)))
  (set-allout-regexp))
;;;_   X allout-lead-with-comment-string (header-lead)
(defun allout-lead-with-comment-string (&optional header-lead)
  "*Set the topic-header leading string to specified string.

Useful when for encapsulating outline structure in programming
language comments.  Returns the leading string."

  (interactive "P")
  (if (not (stringp header-lead))
      (setq header-lead (read-string
                         "String prefix for topic headers: ")))
  (setq allout-reindent-bodies nil)
  (allout-reset-header-lead header-lead)
  header-lead)
;;;_   > allout-infer-header-lead ()
(defun allout-infer-header-lead ()
  "Determine appropriate `allout-header-prefix'.

Works according to settings of:

       `comment-start'
       `allout-header-prefix' (default)
       `allout-use-mode-specific-leader'
and    `allout-mode-leaders'.

Apply this via \(re)activation of `allout-mode', rather than
invoking it directly."
  (let* ((use-leader (and (boundp 'allout-use-mode-specific-leader)
			  (if (or (stringp allout-use-mode-specific-leader)
				  (memq allout-use-mode-specific-leader
					'(allout-mode-leaders
					  comment-start
					  t)))
			      allout-use-mode-specific-leader
			    ;; Oops - garbled value, equate with effect of 't:
			    t)))
	 (leader
	  (cond
	   ((not use-leader) nil)
	   ;; Use the explicitly designated leader:
	   ((stringp use-leader) use-leader)
	   (t (or (and (memq use-leader '(t allout-mode-leaders))
		       ;; Get it from outline mode leaders?
		       (cdr (assq major-mode allout-mode-leaders)))
		  ;; ... didn't get from allout-mode-leaders...
		  (and (memq use-leader '(t comment-start))
		       comment-start
		       ;; Use comment-start, maybe tripled, and with
		       ;; underscore:
		       (concat
			(if (string= " "
				     (substring comment-start
						(1- (length comment-start))))
			    ;; Use comment-start, sans trailing space:
			    (substring comment-start 0 -1)
			  (concat comment-start comment-start comment-start))
			;; ... and append underscore, whichever:
			"_")))))))
    (if (not leader)
	nil
      (if (string= leader allout-header-prefix)
	  nil				; no change, nothing to do.
	(setq allout-header-prefix leader)
	allout-header-prefix))))
;;;_   > allout-infer-body-reindent ()
(defun allout-infer-body-reindent ()
  "Determine proper setting for `allout-reindent-bodies'.

Depends on default setting of `allout-reindent-bodies' \(which see)
and presence of setting for `comment-start', to tell whether the
file is programming code."
  (if (and allout-reindent-bodies
	   comment-start
	   (not (eq 'force allout-reindent-bodies)))
      (setq allout-reindent-bodies nil)))
;;;_   > set-allout-regexp ()
(defun set-allout-regexp ()
  "Generate proper topic-header regexp form for outline functions.

Works with respect to `allout-plain-bullets-string' and
`allout-distinctive-bullets-string'."

  (interactive)
  ;; Derive allout-bullets-string from user configured components:
  (setq allout-bullets-string "")
  (let ((strings (list 'allout-plain-bullets-string
                       'allout-distinctive-bullets-string
                       'allout-primary-bullet))
        cur-string
        cur-len
        cur-char
        index)
    (while strings
      (setq index 0)
      (setq cur-len (length (setq cur-string (symbol-value (car strings)))))
      (while (< index cur-len)
        (setq cur-char (aref cur-string index))
        (setq allout-bullets-string
              (concat allout-bullets-string
                      (cond
                                        ; Single dash would denote a
                                        ; sequence, repeated denotes
                                        ; a dash:
                       ((eq cur-char ?-) "--")
                                        ; literal close-square-bracket
                                        ; doesn't work right in the
                                        ; expr, exclude it:
                       ((eq cur-char ?\]) "")
                       (t (regexp-quote  (char-to-string cur-char))))))
        (setq index (1+ index)))
      (setq strings (cdr strings)))
    )
  ;; Derive next for repeated use in allout-pending-bullet:
  (setq allout-plain-bullets-string-len (length allout-plain-bullets-string))
  (setq allout-header-subtraction (1- (length allout-header-prefix)))
  ;; Produce the new allout-regexp:
  (setq allout-regexp (concat "\\(\\"
                               allout-header-prefix
                               "[ \t]*["
                               allout-bullets-string
                               "]\\)\\|\\"
                               allout-primary-bullet
                               "+\\|\^l"))
  (setq allout-line-boundary-regexp
        (concat "\\(\n\\)\\(" allout-regexp "\\)"))
  (setq allout-bob-regexp
        (concat "\\(\\`\\)\\(" allout-regexp "\\)"))
  )
;;;_  : Key bindings
;;;_   = allout-mode-map
(defvar allout-mode-map nil "Keybindings for (allout) outline minor mode.")
;;;_   > produce-allout-mode-map (keymap-alist &optional base-map)
(defun produce-allout-mode-map (keymap-list &optional base-map)
  "Produce keymap for use as allout-mode-map, from KEYMAP-LIST.

Built on top of optional BASE-MAP, or empty sparse map if none specified.
See doc string for allout-keybindings-list for format of binding list."
  (let ((map (or base-map (make-sparse-keymap)))
	(pref (list allout-command-prefix)))
    (mapcar (function
	     (lambda (cell)
	       (let ((add-pref (null (cdr (cdr cell))))
		     (key-suff (list (car cell))))
		 (apply 'define-key
			(list map
			      (apply 'concat (if add-pref
						 (append pref key-suff)
					       key-suff))
			      (car (cdr cell)))))))
	    keymap-list)
    map))
;;;_   = allout-prior-bindings - being deprecated.
(defvar allout-prior-bindings nil
  "Variable for use in V18, with allout-added-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_   = allout-added-bindings - being deprecated
(defvar allout-added-bindings nil
  "Variable for use in V18, with allout-prior-bindings, for
resurrecting, on mode deactivation, bindings that existed before
activation.  Being deprecated.")
;;;_  : Menu bar
(defvar allout-mode-exposure-menu)
(defvar allout-mode-editing-menu)
(defvar allout-mode-navigation-menu)
(defvar allout-mode-misc-menu)
(defun produce-allout-mode-menubar-entries ()
  (require 'easymenu)
  (easy-menu-define allout-mode-exposure-menu
		    allout-mode-map
		    "Allout outline exposure menu."
		    '("Exposure"
		      ["Show Entry" allout-show-current-entry t]
		      ["Show Children" allout-show-children t]
		      ["Show Subtree" allout-show-current-subtree t]
		      ["Hide Subtree" allout-hide-current-subtree t]
		      ["Hide Leaves" allout-hide-current-leaves t]
		      "----"
		      ["Show All" allout-show-all t]))
  (easy-menu-define allout-mode-editing-menu
		    allout-mode-map
		    "Allout outline editing menu."
		    '("Headings"
		      ["Open Sibling" allout-open-sibtopic t]
		      ["Open Subtopic" allout-open-subtopic t]
		      ["Open Supertopic" allout-open-supertopic t]
		      "----"
		      ["Shift Topic In" allout-shift-in t]
		      ["Shift Topic Out" allout-shift-out t]
		      ["Rebullet Topic" allout-rebullet-topic t]
		      ["Rebullet Heading" allout-rebullet-current-heading t]
		      ["Number Siblings" allout-number-siblings t]
		      "----"
                      ["Toggle Topic Encryption"
                       allout-toggle-current-subtree-encryption
                       (> (allout-current-depth) 1)]))
  (easy-menu-define allout-mode-navigation-menu
		    allout-mode-map
		    "Allout outline navigation menu."
		    '("Navigation"
		      ["Next Visible Heading" allout-next-visible-heading t]
		      ["Previous Visible Heading"
		       allout-previous-visible-heading t]
		      "----"
		      ["Up Level" allout-up-current-level t]
		      ["Forward Current Level" allout-forward-current-level t]
		      ["Backward Current Level"
		       allout-backward-current-level t]
		      "----"
		      ["Beginning of Entry"
		       allout-beginning-of-current-entry t]
		      ["End of Entry" allout-end-of-entry t]
		      ["End of Subtree" allout-end-of-current-subtree t]))
  (easy-menu-define allout-mode-misc-menu
		    allout-mode-map
		    "Allout outlines miscellaneous bindings."
		    '("Misc"
		      ["Version" allout-version t]
		      "----"
		      ["Duplicate Exposed" allout-copy-exposed-to-buffer t]
		      ["Duplicate Exposed, numbered"
		       allout-flatten-exposed-to-buffer t]
		      ["Duplicate Exposed, indented"
		       allout-indented-exposed-to-buffer t]
		      "----"
		      ["Set Header Lead" allout-reset-header-lead t]
		      ["Set New Exposure" allout-expose-topic t])))
;;;_  : Mode-Specific Variable Maintenance Utilities
;;;_   = allout-mode-prior-settings
(defvar allout-mode-prior-settings nil
  "Internal `allout-mode' use; settings to be resumed on mode deactivation.")
(make-variable-buffer-local 'allout-mode-prior-settings)
;;;_   > allout-resumptions (name &optional value)
(defun allout-resumptions (name &optional value)

  "Registers or resumes settings over `allout-mode' activation/deactivation.

First arg is NAME of variable affected.  Optional second arg is list
containing allout-mode-specific VALUE to be imposed on named
variable, and to be registered.  \(It's a list so you can specify
registrations of null values.)  If no value is specified, the
registered value is returned (encapsulated in the list, so the caller
can distinguish nil vs no value), and the registration is popped
from the list."

  (let ((on-list (assq name allout-mode-prior-settings))
        prior-capsule                   ; By `capsule' i mean a list
                                        ; containing a value, so we can
                                        ; distinguish nil from no value.
        )

    (if value

        ;; Registering:
        (progn
          (if on-list
              nil 	; Already preserved prior value - don't mess with it.
            ;; Register the old value, or nil if previously unbound:
            (setq allout-mode-prior-settings
                  (cons (list name
                              (if (boundp name) (list (symbol-value name))))
                        allout-mode-prior-settings)))
                                        ; And impose the new value, locally:
	  (progn (make-local-variable name)
		 (set name (car value))))

      ;; Relinquishing:
      (if (not on-list)

          ;; Oops, not registered - leave it be:
          nil

        ;; Some registration:
                                        ; reestablish it:
        (setq prior-capsule (car (cdr on-list)))
        (if prior-capsule
            (set name (car prior-capsule)) ; Some prior value - reestablish it.
          (makunbound name))		; Previously unbound - demolish var.
                                        ; Remove registration:
        (let (rebuild)
          (while allout-mode-prior-settings
            (if (not (eq (car allout-mode-prior-settings)
                         on-list))
                (setq rebuild
                      (cons (car allout-mode-prior-settings)
                            rebuild)))
            (setq allout-mode-prior-settings
                  (cdr allout-mode-prior-settings)))
          (setq allout-mode-prior-settings rebuild)))))
  )
;;;_  : Mode-specific incidentals
;;;_   > allout-unprotected (expr)
(defmacro allout-unprotected (expr)
  "Enable internal outline operations to alter invisible text."
  `(let ((inhibit-read-only t))
     ,expr))
;;;_   = allout-mode-hook
(defvar allout-mode-hook nil
  "*Hook that's run when allout mode starts.")
;;;_   = allout-overlay-category
(defvar allout-overlay-category nil
  "Symbol for use in allout invisible-text overlays as the category.")
;;;_   = allout-view-change-hook
(defvar allout-view-change-hook nil
  "*Hook that's run after allout outline visibility changes.")

;;;_   = allout-outside-normal-auto-fill-function
(defvar allout-outside-normal-auto-fill-function nil
  "Value of normal-auto-fill-function outside of allout mode.

Used by allout-auto-fill to do the mandated normal-auto-fill-function
wrapped within allout's automatic fill-prefix setting.")
(make-variable-buffer-local 'allout-outside-normal-auto-fill-function)
;;;_   = file-var-bug hack
(defvar allout-v18/19-file-var-hack nil
  "Horrible hack used to prevent invalid multiple triggering of outline
mode from prop-line file-var activation.  Used by `allout-mode' function
to track repeats.")
;;;_   = allout-file-passphrase-verifier-string
(defvar allout-file-passphrase-verifier-string nil
  "Name for use as a file variable for verifying encryption passphrase
across sessions.")
(make-variable-buffer-local 'allout-file-passphrase-verifier-string)
;;;_   = allout-passphrase-verifier-string
(defvar allout-passphrase-verifier-string nil
  "Setting used to test solicited encryption passphrases against the one
already associated with a file.

It consists of an encrypted random string useful only to verify that a
passphrase entered by the user is effective for decryption.  The passphrase
itself is \*not* recorded in the file anywhere, and the encrypted contents
are random binary characters to avoid exposing greater susceptibility to
search attacks.

The verifier string is retained as an Emacs file variable, as well as in
the emacs buffer state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-passphrase-verifier-string)
;;;_   = allout-passphrase-hint-string
(defvar allout-passphrase-hint-string ""
  "Variable used to retain reminder string for file's encryption passphrase.

See the description of `allout-passphrase-hint-handling' for details about how
the reminder is deployed.

The hint is retained as an Emacs file variable, as well as in the emacs buffer
state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-passphrase-hint-string)
(setq-default allout-passphrase-hint-string "")
;;;_   = allout-after-save-decrypt
(defvar allout-after-save-decrypt nil
  "Internal variable, is nil or has the value of two points:

 - the location of a topic to be decrypted after saving is done
 - where to situate the cursor after the decryption is performed

This is used to decrypt the topic that was currently being edited, if it
was encrypted automatically as part of a file write or autosave.")
(make-variable-buffer-local 'allout-after-save-decrypt)
;;;_   > allout-mode-p ()
;; Must define this macro above any uses, or byte compilation will lack
;; proper def, if file isn't loaded - eg, during emacs build!
(defmacro allout-mode-p ()
  "Return t if `allout-mode' is active in current buffer."
  'allout-mode)
;;;_   > allout-write-file-hook-handler ()
(defun allout-write-file-hook-handler ()
  "Implement `allout-encrypt-unencrypted-on-saves' policy for file writes."

  (if (or (not (allout-mode-p))
          (not (boundp 'allout-encrypt-unencrypted-on-saves))
          (not allout-encrypt-unencrypted-on-saves))
      nil
    (let ((except-mark (and (equal allout-encrypt-unencrypted-on-saves
                                   'except-current)
                            (point-marker))))
      (if (save-excursion (goto-char (point-min))
                          (allout-next-topic-pending-encryption except-mark))
          (progn
            (message "auto-encrypting pending topics")
            (sit-for 0)
            (condition-case failure
                (setq allout-after-save-decrypt
                      (allout-encrypt-decrypted except-mark))
              (error (progn
                       (message
                        "allout-write-file-hook-handler suppressing error %s"
                        failure)
                       (sit-for 2))))))
      ))
    nil)
;;;_   > allout-auto-save-hook-handler ()
(defun allout-auto-save-hook-handler ()
  "Implement `allout-encrypt-unencrypted-on-saves' policy for auto save."

  (if (and (allout-mode-p) allout-encrypt-unencrypted-on-saves)
      ;; Always implement 'except-current policy when enabled.
      (let ((allout-encrypt-unencrypted-on-saves 'except-current))
        (allout-write-file-hook-handler))))
;;;_   > allout-after-saves-handler ()
(defun allout-after-saves-handler ()
  "Decrypt topic encrypted for save, if it's currently being edited.

Ie, if it was pending encryption and contained the point in its body before
the save.

We use values stored in `allout-after-save-decrypt' to locate the topic
and the place for the cursor after the decryption is done."
  (if (not (and (allout-mode-p)
                (boundp 'allout-after-save-decrypt)
                allout-after-save-decrypt))
      t
    (goto-char (car allout-after-save-decrypt))
    (let ((was-modified (buffer-modified-p)))
      (allout-toggle-subtree-encryption)
      (if (not was-modified)
          (set-buffer-modified-p nil)))
    (goto-char (cadr allout-after-save-decrypt))
    (setq allout-after-save-decrypt nil))
  )

;;;_ #2 Mode activation
;;;_  = allout-explicitly-deactivated
(defvar allout-explicitly-deactivated nil
  "If t, `allout-mode's last deactivation was deliberate.
So `allout-post-command-business' should not reactivate it...")
(make-variable-buffer-local 'allout-explicitly-deactivated)
;;;_  > allout-init (&optional mode)
(defun allout-init (&optional mode)
  "Prime `allout-mode' to enable/disable auto-activation, wrt `allout-layout'.

MODE is one of the following symbols:

 - nil \(or no argument) deactivate auto-activation/layout;
 - `activate', enable auto-activation only;
 - `ask', enable auto-activation, and enable auto-layout but with
   confirmation for layout operation solicited from user each time;
 - `report', just report and return the current auto-activation state;
 - anything else \(eg, t) for auto-activation and auto-layout, without
   any confirmation check.

Use this function to setup your Emacs session for automatic activation
of allout outline mode, contingent to the buffer-specific setting of
the `allout-layout' variable.  (See `allout-layout' and
`allout-expose-topic' docstrings for more details on auto layout).

`allout-init' works by setting up (or removing) the `allout-mode'
find-file-hook, and giving `allout-auto-activation' a suitable
setting.

To prime your Emacs session for full auto-outline operation, include
the following two lines in your Emacs init file:

\(require 'allout)
\(allout-init t)"

  (interactive)
  (if (interactive-p)
      (progn
	(setq mode
	      (completing-read
	       (concat "Select outline auto setup mode "
		       "(empty for report, ? for options) ")
	       '(("nil")("full")("activate")("deactivate")
		 ("ask") ("report") (""))
	       nil
	       t))
	(if (string= mode "")
	    (setq mode 'report)
	  (setq mode (intern-soft mode)))))
  (let
      ;; convenience aliases, for consistent ref to respective vars:
      ((hook 'allout-find-file-hook)
       (find-file-hook-var-name (if (boundp 'find-file-hook)
                                    'find-file-hook
                                  'find-file-hooks))
       (curr-mode 'allout-auto-activation))

    (cond ((not mode)
	   (set find-file-hook-var-name
                (delq hook (symbol-value find-file-hook-var-name)))
	   (if (interactive-p)
	       (message "Allout outline mode auto-activation inhibited.")))
	  ((eq mode 'report)
	   (if (not (memq hook (symbol-value find-file-hook-var-name)))
	       (allout-init nil)
	     ;; Just punt and use the reports from each of the modes:
	     (allout-init (symbol-value curr-mode))))
	  (t (add-hook find-file-hook-var-name hook)
	     (set curr-mode		; `set', not `setq'!
		  (cond ((eq mode 'activate)
			 (message
			  "Outline mode auto-activation enabled.")
			 'activate)
			((eq mode 'report)
			 ;; Return the current mode setting:
			 (allout-init mode))
			((eq mode 'ask)
			 (message
			  (concat "Outline mode auto-activation and "
				  "-layout \(upon confirmation) enabled."))
			 'ask)
			((message
			  "Outline mode auto-activation and -layout enabled.")
			 'full)))))))
;;;_  > allout-setup-menubar ()
(defun allout-setup-menubar ()
  "Populate the current buffer's menubar with `allout-mode' stuff."
  (let ((menus (list allout-mode-exposure-menu
		     allout-mode-editing-menu
		     allout-mode-navigation-menu
		     allout-mode-misc-menu))
	cur)
    (while menus
      (setq cur (car menus)
	    menus (cdr menus))
      (easy-menu-add cur))))
;;;_  > allout-set-overlay-category
(defun allout-set-overlay-category ()
  "Set the properties of the allout invisible-text overlay."
  (setplist 'allout-overlay-category nil)
  (put 'allout-overlay-category 'invisible 'allout)
  (put 'allout-overlay-category 'evaporate t)
  ;; XXX We use isearch-open-invisible *and* isearch-mode-end-hook.  The
  ;; latter would be sufficient, but it seems that a separate behavior -
  ;; the _transient_ opening of invisible text during isearch - is keyed to
  ;; presence of the isearch-open-invisible property - even though this
  ;; property controls the isearch _arrival_ behavior.  This is the case at
  ;; least in emacs 21, 22.0, and xemacs 21.4.
  (put 'allout-overlay-category 'isearch-open-invisible
       'allout-isearch-end-handler)
  (if (featurep 'xemacs)
      (put 'allout-overlay-category 'start-open t)
    (put 'allout-overlay-category 'insert-in-front-hooks
         '(allout-overlay-insert-in-front-handler)))
  (if (featurep 'xemacs)
      (progn (make-variable-buffer-local 'before-change-functions)
             (add-hook 'before-change-functions
                       'allout-before-change-handler))
    (put 'allout-overlay-category 'modification-hooks
         '(allout-overlay-interior-modification-handler))))
;;;_  > allout-mode (&optional toggle)
;;;_   : Defun:
;;;###autoload
(defun allout-mode (&optional toggle)
;;;_    . Doc string:
  "Toggle minor mode for controlling exposure and editing of text outlines.
\\<allout-mode-map>

Optional arg forces mode to re-initialize iff arg is positive num or
symbol.  Allout outline mode always runs as a minor mode.

Allout outline mode provides extensive outline oriented formatting and
manipulation.  It enables structural editing of outlines, as well as
navigation and exposure.  It also is specifically aimed at
accommodating syntax-sensitive text like programming languages.  \(For
an example, see the allout code itself, which is organized as an allout
outline.)

In addition to outline navigation and exposure, allout includes:

 - topic-oriented repositioning, promotion/demotion, cut, and paste
 - integral outline exposure-layout
 - incremental search with dynamic exposure and reconcealment of hidden text
 - automatic topic-number maintenance
 - easy topic encryption and decryption
 - \"Hot-spot\" operation, for single-keystroke maneuvering and
    exposure control.  \(See the allout-mode docstring.)

and many other features.

Below is a description of the bindings, and then explanation of
special `allout-mode' features and terminology.  See also the outline
menubar additions for quick reference to many of the features, and see
the docstring of the function `allout-init' for instructions on
priming your emacs session for automatic activation of `allout-mode'.


The bindings are dictated by the `allout-keybindings-list' and
`allout-command-prefix' variables.

	Navigation:				   Exposure Control:
	----------                                 ----------------
\\[allout-next-visible-heading] allout-next-visible-heading     | \\[allout-hide-current-subtree] allout-hide-current-subtree
\\[allout-previous-visible-heading] allout-previous-visible-heading | \\[allout-show-children] allout-show-children
\\[allout-up-current-level] allout-up-current-level         | \\[allout-show-current-subtree] allout-show-current-subtree
\\[allout-forward-current-level] allout-forward-current-level    | \\[allout-show-current-entry] allout-show-current-entry
\\[allout-backward-current-level] allout-backward-current-level   | \\[allout-show-all] allout-show-all
\\[allout-end-of-entry] allout-end-of-entry
\\[allout-beginning-of-current-entry] allout-beginning-of-current-entry, alternately, goes to hot-spot

	Topic Header Production:
	-----------------------
\\[allout-open-sibtopic]	allout-open-sibtopic	Create a new sibling after current topic.
\\[allout-open-subtopic]	allout-open-subtopic	... an offspring of current topic.
\\[allout-open-supertopic]	allout-open-supertopic	... a sibling of the current topic's parent.

	Topic Level and Prefix Adjustment:
	---------------------------------
\\[allout-shift-in]	allout-shift-in	Shift current topic and all offspring deeper.
\\[allout-shift-out]	allout-shift-out	... less deep.
\\[allout-rebullet-current-heading]	allout-rebullet-current-heading Prompt for alternate bullet for
					 current topic.
\\[allout-rebullet-topic]	allout-rebullet-topic	Reconcile bullets of topic and its offspring
				- distinctive bullets are not changed, others
				  alternated according to nesting depth.
\\[allout-number-siblings]	allout-number-siblings	Number bullets of topic and siblings - the
				offspring are not affected.  With repeat
				count, revoke numbering.

	Topic-oriented Killing and Yanking:
	----------------------------------
\\[allout-kill-topic]	allout-kill-topic	Kill current topic, including offspring.
\\[allout-kill-line]	allout-kill-line	Like kill-line, but reconciles numbering, etc.
\\[allout-yank]	allout-yank		Yank, adjusting depth of yanked topic to
				depth of heading if yanking into bare topic
				heading (ie, prefix sans text).
\\[allout-yank-pop]	allout-yank-pop	Is to allout-yank as yank-pop is to yank

	Topic-oriented Encryption:
	-------------------------
\\[allout-toggle-current-subtree-encryption]	allout-toggle-current-subtree-encryption Encrypt/Decrypt topic content

	Misc commands:
	-------------
M-x outlineify-sticky		Activate outline mode for current buffer,
				and establish a default file-var setting
				for `allout-layout'.
\\[allout-mark-topic]   	allout-mark-topic
\\[allout-copy-exposed-to-buffer]     allout-copy-exposed-to-buffer
				Duplicate outline, sans concealed text, to
				buffer with name derived from derived from that
				of current buffer - \"*BUFFERNAME exposed*\".
\\[allout-flatten-exposed-to-buffer]	allout-flatten-exposed-to-buffer
				Like above 'copy-exposed', but convert topic
				prefixes to section.subsection... numeric
				format.
\\[eval-expression] (allout-init t)	Setup Emacs session for outline mode
				auto-activation.

                  Topic Encryption

Outline mode supports gpg encryption of topics, with support for
symmetric and key-pair modes, passphrase timeout, passphrase
consistency checking, user-provided hinting for symmetric key
mode, and auto-encryption of topics pending encryption on save.
\(Topics pending encryption are, by default, automatically
encrypted during file saves; if you're editing the contents of
such a topic, it is automatically decrypted for continued
editing.)  The aim is reliable topic privacy while preventing
accidents like neglected encryption before saves, forgetting
which passphrase was used, and other practical pitfalls.

See `allout-toggle-current-subtree-encryption' function docstring and
`allout-encrypt-unencrypted-on-saves' customization variable for details.

		 HOT-SPOT Operation

Hot-spot operation provides a means for easy, single-keystroke outline
navigation and exposure control.

When the text cursor is positioned directly on the bullet character of
a topic, regular characters (a to z) invoke the commands of the
corresponding allout-mode keymap control chars.  For example, \"f\"
would invoke the command typically bound to \"C-c<space>C-f\"
\(\\[allout-forward-current-level] `allout-forward-current-level').

Thus, by positioning the cursor on a topic bullet, you can
execute the outline navigation and manipulation commands with a
single keystroke.  Regular navigation keys (eg, \\[forward-char], \\[next-line]) never get
this special translation, so you can use them to get out of the
hot-spot and back to normal operation.

Note that the command `allout-beginning-of-current-entry' \(\\[allout-beginning-of-current-entry]\)
will move to the hot-spot when the cursor is already located at the
beginning of the current entry, so you usually can hit \\[allout-beginning-of-current-entry]
twice in a row to get to the hot-spot.

			    Terminology

Topic hierarchy constituents - TOPICS and SUBTOPICS:

TOPIC:	A basic, coherent component of an Emacs outline.  It can
	contain and be contained by other topics.
CURRENT topic:
	The visible topic most immediately containing the cursor.
DEPTH:	The degree of nesting of a topic; it increases with
	containment.  Also called the:
LEVEL:	The same as DEPTH.

ANCESTORS:
	The topics that contain a topic.
PARENT:	A topic's immediate ancestor.  It has a depth one less than
	the topic.
OFFSPRING:
	The topics contained by a topic;
SUBTOPIC:
	An immediate offspring of a topic;
CHILDREN:
	The immediate offspring of a topic.
SIBLINGS:
	Topics having the same parent and depth.

Topic text constituents:

HEADER:	The first line of a topic, include the topic PREFIX and header
	text.
PREFIX: The leading text of a topic which distinguishes it from normal
        text.  It has a strict form, which consists of a prefix-lead
        string, padding, and a bullet.  The bullet may be followed by a
        number, indicating the ordinal number of the topic among its
        siblings, a space, and then the header text.

	The relative length of the PREFIX determines the nesting depth
	of the topic.
PREFIX-LEAD:
	The string at the beginning of a topic prefix, normally a `.'.
	It can be customized by changing the setting of
	`allout-header-prefix' and then reinitializing `allout-mode'.

	By setting the prefix-lead to the comment-string of a
	programming language, you can embed outline structuring in
	program code without interfering with the language processing
	of that code.  See `allout-use-mode-specific-leader'
	docstring for more detail.
PREFIX-PADDING:
	Spaces or asterisks which separate the prefix-lead and the
	bullet, determining the depth of the topic.
BULLET: A character at the end of the topic prefix, it must be one of
	the characters listed on `allout-plain-bullets-string' or
        `allout-distinctive-bullets-string'.  (See the documentation
        for these variables for more details.)  The default choice of
	bullet when generating topics varies in a cycle with the depth of
	the topic.
ENTRY:	The text contained in a topic before any offspring.
BODY:	Same as ENTRY.


EXPOSURE:
 	The state of a topic which determines the on-screen visibility
	of its offspring and contained text.
CONCEALED:
	Topics and entry text whose display is inhibited.  Contiguous
	units of concealed text is represented by `...' ellipses.

	Concealed topics are effectively collapsed within an ancestor.
CLOSED:	A topic whose immediate offspring and body-text is concealed.
OPEN:	A topic that is not closed, though its offspring or body may be."
;;;_    . Code
  (interactive "P")

  (let* ((active (and (not (equal major-mode 'outline))
		     (allout-mode-p)))
				       ; Massage universal-arg `toggle' val:
	 (toggle (and toggle
		     (or (and (listp toggle)(car toggle))
			 toggle)))
				       ; Activation specifically demanded?
	 (explicit-activation (and toggle
				   (or (symbolp toggle)
				       (and (wholenump toggle)
					    (not (zerop toggle))))))
	 ;; allout-mode already called once during this complex command?
	 (same-complex-command (eq allout-v18/19-file-var-hack
				  (car command-history)))
         (write-file-hook-var-name (cond ((boundp 'write-file-functions)
                                          'write-file-functions)
                                         ((boundp 'write-file-hooks)
                                          'write-file-hooks)
                                         (t 'local-write-file-hooks)))
	 do-layout
	 )

				       ; See comments below re v19.18,.19 bug.
    (setq allout-v18/19-file-var-hack (car command-history))

    (cond

     ;; Provision for v19.18, 19.19 bug -
     ;; Emacs v 19.18, 19.19 file-var code invokes prop-line-designated
     ;; modes twice when file is visited.  We have to avoid toggling mode
     ;; off on second invocation, so we detect it as best we can, and
     ;; skip everything.
     ((and same-complex-command		; Still in same complex command
                                        ; as last time `allout-mode' invoked.
	  active			; Already activated.
	  (not explicit-activation)	; Prop-line file-vars don't have args.
	  (string-match "^19.1[89]"	; Bug only known to be in v19.18 and
			emacs-version)); 19.19.
      t)

     ;; Deactivation:
     ((and (not explicit-activation)
	  (or active toggle))
				       ; Activation not explicitly
				       ; requested, and either in
				       ; active state or *de*activation
				       ; specifically requested:
      (setq allout-explicitly-deactivated t)
      (if (string-match "^18\." emacs-version)
				       ; Revoke those keys that remain
				       ; as we set them:
	  (let ((curr-loc (current-local-map)))
	   (mapcar (function
		    (lambda (cell)
		      (if (eq (lookup-key curr-loc (car cell))
			      (car (cdr cell)))
			  (define-key curr-loc (car cell)
			    (assq (car cell) allout-prior-bindings)))))
		   allout-added-bindings)
	   (allout-resumptions 'allout-added-bindings)
	   (allout-resumptions 'allout-prior-bindings)))

      (if allout-old-style-prefixes
	  (progn
	   (allout-resumptions 'allout-primary-bullet)
	   (allout-resumptions 'allout-old-style-prefixes)))
      ;;(allout-resumptions 'selective-display)
      (remove-from-invisibility-spec '(allout . t))
      (set write-file-hook-var-name
	   (delq 'allout-write-file-hook-handler
                 (symbol-value write-file-hook-var-name)))
      (setq auto-save-hook
	   (delq 'allout-auto-save-hook-handler
		 auto-save-hook))
      (allout-resumptions 'paragraph-start)
      (allout-resumptions 'paragraph-separate)
      (allout-resumptions 'auto-fill-function)
      (allout-resumptions 'normal-auto-fill-function)
      (allout-resumptions 'allout-former-auto-filler)
      (setq allout-mode nil))

     ;; Activation:
     ((not active)
      (setq allout-explicitly-deactivated nil)
      (if allout-old-style-prefixes
	  (progn			; Inhibit all the fancy formatting:
	   (allout-resumptions 'allout-primary-bullet '("*"))
	   (allout-resumptions 'allout-old-style-prefixes '(()))))

      (allout-set-overlay-category)     ; Doesn't hurt to redo this.

      (allout-infer-header-lead)
      (allout-infer-body-reindent)

      (set-allout-regexp)

				       ; Produce map from current version
				       ; of allout-keybindings-list:
      (if (boundp 'minor-mode-map-alist)

	  (progn			; V19, and maybe lucid and
				       ; epoch, minor-mode key bindings:
	   (setq allout-mode-map
		 (produce-allout-mode-map allout-keybindings-list))
	   (produce-allout-mode-menubar-entries)
	   (fset 'allout-mode-map allout-mode-map)
				       ; Include on minor-mode-map-alist,
				       ; if not already there:
	   (if (not (member '(allout-mode . allout-mode-map)
			    minor-mode-map-alist))
	       (setq minor-mode-map-alist
		     (cons '(allout-mode . allout-mode-map)
			   minor-mode-map-alist))))

				       ; V18 minor-mode key bindings:
				       ; Stash record of added bindings
				       ; for later revocation:
	(allout-resumptions 'allout-added-bindings
			    (list allout-keybindings-list))
	(allout-resumptions 'allout-prior-bindings
			    (list (current-local-map)))
				       ; and add them:
	(use-local-map (produce-allout-mode-map allout-keybindings-list
						(current-local-map)))
	)

      (add-to-invisibility-spec '(allout . t))
      (make-local-variable 'line-move-ignore-invisible)
      (setq line-move-ignore-invisible t)
      (add-hook 'pre-command-hook 'allout-pre-command-business)
      (add-hook 'post-command-hook 'allout-post-command-business)
      (add-hook 'isearch-mode-end-hook 'allout-isearch-end-handler)
      (add-hook write-file-hook-var-name 'allout-write-file-hook-handler)
      (add-hook 'auto-save-hook 'allout-auto-save-hook-handler)
				       ; Custom auto-fill func, to support
				       ; respect for topic headline,
				       ; hanging-indents, etc:
      ;; Register prevailing fill func for use by allout-auto-fill:
      (allout-resumptions 'allout-former-auto-filler (list auto-fill-function))
      ;; Register allout-auto-fill to be used if filling is active:
      (allout-resumptions 'auto-fill-function '(allout-auto-fill))
      (allout-resumptions 'allout-outside-normal-auto-fill-function
                          (list normal-auto-fill-function))
      (allout-resumptions 'normal-auto-fill-function '(allout-auto-fill))
      ;; Paragraphs are broken by topic headlines.
      (make-local-variable 'paragraph-start)
      (allout-resumptions 'paragraph-start
			  (list (concat paragraph-start "\\|^\\("
					allout-regexp "\\)")))
      (make-local-variable 'paragraph-separate)
      (allout-resumptions 'paragraph-separate
			  (list (concat paragraph-separate "\\|^\\("
					allout-regexp "\\)")))

      (or (assq 'allout-mode minor-mode-alist)
	  (setq minor-mode-alist
	       (cons '(allout-mode " Allout") minor-mode-alist)))

      (allout-setup-menubar)

      (if allout-layout
	  (setq do-layout t))

      (run-hooks 'allout-mode-hook)
      (setq allout-mode t))

     ;; Reactivation:
     ((setq do-layout t)
      (allout-infer-body-reindent))
     )					; cond

    (if (and do-layout
	     allout-auto-activation
	     (listp allout-layout)
	     (and (not (eq allout-auto-activation 'activate))
		  (if (eq allout-auto-activation 'ask)
		      (if (y-or-n-p (format "Expose %s with layout '%s'? "
					    (buffer-name)
					    allout-layout))
			  t
			(message "Skipped %s layout." (buffer-name))
			nil)
		    t)))
	(save-excursion
	  (message "Adjusting '%s' exposure..." (buffer-name))
	  (goto-char 0)
	  (allout-this-or-next-heading)
	  (condition-case err
	      (progn
		(apply 'allout-expose-topic (list allout-layout))
		(message "Adjusting '%s' exposure... done." (buffer-name)))
	    ;; Problem applying exposure - notify user, but don't
	    ;; interrupt, eg, file visit:
	    (error (message "%s" (car (cdr err)))
		   (sit-for 1)))))
    allout-mode
    )					; let*
  )  					; defun

;;;_  - Position Assessment
;;;_   > allout-hidden-p (&optional pos)
(defsubst allout-hidden-p (&optional pos)
  "Non-nil if the character after point is invisible."
  (get-char-property (or pos (point)) 'invisible))

;;;_  > allout-minor-mode
(defalias 'allout-minor-mode 'allout-mode)

;;;_  > allout-overlay-insert-in-front-handler (ol after beg end
;;;                                                &optional prelen)
(defun allout-overlay-insert-in-front-handler (ol after beg end
                                                  &optional prelen)
  "Shift the overlay so stuff inserted in front of it are excluded."
  (if after
      (move-overlay ol (1+ beg) (overlay-end ol))))
;;;_  > allout-overlay-interior-modification-handler (ol after beg end
;;;                                                      &optional prelen)
(defun allout-overlay-interior-modification-handler (ol after beg end
                                                        &optional prelen)
  "Get confirmation before making arbitrary changes to invisible text.

We expose the invisible text and ask for confirmation.  Refusal or
keyboard-quit abandons the changes, with keyboard-quit additionally
reclosing the opened text.

No confirmation is necessary when inhibit-read-only is set - eg, allout
internal functions use this feature cohesively bunch changes."

  (when (and (not inhibit-read-only) (not after))
    (let ((start (point))
          (ol-start (overlay-start ol))
          (ol-end (overlay-end ol))
          (msg "Change within concealed text disallowed.")
          opened
          first)
      (goto-char beg)
      (while (< (point) end)
        (when (allout-hidden-p)
          (allout-show-to-offshoot)
          (if (allout-hidden-p)
              (save-excursion (forward-char 1)
                              (allout-show-to-offshoot)))
          (when (not first)
            (setq opened t)
            (setq first (point))))
        (goto-char (if (featurep 'xemacs)
                       (next-property-change (1+ (point)) nil end)
                     (next-char-property-change (1+ (point)) end))))
      (when first
        (goto-char first)
        (condition-case nil
            (if (not
                 (yes-or-no-p
                  (substitute-command-keys
                   (concat "Modify this concealed text?  (\"no\" aborts,"
                           " \\[keyboard-quit] also reconceals) "))))
                (progn (goto-char start)
                       (error "Concealed-text change refused.")))
          (quit (allout-flag-region ol-start ol-end nil)
                (allout-flag-region ol-start ol-end t)
                (error "Concealed-text change abandoned, text reconcealed."))))
      (goto-char start))))
;;;_  > allout-before-change-handler (beg end)
(defun allout-before-change-handler (beg end)
  "Protect against changes to invisible text.

See allout-overlay-interior-modification-handler for details.

This before-change handler is used only where modification-hooks
overlay property is not supported."
  (if (not allout-mode)
      nil
    (allout-overlay-interior-modification-handler nil nil beg end nil)))
;;;_  > allout-isearch-end-handler (&optional overlay)
(defun allout-isearch-end-handler (&optional overlay)
  "Reconcile allout outline exposure on arriving in hidden text after isearch.

Optional OVERLAY parameter is for when this function is used by
`isearch-open-invisible' overlay property.  It is otherwise unused, so this
function can also be used as an `isearch-mode-end-hook'."

  (if (and (allout-mode-p) (allout-hidden-p))
      (allout-show-to-offshoot)))

;;;_ #3 Internal Position State-Tracking - "allout-recent-*" funcs
;;; All the basic outline functions that directly do string matches to
;;; evaluate heading prefix location set the variables
;;; `allout-recent-prefix-beginning'  and `allout-recent-prefix-end'
;;; when successful.  Functions starting with `allout-recent-' all
;;; use this state, providing the means to avoid redundant searches
;;; for just-established data.  This optimization can provide
;;; significant speed improvement, but it must be employed carefully.
;;;_  = allout-recent-prefix-beginning
(defvar allout-recent-prefix-beginning 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-beginning)
;;;_  = allout-recent-prefix-end
(defvar allout-recent-prefix-end 0
  "Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-end)
;;;_  = allout-recent-end-of-subtree
(defvar allout-recent-end-of-subtree 0
  "Buffer point last returned by `allout-end-of-current-subtree'.")
(make-variable-buffer-local 'allout-recent-end-of-subtree)
;;;_  > allout-prefix-data (beg end)
(defmacro allout-prefix-data (beg end)
  "Register allout-prefix state data - BEGINNING and END of prefix.

For reference by `allout-recent' funcs.  Returns BEGINNING."
  `(setq allout-recent-prefix-end ,end
         allout-recent-prefix-beginning ,beg))
;;;_  > allout-recent-depth ()
(defmacro allout-recent-depth ()
  "Return depth of last heading encountered by an outline maneuvering function.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth."

  '(max 1 (- allout-recent-prefix-end
	     allout-recent-prefix-beginning
	     allout-header-subtraction)))
;;;_  > allout-recent-prefix ()
(defmacro allout-recent-prefix ()
  "Like `allout-recent-depth', but returns text of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth."
  '(buffer-substring allout-recent-prefix-beginning
		     allout-recent-prefix-end))
;;;_  > allout-recent-bullet ()
(defmacro allout-recent-bullet ()
  "Like allout-recent-prefix, but returns bullet of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth of the most recently matched topic."
  '(buffer-substring (1- allout-recent-prefix-end)
		     allout-recent-prefix-end))

;;;_ #4 Navigation

;;;_   : Location Predicates
;;;_    > allout-on-current-heading-p ()
(defun allout-on-current-heading-p ()
  "Return non-nil if point is on current visible topics' header line.

Actually, returns prefix beginning point."
  (save-excursion
    (allout-beginning-of-current-line)
    (and (looking-at allout-regexp)
	 (allout-prefix-data (match-beginning 0) (match-end 0)))))
;;;_    > allout-on-heading-p ()
(defalias 'allout-on-heading-p 'allout-on-current-heading-p)
;;;_    > allout-e-o-prefix-p ()
(defun allout-e-o-prefix-p ()
  "True if point is located where current topic prefix ends, heading begins."
  (and (save-excursion (beginning-of-line)
		       (looking-at allout-regexp))
       (= (point)(save-excursion (allout-end-of-prefix)(point)))))
;;;_   : Location attributes
;;;_    > allout-depth ()
(defun allout-depth ()
  "Return depth of topic most immediately containing point.

Return zero if point is not within any topic.

Like `allout-current-depth', but respects hidden as well as visible topics."
  (save-excursion
    (let ((start-point (point)))
      (if (and (allout-goto-prefix)
               (not (< start-point (point))))
          (allout-recent-depth)
        (progn
          ;; Oops, no prefix, zero prefix data:
          (allout-prefix-data (point)(point))
          ;; ... and return 0:
          0)))))
;;;_    > allout-current-depth ()
(defun allout-current-depth ()
  "Return depth of visible topic most immediately containing point.

Return zero if point is not within any topic."
  (save-excursion
    (if (allout-back-to-current-heading)
        (max 1
             (- allout-recent-prefix-end
                allout-recent-prefix-beginning
                allout-header-subtraction))
      0)))
;;;_    > allout-get-current-prefix ()
(defun allout-get-current-prefix ()
  "Topic prefix of the current topic."
  (save-excursion
    (if (allout-goto-prefix)
	(allout-recent-prefix))))
;;;_    > allout-get-bullet ()
(defun allout-get-bullet ()
  "Return bullet of containing topic (visible or not)."
  (save-excursion
    (and (allout-goto-prefix)
	 (allout-recent-bullet))))
;;;_    > allout-current-bullet ()
(defun allout-current-bullet ()
  "Return bullet of current (visible) topic heading, or none if none found."
  (condition-case nil
      (save-excursion
	(allout-back-to-current-heading)
	(buffer-substring (- allout-recent-prefix-end 1)
			  allout-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    ('args-out-of-range nil))
  )
;;;_    > allout-get-prefix-bullet (prefix)
(defun allout-get-prefix-bullet (prefix)
  "Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match allout-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))
;;;_    > allout-sibling-index (&optional depth)
(defun allout-sibling-index (&optional depth)
  "Item number of this prospective topic among its siblings.

If optional arg DEPTH is greater than current depth, then we're
opening a new level, and return 0.

If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (allout-depth)))
           (let ((index 1))
             (while (allout-previous-sibling (allout-recent-depth) nil)
	       (setq index (1+ index)))
             index))
          ((< depth (allout-recent-depth))
           (allout-ascend-to-depth depth)
           (allout-sibling-index))
          (0))))
;;;_    > allout-topic-flat-index ()
(defun allout-topic-flat-index ()
  "Return a list indicating point's numeric section.subsect.subsubsect...
Outermost is first."
  (let* ((depth (allout-depth))
	 (next-index (allout-sibling-index depth))
	 (rev-sibls nil))
    (while (> next-index 0)
      (setq rev-sibls (cons next-index rev-sibls))
      (setq depth (1- depth))
      (setq next-index (allout-sibling-index depth)))
    rev-sibls)
  )

;;;_  - Navigation routines
;;;_   > allout-beginning-of-current-line ()
(defun allout-beginning-of-current-line ()
  "Like beginning of line, but to visible text."

  ;; XXX We would use `(move-beginning-of-line 1)', but it gets
  ;; stuck on some hidden newlines, eg at column 80, as of GNU Emacs 22.0.50.
  ;; Conversely, `beginning-of-line' can make no progress in other
  ;; situations.  Both are necessary, in the order used below.
  (move-beginning-of-line 1)
  (beginning-of-line)
  (while (or (not (bolp)) (allout-hidden-p))
    (beginning-of-line)
    (if (or (allout-hidden-p) (not (bolp)))
        (forward-char -1))))
;;;_   > allout-end-of-current-line ()
(defun allout-end-of-current-line ()
  "Move to the end of line, past concealed text if any."
  ;; XXX This is for symmetry with `allout-beginning-of-current-line' -
  ;; `move-end-of-line' doesn't suffer the same problem as
  ;; `move-beginning-of-line'.
  (end-of-line)
  (while (allout-hidden-p)
    (end-of-line)
    (if (allout-hidden-p) (forward-char 1))))
;;;_   > allout-next-heading ()
(defsubst allout-next-heading ()
  "Move to the heading for the topic \(possibly invisible) before this one.

Returns the location of the heading, or nil if none found."

  (if (and (bobp) (not (eobp)))
       (forward-char 1))

  (if (re-search-forward allout-line-boundary-regexp nil 0)
      (allout-prefix-data		; Got valid location state - set vars:
       (goto-char (or (match-beginning 2)
		      allout-recent-prefix-beginning))
       (or (match-end 2) allout-recent-prefix-end))))
;;;_   > allout-this-or-next-heading
(defun allout-this-or-next-heading ()
  "Position cursor on current or next heading."
  ;; A throwaway non-macro that is defined after allout-next-heading
  ;; and usable by allout-mode.
  (if (not (allout-goto-prefix)) (allout-next-heading)))
;;;_   > allout-previous-heading ()
(defmacro allout-previous-heading ()
  "Move to the prior \(possibly invisible) heading line.

Return the location of the beginning of the heading, or nil if not found."

  '(if (bobp)
       nil
     (allout-goto-prefix)
     (if
	 ;; searches are unbounded and return nil if failed:
	 (or (re-search-backward allout-line-boundary-regexp nil 0)
	     (looking-at allout-bob-regexp))
	 (progn				; Got valid location state - set vars:
	   (allout-prefix-data
	    (goto-char (or (match-beginning 2)
			   allout-recent-prefix-beginning))
	    (or (match-end 2) allout-recent-prefix-end))))))
;;;_   > allout-get-invisibility-overlay ()
(defun allout-get-invisibility-overlay ()
  "Return the overlay at point that dictates allout invisibility."
  (let ((overlays (overlays-at (point)))
        got)
    (while (and overlays (not got))
      (if (equal (overlay-get (car overlays) 'invisible) 'allout)
          (setq got (car overlays))))
    got))
;;;_   > allout-back-to-visible-text ()
(defun allout-back-to-visible-text ()
  "Move to most recent prior character that is visible, and return point."
    (if (allout-hidden-p)
      (goto-char (overlay-start (allout-get-invisibility-overlay))))
    (point))

;;;_  - Subtree Charting
;;;_   " These routines either produce or assess charts, which are
;;; nested lists of the locations of topics within a subtree.
;;;
;;; Use of charts enables efficient navigation of subtrees, by
;;; requiring only a single regexp-search based traversal, to scope
;;; out the subtopic locations.  The chart then serves as the basis
;;; for assessment or adjustment of the subtree, without redundant
;;; traversal of the structure.

;;;_   > allout-chart-subtree (&optional levels orig-depth prev-depth)
(defun allout-chart-subtree (&optional levels orig-depth prev-depth)
  "Produce a location \"chart\" of subtopics of the containing topic.

Optional argument LEVELS specifies the depth \(relative to start
depth) for the chart.  Subsequent optional args are not for public
use.

Point is left at the end of the subtree.

Charts are used to capture outline structure, so that outline-altering
routines need assess the structure only once, and then use the chart
for their elaborate manipulations.

Topics are entered in the chart so the last one is at the car.
The entry for each topic consists of an integer indicating the point
at the beginning of the topic.  Charts for offspring consists of a
list containing, recursively, the charts for the respective subtopics.
The chart for a topics' offspring precedes the entry for the topic
itself.

The other function parameters are for internal recursion, and should
not be specified by external callers.  ORIG-DEPTH is depth of topic at
starting point, and PREV-DEPTH is depth of prior topic."

  (let ((original (not orig-depth))	; `orig-depth' set only in recursion.
	chart curr-depth)

    (if original			; Just starting?
					; Register initial settings and
					; position to first offspring:
	(progn (setq orig-depth (allout-depth))
	       (or prev-depth (setq prev-depth (1+ orig-depth)))
	       (allout-next-heading)))

    ;; Loop over the current levels' siblings.  Besides being more
    ;; efficient than tail-recursing over a level, it avoids exceeding
    ;; the typically quite constrained Emacs max-lisp-eval-depth.
    ;;
    ;; Probably would speed things up to implement loop-based stack
    ;; operation rather than recursing for lower levels.  Bah.

    (while (and (not (eobp))
					; Still within original topic?
		(< orig-depth (setq curr-depth (allout-recent-depth)))
		(cond ((= prev-depth curr-depth)
		       ;; Register this one and move on:
		       (setq chart (cons (point) chart))
		       (if (and levels (<= levels 1))
			   ;; At depth limit - skip sublevels:
			   (or (allout-next-sibling curr-depth)
			       ;; or no more siblings - proceed to
			       ;; next heading at lesser depth:
			       (while (and (<= curr-depth
					       (allout-recent-depth))
					   (allout-next-heading))))
			 (allout-next-heading)))

		      ((and (< prev-depth curr-depth)
			    (or (not levels)
				(> levels 0)))
		       ;; Recurse on deeper level of curr topic:
		       (setq chart
			     (cons (allout-chart-subtree (and levels
							       (1- levels))
							  orig-depth
							  curr-depth)
				   chart))
		       ;; ... then continue with this one.
		       )

		      ;; ... else nil if we've ascended back to prev-depth.

		      )))

    (if original			; We're at the last sibling on
					; the original level.  Position
					; to the end of it:
	(progn (and (not (eobp)) (forward-char -1))
	       (and (= (preceding-char) ?\n)
		    (= (aref (buffer-substring (max 1 (- (point) 3))
                                               (point))
                             1)
                       ?\n)
		    (forward-char -1))
	       (setq allout-recent-end-of-subtree (point))))

    chart				; (nreverse chart) not necessary,
					; and maybe not preferable.
    ))
;;;_   > allout-chart-siblings (&optional start end)
(defun allout-chart-siblings (&optional start end)
  "Produce a list of locations of this and succeeding sibling topics.
Effectively a top-level chart of siblings.  See `allout-chart-subtree'
for an explanation of charts."
  (save-excursion
    (if (allout-goto-prefix)
	(let ((chart (list (point))))
	  (while (allout-next-sibling)
	    (setq chart (cons (point) chart)))
	  (if chart (setq chart (nreverse chart)))))))
;;;_   > allout-chart-to-reveal (chart depth)
(defun allout-chart-to-reveal (chart depth)

  "Return a flat list of hidden points in subtree CHART, up to DEPTH.

Note that point can be left at any of the points on chart, or at the
start point."

  (let (result here)
    (while (and (or (eq depth t) (> depth 0))
		chart)
      (setq here (car chart))
      (if (listp here)
	  (let ((further (allout-chart-to-reveal here (or (eq depth t)
							   (1- depth)))))
	    ;; We're on the start of a subtree - recurse with it, if there's
	    ;; more depth to go:
	    (if further (setq result (append further result)))
	    (setq chart (cdr chart)))
	(goto-char here)
        (if (allout-hidden-p)
	    (setq result (cons here result)))
	(setq chart (cdr chart))))
    result))
;;;_   X allout-chart-spec (chart spec &optional exposing)
;; (defun allout-chart-spec (chart spec &optional exposing)
;;   "Not yet \(if ever) implemented.

;; Produce exposure directives given topic/subtree CHART and an exposure SPEC.

;; Exposure spec indicates the locations to be exposed and the prescribed
;; exposure status.  Optional arg EXPOSING is an integer, with 0
;; indicating pending concealment, anything higher indicating depth to
;; which subtopic headers should be exposed, and negative numbers
;; indicating (negative of) the depth to which subtopic headers and
;; bodies should be exposed.

;; The produced list can have two types of entries.  Bare numbers
;; indicate points in the buffer where topic headers that should be
;; exposed reside.

;;  - bare negative numbers indicates that the topic starting at the
;;    point which is the negative of the number should be opened,
;;    including their entries.
;;  - bare positive values indicate that this topic header should be
;;    opened.
;;  - Lists signify the beginning and end points of regions that should
;;    be flagged, and the flag to employ.  (For concealment: `\(\?r\)', and
;;    exposure:"
;;   (while spec
;;     (cond ((listp spec)
;; 	   )
;; 	  )
;;     (setq spec (cdr spec)))
;;   )

;;;_  - Within Topic
;;;_   > allout-goto-prefix ()
(defun allout-goto-prefix ()
  "Put point at beginning of immediately containing outline topic.

Goes to most immediate subsequent topic if none immediately containing.

Not sensitive to topic visibility.

Returns the point at the beginning of the prefix, or nil if none."

  (let (done)
    (while (and (not done)
		(search-backward "\n" nil 1))
      (forward-char 1)
      (if (looking-at allout-regexp)
	  (setq done (allout-prefix-data (match-beginning 0)
					  (match-end 0)))
	(forward-char -1)))
    (if (bobp)
	(cond ((looking-at allout-regexp)
	       (allout-prefix-data (match-beginning 0)(match-end 0)))
	      ((allout-next-heading))
	      (done))
      done)))
;;;_   > allout-end-of-prefix ()
(defun allout-end-of-prefix (&optional ignore-decorations)
  "Position cursor at beginning of header text.

If optional IGNORE-DECORATIONS is non-nil, put just after bullet,
otherwise skip white space between bullet and ensuing text."

  (if (not (allout-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (match-end 0))
      (if ignore-decorations
	  t
	(while (looking-at "[0-9]") (forward-char 1))
	(if (and (not (eolp)) (looking-at "\\s-")) (forward-char 1)))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (allout-current-depth)))
;;;_   > allout-current-bullet-pos ()
(defun allout-current-bullet-pos ()
  "Return position of current \(visible) topic's bullet."

 (if (not (allout-current-depth))
      nil
   (1- (match-end 0))))
;;;_   > allout-back-to-current-heading ()
(defun allout-back-to-current-heading ()
  "Move to heading line of current topic, or beginning if already on the line.

Return value of point, unless we started outside of (before any) topics,
in which case we return nil."

  (allout-beginning-of-current-line)
  (if (or (allout-on-current-heading-p)
          (and (re-search-backward (concat "^\\(" allout-regexp "\\)")
                                   nil 'move)
               (progn (while (allout-hidden-p)
                        (allout-beginning-of-current-line)
                        (if (not (looking-at allout-regexp))
                            (re-search-backward (concat
                                                 "^\\(" allout-regexp "\\)")
                                                nil 'move)))
                      (allout-prefix-data (match-beginning 1)
                                          (match-end 1)))))
      (if (interactive-p)
          (allout-end-of-prefix)
        (point))))
;;;_   > allout-back-to-heading ()
(defalias 'allout-back-to-heading 'allout-back-to-current-heading)
;;;_   > allout-pre-next-prefix ()
(defun allout-pre-next-prefix ()
  "Skip forward to just before the next heading line.

Returns that character position."

  (if (re-search-forward allout-line-boundary-regexp nil 'move)
      (prog1 (goto-char (match-beginning 0))
             (allout-prefix-data (match-beginning 2)(match-end 2)))))
;;;_   > allout-end-of-subtree (&optional current include-trailing-blank)
(defun allout-end-of-subtree (&optional current include-trailing-blank)
  "Put point at the end of the last leaf in the containing topic.

Optional CURRENT means put point at the end of the containing
visible topic.

Optional INCLUDE-TRAILING-BLANK means include a trailing blank line, if
any, as part of the subtree.  Otherwise, that trailing blank will be
excluded as delimiting whitespace between topics.

Returns the value of point."
  (interactive "P")
  (if current
      (allout-back-to-current-heading)
    (allout-goto-prefix))
  (let ((level (allout-recent-depth)))
    (allout-next-heading)
    (while (and (not (eobp))
                (> (allout-recent-depth) level))
      (allout-next-heading))
    (and (not (eobp)) (forward-char -1))
    (if (and (not include-trailing-blank) (= ?\n (preceding-char)))
         (forward-char -1))
    (setq allout-recent-end-of-subtree (point))))
;;;_   > allout-end-of-current-subtree (&optional include-trailing-blank)
(defun allout-end-of-current-subtree (&optional include-trailing-blank)

  "Put point at end of last leaf in currently visible containing topic.

Optional INCLUDE-TRAILING-BLANK means include a trailing blank line, if
any, as part of the subtree.  Otherwise, that trailing blank will be
excluded as delimiting whitespace between topics.

Returns the value of point."
  (interactive)
  (allout-end-of-subtree t include-trailing-blank))
;;;_   > allout-beginning-of-current-entry ()
(defun allout-beginning-of-current-entry ()
  "When not already there, position point at beginning of current topic header.

If already there, move cursor to bullet for hot-spot operation.
\(See `allout-mode' doc string for details on hot-spot operation.)"
  (interactive)
  (let ((start-point (point)))
    (allout-end-of-prefix)
    (if (and (interactive-p)
	     (= (point) start-point))
	(goto-char (allout-current-bullet-pos)))))
;;;_   > allout-end-of-entry (&optional inclusive)
(defun allout-end-of-entry (&optional inclusive)
  "Position the point at the end of the current topics' entry.

Optional INCLUSIVE means also include trailing empty line, if any.  When
unset, whitespace between items separates them even when the items are
collapsed."
  (interactive)
  (allout-pre-next-prefix)
  (if (and (not inclusive) (not (bobp)) (= ?\n (preceding-char)))
      (forward-char -1))
  (point))
;;;_   > allout-end-of-current-heading ()
(defun allout-end-of-current-heading ()
  (interactive)
  (allout-beginning-of-current-entry)
  (search-forward "\n" nil t)
  (forward-char -1))
(defalias 'allout-end-of-heading 'allout-end-of-current-heading)
;;;_   > allout-get-body-text ()
(defun allout-get-body-text ()
  "Return the unmangled body text of the topic immediately containing point."
  (save-excursion
    (allout-end-of-prefix)
    (if (not (search-forward "\n" nil t))
        nil
      (backward-char 1)
      (let ((pre-body (point)))
        (if (not pre-body)
            nil
          (allout-end-of-entry t)
          (if (not (= pre-body (point)))
              (buffer-substring-no-properties (1+ pre-body) (point))))
        )
      )
    )
  )

;;;_  - Depth-wise
;;;_   > allout-ascend-to-depth (depth)
(defun allout-ascend-to-depth (depth)
  "Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (allout-depth)))
      (let ((last-good (point)))
        (while (and (< depth (allout-depth))
                    (setq last-good (point))
                    (allout-beginning-of-level)
                    (allout-previous-heading)))
        (if (= (allout-recent-depth) depth)
            (progn (goto-char allout-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))
    (if (interactive-p) (allout-end-of-prefix))))
;;;_   > allout-ascend ()
(defun allout-ascend ()
  "Ascend one level, returning t if successful, nil if not."
  (prog1
      (if (allout-beginning-of-level)
	  (allout-previous-heading))
    (if (interactive-p) (allout-end-of-prefix))))
;;;_   > allout-descend-to-depth (depth)
(defun allout-descend-to-depth (depth)
  "Descend to depth DEPTH within current topic.

Returning depth if successful, nil if not."
  (let ((start-point (point))
        (start-depth (allout-depth)))
    (while
        (and (> (allout-depth) 0)
             (not (= depth (allout-recent-depth))) ; ... not there yet
             (allout-next-heading)     ; ... go further
             (< start-depth (allout-recent-depth)))) ; ... still in topic
    (if (and (> (allout-depth) 0)
             (= (allout-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )
;;;_   > allout-up-current-level (arg &optional dont-complain)
(defun allout-up-current-level (arg &optional dont-complain)
  "Move out ARG levels from current visible topic.

Positions on heading line of containing topic.  Error if unable to
ascend that far, or nil if unable to ascend but optional arg
DONT-COMPLAIN is non-nil."
  (interactive "p")
  (allout-back-to-current-heading)
  (let ((present-level (allout-recent-depth))
	(last-good (point))
	failed)
    ;; Loop for iterating arg:
    (while (and (> (allout-recent-depth) 1)
                (> arg 0)
                (not (bobp))
		(not failed))
      (setq last-good (point))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (allout-recent-depth) present-level))
		  (or (allout-previous-visible-heading 1)
		      (not (setq failed present-level)))))
      (setq present-level (allout-current-depth))
      (setq arg (- arg 1)))
    (if (or failed
	    (> arg 0))
	(progn (goto-char last-good)
	       (if (interactive-p) (allout-end-of-prefix))
	       (if (not dont-complain)
		   (error "Can't ascend past outermost level")
		 (if (interactive-p) (allout-end-of-prefix))
		 nil))
      (if (interactive-p) (allout-end-of-prefix))
      allout-recent-prefix-beginning)))

;;;_  - Linear
;;;_   > allout-next-sibling (&optional depth backward)
(defun allout-next-sibling (&optional depth backward)
  "Like `allout-forward-current-level', but respects invisible topics.

Traverse at optional DEPTH, or current depth if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (or depth (allout-depth)))
          (start-point (point))
	  last-depth)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (allout-previous-heading)
                    (allout-next-heading))
                  (> (setq last-depth (allout-recent-depth)) start-depth)))
      (if (and (not (eobp))
               (and (> (or last-depth (allout-depth)) 0)
                    (= (allout-recent-depth) start-depth)))
          allout-recent-prefix-beginning
        (goto-char start-point)
	(if depth (allout-depth) start-depth)
        nil))))
;;;_   > allout-previous-sibling (&optional depth backward)
(defun allout-previous-sibling (&optional depth backward)
  "Like `allout-forward-current-level' backwards, respecting invisible topics.

Optional DEPTH specifies depth to traverse, default current depth.

Optional BACKWARD reverses direction.

Return depth if successful, nil otherwise."
  (allout-next-sibling depth (not backward))
  )
;;;_   > allout-snug-back ()
(defun allout-snug-back ()
  "Position cursor at end of previous topic.

Presumes point is at the start of a topic prefix."
 (if (or (bobp) (eobp))
     nil
   (forward-char -1))
 (if (or (bobp) (not (= ?\n (preceding-char))))
     nil
   (forward-char -1))
 (point))
;;;_   > allout-beginning-of-level ()
(defun allout-beginning-of-level ()
  "Go back to the first sibling at this level, visible or not."
  (allout-end-of-level 'backward))
;;;_   > allout-end-of-level (&optional backward)
(defun allout-end-of-level (&optional backward)
  "Go to the last sibling at this level, visible or not."

  (let ((depth (allout-depth)))
    (while (allout-previous-sibling depth nil))
    (prog1 (allout-recent-depth)
      (if (interactive-p) (allout-end-of-prefix)))))
;;;_   > allout-next-visible-heading (arg)
(defun allout-next-visible-heading (arg)
  "Move to the next ARG'th visible heading line, backward if arg is negative.

Move to buffer limit in indicated direction if headings are exhausted."

  (interactive "p")
  (let* ((backward (if (< arg 0) (setq arg (* -1 arg))))
	 (step (if backward -1 1))
	 prev got)

    (while (> arg 0)			; limit condition
      (while (and (not (if backward (bobp)(eobp))) ; boundary condition
		  ;; Move, skipping over all those concealed lines:
		  (prog1 (condition-case nil (or (line-move step) t)
                           (error nil))
                    (allout-beginning-of-current-line))
		  (not (setq got (looking-at allout-regexp)))))
      ;; Register this got, it may be the last:
      (if got (setq prev got))
      (setq arg (1- arg)))
    (cond (got				; Last move was to a prefix:
	   (allout-prefix-data (match-beginning 0) (match-end 0))
	   (allout-end-of-prefix))
	  (prev				; Last move wasn't, but prev was:
	   (allout-prefix-data (match-beginning 0) (match-end 0)))
	  ((not backward) (end-of-line) nil))))
;;;_   > allout-previous-visible-heading (arg)
(defun allout-previous-visible-heading (arg)
  "Move to the previous heading line.

With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that `allout-regexp'
matches)."
  (interactive "p")
  (allout-next-visible-heading (- arg)))
;;;_   > allout-forward-current-level (arg)
(defun allout-forward-current-level (arg)
  "Position point at the next heading of the same level.

Takes optional repeat-count, goes backward if count is negative.

Returns resulting position, else nil if none found."
  (interactive "p")
  (let ((start-depth (allout-current-depth))
	(start-arg arg)
	(backward (> 0 arg))
	last-depth
	(last-good (point))
	at-boundary)
    (if (= 0 start-depth)
	(error "No siblings, not in a topic..."))
    (if backward (setq arg (* -1 arg)))
    (while (not (or (zerop arg)
		    at-boundary))
      (while (and (not (if backward (bobp) (eobp)))
		  (if backward (allout-previous-visible-heading 1)
		    (allout-next-visible-heading 1))
		  (> (setq last-depth (allout-recent-depth)) start-depth)))
      (if (and last-depth (= last-depth start-depth)
	       (not (if backward (bobp) (eobp))))
	  (setq last-good (point)
		arg (1- arg))
	(setq at-boundary t)))
    (if (and (not (eobp))
	     (= arg 0)
	     (and (> (or last-depth (allout-depth)) 0)
		  (= (allout-recent-depth) start-depth)))
	allout-recent-prefix-beginning
      (goto-char last-good)
      (if (not (interactive-p))
	  nil
	(allout-end-of-prefix)
	(error "Hit %s level %d topic, traversed %d of %d requested"
	       (if backward "first" "last")
	       (allout-recent-depth)
	       (- (abs start-arg) arg)
	       (abs start-arg))))))
;;;_   > allout-backward-current-level (arg)
(defun allout-backward-current-level (arg)
  "Inverse of `allout-forward-current-level'."
  (interactive "p")
  (if (interactive-p)
      (let ((current-prefix-arg (* -1 arg)))
	(call-interactively 'allout-forward-current-level))
    (allout-forward-current-level (* -1 arg))))

;;;_ #5 Alteration

;;;_  - Fundamental
;;;_   = allout-post-goto-bullet
(defvar allout-post-goto-bullet nil
  "Outline internal var, for `allout-pre-command-business' hot-spot operation.

When set, tells post-processing to reposition on topic bullet, and
then unset it.  Set by `allout-pre-command-business' when implementing
hot-spot operation, where literal characters typed over a topic bullet
are mapped to the command of the corresponding control-key on the
`allout-mode-map'.")
(make-variable-buffer-local 'allout-post-goto-bullet)
;;;_   > allout-post-command-business ()
(defun allout-post-command-business ()
  "Outline `post-command-hook' function.

- Implement (and clear) `allout-post-goto-bullet', for hot-spot
  outline commands.

- Decrypt topic currently being edited if it was encrypted for a save."

					; Apply any external change func:
  (if (not (allout-mode-p))		; In allout-mode.
      nil

    (if (and (boundp 'allout-after-save-decrypt)
             allout-after-save-decrypt)
        (allout-after-saves-handler))

    ;; Implement -post-goto-bullet, if set:
    (if (and allout-post-goto-bullet
	     (allout-current-bullet-pos))
	(progn (goto-char (allout-current-bullet-pos))
	       (setq allout-post-goto-bullet nil)))
    ))
;;;_   > allout-pre-command-business ()
(defun allout-pre-command-business ()
  "Outline `pre-command-hook' function for outline buffers.
Implements special behavior when cursor is on bullet character.

When the cursor is on the bullet character, self-insert characters are
reinterpreted as the corresponding control-character in the
`allout-mode-map'.  The `allout-mode' `post-command-hook' insures that
the cursor which has moved as a result of such reinterpretation is
positioned on the bullet character of the destination topic.

The upshot is that you can get easy, single (ie, unmodified) key
outline maneuvering operations by positioning the cursor on the bullet
char.  When in this mode you can use regular cursor-positioning
command/keystrokes to relocate the cursor off of a bullet character to
return to regular interpretation of self-insert characters."

  (if (not (allout-mode-p))
      ;; Shouldn't be invoked if not in allout-mode, but just in case:
      nil
    ;; Hot-spot navigation provisions:
    (if (and (eq this-command 'self-insert-command)
	     (eq (point)(allout-current-bullet-pos)))
	(let* ((this-key-num (cond
			      ((numberp last-command-char)
			       last-command-char)
			      ;; Only xemacs has characterp.
			      ((and (fboundp 'characterp)
				    (apply 'characterp
                                           (list last-command-char)))
			       (apply 'char-to-int (list last-command-char)))
			      (t 0)))
	       mapped-binding)
	  (if (zerop this-key-num)
	      nil
					; Map upper-register literals
					; to lower register:
	    (if (<= 96 this-key-num)
		(setq this-key-num (- this-key-num 32)))
					; Check if we have a literal:
	    (if (and (<= 64 this-key-num)
		     (>= 96 this-key-num))
		(setq mapped-binding
		      (lookup-key 'allout-mode-map
				  (concat allout-command-prefix
					  (char-to-string (- this-key-num
							     64))))))
	    (if mapped-binding
		(setq allout-post-goto-bullet t
		      this-command mapped-binding)))))))
;;;_   > allout-find-file-hook ()
(defun allout-find-file-hook ()
  "Activate `allout-mode' when `allout-auto-activation', `allout-layout' non-nil.

See `allout-init' for setup instructions."
  (if (and allout-auto-activation
	   (not (allout-mode-p))
	   allout-layout)
      (allout-mode t)))

;;;_  - Topic Format Assessment
;;;_   > allout-solicit-alternate-bullet (depth &optional current-bullet)
(defun allout-solicit-alternate-bullet (depth &optional current-bullet)

  "Prompt for and return a bullet char as an alternative to the current one.

Offer one suitable for current depth DEPTH as default."

  (let* ((default-bullet (or (and (stringp current-bullet) current-bullet)
                             (allout-bullet-for-depth depth)))
	 (sans-escapes (regexp-sans-escapes allout-bullets-string))
	 choice)
    (save-excursion
      (goto-char (allout-current-bullet-pos))
      (setq choice (solicit-char-in-string
                    (format "Select bullet: %s ('%s' default): "
                            sans-escapes
                            default-bullet)
                    sans-escapes
                    t)))
    (message "")
    (if (string= choice "") default-bullet choice))
  )
;;;_   > allout-distinctive-bullet (bullet)
(defun allout-distinctive-bullet (bullet)
  "True if BULLET is one of those on `allout-distinctive-bullets-string'."
  (string-match (regexp-quote bullet) allout-distinctive-bullets-string))
;;;_   > allout-numbered-type-prefix (&optional prefix)
(defun allout-numbered-type-prefix (&optional prefix)
  "True if current header prefix bullet is numbered bullet."
  (and allout-numbered-bullet
        (string= allout-numbered-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-encrypted-type-prefix (&optional prefix)
(defun allout-encrypted-type-prefix (&optional prefix)
  "True if current header prefix bullet is for an encrypted entry \(body)."
  (and allout-topic-encryption-bullet
        (string= allout-topic-encryption-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-bullet-for-depth (&optional depth)
(defun allout-bullet-for-depth (&optional depth)
  "Return outline topic bullet suited to optional DEPTH, or current depth."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if allout-stylish-prefixes
      (char-to-string (aref allout-plain-bullets-string
                            (% (max 0 (- depth 2))
                               allout-plain-bullets-string-len)))
    allout-primary-bullet)
  )

;;;_  - Topic Production
;;;_   > allout-make-topic-prefix (&optional prior-bullet
(defun allout-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            solicit
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Solicit dominates specified bullet-char.
;;;_    . Doc string:
  "Generate a topic prefix suitable for optional arg DEPTH, or current depth.

All the arguments are optional.

PRIOR-BULLET indicates the bullet of the prefix being changed, or
nil if none.  This bullet may be preserved (other options
notwithstanding) if it is on the `allout-distinctive-bullets-string',
for instance.

Second arg NEW indicates that a new topic is being opened after the
topic at point, if non-nil.  Default bullet for new topics, eg, may
be set (contingent to other args) to numbered bullets if previous
sibling is one.  The implication otherwise is that the current topic
is being adjusted - shifted or rebulleted - and we don't consider
bullet or previous sibling.

Third arg DEPTH forces the topic prefix to that depth, regardless of
the current topics' depth.

If SOLICIT is non-nil, then the choice of bullet is solicited from
user.  If it's a character, then that character is offered as the
default, otherwise the one suited to the context \(according to
distinction or depth) is offered.  \(This overrides other options,
including, eg, a distinctive PRIOR-BULLET.)  If non-nil, then the
context-specific bullet is used.

Fifth arg, NUMBER-CONTROL, matters only if `allout-numbered-bullet'
is non-nil *and* soliciting was not explicitly invoked.  Then
NUMBER-CONTROL non-nil forces prefix to either numbered or
denumbered format, depending on the value of the sixth arg, INDEX.

\(Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
the prefix of the topic is forced to be numbered.  Non-nil
NUMBER-CONTROL and nil INDEX forces non-numbered format on the
bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
that the index for the numbered prefix will be derived, by counting
siblings back to start of level.  If INDEX is a number, then that
number is used as the index for the numbered prefix (allowing, eg,
sequential renumbering to not require this function counting back the
index for each successive sibling)."
;;;_    . Code:
  ;; The options are ordered in likely frequence of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (allout-depth)))
         (header-lead allout-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq header-lead "") allout-primary-bullet)
                                        ; Simple, too: all asterisks:
           (allout-old-style-prefixes
            ;; Cheat - make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char allout-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (allout-solicit-alternate-bullet depth solicit)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and allout-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got allout-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and allout-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                allout-numbered-bullet
              (if (and prior-bullet
                       (not (string= allout-numbered-bullet
                                     prior-bullet)))
                  prior-bullet
                (allout-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (allout-depth))
                 allout-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (allout-depth))
                              (allout-ascend-to-depth depth))
                          (allout-get-bullet))))
                   (if (and sibling-bullet
                            (string= allout-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (allout-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and allout-numbered-bullet
                               (string= prior-bullet allout-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((allout-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (allout-sibling-index depth)))
                                   ((allout-sibling-index))))))
    )
  )
;;;_   > allout-open-topic (relative-depth &optional before offer-recent-bullet)
(defun allout-open-topic (relative-depth &optional before offer-recent-bullet)
  "Open a new topic at depth DEPTH.

New topic is situated after current one, unless optional flag BEFORE
is non-nil, or unless current line is completely empty - lacking even
whitespace - in which case open is done on the current line.

When adding an offspring, it will be added immediately after the parent if
the other offspring are exposed, or after the last child if the offspring
are hidden.  \(The intervening offspring will be exposed in the latter
case.)

If OFFER-RECENT-BULLET is true, offer to use the bullet of the prior sibling.

Nuances:

- Creation of new topics is with respect to the visible topic
  containing the cursor, regardless of intervening concealed ones.

- New headers are generally created after/before the body of a
  topic.  However, they are created right at cursor location if the
  cursor is on a blank line, even if that breaks the current topic
  body.  This is intentional, to provide a simple means for
  deliberately dividing topic bodies.

- Double spacing of topic lists is preserved.  Also, the first
  level two topic is created double-spaced (and so would be
  subsequent siblings, if that's left intact).  Otherwise,
  single-spacing is used.

- Creation of sibling or nested topics is with respect to the topic
  you're starting from, even when creating backwards.  This way you
  can easily create a sibling in front of the current topic without
  having to go to its preceding sibling, and then open forward
  from there."

  (allout-beginning-of-current-line)
  (let* ((depth (+ (allout-current-depth) relative-depth))
         (opening-on-blank (if (looking-at "^\$")
                               (not (setq before nil))))
         ;; bunch o vars set while computing ref-topic
         opening-numbered
         ref-depth
         ref-bullet
         (ref-topic (save-excursion
                      (cond ((< relative-depth 0)
                             (allout-ascend-to-depth depth))
                            ((>= relative-depth 1) nil)
                            (t (allout-back-to-current-heading)))
                      (setq ref-depth (allout-recent-depth))
                      (setq ref-bullet
                            (if (> allout-recent-prefix-end 1)
                                (allout-recent-bullet)
                              ""))
                      (setq opening-numbered
                            (save-excursion
                              (and allout-numbered-bullet
                                   (or (<= relative-depth 0)
                                       (allout-descend-to-depth depth))
                                   (if (allout-numbered-type-prefix)
                                       allout-numbered-bullet))))
                      (point)))
         dbl-space
         doing-beginning)

    (if (not opening-on-blank)
                                        ; Positioning and vertical
                                        ; padding - only if not
                                        ; opening-on-blank:
        (progn
          (goto-char ref-topic)
          (setq dbl-space               ; Determine double space action:
                (or (and (<= relative-depth 0)	; not descending;
                         (save-excursion
                           ;; at b-o-b or preceded by a blank line?
                           (or (> 0 (forward-line -1))
                               (looking-at "^\\s-*$")
			       (bobp)))
                         (save-excursion
                           ;; succeeded by a blank line?
                           (allout-end-of-current-subtree)
                           (looking-at "\n\n")))
                    (and (= ref-depth 1)
                         (or before
                             (= depth 1)
                             (save-excursion
                               ;; Don't already have following
                               ;; vertical padding:
                               (not (allout-pre-next-prefix)))))))

          ;; Position to prior heading, if inserting backwards, and not
          ;; going outwards:
          (if (and before (>= relative-depth 0))
	      (progn (allout-back-to-current-heading)
                            (setq doing-beginning (bobp))
                            (if (not (bobp))
                                (allout-previous-heading)))
	    (if (and before (bobp))
		(open-line 1)))

          (if (<= relative-depth 0)
              ;; Not going inwards, don't snug up:
              (if doing-beginning
                  (if (not dbl-space)
                      (open-line 1)
                    (open-line 2))
		(if before
		    (progn (end-of-line)
			   (allout-pre-next-prefix)
                           (while (and (= ?\n (following-char))
                                       (save-excursion
                                         (forward-char 1)
                                         (allout-hidden-p)))
                             (forward-char 1))
			   (if (not (looking-at "^$"))
                               (open-line 1)))
		  (allout-end-of-current-subtree)
                  (if (looking-at "\n\n") (forward-char 1))))
            ;; Going inwards - double-space if first offspring is
            ;; double-spaced, otherwise snug up.
            (allout-end-of-entry)
            (line-move 1)
            (allout-beginning-of-current-line)
            (backward-char 1)
            (if (bolp)
                ;; Blank lines between current header body and next
                ;; header - get to last substantive (non-white-space)
                ;; line in body:
                (progn (setq dbl-space t)
                       (re-search-backward "[^ \t\n]" nil t)))
            (if (looking-at "\n\n")
                (setq dbl-space t))
            (if (save-excursion
                  (allout-next-heading)
                  (when (> (allout-recent-depth) ref-depth)
                    ;; This is an offspring.
                    (forward-line -1)
                    (looking-at "^\\s-*$")))
                (progn (forward-line 1)
                       (open-line 1)
                       (forward-line 1)))
            (allout-end-of-current-line))

          ;;(if doing-beginning (goto-char doing-beginning))
          (if (not (bobp))
              ;; We insert a newline char rather than using open-line to
              ;; avoid rear-stickiness inheritence of read-only property.
              (progn (if (and (not (> depth ref-depth))
                              (not before))
                         (open-line 1)
		       (if (and (not dbl-space) (> depth ref-depth))
                           (newline 1)
			 (if dbl-space
                             (open-line 1)
			   (if (not before)
                               (newline 1)))))
                     (if (and dbl-space (not (> relative-depth 0)))
			 (newline 1))
                     (if (and (not (eobp))
                              (not (bolp)))
                         (forward-char 1))))
          ))
    (insert (concat (allout-make-topic-prefix opening-numbered t depth)
                    " "))

    (allout-rebullet-heading (and offer-recent-bullet ref-bullet)
                              depth nil nil t)
    (if (> relative-depth 0)
        (save-excursion (goto-char ref-topic)
                        (allout-show-children)))
    (end-of-line)
    )
  )
;;;_   > allout-open-subtopic (arg)
(defun allout-open-subtopic (arg)
  "Open new topic header at deeper level than the current one.

Negative universal arg means to open deeper, but place the new topic
prior to the current one."
  (interactive "p")
  (allout-open-topic 1 (> 0 arg) (< 1 arg)))
;;;_   > allout-open-sibtopic (arg)
(defun allout-open-sibtopic (arg)
  "Open new topic header at same level as the current one.

Positive universal arg means to use the bullet of the prior sibling.

Negative universal arg means to place the new topic prior to the current
one."
  (interactive "p")
  (allout-open-topic 0 (> 0 arg) (not (= 1 arg))))
;;;_   > allout-open-supertopic (arg)
(defun allout-open-supertopic (arg)
  "Open new topic header at shallower level than the current one.

Negative universal arg means to open shallower, but place the new
topic prior to the current one."

  (interactive "p")
  (allout-open-topic -1 (> 0 arg) (< 1 arg)))

;;;_  - Outline Alteration
;;;_   : Topic Modification
;;;_    = allout-former-auto-filler
(defvar allout-former-auto-filler nil
  "Name of modal fill function being wrapped by `allout-auto-fill'.")
;;;_    > allout-auto-fill ()
(defun allout-auto-fill ()
  "`allout-mode' autofill function.

Maintains outline hanging topic indentation if
`allout-use-hanging-indents' is set."
  (let ((fill-prefix (if allout-use-hanging-indents
                         ;; Check for topic header indentation:
                         (save-excursion
                           (beginning-of-line)
                           (if (looking-at allout-regexp)
                               ;; ... construct indentation to account for
                               ;; length of topic prefix:
                               (make-string (progn (allout-end-of-prefix)
                                                   (current-column))
                                            ?\ )))))
        (use-auto-fill-function (or allout-outside-normal-auto-fill-function
                                    auto-fill-function
                                    'do-auto-fill)))
    (if (or allout-former-auto-filler allout-use-hanging-indents)
        (funcall use-auto-fill-function))))
;;;_    > allout-reindent-body (old-depth new-depth &optional number)
(defun allout-reindent-body (old-depth new-depth &optional number)
  "Reindent body lines which were indented at OLD-DEPTH to NEW-DEPTH.

Optional arg NUMBER indicates numbering is being added, and it must
be accommodated.

Note that refill of indented paragraphs is not done."

  (save-excursion
    (allout-end-of-prefix)
    (let* ((new-margin (current-column))
	   excess old-indent-begin old-indent-end
	   ;; We want the column where the header-prefix text started
	   ;; *before* the prefix was changed, so we infer it relative
	   ;; to the new margin and the shift in depth:
	   (old-margin (+ old-depth (- new-margin new-depth))))

      ;; Process lines up to (but excluding) next topic header:
      (allout-unprotected
       (save-match-data
         (while
	     (and (re-search-forward "\n\\(\\s-*\\)"
				     nil
				     t)
		  ;; Register the indent data, before we reset the
		  ;; match data with a subsequent `looking-at':
		  (setq old-indent-begin (match-beginning 1)
			old-indent-end (match-end 1))
		  (not (looking-at allout-regexp)))
	   (if (> 0 (setq excess (- (- old-indent-end old-indent-begin)
                                    old-margin)))
	       ;; Text starts left of old margin - don't adjust:
	       nil
	     ;; Text was hanging at or right of old left margin -
	     ;; reindent it, preserving its existing indentation
	     ;; beyond the old margin:
	     (delete-region old-indent-begin old-indent-end)
             (indent-to (+ new-margin excess (current-column))))))))))
;;;_    > allout-rebullet-current-heading (arg)
(defun allout-rebullet-current-heading (arg)
  "Solicit new bullet for current visible heading."
  (interactive "p")
  (let ((initial-col (current-column))
	(on-bullet (eq (point)(allout-current-bullet-pos)))
	(backwards (if (< arg 0)
		       (setq arg (* arg -1)))))
    (while (> arg 0)
      (save-excursion (allout-back-to-current-heading)
		      (allout-end-of-prefix)
		      (allout-rebullet-heading t	;;; solicit
						nil	;;; depth
						nil	;;; number-control
						nil	;;; index
						t))	;;; do-successors
      (setq arg (1- arg))
      (if (<= arg 0)
	  nil
	(setq initial-col nil)		; Override positioning back to init col
	(if (not backwards)
	    (allout-next-visible-heading 1)
	  (allout-goto-prefix)
	  (allout-next-visible-heading -1))))
    (message "Done.")
    (cond (on-bullet (goto-char (allout-current-bullet-pos)))
	  (initial-col (move-to-column initial-col)))))
;;;_    > allout-rebullet-heading (&optional solicit ...)
(defun allout-rebullet-heading (&optional solicit
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "Adjust bullet of current topic prefix.

All args are optional.

If SOLICIT is non-nil, then the choice of bullet is solicited from
user.  If it's a character, then that character is offered as the
default, otherwise the one suited to the context \(according to
distinction or depth) is offered.  If non-nil, then the
context-specific bullet is just used.

Second arg DEPTH forces the topic prefix to that depth, regardless
of the topic's current depth.

Third arg NUMBER-CONTROL can force the prefix to or away from
numbered form.  It has effect only if `allout-numbered-bullet' is
non-nil and soliciting was not explicitly invoked (via first arg).
Its effect, numbering or denumbering, then depends on the setting
of the forth arg, INDEX.

If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
prefix of the topic is forced to be non-numbered.  Null index and
non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
INDEX is a number, then that number is used for the numbered
prefix.  Non-nil and non-number means that the index for the
numbered prefix will be derived by allout-make-topic-prefix.

Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
siblings.

Cf vars `allout-stylish-prefixes', `allout-old-style-prefixes',
and `allout-numbered-bullet', which all affect the behavior of
this function."

  (let* ((current-depth (allout-depth))
         (new-depth (or new-depth current-depth))
         (mb allout-recent-prefix-beginning)
         (me allout-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (allout-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                solicit
                                                number-control
                                                index)))

    ;; Is new one is identical to old?
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
	;; Nothing to do:
        t

      ;; New prefix probably different from old:
					; get rid of old one:
      (allout-unprotected (delete-region mb me))
      (goto-char mb)
					; Dispense with number if
					; numbered-bullet prefix:
      (if (and allout-numbered-bullet
               (string= allout-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
	  (allout-unprotected
	   (delete-region (match-beginning 0)(match-end 0))))

					; Put in new prefix:
      (allout-unprotected (insert new-prefix))

      ;; Reindent the body if elected, margin changed, and not encrypted body:
      (if (and allout-reindent-bodies
	       (not (= new-depth current-depth))
               (not (allout-encrypted-topic-p)))
	  (allout-reindent-body current-depth new-depth))

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (allout-next-sibling new-depth nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (allout-sibling-index))))
	      (if (allout-numbered-type-prefix)
		  (allout-rebullet-heading nil		;;; solicit
					    new-depth	;;; new-depth
					    number-control;;; number-control
					    index	;;; index
					    nil)))))	;;;(dont!)do-successors
      )	; (if (and (= current-depth new-depth)...))
    ) ; let* ((current-depth (allout-depth))...)
  ) ; defun
;;;_    > allout-rebullet-topic (arg)
(defun allout-rebullet-topic (arg)
  "Rebullet the visible topic containing point and all contained subtopics.

Descends into invisible as well as visible topics, however.

With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (allout-back-to-current-heading)
      (if (<= (+ (allout-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (allout-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg)))))
;;;_     > allout-rebullet-topic-grunt (&optional relative-depth ...)
(defun allout-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)
  "Like `allout-rebullet-topic', but on nearest containing topic
\(visible or not).

See `allout-rebullet-heading' for rebulleting behavior.

All arguments are optional.

First arg RELATIVE-DEPTH means to shift the depth of the entire
topic that amount.

The rest of the args are for internal recursive use by the function
itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (allout-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (allout-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1"))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (allout-rebullet-heading nil		;;; solicit
                                     (+ starting-depth	;;; starting-depth
                                        relative-depth)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (allout-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (allout-recent-depth)))
             (setq index (1+ index))
             (allout-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (allout-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (allout-recent-depth) starting-depth)
                           (= (allout-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (allout-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (allout-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_    > allout-renumber-to-depth (&optional depth)
(defun allout-renumber-to-depth (&optional depth)
  "Renumber siblings at current depth.

Affects superior topics if optional arg DEPTH is less than current depth.

Returns final depth."

  ;; Proceed by level, processing subsequent siblings on each,
  ;; ascending until we get shallower than the start depth:

  (let ((ascender (allout-depth))
	was-eobp)
    (while (and (not (eobp))
		(allout-depth)
                (>= (allout-recent-depth) depth)
                (>= ascender depth))
                                        ; Skip over all topics at
                                        ; lesser depths, which can not
                                        ; have been disturbed:
      (while (and (not (setq was-eobp (eobp)))
		  (> (allout-recent-depth) ascender))
        (allout-next-heading))
                                        ; Prime ascender for ascension:
      (setq ascender (1- (allout-recent-depth)))
      (if (>= (allout-recent-depth) depth)
          (allout-rebullet-heading nil	;;; solicit
                                    nil	;;; depth
                                    nil	;;; number-control
                                    nil	;;; index
                                    t)) ;;; do-successors
      (if was-eobp (goto-char (point-max)))))
  (allout-recent-depth))
;;;_    > allout-number-siblings (&optional denumber)
(defun allout-number-siblings (&optional denumber)
  "Assign numbered topic prefix to this topic and its siblings.

With universal argument, denumber - assign default bullet to this
topic and its siblings.

With repeated universal argument (`^U^U'), solicit bullet for each
rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (allout-back-to-current-heading)
    (allout-beginning-of-level)
    (let ((depth (allout-recent-depth))
	  (index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (allout-rebullet-heading use-bullet		;;; solicit
                                  depth			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (allout-next-sibling depth nil))))))
;;;_    > allout-shift-in (arg)
(defun allout-shift-in (arg)
  "Increase depth of current heading and any topics collapsed within it.

We disallow shifts that would result in the topic having a depth more than
one level greater than the immediately previous topic, to avoid containment
discontinuity.  The first topic in the file can be adjusted to any positive
depth, however."
  (interactive "p")
  (if (> arg 0)
      (save-excursion
        (allout-back-to-current-heading)
        (if (not (bobp))
            (let* ((current-depth (allout-recent-depth))
                   (start-point (point))
                   (predecessor-depth (progn
                                        (forward-char -1)
                                        (allout-goto-prefix)
                                        (if (< (point) start-point)
                                            (allout-recent-depth)
                                          0))))
              (if (and (> predecessor-depth 0)
                       (> (+ current-depth arg)
                          (1+ predecessor-depth)))
                  (error (concat "Disallowed shift deeper than"
                                 " containing topic's children.")))))))
  (allout-rebullet-topic arg))
;;;_    > allout-shift-out (arg)
(defun allout-shift-out (arg)
  "Decrease depth of current heading and any topics collapsed within it.

We disallow shifts that would result in the topic having a depth more than
one level greater than the immediately previous topic, to avoid containment
discontinuity.  The first topic in the file can be adjusted to any positive
depth, however."
  (interactive "p")
  (if (< arg 0)
      (allout-shift-in (* arg -1)))
  (allout-rebullet-topic (* arg -1)))
;;;_   : Surgery (kill-ring) functions with special provisions for outlines:
;;;_    > allout-kill-line (&optional arg)
(defun allout-kill-line (&optional arg)
  "Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")

  (if (or (not (allout-mode-p))
          (not (bolp))
          (not (looking-at allout-regexp)))
      ;; Above conditions do not obtain - just do a regular kill:
      (kill-line arg)
    ;; Ah, have to watch out for adjustments:
    (let* ((beg (point))
           (beg-hidden (allout-hidden-p))
           (end-hidden (save-excursion (allout-end-of-current-line)
                                       (allout-hidden-p)))
           (depth (allout-depth))
           (collapsed (allout-current-topic-collapsed-p)))

      (if collapsed
          (put-text-property beg (1+ beg) 'allout-was-collapsed t)
        (remove-text-properties beg (1+ beg) '(allout-was-collapsed t)))

      (if (and (not beg-hidden) (not end-hidden))
          (allout-unprotected (kill-line arg))
        (kill-line arg))
                                        ; Provide some feedback:
      (sit-for 0)
      (if allout-numbered-bullet
          (save-excursion               ; Renumber subsequent topics if needed:
            (if (not (looking-at allout-regexp))
                (allout-next-heading))
            (allout-renumber-to-depth depth))))))
;;;_    > allout-kill-topic ()
(defun allout-kill-topic ()
  "Kill topic together with subtopics.

Trailing whitespace is killed with a topic if that whitespace:

 - would separate the topic from a subsequent sibling
 - would separate the topic from the end of buffer
 - would not be added to whitespace already separating the topic from the
   previous one.

Completely collapsed topics are marked as such, for re-collapse
when yank with allout-yank into an outline as a heading."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is deleted, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* a kill has been performed.

  (interactive)
  (let* ((collapsed (allout-current-topic-collapsed-p))
         (beg (prog1 (allout-back-to-current-heading) (beginning-of-line)))
         (depth (allout-recent-depth)))
    (allout-end-of-current-subtree)
    (if (and (/= (current-column) 0) (not (eobp)))
        (forward-char 1))
    (if (not (eobp))
	(if (and (looking-at "\n")
                 (or (save-excursion
                       (or (not (allout-next-heading))
                           (= depth (allout-recent-depth))))
                     (and (> (- beg (point-min)) 3)
                          (string= (buffer-substring (- beg 2) beg) "\n\n"))))
	    (forward-char 1)))

    (if collapsed
        (put-text-property beg (1+ beg) 'allout-was-collapsed t)
      (remove-text-properties beg (1+ beg) '(allout-was-collapsed t)))
    (allout-unprotected (kill-region beg (point)))
    (sit-for 0)
    (save-excursion
      (allout-renumber-to-depth depth))))
;;;_    > allout-yank-processing ()
(defun allout-yank-processing (&optional arg)

  "Incidental allout-specific business to be done just after text yanks.

Does depth adjustment of yanked topics, when:

1 the stuff being yanked starts with a valid outline header prefix, and
2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

Also, adjusts numbering of subsequent siblings when appropriate.

Depth adjustment alters the depth of all the topics being yanked
the amount it takes to make the first topic have the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, are left exactly like normal, non-allout-specific yanks."

  (interactive "*P")
					; Get to beginning, leaving
					; region around subject:
  (if (< (allout-mark-marker t) (point))
      (exchange-point-and-mark))
  (let* ((subj-beg (point))
         (into-bol (bolp))
	 (subj-end (allout-mark-marker t))
         (was-collapsed (get-text-property subj-beg 'allout-was-collapsed))
	 ;; 'resituate' if yanking an entire topic into topic header:
	 (resituate (and (allout-e-o-prefix-p)
			 (looking-at (concat "\\(" allout-regexp "\\)"))
			 (allout-prefix-data (match-beginning 1)
					      (match-end 1))))
	 ;; `rectify-numbering' if resituating (where several topics may
	 ;; be resituating) or yanking a topic into a topic slot (bol):
	 (rectify-numbering (or resituate
				(and into-bol (looking-at allout-regexp)))))
    (if resituate
                                        ; The yanked stuff is a topic:
	(let* ((prefix-len (- (match-end 1) subj-beg))
	       (subj-depth (allout-recent-depth))
	       (prefix-bullet (allout-recent-bullet))
	       (adjust-to-depth
		;; Nil if adjustment unnecessary, otherwise depth to which
		;; adjustment should be made:
		(save-excursion
		  (and (goto-char subj-end)
		       (eolp)
		       (goto-char subj-beg)
		       (and (looking-at allout-regexp)
			    (progn
			      (beginning-of-line)
			      (not (= (point) subj-beg)))
			    (looking-at allout-regexp)
			    (allout-prefix-data (match-beginning 0)
						 (match-end 0)))
		       (allout-recent-depth))))
	       (more t))
	  (setq rectify-numbering allout-numbered-bullet)
	  (if adjust-to-depth
                                        ; Do the adjustment:
	      (progn
		(message "... yanking") (sit-for 0)
		(save-restriction
		  (narrow-to-region subj-beg subj-end)
                                        ; Trim off excessive blank
                                        ; line at end, if any:
		  (goto-char (point-max))
		  (if (looking-at "^$")
		      (allout-unprotected (delete-char -1)))
                                        ; Work backwards, with each
                                        ; shallowest level,
                                        ; successively excluding the
                                        ; last processed topic from
                                        ; the narrow region:
		  (while more
		    (allout-back-to-current-heading)
                                        ; go as high as we can in each bunch:
		    (while (allout-ascend-to-depth (1- (allout-depth))))
		    (save-excursion
		      (allout-rebullet-topic-grunt (- adjust-to-depth
						       subj-depth))
		      (allout-depth))
		    (if (setq more (not (bobp)))
			(progn (widen)
			       (forward-char -1)
			       (narrow-to-region subj-beg (point))))))
		(message "")
		;; Preserve new bullet if it's a distinctive one, otherwise
		;; use old one:
		(if (string-match (regexp-quote prefix-bullet)
				  allout-distinctive-bullets-string)
                                        ; Delete from bullet of old to
                                        ; before bullet of new:
		    (progn
		      (beginning-of-line)
		      (delete-region (point) subj-beg)
		      (set-marker (allout-mark-marker t) subj-end)
		      (goto-char subj-beg)
		      (allout-end-of-prefix))
                                        ; Delete base subj prefix,
                                        ; leaving old one:
		  (delete-region (point) (+ (point)
					    prefix-len
					    (- adjust-to-depth subj-depth)))
                                        ; and delete residual subj
                                        ; prefix digits and space:
		  (while (looking-at "[0-9]") (delete-char 1))
		  (if (looking-at " ") (delete-char 1))))
	    (exchange-point-and-mark))))
    (if rectify-numbering
	(progn
	  (save-excursion
                                        ; Give some preliminary feedback:
	    (message "... reconciling numbers") (sit-for 0)
                                        ; ... and renumber, in case necessary:
	    (goto-char subj-beg)
	    (if (allout-goto-prefix)
		(allout-rebullet-heading nil	;;; solicit
					  (allout-depth) ;;; depth
					  nil	;;; number-control
					  nil	;;; index
					  t))
	    (message ""))))
    (when (and (or into-bol resituate) was-collapsed)
      (remove-text-properties subj-beg (1+ subj-beg) '(allout-was-collapsed))
      (allout-hide-current-subtree))
    (if (not resituate)
      (exchange-point-and-mark))))
;;;_    > allout-yank (&optional arg)
(defun allout-yank (&optional arg)
  "`allout-mode' yank, with depth and numbering adjustment of yanked topics.

Non-topic yanks work no differently than normal yanks.

If a topic is being yanked into a bare topic prefix, the depth of the
yanked topic is adjusted to the depth of the topic prefix.

  1 we're yanking in an `allout-mode' buffer
  2 the stuff being yanked starts with a valid outline header prefix, and
  3 it is being yanked at the end of a line which consists of only a valid
    topic prefix.

If these conditions hold then the depth of the yanked topics are all
adjusted the amount it takes to make the first one at the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, (ones that don't qualify for adjustment) are handled
exactly like normal yanks.

Numbering of yanked topics, and the successive siblings at the depth
into which they're being yanked, is adjusted.

`allout-yank-pop' works with `allout-yank' just like normal `yank-pop'
works with normal `yank' in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (yank arg)
  (if (allout-mode-p)
      (allout-yank-processing))
)
;;;_    > allout-yank-pop (&optional arg)
(defun allout-yank-pop (&optional arg)
  "Yank-pop like `allout-yank' when popping to bare outline prefixes.

Adapts level of popped topics to level of fresh prefix.

Note - prefix changes to distinctive bullets will stick, if followed
by pops to non-distinctive yanks.  Bug..."

  (interactive "*p")
  (setq this-command 'yank)
  (yank-pop arg)
  (if (allout-mode-p)
      (allout-yank-processing)))

;;;_  - Specialty bullet functions
;;;_   : File Cross references
;;;_    > allout-resolve-xref ()
(defun allout-resolve-xref ()
  "Pop to file associated with current heading, if it has an xref bullet.

\(Works according to setting of `allout-file-xref-bullet')."
  (interactive)
  (if (not allout-file-xref-bullet)
      (error
       "Outline cross references disabled - no `allout-file-xref-bullet'")
    (if (not (string= (allout-current-bullet) allout-file-xref-bullet))
        (error "Current heading lacks cross-reference bullet `%s'"
               allout-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start allout-recent-prefix-end)
                 (heading-end (progn (end-of-line) (point))))
            (goto-char text-start)
            (setq file-name
                  (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                      (buffer-substring (match-beginning 1) (match-end 1))))))
        (setq file-name (expand-file-name file-name))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              ('error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )

;;;_ #6 Exposure Control

;;;_  - Fundamental
;;;_   > allout-flag-region (from to flag)
(defun allout-flag-region (from to flag)
  "Conceal text from FROM to TO if FLAG is non-nil, else reveal it.

Text is shown if flag is nil and hidden otherwise."
  ;; We use outline invisibility spec.
  (remove-overlays from to 'category 'allout-overlay-category)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'category 'allout-overlay-category)
      (when (featurep 'xemacs)
        (let ((props (symbol-plist 'allout-overlay-category)))
          (while props
            (overlay-put o (pop props) (pop props)))))))
  (run-hooks 'allout-view-change-hook))
;;;_   > allout-flag-current-subtree (flag)
(defun allout-flag-current-subtree (flag)
  "Conceal currently-visible topic's subtree if FLAG non-nil, else reveal it."

  (save-excursion
    (allout-back-to-current-heading)
    (end-of-line)
    (allout-flag-region (point)
                        ;; Exposing must not leave trailing blanks hidden,
                        ;; but can leave them exposed when hiding, so we
                        ;; can use flag's inverse as the
                        ;; include-trailing-blank cue:
                        (allout-end-of-current-subtree (not flag))
                        flag)))

;;;_  - Topic-specific
;;;_   > allout-show-entry (&optional inclusive)
(defun allout-show-entry (&optional inclusive)
  "Like `allout-show-current-entry', reveals entries nested in hidden topics.

This is a way to give restricted peek at a concealed locality without the
expense of exposing its context, but can leave the outline with aberrant
exposure.  `allout-show-offshoot' should be used after the peek to rectify
the exposure."

  (interactive)
  (save-excursion
    (let (beg end)
      (allout-goto-prefix)
      (setq beg (if (allout-hidden-p) (1- (point)) (point)))
      (setq end (allout-pre-next-prefix))
      (allout-flag-region beg end nil)
      (list beg end))))
;;;_   > allout-show-children (&optional level strict)
(defun allout-show-children (&optional level strict)

  "If point is visible, show all direct subheadings of this heading.

Otherwise, do `allout-show-to-offshoot', and then show subheadings.

Optional LEVEL specifies how many levels below the current level
should be shown, or all levels if t.  Default is 1.

Optional STRICT means don't resort to -show-to-offshoot, no matter
what.  This is basically so -show-to-offshoot, which is called by
this function, can employ the pure offspring-revealing capabilities of
it.

Returns point at end of subtree that was opened, if any.  (May get a
point of non-opened subtree?)"

  (interactive "p")
  (let ((start-point (point)))
    (if (and (not strict)
             (allout-hidden-p))

        (progn (allout-show-to-offshoot) ; Point's concealed, open to
                                        ; expose it.
               ;; Then recurse, but with "strict" set so we don't
               ;; infinite regress:
               (allout-show-children level t))

      (save-excursion
        (allout-beginning-of-current-line)
        (save-restriction
          (let* ((chart (allout-chart-subtree (or level 1)))
                 (to-reveal (allout-chart-to-reveal chart (or level 1))))
            (goto-char start-point)
            (when (and strict (allout-hidden-p))
              ;; Concealed root would already have been taken care of,
              ;; unless strict was set.
              (allout-flag-region (point) (allout-snug-back) nil)
              (when allout-show-bodies
                (goto-char (car to-reveal))
                (allout-show-current-entry)))
            (while to-reveal
              (goto-char (car to-reveal))
              (allout-flag-region (save-excursion (allout-snug-back) (point))
                                  (progn (search-forward "\n" nil t)
                                         (1- (point)))
                                  nil)
              (when allout-show-bodies
                (goto-char (car to-reveal))
                (allout-show-current-entry))
              (setq to-reveal (cdr to-reveal)))))))
    ;; Compensate for `save-excursion's maintenance of point
    ;; within invisible text:
    (goto-char start-point)))
;;;_   > allout-show-to-offshoot ()
(defun allout-show-to-offshoot ()
  "Like `allout-show-entry', but reveals all concealed ancestors, as well.

Useful for coherently exposing to a random point in a hidden region."
  (interactive)
  (save-excursion
    (let ((orig-pt (point))
	  (orig-pref (allout-goto-prefix))
	  (last-at (point))
	  bag-it)
      (while (or bag-it (allout-hidden-p))
        (while (allout-hidden-p)
          ;; XXX We would use `(move-beginning-of-line 1)', but it gets
          ;; stuck on hidden newlines at column 80, as of GNU Emacs 22.0.50.
          (beginning-of-line)
          (if (allout-hidden-p) (forward-char -1)))
	(if (= last-at (setq last-at (point)))
	    ;; Oops, we're not making any progress!  Show the current
	    ;; topic completely, and bag this try.
	    (progn (beginning-of-line)
		   (allout-show-current-subtree)
		   (goto-char orig-pt)
		   (setq bag-it t)
		   (beep)
		   (message "%s: %s"
			    "allout-show-to-offshoot: "
			    "Aberrant nesting encountered.")))
	(allout-show-children)
	(goto-char orig-pref))
      (goto-char orig-pt)))
  (if (allout-hidden-p)
      (allout-show-entry)))
;;;_   > allout-hide-current-entry ()
(defun allout-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (allout-back-to-current-heading)
  (save-excursion
    (end-of-line)
    (allout-flag-region (point)
                        (progn (allout-end-of-entry) (point))
                        t)))
;;;_   > allout-show-current-entry (&optional arg)
(defun allout-show-current-entry (&optional arg)

  "Show body following current heading, or hide entry with universal argument."

  (interactive "P")
  (if arg
      (allout-hide-current-entry)
    (save-excursion (allout-show-to-offshoot))
    (save-excursion
      (allout-flag-region (point)
                          (progn (allout-end-of-entry t) (point))
                          nil)
      )))
;;;_   > allout-show-current-subtree (&optional arg)
(defun allout-show-current-subtree (&optional arg)
  "Show everything within the current topic.  With a repeat-count,
expose this topic and its siblings."
  (interactive "P")
  (save-excursion
    (if (<= (allout-current-depth) 0)
	;; Outside any topics - try to get to the first:
	(if (not (allout-next-heading))
	    (error "No topics")
	  ;; got to first, outermost topic - set to expose it and siblings:
	  (message "Above outermost topic - exposing all.")
	  (allout-flag-region (point-min)(point-max) nil))
      (allout-beginning-of-current-line)
      (if (not arg)
	  (allout-flag-current-subtree nil)
	(allout-beginning-of-level)
	(allout-expose-topic '(* :))))))
;;;_   > allout-current-topic-collapsed-p (&optional include-single-liners)
(defun allout-current-topic-collapsed-p (&optional include-single-liners)
  "True if the currently visible containing topic is already collapsed.

If optional INCLUDE-SINGLE-LINERS is true, then include single-line
topics \(which intrinsically can be considered both collapsed and
not\), as collapsed.  Otherwise they are considered uncollapsed."
  (save-excursion
      (and 
       (= (progn (allout-back-to-current-heading)
                 (move-end-of-line 1)
                 (point))
          (allout-end-of-current-subtree))
       (or include-single-liners
           (progn (backward-char 1) (allout-hidden-p))))))
;;;_   > allout-hide-current-subtree (&optional just-close)
(defun allout-hide-current-subtree (&optional just-close)
  "Close the current topic, or containing topic if this one is already closed.

If this topic is closed and it's a top level topic, close this topic
and its siblings.

If optional arg JUST-CLOSE is non-nil, do not close the parent or
siblings, even if the target topic is already closed."

  (interactive)
  (let* ((from (point))
         (sibs-msg "Top-level topic already closed - closing siblings...")
         (current-exposed (not (allout-current-topic-collapsed-p t))))
    (cond (current-exposed (allout-flag-current-subtree t))
          (just-close nil)
          ((allout-up-current-level 1 t) (allout-hide-current-subtree))
          (t (goto-char 0)
             (message sibs-msg)
             (allout-expose-topic '(0 :))
             (message (concat sibs-msg "  Done."))))
    (goto-char from)))
;;;_   > allout-show-current-branches ()
(defun allout-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (beginning-of-line)
  (allout-show-children t))
;;;_   > allout-hide-current-leaves ()
(defun allout-hide-current-leaves ()
  "Hide the bodies of the current topic and all its offspring."
  (interactive)
  (allout-back-to-current-heading)
  (allout-hide-region-body (point) (progn (allout-end-of-current-subtree)
                                           (point))))

;;;_  - Region and beyond
;;;_   > allout-show-all ()
(defun allout-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (message "Exposing entire buffer...")
  (allout-flag-region (point-min) (point-max) nil)
  (message "Exposing entire buffer...  Done."))
;;;_   > allout-hide-bodies ()
(defun allout-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (allout-hide-region-body (point-min) (point-max)))
;;;_   > allout-hide-region-body (start end)
(defun allout-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
	(allout-flag-region (point) (allout-end-of-entry) t)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "\n\n")
		 2 1)))))))

;;;_   > allout-expose-topic (spec)
(defun allout-expose-topic (spec)
  "Apply exposure specs to successive outline topic items.

Use the more convenient frontend, `allout-new-exposure', if you don't
need evaluation of the arguments, or even better, the `allout-layout'
variable-keyed mode-activation/auto-exposure feature of allout outline
mode.  See the respective documentation strings for more details.

Cursor is left at start position.

SPEC is either a number or a list.

Successive specs on a list are applied to successive sibling topics.

A simple spec \(either a number, one of a few symbols, or the null
list) dictates the exposure for the corresponding topic.

Non-null lists recursively designate exposure specs for respective
subtopics of the current topic.

The `:' repeat spec is used to specify exposure for any number of
successive siblings, up to the trailing ones for which there are
explicit specs following the `:'.

Simple (numeric and null-list) specs are interpreted as follows:

 Numbers indicate the relative depth to open the corresponding topic.
     - negative numbers force the topic to be closed before opening to the
       absolute value of the number, so all siblings are open only to
       that level.
     - positive numbers open to the relative depth indicated by the
       number, but do not force already opened subtopics to be closed.
     - 0 means to close topic - hide all offspring.
  :  - `repeat'
       apply prior element to all siblings at current level, *up to*
       those siblings that would be covered by specs following the `:'
       on the list.  Ie, apply to all topics at level but the last
       ones.  \(Only first of multiple colons at same level is
       respected - subsequent ones are discarded.)
  *  - completely opens the topic, including bodies.
  +  - shows all the sub headers, but not the bodies
  -  - exposes the body of the corresponding topic.

Examples:
\(allout-expose-topic '(-1 : 0))
	Close this and all following topics at current level, exposing
	only their immediate children, but close down the last topic
	at this current level completely.
\(allout-expose-topic '(-1 () : 1 0))
	Close current topic so only the immediate subtopics are shown;
	show the children in the second to last topic, and completely
	close the last one.
\(allout-expose-topic '(-2 : -1 *))
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely open the last one."

  (interactive "xExposure spec: ")
  (if (not (listp spec))
      nil
    (let ((depth (allout-depth))
	  (max-pos 0)
	  prev-elem curr-elem
	  stay)
      (while spec
	(setq prev-elem curr-elem
	      curr-elem (car spec)
	      spec (cdr spec))
	(cond				; Do current element:
	 ((null curr-elem) nil)
	 ((symbolp curr-elem)
	  (cond ((eq curr-elem '*) (allout-show-current-subtree)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
		((eq curr-elem '+) (allout-show-current-branches)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
		((eq curr-elem '-) (allout-show-current-entry))
		((eq curr-elem ':)
		 (setq stay t)
		 ;; Expand the `repeat' spec to an explicit version,
		 ;; w.r.t. remaining siblings:
		 (let ((residue	   ; = # of sibs not covered by remaining spec
			;; Dang - could be nice to make use of the chart, sigh:
			(- (length (allout-chart-siblings))
			   (length spec))))
		   (if (< 0 residue)
		       ;; Some residue - cover it with prev-elem:
		       (setq spec (append (make-list residue prev-elem)
					  spec)))))))
	 ((numberp curr-elem)
	  (if (and (>= 0 curr-elem) (not (allout-hidden-p)))
	      (save-excursion (allout-hide-current-subtree t)
			      (if (> 0 curr-elem)
				  nil
				(if (> allout-recent-end-of-subtree max-pos)
				    (setq max-pos
					  allout-recent-end-of-subtree)))))
	  (if (> (abs curr-elem) 0)
	      (progn (allout-show-children (abs curr-elem))
		     (if (> allout-recent-end-of-subtree max-pos)
			 (setq max-pos allout-recent-end-of-subtree)))))
	  ((listp curr-elem)
	   (if (allout-descend-to-depth (1+ depth))
	       (let ((got (allout-expose-topic curr-elem)))
		 (if (and got (> got max-pos)) (setq max-pos got))))))
	(cond (stay (setq stay nil))
	      ((listp (car spec)) nil)
	      ((> max-pos (point))
	       ;; Capitalize on max-pos state to get us nearer next sibling:
	       (progn (goto-char (min (point-max) max-pos))
		      (allout-next-heading)))
	      ((allout-next-sibling depth))))
      max-pos)))
;;;_   > allout-old-expose-topic (spec &rest followers)
(defun allout-old-expose-topic (spec &rest followers)

  "Deprecated.  Use `allout-expose-topic' \(with different schema
format) instead.

Dictate wholesale exposure scheme for current topic, according to SPEC.

SPEC is either a number or a list.  Optional successive args
dictate exposure for subsequent siblings of current topic.

A simple spec (either a number, a special symbol, or the null list)
dictates the overall exposure for a topic.  Non null lists are
composite specs whose first element dictates the overall exposure for
a topic, with the subsequent elements in the list interpreted as specs
that dictate the exposure for the successive offspring of the topic.

Simple (numeric and null-list) specs are interpreted as follows:

 - Numbers indicate the relative depth to open the corresponding topic:
  - negative numbers force the topic to be close before opening to the
    absolute value of the number.
  - positive numbers just open to the relative depth indicated by the number.
  - 0 just closes
 - `*' completely opens the topic, including bodies.
 - `+' shows all the sub headers, but not the bodies
 - `-' exposes the body and immediate offspring of the corresponding topic.

If the spec is a list, the first element must be a number, which
dictates the exposure depth of the topic as a whole.  Subsequent
elements of the list are nested SPECs, dictating the specific exposure
for the corresponding offspring of the topic.

Optional FOLLOWERS arguments dictate exposure for succeeding siblings."

  (interactive "xExposure spec: ")
  (let ((depth (allout-current-depth))
	max-pos)
    (cond ((null spec) nil)
	  ((symbolp spec)
	   (if (eq spec '*) (allout-show-current-subtree))
	   (if (eq spec '+) (allout-show-current-branches))
	   (if (eq spec '-) (allout-show-current-entry)))
	  ((numberp spec)
	   (if (>= 0 spec)
	       (save-excursion (allout-hide-current-subtree t)
			       (end-of-line)
			       (if (or (not max-pos)
				       (> (point) max-pos))
				   (setq max-pos (point)))
			       (if (> 0 spec)
				   (setq spec (* -1 spec)))))
	   (if (> spec 0)
	     (allout-show-children spec)))
	  ((listp spec)
	   ;(let ((got (allout-old-expose-topic (car spec))))
	   ;  (if (and got (or (not max-pos) (> got max-pos)))
	   ;	 (setq max-pos got)))
	   (let ((new-depth  (+ (allout-current-depth) 1))
		 got)
	     (setq max-pos (allout-old-expose-topic (car spec)))
	     (setq spec (cdr spec))
	     (if (and spec
		      (allout-descend-to-depth new-depth)
		      (not (allout-hidden-p)))
		 (progn (setq got (apply 'allout-old-expose-topic spec))
			(if (and got (or (not max-pos) (> got max-pos)))
			    (setq max-pos got)))))))
    (while (and followers
		(progn (if (and max-pos (< (point) max-pos))
			   (progn (goto-char max-pos)
				  (setq max-pos nil)))
		       (end-of-line)
		       (allout-next-sibling depth)))
      (allout-old-expose-topic (car followers))
      (setq followers (cdr followers)))
    max-pos))
;;;_   > allout-new-exposure '()
(defmacro allout-new-exposure (&rest spec)
  "Literal frontend for `allout-expose-topic', doesn't evaluate arguments.
Some arguments that would need to be quoted in `allout-expose-topic'
need not be quoted in `allout-new-exposure'.

Cursor is left at start position.

Use this instead of obsolete `allout-exposure'.

Examples:
\(allout-new-exposure (-1 () () () 1) 0)
	Close current topic at current level so only the immediate
	subtopics are shown, except also show the children of the
	third subtopic; and close the next topic at the current level.
\(allout-new-exposure : -1 0)
	Close all topics at current level to expose only their
	immediate children, except for the last topic at the current
	level, in which even its immediate children are hidden.
\(allout-new-exposure -2 : -1 *)
        Expose children and grandchildren of first topic at current
	level, and expose children of subsequent topics at current
	level *except* for the last, which should be opened completely."
  (list 'save-excursion
	'(if (not (or (allout-goto-prefix)
		      (allout-next-heading)))
	     (error "allout-new-exposure: Can't find any outline topics"))
	(list 'allout-expose-topic (list 'quote spec))))

;;;_ #7 Systematic outline presentation - copying, printing, flattening

;;;_  - Mapping and processing of topics
;;;_   ( See also Subtree Charting, in Navigation code.)
;;;_   > allout-stringify-flat-index (flat-index)
(defun allout-stringify-flat-index (flat-index &optional context)
  "Convert list representing section/subsection/... to document string.

Optional arg CONTEXT indicates interior levels to include."
  (let ((delim ".")
	result
	numstr
	(context-depth (or (and context 2) 1)))
    ;; Take care of the explicit context:
    (while (> context-depth 0)
      (setq numstr (int-to-string (car flat-index))
	    flat-index (cdr flat-index)
	    result (if flat-index
		       (cons delim (cons numstr result))
		       (cons numstr result))
	    context-depth (if flat-index (1- context-depth) 0)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-plain (flat-index)
(defun allout-stringify-flat-index-plain (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result)
	(while flat-index
	  (setq result (cons (int-to-string (car flat-index))
			     (if result
				 (cons delim result))))
	  (setq flat-index (cdr flat-index)))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-indented (flat-index)
(defun allout-stringify-flat-index-indented (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result
	numstr)
    ;; Take care of the explicit context:
    (setq numstr (int-to-string (car flat-index))
	  flat-index (cdr flat-index)
	  result (if flat-index
		     (cons delim (cons numstr result))
		   (cons numstr result)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-listify-exposed (&optional start end format)
(defun allout-listify-exposed (&optional start end format)

  "Produce a list representing exposed topics in current region.

This list can then be used by `allout-process-exposed' to manipulate
the subject region.

Optional START and END indicate bounds of region.

optional arg, FORMAT, designates an alternate presentation form for
the prefix:

 list - Present prefix as numeric section.subsection..., starting with
	section indicated by the list, innermost nesting first.
 `indent' \(symbol) -  Convert header prefixes to all white space,
		       except for distinctive bullets.

The elements of the list produced are lists that represents a topic
header and body.  The elements of that list are:

 - a number representing the depth of the topic,
 - a string representing the header-prefix, including trailing whitespace and
   bullet.
 - a string representing the bullet character,
 - and a series of strings, each containing one line of the exposed
   portion of the topic entry."

  (interactive "r")
  (save-excursion
    (let*
	;; state vars:
	(strings prefix result depth new-depth out gone-out bullet beg
		 next done)

      (goto-char start)
      (beginning-of-line)
      ;; Goto initial topic, and register preceeding stuff, if any:
      (if (> (allout-goto-prefix) start)
	  ;; First topic follows beginning point - register preliminary stuff:
	  (setq result (list (list 0 "" nil
				   (buffer-substring start (1- (point)))))))
      (while (and (not done)
		  (not (eobp))		; Loop until we've covered the region.
		  (not (> (point) end)))
	(setq depth (allout-recent-depth) 	; Current topics depth,
	      bullet (allout-recent-bullet)	; ... bullet,
	      prefix (allout-recent-prefix)
	      beg (progn (allout-end-of-prefix t) (point))) ; and beginning.
	(setq done			; The boundary for the current topic:
	      (not (allout-next-visible-heading 1)))
	(setq new-depth (allout-recent-depth))
	(setq gone-out out
	      out (< new-depth depth))
	(beginning-of-line)
	(setq next (point))
	(goto-char beg)
	(setq strings nil)
	(while (> next (point))		; Get all the exposed text in
	  (setq strings
		(cons (buffer-substring
		       beg
					;To hidden text or end of line:
		       (progn
                         (end-of-line)
                         (allout-back-to-visible-text)))
		      strings))
	  (when (< (point) next)      ; Resume from after hid text, if any.
            (line-move 1))
	  (setq beg (point)))
	;; Accumulate list for this topic:
	(setq strings (nreverse strings))
	(setq result
	      (cons
	       (if format
		   (let ((special (if (string-match
				       (regexp-quote bullet)
				       allout-distinctive-bullets-string)
				      bullet)))
		     (cond ((listp format)
			    (list depth
				  (if allout-abbreviate-flattened-numbering
				      (allout-stringify-flat-index format
								    gone-out)
				      (allout-stringify-flat-index-plain
				       format))
				  strings
				  special))
			   ((eq format 'indent)
			    (if special
				(list depth
				      (concat (make-string (1+ depth) ? )
					      (substring prefix -1))
				      strings)
			      (list depth
				    (make-string depth ? )
				    strings)))
			   (t (error "allout-listify-exposed: %s %s"
				     "invalid format" format))))
		 (list depth prefix strings))
		    result))
	;; Reasses format, if any:
	(if (and format (listp format))
	    (cond ((= new-depth depth)
		   (setq format (cons (1+ (car format))
					  (cdr format))))
		  ((> new-depth depth)	; descending - assume by 1:
		   (setq format (cons 1 format)))
		  (t
					; Pop the residue:
		   (while (< new-depth depth)
		       (setq format (cdr format))
		       (setq depth (1- depth)))
					; And increment the current one:
		     (setq format
			   (cons (1+ (or (car format)
					 -1))
				 (cdr format)))))))
      ;; Put the list with first at front, to last at back:
      (nreverse result))))
;;;_   > my-region-active-p ()
(defmacro my-region-active-p ()
  (if (fboundp 'region-active-p)
      '(region-active-p)
    'mark-active))
;;;_   > allout-process-exposed (&optional func from to frombuf
;;;					    tobuf format)
(defun allout-process-exposed (&optional func from to frombuf tobuf
					  format start-num)
  "Map function on exposed parts of current topic; results to another buffer.

All args are options; default values itemized below.

Apply FUNCTION to exposed portions FROM position TO position in buffer
FROMBUF to buffer TOBUF.  Sixth optional arg, FORMAT, designates an
alternate presentation form:

 `flat' - Present prefix as numeric section.subsection..., starting with
	 section indicated by the start-num, innermost nesting first.
 X`flat-indented' - Prefix is like `flat' for first topic at each
 X		   level, but subsequent topics have only leaf topic
 X		   number, padded with blanks to line up with first.
 `indent' \(symbol) -  Convert header prefixes to all white space,
		       except for distinctive bullets.

Defaults:
  FUNCTION:	`allout-insert-listified'
  FROM:		region start, if region active, else start of buffer
  TO:		region end, if region active, else end of buffer
  FROMBUF:	current buffer
  TOBUF:	buffer name derived: \"*current-buffer-name exposed*\"
  FORMAT:	nil"

					; Resolve arguments,
					; defaulting if necessary:
  (if (not func) (setq func 'allout-insert-listified))
  (if (not (and from to))
      (if (my-region-active-p)
	  (setq from (region-beginning) to (region-end))
	(setq from (point-min) to (point-max))))
  (if frombuf
      (if (not (bufferp frombuf))
	  ;; Specified but not a buffer - get it:
	  (let ((got (get-buffer frombuf)))
	    (if (not got)
		(error (concat "allout-process-exposed: source buffer "
			       frombuf
			       " not found."))
	      (setq frombuf got))))
    ;; not specified - default it:
    (setq frombuf (current-buffer)))
  (if tobuf
      (if (not (bufferp tobuf))
	  (setq tobuf (get-buffer-create tobuf)))
    ;; not specified - default it:
    (setq tobuf (concat "*" (buffer-name frombuf) " exposed*")))
  (if (listp format)
      (nreverse format))

  (let* ((listified
	  (progn (set-buffer frombuf)
		 (allout-listify-exposed from to format))))
    (set-buffer tobuf)
    (mapcar func listified)
    (pop-to-buffer tobuf)))

;;;_  - Copy exposed
;;;_   > allout-insert-listified (listified)
(defun allout-insert-listified (listified)
  "Insert contents of listified outline portion in current buffer.

LISTIFIED is a list representing each topic header and body:

 \`(depth prefix text)'

or \`(depth prefix text bullet-plus)'

If `bullet-plus' is specified, it is inserted just after the entire prefix."
  (setq listified (cdr listified))
  (let ((prefix (prog1
		    (car listified)
		  (setq listified (cdr listified))))
	(text (prog1
		  (car listified)
		(setq listified (cdr listified))))
	(bullet-plus (car listified)))
    (insert prefix)
    (if bullet-plus (insert (concat " " bullet-plus)))
    (while text
      (insert (car text))
      (if (setq text (cdr text))
	  (insert "\n")))
    (insert "\n")))
;;;_   > allout-copy-exposed-to-buffer (&optional arg tobuf format)
(defun allout-copy-exposed-to-buffer (&optional arg tobuf format)
  "Duplicate exposed portions of current outline to another buffer.

Other buffer has current buffers name with \" exposed\" appended to it.

With repeat count, copy the exposed parts of only the current topic.

Optional second arg TOBUF is target buffer name.

Optional third arg FORMAT, if non-nil, symbolically designates an
alternate presentation format for the outline:

 `flat'   - Convert topic header prefixes to numeric
	    section.subsection... identifiers.
 `indent' - Convert header prefixes to all white space, except for
	    distinctive bullets.
 `indent-flat' - The best of both - only the first of each level has
		 the full path, the rest have only the section number
		 of the leaf, preceded by the right amount of indentation."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf (get-buffer-create (concat "*" (buffer-name) " exposed*"))))
  (let* ((start-pt (point))
	 (beg (if arg (allout-back-to-current-heading) (point-min)))
	 (end (if arg (allout-end-of-current-subtree) (point-max)))
	 (buf (current-buffer))
	 (start-list ()))
    (if (eq format 'flat)
	(setq format (if arg (save-excursion
				   (goto-char beg)
				   (allout-topic-flat-index))
			   '(1))))
    (save-excursion (set-buffer tobuf)(erase-buffer))
    (allout-process-exposed 'allout-insert-listified
			     beg
			     end
			     (current-buffer)
			     tobuf
			     format start-list)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))
;;;_   > allout-flatten-exposed-to-buffer (&optional arg tobuf)
(defun allout-flatten-exposed-to-buffer (&optional arg tobuf)
  "Present numeric outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode - use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-indented-exposed-to-buffer' for indented presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'flat))
;;;_   > allout-indented-exposed-to-buffer (&optional arg tobuf)
(defun allout-indented-exposed-to-buffer (&optional arg tobuf)
  "Present indented outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode - use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-flatten-exposed-to-buffer' for numeric sectional presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'indent))

;;;_  - LaTeX formatting
;;;_   > allout-latex-verb-quote (string &optional flow)
(defun allout-latex-verb-quote (string &optional flow)
  "Return copy of STRING for literal reproduction across LaTeX processing.
Expresses the original characters \(including carriage returns) of the
string across LaTeX processing."
  (mapconcat (function
	      (lambda (char)
		(cond ((memq char '(?\\ ?$ ?% ?# ?& ?{ ?} ?_ ?^ ?- ?*))
		       (concat "\\char" (number-to-string char) "{}"))
		      ((= char ?\n) "\\\\")
		      (t (char-to-string char)))))
	     string
	     ""))
;;;_   > allout-latex-verbatim-quote-curr-line ()
(defun allout-latex-verbatim-quote-curr-line ()
  "Express line for exact \(literal) representation across LaTeX processing.

Adjust line contents so it is unaltered \(from the original line)
across LaTeX processing, within the context of a `verbatim'
environment.  Leaves point at the end of the line."
  (beginning-of-line)
  (let ((beg (point))
	(end (progn (end-of-line)(point))))
    (goto-char beg)
    (while (re-search-forward "\\\\"
	    ;;"\\\\\\|\\{\\|\\}\\|\\_\\|\\$\\|\\\"\\|\\&\\|\\^\\|\\-\\|\\*\\|#"
			      end	; bounded by end-of-line
			      1)	; no matches, move to end & return nil
      (goto-char (match-beginning 0))
      (insert "\\")
      (setq end (1+ end))
      (goto-char (1+ (match-end 0))))))
;;;_   > allout-insert-latex-header (buffer)
(defun allout-insert-latex-header (buffer)
  "Insert initial LaTeX commands at point in BUFFER."
  ;; Much of this is being derived from the stuff in appendix of E in
  ;; the TeXBook, pg 421.
  (set-buffer buffer)
  (let ((doc-style (format "\n\\documentstyle{%s}\n"
			   "report"))
	(page-numbering (if allout-number-pages
			    "\\pagestyle{empty}\n"
			  ""))
	(titlecmd (format "\\newcommand{\\titlecmd}[1]{{%s #1}}\n"
			  allout-title-style))
	(labelcmd (format "\\newcommand{\\labelcmd}[1]{{%s #1}}\n"
			  allout-label-style))
	(headlinecmd (format "\\newcommand{\\headlinecmd}[1]{{%s #1}}\n"
			     allout-head-line-style))
	(bodylinecmd (format "\\newcommand{\\bodylinecmd}[1]{{%s #1}}\n"
			     allout-body-line-style))
	(setlength (format "%s%s%s%s"
			   "\\newlength{\\stepsize}\n"
			   "\\setlength{\\stepsize}{"
			   allout-indent
			   "}\n"))
	(oneheadline (format "%s%s%s%s%s%s%s"
			     "\\newcommand{\\OneHeadLine}[3]{%\n"
			     "\\noindent%\n"
			     "\\hspace*{#2\\stepsize}%\n"
			     "\\labelcmd{#1}\\hspace*{.2cm}"
			     "\\headlinecmd{#3}\\\\["
			     allout-line-skip
			     "]\n}\n"))
	(onebodyline (format "%s%s%s%s%s%s"
			       "\\newcommand{\\OneBodyLine}[2]{%\n"
			       "\\noindent%\n"
			       "\\hspace*{#1\\stepsize}%\n"
			       "\\bodylinecmd{#2}\\\\["
			       allout-line-skip
			       "]\n}\n"))
	(begindoc "\\begin{document}\n\\begin{center}\n")
	(title (format "%s%s%s%s"
		       "\\titlecmd{"
		       (allout-latex-verb-quote (if allout-title
						(condition-case nil
						    (eval allout-title)
						  ('error "<unnamed buffer>"))
					      "Unnamed Outline"))
		       "}\n"
		       "\\end{center}\n\n"))
	(hsize "\\hsize = 7.5 true in\n")
	(hoffset "\\hoffset = -1.5 true in\n")
	(vspace "\\vspace{.1cm}\n\n"))
    (insert (concat doc-style
		    page-numbering
		    titlecmd
		    labelcmd
		    headlinecmd
		    bodylinecmd
		    setlength
		    oneheadline
		    onebodyline
		    begindoc
		    title
		    hsize
		    hoffset
		    vspace)
	    )))
;;;_   > allout-insert-latex-trailer (buffer)
(defun allout-insert-latex-trailer (buffer)
  "Insert concluding LaTeX commands at point in BUFFER."
  (set-buffer buffer)
  (insert "\n\\end{document}\n"))
;;;_   > allout-latexify-one-item (depth prefix bullet text)
(defun allout-latexify-one-item (depth prefix bullet text)
  "Insert LaTeX commands for formatting one outline item.

Args are the topics numeric DEPTH, the header PREFIX lead string, the
BULLET string, and a list of TEXT strings for the body."
  (let* ((head-line (if text (car text)))
	 (body-lines (cdr text))
	 (curr-line)
	 body-content bop)
					; Do the head line:
    (insert (concat "\\OneHeadLine{\\verb\1 "
                    (allout-latex-verb-quote bullet)
                    "\1}{"
                    depth
                    "}{\\verb\1 "
                    (if head-line
                        (allout-latex-verb-quote head-line)
                      "")
                    "\1}\n"))
    (if (not body-lines)
	nil
      ;;(insert "\\beginlines\n")
      (insert "\\begin{verbatim}\n")
      (while body-lines
	(setq curr-line (car body-lines))
	(if (and (not body-content)
		 (not (string-match "^\\s-*$" curr-line)))
	    (setq body-content t))
					; Mangle any occurrences of
					; "\end{verbatim}" in text,
					; it's special:
	(if (and body-content
		 (setq bop (string-match "\\end{verbatim}" curr-line)))
	    (setq curr-line (concat (substring curr-line 0 bop)
				    ">"
				    (substring curr-line bop))))
	;;(insert "|" (car body-lines) "|")
	(insert curr-line)
	(allout-latex-verbatim-quote-curr-line)
	(insert "\n")
	(setq body-lines (cdr body-lines)))
      (if body-content
	  (setq body-content nil)
	(forward-char -1)
	(insert "\\ ")
	(forward-char 1))
      ;;(insert "\\endlines\n")
      (insert "\\end{verbatim}\n")
      )))
;;;_   > allout-latexify-exposed (arg &optional tobuf)
(defun allout-latexify-exposed (arg &optional tobuf)
  "Format current topics exposed portions to TOBUF for LaTeX processing.
TOBUF defaults to a buffer named the same as the current buffer, but
with \"*\" prepended and \" latex-formed*\" appended.

With repeat count, copy the exposed portions of entire buffer."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf
	    (get-buffer-create (concat "*" (buffer-name) " latexified*"))))
  (let* ((start-pt (point))
	 (beg (if arg (point-min) (allout-back-to-current-heading)))
	 (end (if arg (point-max) (allout-end-of-current-subtree)))
	 (buf (current-buffer)))
    (set-buffer tobuf)
    (erase-buffer)
    (allout-insert-latex-header tobuf)
    (goto-char (point-max))
    (allout-process-exposed 'allout-latexify-one-item
			     beg
			     end
			     buf
			     tobuf)
    (goto-char (point-max))
    (allout-insert-latex-trailer tobuf)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))

;;;_ #8 Encryption
;;;_  > allout-toggle-current-subtree-encryption (&optional fetch-pass)
(defun allout-toggle-current-subtree-encryption (&optional fetch-pass)
  "Encrypt clear or decrypt encoded text of visibly-containing topic's contents.

Optional FETCH-PASS universal argument provokes key-pair encryption with
single universal argument.  With doubled universal argument \(value = 16),
it forces prompting for the passphrase regardless of availability from the
passphrase cache.  With no universal argument, the appropriate passphrase
is obtained from the cache, if available, else from the user.

Currently only GnuPG encryption is supported.

\**NOTE WELL** that the encrypted text must be ascii-armored.  For gnupg
encryption, include the option ``armor'' in your ~/.gnupg/gpg.conf file.

Both symmetric-key and key-pair encryption is implemented.  Symmetric is
the default, use a single \(x4) universal argument for keypair mode.

Encrypted topic's bullet is set to a `~' to signal that the contents of the
topic \(body and subtopics, but not heading) is pending encryption or
encrypted.  `*' asterisk immediately after the bullet signals that the body
is encrypted, its' absence means the topic is meant to be encrypted but is
not.  When a file with topics pending encryption is saved, topics pending
encryption are encrypted.  See allout-encrypt-unencrypted-on-saves for
auto-encryption specifics.

\**NOTE WELL** that automatic encryption that happens during saves will
default to symmetric encryption - you must manually \(re)encrypt key-pair
encrypted topics if you want them to continue to use the key-pair cipher.

Level-1 topics, with prefix consisting solely of an `*' asterisk, cannot be
encrypted.  If you want to encrypt the contents of a top-level topic, use
\\[allout-shift-in] to increase its depth.

  Passphrase Caching

The encryption passphrase is solicited if not currently available in the
passphrase cache from a recent encryption action.

The solicited passphrase is retained for reuse in a buffer-specific cache
for some set period of time \(default, 60 seconds), after which the string
is nulled.  The passphrase cache timeout is customized by setting
`pgg-passphrase-cache-expiry'.

  Symmetric Passphrase Hinting and Verification

If the file previously had no associated passphrase, or had a different
passphrase than specified, the user is prompted to repeat the new one for
corroboration.  A random string encrypted by the new passphrase is set on
the buffer-specific variable `allout-passphrase-verifier-string', for
confirmation of the passphrase when next obtained, before encrypting or
decrypting anything with it.  This helps avoid mistakenly shifting between
keys.

If allout customization var `allout-passphrase-verifier-handling' is
non-nil, an entry for `allout-passphrase-verifier-string' and its value is
added to an Emacs 'local variables' section at the end of the file, which
is created if necessary.  That setting is for retention of the passphrase
verifier across emacs sessions.

Similarly, `allout-passphrase-hint-string' stores a user-provided reminder
about their passphrase, and `allout-passphrase-hint-handling' specifies
when the hint is presented, or if passphrase hints are disabled.  If
enabled \(see the `allout-passphrase-hint-handling' docstring for details),
the hint string is stored in the local-variables section of the file, and
solicited whenever the passphrase is changed."
  (interactive "P")
  (save-excursion
    (allout-back-to-current-heading)
    (allout-toggle-subtree-encryption fetch-pass)
    )
  )
;;;_  > allout-toggle-subtree-encryption (&optional fetch-pass)
(defun allout-toggle-subtree-encryption (&optional fetch-pass)
  "Encrypt clear text or decrypt encoded topic contents \(body and subtopics.)

Optional FETCH-PASS universal argument provokes key-pair encryption with
single universal argument.  With doubled universal argument \(value = 16),
it forces prompting for the passphrase regardless of availability from the
passphrase cache.  With no universal argument, the appropriate passphrase
is obtained from the cache, if available, else from the user.

Currently only GnuPG encryption is supported.

\**NOTE WELL** that the encrypted text must be ascii-armored.  For gnupg
encryption, include the option ``armor'' in your ~/.gnupg/gpg.conf file.

See `allout-toggle-current-subtree-encryption' for more details."

  (interactive "P")
  (save-excursion
    (allout-end-of-prefix t)

    (if (= (allout-recent-depth) 1)
        (error (concat "Cannot encrypt or decrypt level 1 topics -"
                       " shift it in to make it encryptable")))

    (let* ((allout-buffer (current-buffer))
           ;; Asses location:
           (after-bullet-pos (point))
           (was-encrypted
            (progn (if (= (point-max) after-bullet-pos)
                       (error "no body to encrypt"))
                   (allout-encrypted-topic-p)))
           (was-collapsed (if (not (search-forward "\n" nil t))
                              nil
                            (backward-char 1)
                            (allout-hidden-p)))
           (subtree-beg (1+ (point)))
           (subtree-end (allout-end-of-subtree))
           (subject-text (buffer-substring-no-properties subtree-beg
                                                         subtree-end))
           (subtree-end-char (char-after (1- subtree-end)))
           (subtree-trailing-char (char-after subtree-end))
           ;; kluge - result-text needs to be nil, but we also want to
           ;;         check for the error condition
           (result-text (if (or (string= "" subject-text)
                                (string= "\n" subject-text))
                            (error "No topic contents to %scrypt"
                                   (if was-encrypted "de" "en"))
                          nil))
           ;; Assess key parameters:
           (key-info (or
                      ;; detect the type by which it is already encrypted
                      (and was-encrypted
                           (allout-encrypted-key-info subject-text))
                      (and (member fetch-pass '(4 (4)))
                           '(keypair nil))
                      '(symmetric nil)))
           (for-key-type (car key-info))
           (for-key-identity (cadr key-info))
           (fetch-pass (and fetch-pass (member fetch-pass '(16 (16))))))

      (setq result-text
            (allout-encrypt-string subject-text was-encrypted
                                    (current-buffer)
                                    for-key-type for-key-identity fetch-pass))

       ;; Replace the subtree with the processed product.
      (allout-unprotected
       (progn
         (set-buffer allout-buffer)
         (delete-region subtree-beg subtree-end)
         (insert result-text)
         (if was-collapsed
             (allout-flag-region (1- subtree-beg) (point) t))
         ;; adjust trailing-blank-lines to preserve topic spacing:
         (if (not was-encrypted)
             (if (and (= subtree-end-char ?\n)
                      (= subtree-trailing-char ?\n))
                 (insert subtree-trailing-char)))
         ;; Ensure that the item has an encrypted-entry bullet:
         (if (not (string= (buffer-substring-no-properties
                            (1- after-bullet-pos) after-bullet-pos)
                           allout-topic-encryption-bullet))
             (progn (goto-char (1- after-bullet-pos))
                    (delete-char 1)
                    (insert allout-topic-encryption-bullet)))
         (if was-encrypted
             ;; Remove the is-encrypted bullet qualifier:
             (progn (goto-char after-bullet-pos)
                    (delete-char 1))
           ;; Add the is-encrypted bullet qualifier:
           (goto-char after-bullet-pos)
           (insert "*"))
         )
       )
      )
    )
  )
;;;_  > allout-encrypt-string (text decrypt allout-buffer key-type for-key
;;;                                  fetch-pass &optional retried verifying
;;;                                  passphrase)
(defun allout-encrypt-string (text decrypt allout-buffer key-type for-key
                                       fetch-pass &optional retried verifying
                                       passphrase)
  "Encrypt or decrypt message TEXT.

If DECRYPT is true (default false), then decrypt instead of encrypt.

FETCH-PASS (default false) forces fresh prompting for the passphrase.

KEY-TYPE indicates whether to use a 'symmetric or 'keypair cipher.

FOR-KEY is human readable identification of the first of the user's
eligible secret keys a keypair decryption targets, or else nil.

Optional RETRIED is for internal use - conveys the number of failed keys
that have been solicited in sequence leading to this current call.

Optional PASSPHRASE enables explicit delivery of the decryption passphrase,
for verification purposes.

Returns the resulting string, or nil if the transformation fails."

  (require 'pgg)

  (if (not (fboundp 'pgg-encrypt-symmetric))
      (error "Allout encryption depends on a newer version of pgg"))

  (let* ((scheme (upcase
                  (format "%s" (or pgg-scheme pgg-default-scheme "GPG"))))
         (for-key (and (equal key-type 'keypair)
                       (or for-key
                           (split-string (read-string
                                          (format "%s message recipients: "
                                                  scheme))
                                         "[ \t,]+"))))
         (target-prompt-id (if (equal key-type 'keypair)
                               (if (= (length for-key) 1)
                                   (car for-key) for-key)
                             (buffer-name allout-buffer)))
         (target-cache-id (format "%s-%s"
                                  key-type
                                  (if (equal key-type 'keypair)
                                      target-prompt-id
                                    (or (buffer-file-name allout-buffer)
                                        target-prompt-id))))
         result-text status)

    (if (and fetch-pass (not passphrase))
        ;; Force later fetch by evicting passphrase from the cache.
        (pgg-remove-passphrase-from-cache target-cache-id t))

    (catch 'encryption-failed

        ;; Obtain the passphrase if we don't already have one and we're not
        ;; doing a keypair encryption:
        (if (not (or passphrase
                     (and (equal key-type 'keypair)
                          (not decrypt))))

            (setq passphrase (allout-obtain-passphrase for-key
                                                       target-cache-id
                                                       target-prompt-id
                                                       key-type
                                                       allout-buffer
                                                       retried fetch-pass)))
        (with-temp-buffer

          (insert text)

          (cond

           ;; symmetric:
           ((equal key-type 'symmetric)
            (setq status
                  (if decrypt

                      (pgg-decrypt (point-min) (point-max) passphrase)

                    (pgg-encrypt-symmetric (point-min) (point-max)
                                           passphrase)))

            (if status
                (pgg-situate-output (point-min) (point-max))
              ;; failed - handle passphrase caching
              (if verifying
                  (throw 'encryption-failed nil)
                (pgg-remove-passphrase-from-cache target-cache-id t)
                (error "Symmetric-cipher encryption failed - %s"
                       "try again with different passphrase."))))

           ;; encrypt 'keypair:
           ((not decrypt)

            (setq status

                  (pgg-encrypt for-key
                               nil (point-min) (point-max) passphrase))

            (if status
                (pgg-situate-output (point-min) (point-max))
              (error (pgg-remove-passphrase-from-cache target-cache-id t)
                     (error "encryption failed"))))

           ;; decrypt 'keypair:
           (t

            (setq status
                  (pgg-decrypt (point-min) (point-max) passphrase))

            (if status
                (pgg-situate-output (point-min) (point-max))
              (error (pgg-remove-passphrase-from-cache target-cache-id t)
                     (error "decryption failed"))))
           )

          (setq result-text
                (buffer-substring 1 (- (point-max) (if decrypt 0 1))))

          ;; validate result - non-empty
          (cond ((not result-text)
                 (if verifying
                     nil
                   ;; transform was fruitless, retry w/new passphrase.
                   (pgg-remove-passphrase-from-cache target-cache-id t)
                   (allout-encrypt-string text allout-buffer decrypt nil
                                          (if retried (1+ retried) 1)
                                          passphrase)))

                ;; Barf if encryption yields extraordinary control chars:
                ((and (not decrypt)
                      (string-match "[\C-a\C-k\C-o-\C-z\C-@]"
                                    result-text))
                 (error (concat "encryption produced unusable"
                                " non-armored text - reconfigure!")))

                ;; valid result and just verifying or non-symmetric:
                ((or verifying (not (equal key-type 'symmetric)))
                 (if (or verifying decrypt)
                     (pgg-add-passphrase-to-cache target-cache-id
                                                  passphrase t))
                 result-text)

                ;; valid result and regular symmetric - "register"
                ;; passphrase with mnemonic aids/cache.
                (t
                 (set-buffer allout-buffer)
                 (if passphrase
                     (pgg-add-passphrase-to-cache target-cache-id
                                                  passphrase t))
                 (allout-update-passphrase-mnemonic-aids for-key passphrase
                                                         allout-buffer)
                 result-text)
                )
          )
        )
    )
  )
;;;_  > allout-obtain-passphrase (for-key cache-id prompt-id key-type
;;;                                       allout-buffer retried fetch-pass)
(defun allout-obtain-passphrase (for-key cache-id prompt-id key-type 
                                         allout-buffer retried fetch-pass)
  "Obtain passphrase for a key from the cache or else from the user.

When obtaining from the user, symmetric-cipher passphrases are verified
against either, if available and enabled, a random string that was
encrypted against the passphrase, or else against repeated entry by the
user for corroboration.

FOR-KEY is the key for which the passphrase is being obtained.

CACHE-ID is the cache id of the key for the passphrase.

PROMPT-ID is the id for use when prompting the user.

KEY-TYPE is either 'symmetric or 'keypair.

ALLOUT-BUFFER is the buffer containing the entry being en/decrypted.

RETRIED is the number of this attempt to obtain this passphrase.

FETCH-PASS causes the passphrase to be solicited from the user, regardless
of the availability of a cached copy."

  (if (not (equal key-type 'symmetric))
      ;; do regular passphrase read on non-symmetric passphrase:
      (pgg-read-passphrase (format "%s passphrase%s: "
                                   (upcase (format "%s" (or pgg-scheme
                                                            pgg-default-scheme
                                                            "GPG")))
                                     (if prompt-id
                                         (format " for %s" prompt-id)
                                       ""))
                           cache-id t)

    ;; Symmetric hereon:

    (save-excursion
      (set-buffer allout-buffer)
      (let* ((hint (if (and (not (string= allout-passphrase-hint-string ""))
                            (or (equal allout-passphrase-hint-handling 'always)
                                (and (equal allout-passphrase-hint-handling
                                            'needed)
                                     retried)))
                       (format " [%s]" allout-passphrase-hint-string)
                     ""))
             (retry-message (if retried (format " (%s retry)" retried) ""))
             (prompt-sans-hint (format "'%s' symmetric passphrase%s: "
                                       prompt-id retry-message))
             (full-prompt (format "'%s' symmetric passphrase%s%s: "
                                  prompt-id hint retry-message))
             (prompt full-prompt)
             (verifier-string (allout-get-encryption-passphrase-verifier))

             (cached (and (not fetch-pass)
                          (pgg-read-passphrase-from-cache cache-id t)))
             (got-pass (or cached
                           (pgg-read-passphrase full-prompt cache-id t)))

             confirmation)

        (if (not got-pass)
            nil

          ;; Duplicate our handle on the passphrase so it's not clobbered by
          ;; deactivate-passwd memory clearing:
          (setq got-pass (format "%s" got-pass))

          (cond (verifier-string
                 (save-window-excursion
                   (if (allout-encrypt-string verifier-string 'decrypt
                                              allout-buffer 'symmetric
                                              for-key nil 0 'verifying
                                              got-pass)
                       (setq confirmation (format "%s" got-pass))))

                 (if (and (not confirmation)
                          (if (yes-or-no-p
                               (concat "Passphrase differs from established"
                                       " - use new one instead? "))
                              ;; deactivate password for subsequent
                              ;; confirmation:
                              (progn
                                (pgg-remove-passphrase-from-cache cache-id t)
                                (setq prompt prompt-sans-hint)
                                nil)
                            t))
                     (progn (pgg-remove-passphrase-from-cache cache-id t)
                            (error "Wrong passphrase."))))
                ;; No verifier string - force confirmation by repetition of
                ;; (new) passphrase:
                ((or fetch-pass (not cached))
                 (pgg-remove-passphrase-from-cache cache-id t))))
        ;; confirmation vs new input - doing pgg-read-passphrase will do the
        ;; right thing, in either case:
        (if (not confirmation)
            (setq confirmation
                  (pgg-read-passphrase (concat prompt
                                               " ... confirm spelling: ")
                                       cache-id t)))
        (prog1
            (if (equal got-pass confirmation)
                confirmation
              (if (yes-or-no-p (concat "spelling of original and"
                                       " confirmation differ - retry? "))
                  (progn (setq retried (if retried (1+ retried) 1))
                         (pgg-remove-passphrase-from-cache cache-id t)
                         ;; recurse to this routine:
                         (pgg-read-passphrase prompt-sans-hint cache-id t))
                (pgg-remove-passphrase-from-cache cache-id t)
                (error "Confirmation failed.")))
          ;; reduce opportunity for memory cherry-picking by zeroing duplicate:
          (dotimes (i (length got-pass))
            (aset got-pass i 0))
          )
        )
      )
    )
  )
;;;_  > allout-encrypted-topic-p ()
(defun allout-encrypted-topic-p ()
  "True if the current topic is encryptable and encrypted."
  (save-excursion
    (allout-end-of-prefix t)
    (and (string= (buffer-substring-no-properties (1- (point)) (point))
                  allout-topic-encryption-bullet)
         (looking-at "\\*"))
    )
  )
;;;_  > allout-encrypted-key-info (text)
;; XXX gpg-specific, alas
(defun allout-encrypted-key-info (text)
  "Return a pair of the key type and identity of a recipient's secret key.

The key type is one of 'symmetric or 'keypair.

if 'keypair, and some of the user's secret keys are among those for which
the message was encoded, return the identity of the first.  otherwise,
return nil for the second item of the pair.

An error is raised if the text is not encrypted."
  (require 'pgg-parse)
  (save-excursion
    (with-temp-buffer
      (insert text)
      (let* ((parsed-armor (pgg-parse-armor-region (point-min) (point-max)))
             (type (if (pgg-gpg-symmetric-key-p parsed-armor)
                       'symmetric
                     'keypair))
             secret-keys first-secret-key for-key-owner)
        (if (equal type 'keypair)
            (setq secret-keys (pgg-gpg-lookup-all-secret-keys)
                  first-secret-key (pgg-gpg-select-matching-key parsed-armor
                                                                secret-keys)
                  for-key-owner (and first-secret-key
                                     (pgg-gpg-lookup-key-owner
                                      first-secret-key))))
        (list type (pgg-gpg-key-id-from-key-owner for-key-owner))
        )
      )
    )
  )
;;;_  > allout-create-encryption-passphrase-verifier (passphrase)
(defun allout-create-encryption-passphrase-verifier (passphrase)
  "Encrypt random message for later validation of symmetric key's passphrase."
  ;; use 20 random ascii characters, across the entire ascii range.
  (random t)
  (let ((spew (make-string 20 ?\0)))
    (dotimes (i (length spew))
      (aset spew i (1+ (random 254))))
    (allout-encrypt-string spew nil (current-buffer) 'symmetric
                           nil nil 0 passphrase))
  )
;;;_  > allout-update-passphrase-mnemonic-aids (for-key passphrase
;;;                                                     outline-buffer) 
(defun allout-update-passphrase-mnemonic-aids (for-key passphrase
                                                       outline-buffer)
  "Update passphrase verifier and hint strings if necessary.

See `allout-passphrase-verifier-string' and `allout-passphrase-hint-string'
settings.

PASSPHRASE is the passphrase being mnemonicized

OUTLINE-BUFFER is the buffer of the outline being adjusted.

These are used to help the user keep track of the passphrase they use for
symmetric encryption in the file.

Behavior is governed by `allout-passphrase-verifier-handling',
`allout-passphrase-hint-handling', and also, controlling whether the values
are preserved on Emacs local file variables,
`allout-enable-file-variable-adjustment'."

  ;; If passphrase doesn't agree with current verifier:
  ;;   - adjust the verifier
  ;;   - if passphrase hint handling is enabled, adjust the passphrase hint
  ;;   - if file var settings are enabled, adjust the file vars

  (let* ((new-verifier-needed (not (allout-verify-passphrase
                                    for-key passphrase outline-buffer)))
         (new-verifier-string
          (if new-verifier-needed
              ;; Collapse to a single line and enclose in string quotes:
              (subst-char-in-string
               ?\n ?\C-a (allout-create-encryption-passphrase-verifier
                          passphrase))))
         new-hint)
    (when new-verifier-string
      ;; do the passphrase hint first, since it's interactive
      (when (and allout-passphrase-hint-handling
                 (not (equal allout-passphrase-hint-handling 'disabled)))
        (setq new-hint
              (read-from-minibuffer "Passphrase hint to jog your memory: "
                                    allout-passphrase-hint-string))
        (when (not (string= new-hint allout-passphrase-hint-string))
          (setq allout-passphrase-hint-string new-hint)
          (allout-adjust-file-variable "allout-passphrase-hint-string"
                                       allout-passphrase-hint-string)))
      (when allout-passphrase-verifier-handling
        (setq allout-passphrase-verifier-string new-verifier-string)
        (allout-adjust-file-variable "allout-passphrase-verifier-string"
                                     allout-passphrase-verifier-string))
      )
    )
  )
;;;_  > allout-get-encryption-passphrase-verifier ()
(defun allout-get-encryption-passphrase-verifier ()
  "Return text of the encrypt passphrase verifier, unmassaged, or nil if none.

Derived from value of `allout-file-passphrase-verifier-string'."

  (let ((verifier-string (and (boundp 'allout-passphrase-verifier-string)
                              allout-passphrase-verifier-string)))
    (if verifier-string
        ;; Return it uncollapsed
        (subst-char-in-string ?\C-a ?\n verifier-string))
   )
  )
;;;_  > allout-verify-passphrase (key passphrase allout-buffer)
(defun allout-verify-passphrase (key passphrase allout-buffer)
  "True if passphrase successfully decrypts verifier, nil otherwise.

\"Otherwise\" includes absence of passphrase verifier."
  (save-excursion
    (set-buffer allout-buffer)
    (and (boundp 'allout-passphrase-verifier-string)
         allout-passphrase-verifier-string
         (allout-encrypt-string (allout-get-encryption-passphrase-verifier)
                                 'decrypt allout-buffer 'symmetric
                                 key nil 0 'verifying passphrase)
         t)))
;;;_  > allout-next-topic-pending-encryption (&optional except-mark)
(defun allout-next-topic-pending-encryption (&optional except-mark)
  "Return the point of the next topic pending encryption, or nil if none.

EXCEPT-MARK identifies a point whose containing topics should be excluded
from encryption.  This supports 'except-current mode of
`allout-encrypt-unencrypted-on-saves'.

Such a topic has the allout-topic-encryption-bullet without an
immediately following '*' that would mark the topic as being encrypted.  It
must also have content."
  (let (done got content-beg)
    (while (not done)

      (if (not (re-search-forward
                (format "\\(\\`\\|\n\\)%s *%s[^*]"
                        (regexp-quote allout-header-prefix)
                        (regexp-quote allout-topic-encryption-bullet))
                nil t))
          (setq got nil
                done t)
        (goto-char (setq got (match-beginning 0)))
        (if (looking-at "\n")
            (forward-char 1))
        (setq got (point)))

      (cond ((not got)
             (setq done t))

            ((not (search-forward "\n"))
             (setq got nil
                   done t))

            ((eobp)
             (setq got nil
                   done t))

            (t
             (setq content-beg (point))
             (backward-char 1)
             (allout-end-of-subtree)
             (if (or (<= (point) content-beg)
                     (and except-mark
                          (<= content-beg except-mark)
                          (>= (point) except-mark)))
                 ;; Continue looking
                 (setq got nil)
               ;; Got it!
               (setq done t)))
            )
      )
    (if got
        (goto-char got))
    )
  )
;;;_  > allout-encrypt-decrypted (&optional except-mark)
(defun allout-encrypt-decrypted (&optional except-mark)
  "Encrypt topics pending encryption except those containing exemption point.

EXCEPT-MARK identifies a point whose containing topics should be excluded
from encryption.  This supports 'except-current mode of
`allout-encrypt-unencrypted-on-saves'.

If a topic that is currently being edited was encrypted, we return a list
containing the location of the topic and the location of the cursor just
before the topic was encrypted.  This can be used, eg, to decrypt the topic
and exactly resituate the cursor if this is being done as part of a file
save.  See `allout-encrypt-unencrypted-on-saves' for more info."

  (interactive "p")
  (save-excursion
    (let* ((current-mark (point-marker))
           (current-mark-position (marker-position current-mark))
           was-modified
           bo-subtree
           editing-topic editing-point)
      (goto-char (point-min))
      (while (allout-next-topic-pending-encryption except-mark)
        (setq was-modified (buffer-modified-p))
        (when (save-excursion
                (and (boundp 'allout-encrypt-unencrypted-on-saves)
                     allout-encrypt-unencrypted-on-saves
                     (setq bo-subtree (re-search-forward "$"))
                     (not (allout-hidden-p))
                     (>= current-mark (point))
                     (allout-end-of-current-subtree)
                     (<= current-mark (point))))
            (setq editing-topic (point)
                  ;; we had to wait for this 'til now so prior topics are
                  ;; encrypted, any relevant text shifts are in place:
                  editing-point (- current-mark-position
                                   (count-trailing-whitespace-region
                                    bo-subtree current-mark-position))))
        (allout-toggle-subtree-encryption)
        (if (not was-modified)
            (set-buffer-modified-p nil))
        )
      (if (not was-modified)
         (set-buffer-modified-p nil))
      (if editing-topic (list editing-topic editing-point))
      )
    )
  )

;;;_ #9 miscellaneous
;;;_  > allout-mark-topic ()
(defun allout-mark-topic ()
  "Put the region around topic currently containing point."
  (interactive)
  (beginning-of-line)
  (allout-goto-prefix)
  (push-mark (point))
  (allout-end-of-current-subtree)
  (exchange-point-and-mark))
;;;_  > outlineify-sticky ()
;; outlinify-sticky is correct spelling; provide this alias for sticklers:
;;;###autoload
(defalias 'outlinify-sticky 'outlineify-sticky)
;;;###autoload
(defun outlineify-sticky (&optional arg)
  "Activate outline mode and establish file var so it is started subsequently.

See doc-string for `allout-layout' and `allout-init' for details on
setup for auto-startup."

  (interactive "P")

  (allout-mode t)

  (save-excursion
    (goto-char (point-min))
    (if (looking-at allout-regexp)
	t
      (allout-open-topic 2)
      (insert (concat "Dummy outline topic header - see"
                      "`allout-mode' docstring: `^Hm'."))
      (allout-adjust-file-variable
       "allout-layout" (format "%s" (or allout-layout '(-1 : 0)))))))
;;;_  > allout-file-vars-section-data ()
(defun allout-file-vars-section-data ()
  "Return data identifying the file-vars section, or nil if none.

Returns list `(beginning-point prefix-string suffix-string)'."
  ;; minimally gleaned from emacs 21.4 files.el hack-local-variables function.
  (let (beg prefix suffix)
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
      (if (let ((case-fold-search t))
	    (not (search-forward "Local Variables:" nil t)))
          nil
        (setq beg (- (point) 16))
        (setq suffix (buffer-substring-no-properties
                      (point)
                      (progn (if (search-forward "\n" nil t)
                                 (forward-char -1))
                             (point))))
        (setq prefix (buffer-substring-no-properties
                      (progn (if (search-backward "\n" nil t)
                                 (forward-char 1))
                             (point))
                      beg))
        (list beg prefix suffix))
      )
    )
  )
;;;_  > allout-adjust-file-variable (varname value)
(defun allout-adjust-file-variable (varname value)
  "Adjust the setting of an emacs file variable named VARNAME to VALUE.

This activity is inhibited if either `enable-local-variables'
`allout-enable-file-variable-adjustment' are nil.

When enabled, an entry for the variable is created if not already present,
or changed if established with a different value.  The section for the file
variables, itself, is created if not already present.  When created, the
section lines \(including the section line) exist as second-level topics in
a top-level topic at the end of the file.

enable-local-variables must be true for any of this to happen."
  (if (not (and enable-local-variables
                allout-enable-file-variable-adjustment))
      nil
    (save-excursion
      (let ((section-data (allout-file-vars-section-data))
            beg prefix suffix)
        (if section-data
            (setq beg (car section-data)
                  prefix (cadr section-data)
                  suffix (car (cddr section-data)))
          ;; create the section
          (goto-char (point-max))
          (open-line 1)
          (allout-open-topic 0)
          (end-of-line)
          (insert "Local emacs vars.\n")
          (allout-open-topic 1)
          (setq beg (point)
                suffix ""
                prefix (buffer-substring-no-properties (progn
                                                         (beginning-of-line)
                                                         (point))
                                                       beg))
          (goto-char beg)
          (insert "Local variables:\n")
          (allout-open-topic 0)
          (insert "End:\n")
          )
        ;; look for existing entry or create one, leaving point for insertion
        ;; of new value:
        (goto-char beg)
        (allout-show-to-offshoot)
        (if (search-forward (concat "\n" prefix varname ":") nil t)
            (let* ((value-beg (point))
                   (line-end (progn (if (search-forward "\n" nil t)
                                        (forward-char -1))
                                    (point)))
                   (value-end (- line-end (length suffix))))
              (if (> value-end value-beg)
                  (delete-region value-beg value-end)))
          (end-of-line)
          (open-line 1)
          (forward-line 1)
          (insert (concat prefix varname ":")))
        (insert (format " %S%s" value suffix))
        )
      )
    )
  )
;;;_  > solicit-char-in-string (prompt string &optional do-defaulting)
(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "Solicit (with first arg PROMPT) choice of a character from string STRING.

Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for `?' character.  (Oughta use minibuffer keymap instead.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area nil)) (read-char))))

      (setq got
	    (cond ((string-match (regexp-quote got) string) got)
		  ((and do-defaulting (string= got "\r"))
		   ;; Return empty string to default:
		   "")
		  ((string= got "\C-g") (signal 'quit nil))
		  (t
		   (setq new-prompt (concat prompt
					    got
					    " ...pick from: "
					    string
					    ""))
		   nil))))
      ;; got something out of loop - return it:
      got)
  )
;;;_  > regexp-sans-escapes (string)
(defun regexp-sans-escapes (regexp &optional successive-backslashes)
  "Return a copy of REGEXP with all character escapes stripped out.

Representations of actual backslashes - '\\\\\\\\' - are left as a
single backslash.

Optional arg SUCCESSIVE-BACKSLASHES is used internally for recursion."

  (if (string= regexp "")
      ""
    ;; Set successive-backslashes to number if current char is
    ;; backslash, or else to nil:
    (setq successive-backslashes
	  (if (= (aref regexp 0) ?\\)
	      (if successive-backslashes (1+ successive-backslashes) 1)
	    nil))
    (if (or (not successive-backslashes) (= 2 successive-backslashes))
	;; Include first char:
	(concat (substring regexp 0 1)
		(regexp-sans-escapes (substring regexp 1)))
      ;; Exclude first char, but maintain count:
      (regexp-sans-escapes (substring regexp 1) successive-backslashes))))
;;;_  > count-trailing-whitespace-region (beg end)
(defun count-trailing-whitespace-region (beg end)
  "Return number of trailing whitespace chars between BEG and END.

If BEG is bigger than END we return 0."
  (if (> beg end)
      0
    (save-excursion
      (goto-char beg)
      (let ((count 0))
        (while (re-search-forward "[ 	][ 	]*$" end t)
          (goto-char (1+ (match-beginning 0)))
          (setq count (1+ count)))
        count))))
;;;_  > allout-mark-marker to accommodate divergent emacsen:
(defun allout-mark-marker (&optional force buffer)
  "Accommodate the different signature for `mark-marker' across Emacsen.

XEmacs takes two optional args, while mainline GNU Emacs does not,
so pass them along when appropriate."
  (if (featurep 'xemacs)
      (apply 'mark-marker force buffer)
    (mark-marker)))
;;;_  > subst-char-in-string if necessary
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
            (newstr (if inplace string (copy-sequence string))))
        (while (> i 0)
          (setq i (1- i))
          (if (eq (aref newstr i) fromchar)
              (aset newstr i tochar)))
        newstr)))
;;;_  > wholenump if necessary
(if (not (fboundp 'wholenump))
    (defalias 'wholenump 'natnump))
;;;_  > remove-overlays if necessary
(if (not (fboundp 'remove-overlays))
    (defun remove-overlays (&optional beg end name val)
      "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (if (< end beg)
          (setq beg (prog1 end (setq end beg))))
      (save-excursion
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o name) val)
            ;; Either push this overlay outside beg...end
            ;; or split it to exclude beg...end
            ;; or delete it entirely (if it is contained in beg...end).
            (if (< (overlay-start o) beg)
                (if (> (overlay-end o) end)
                    (progn
                      (move-overlay (copy-overlay o)
                                    (overlay-start o) beg)
                      (move-overlay o end (overlay-end o)))
                  (move-overlay o (overlay-start o) beg))
              (if (> (overlay-end o) end)
                  (move-overlay o end (overlay-end o))
                (delete-overlay o)))))))
  )
;;;_  > copy-overlay if necessary - xemacs ~ 21.4
(if (not (fboundp 'copy-overlay))
    (defun copy-overlay (o)
      "Return a copy of overlay O."
      (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
                              ;; FIXME: there's no easy way to find the
                              ;; insertion-type of the two markers.
                              (overlay-buffer o)))
            (props (overlay-properties o)))
        (while props
          (overlay-put o1 (pop props) (pop props)))
        o1)))
;;;_  > add-to-invisibility-spec if necessary - xemacs ~ 21.4
(if (not (fboundp 'add-to-invisibility-spec))
    (defun add-to-invisibility-spec (element)
      "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
      (if (eq buffer-invisibility-spec t)
          (setq buffer-invisibility-spec (list t)))
      (setq buffer-invisibility-spec
            (cons element buffer-invisibility-spec))))
;;;_  > remove-from-invisibility-spec if necessary - xemacs ~ 21.4
(if (not (fboundp 'remove-from-invisibility-spec))
    (defun remove-from-invisibility-spec (element)
      "Remove ELEMENT from `buffer-invisibility-spec'."
      (if (consp buffer-invisibility-spec)
          (setq buffer-invisibility-spec (delete element
                                                 buffer-invisibility-spec)))))
;;;_  > move-beginning-of-line if necessary - older emacs, xemacs
(if (not (fboundp 'move-beginning-of-line))
    (defun move-beginning-of-line (arg)
      "Move point to beginning of current line as displayed.
\(This disregards invisible newlines such as those
which are part of the text that an image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This function does not move point across a field boundary unless that
would move point to a different line than the original, unconstrained
result.  If N is nil or 1, and a front-sticky field starts at point,
the point does not move.  To ignore field boundaries bind
`inhibit-field-text-motion' to t."
      (interactive "p")
      (or arg (setq arg 1))
      (if (/= arg 1)
          (condition-case nil (line-move (1- arg)) (error nil)))

      (let ((orig (point)))
        ;; Move to beginning-of-line, ignoring fields and invisibles.
        (skip-chars-backward "^\n")
        (while (and (not (bobp)) (line-move-invisible-p (1- (point))))
          (goto-char (if (featurep 'xemacs)
                         (previous-property-change (point))
                       (previous-char-property-change (point))))
          (skip-chars-backward "^\n"))
        (vertical-motion 0)
        (if (/= orig (point))
            (goto-char (constrain-to-field (point) orig (/= arg 1) t nil)))))
)
;;;_  > move-end-of-line if necessary - older emacs, xemacs
(if (not (fboundp 'move-end-of-line))
    (defun move-end-of-line (arg)
      "Move point to end of current line as displayed.
\(This disregards invisible newlines such as those
which are part of the text that an image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This function does not move point across a field boundary unless that
would move point to a different line than the original, unconstrained
result.  If N is nil or 1, and a rear-sticky field ends at point,
the point does not move.  To ignore field boundaries bind
`inhibit-field-text-motion' to t."
      (interactive "p")
      (or arg (setq arg 1))
      (let ((orig (point))
            done)
        (while (not done)
          (let ((newpos
                 (save-excursion
                   (let ((goal-column 0))
                     (and (condition-case nil
                              (or (line-move arg) t)
                            (error nil))
                          (not (bobp))
                          (progn
                            (while (and (not (bobp)) (line-move-invisible-p (1- (point))))
                              (goto-char (previous-char-property-change (point))))
                            (backward-char 1)))
                     (point)))))
            (goto-char newpos)
            (if (and (> (point) newpos)
                     (eq (preceding-char) ?\n))
                (backward-char 1)
              (if (and (> (point) newpos) (not (eobp))
                       (not (eq (following-char) ?\n)))
                  ;; If we skipped something intangible
                  ;; and now we're not really at eol,
                  ;; keep going.
                  (setq arg 1)
                (setq done t)))))
        (if (/= orig (point))
            (goto-char (constrain-to-field (point) orig (/= arg 1) t
                                           nil)))))
  )
;;;_  > line-move-invisible-p if necessary
(if (not (fboundp 'line-move-invisible-p))
    (defun line-move-invisible-p (pos)
      "Return non-nil if the character after POS is currently invisible."
      (let ((prop
             (get-char-property pos 'invisible)))
        (if (eq buffer-invisibility-spec t)
            prop
          (or (memq prop buffer-invisibility-spec)
              (assq prop buffer-invisibility-spec))))))


;;;_ #10 Unfinished
;;;_  > allout-bullet-isearch (&optional bullet)
(defun allout-bullet-isearch (&optional bullet)
  "Isearch \(regexp) for topic with bullet BULLET."
  (interactive)
  (if (not bullet)
      (setq bullet (solicit-char-in-string
		    "ISearch for topic with bullet: "
		    (regexp-sans-escapes allout-bullets-string))))

  (let ((isearch-regexp t)
	(isearch-string (concat "^"
				allout-header-prefix
				"[ \t]*"
				bullet)))
    (isearch-repeat 'forward)
    (isearch-mode t)))

;;;_ #11 Provide
(provide 'allout)

;;;_* Local emacs vars.
;; The following `allout-layout' local variable setting:
;;  - closes all topics from the first topic to just before the third-to-last,
;;  - shows the children of the third to last (config vars)
;;  - and the second to last (code section),
;;  - and closes the last topic (this local-variables section).
;;Local variables:
;;allout-layout: (0 : -1 -1 0)
;;End:

;; arch-tag: cf38fbc3-c044-450f-8bff-afed8ba5681c
;;; allout.el ends here
