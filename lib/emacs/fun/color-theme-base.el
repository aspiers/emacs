;;; color-theme.el --- install color themes

;; Copyright (C) 1999, 2000  Jonadab the Unsightly One <jonadab@bright.net>
;; Copyright (C) 2000, 2001  Alex Schroeder <alex@gnu.org>

;; Version: 5.3.1
;; Keywords: faces
;; Author: Jonadab the Unsightly One <jonadab@bright.net>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

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
;; along with GNU Emacs; see the file COPYING.	If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Sharing your current color setup:
;;
;; Use `color-theme-submit'.  If you have already invested time in
;; customizing Emacs faces, please consider sharing your current setup.
;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.	Type M-x
;; color-theme-submit RET and mail the result to the maintainer of this
;; package (see above for mail addres).
;;
;; If you want to make sure that all your customization was exported,
;; type M-x list-faces-display RET to get a list of all faces currently
;; defined.  This is the list of faces that `color-theme-print' uses.

;; Installing a color theme:
;;
;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.
;;
;; The main function to call is color-theme-select.  Type M-x
;; color-theme-select RET.  That creates a Color Theme Selection
;; buffer.  Press RET or `i' on a color theme to install it for the
;; rest of your session.
;;
;; If you want to install the color theme as soon as Emacs is started
;; up, read the description of the theme you like and remember the
;; name of the color theme function.  Press `d' on a color theme in
;; the Color Theme Selection buffer to read the description.  Assuming
;; you like the Gnome2 theme, you'll find that the function to use is
;; called `color-theme-gnome2'.	 Add the following to the end of your
;; .emacs (removing the leading `;;').
;;
;; (require 'color-theme)
;; (color-theme-gnome2)

;; Changing menu colors:
;;
;; If are using X, you can set the menu foreground and background using
;; your .Xdefaults file.  If you set emacs*Background and
;; emacs*Foreground, the first frame will be created with these
;; foreground and background colors used for the menu.	If your .emacs
;; loads a color theme, the frame foreground and background colors
;; overwrite the settings from the .Xdefaults file in the frame itself,
;; but not for the menu.  This assumes that you are not setting any menu
;; ressources for Emacs in the .Xdefaults file.	 Here is a sample entry
;; for your .Xdefaults file:
;;
;;   emacs*Background:		DarkSlateGray
;;   emacs*Foreground:		wheat

;; Making a color theme work for both Emacs and XEmacs:
;;
;; The most important thing is to add missing faces for the other
;; editor.  These are the most important faces to check:
;;
;; In Emacs			  In XEmacs
;; `font-lock-builtin-face'	  `font-lock-reference-face'
;; `font-lock-string-face'	  `font-lock-doc-string-face'
;; `font-lock-constant-face'	  `font-lock-preprocessor-face'
;; `modeline'			  `modeline-buffer-id'
;; `modeline'			  `modeline-mousable'
;; `modeline'			  `modeline-mousable-minor-mode'
;; `region'			  `primary-selection'
;; `region'			  `isearch'
;; `region'			  `zmacs-region'
;; `font-lock-string-face'	  `dired-face-boring'
;; `font-lock-function-name-face' `dired-face-directory'
;; `default'			  `dired-face-executable'
;; `font-lock-warning-face'	  `dired-face-flagged'
;; `font-lock-warning-face'	  `dired-face-marked'
;; `default'			  `dired-face-permissions'
;; `default'			  `dired-face-setuid'
;; `default'			  `dired-face-socket'
;; `font-lock-keyword-face'	  `dired-face-symlink'

;; Deriving your own color theme:
;;
;; If you want to derive your own color theme from an existing color
;; theme, press `p' in the Color Theme Selection buffer (it doesn't
;; matter where in the buffer you press `p'.  This creates a buffer with
;; the elisp code needed to install the current color theme.  Copy the
;; entire code to your .emacs and start fooling around with it.	 Read
;; the documentation of `color-theme-install' using C-h f
;; color-theme-install RET.
;;
;; Note that all color themes are cumulative.  You can try to combine
;; several color themes.  This makes sense if one color theme defines
;; faces which another color theme does not.  Install both themes by
;; pressing RET or `i' on them in the Color Theme Selection buffer,
;; press `p' to get the elisp code, paste it into your .emacs and start
;; working on your masterpiece.	 You can switch this behaviour off by
;; setting `color-theme-is-cumulative'.
;;
;; If your color theme is but a variation of an existing color theme,
;; install the parent color theme, make the modifications you want,
;; and then use C-u p or C-u M-x color-theme-print to avoid
;; duplicating settings from the parent color theme.

;;; Thanks

;; S. Pokrovsky <pok@nbsp.nsk.su> for ideas and discussion.
;; Gordon Messmer <yinyang@eburg.com> for ideas and discussion.
;; Sriram Karra <karra@cs.utah.edu> for the color-theme-submit stuff.
;; All the users that contributed their color themes.

;;; Bugs:

;; Emacs 20.6: Some faces are created using copy-face; these faces are
;; not printed correctly.  This causes the following to be non-equal:
;; (copy-face 'bold 'new-bold)
;; (equal (face-attr-construct 'bold)
;;	  (face-attr-construct 'new-bold))
;; A patch was submitted to the Emacs maintainers.
;;
;; XEmacs 21.2: Not compatible with the custom-theme mode.  It should be
;; easy to transform the color-theme source into custom-theme source,
;; however.
;;
;; Note that this package includes a compatibility layer for Emacs and
;; XEmacs which fixes some bugs encountered in Emacs 20.6 (patches
;; submitted).
;;
;; If you are running XEmacs, then only foreground and background color
;; of the default face and only the background color of the text-cursor
;; face will used.  This is due to the fact that these three pieces of
;; information are stored as frame parameters in Emacs.
;;
;; If you are running XEmacs, variables cannot have a frame-local
;; binding.  Therefore, if color-theme-is-global is set to nil, the
;; variable settings in a color theme are ignored.
;;
;; Using Emacs and a non-nil value for color-theme-is-global will
;; install a new color theme for all frames.  Using XEmacs and a non-nil
;; value for color-theme-is-global will install a new color theme only
;; on those frames that are not using a local color theme.
;;
;; If your system does not define the color names used, you will get the
;; error "undefined color".  See the output of `list-colors-display' for
;; a list of colors defined on your display.
;;
;; Emacs 21 is not yet fully supported.	 It work superficially, but as you
;; switch color themes (or create new frames?), bugs appear.
;;
;; Tested with Emacs 20.7 and XEmacs 21.1



;;; Code:

(defconst color-theme-maintainer-address "alex@gnu.org"
  "Address used by `submit-color-theme'.")

;; Emacs / XEmacs compatibility layer

(defvar color-theme-xemacs-p (string-match "XEmacs" emacs-version)
  "Non-nil if running XEmacs.")

(if color-theme-xemacs-p
    (require 'cus-face)); need face-custom-attributes-set and other functions for XEmacs

(if color-theme-xemacs-p
    (progn
      (defalias 'color-theme-reverse-p 'face-reverse-p)
      (defun color-theme-foreground (face)
	"Return the foreground color name of face FACE, or nil if unspecified."
	(let ((color (face-foreground face 'global)))
	  (if (or (null color) (stringp color))
	      color
	    (face-foreground-name face))))
      (defun color-theme-background (face &optional frame)
	"Return the background color name of face FACE, or nil if unspecified."
	(let ((color (face-background face 'global)))
	  (if (or (null color) (stringp color))
	      color
	    (face-background-name face))))
      (defun color-theme-foreground-p (face)
	"Return the foreground color name of face FACE, or nil if unspecified."
	(face-foreground face 'global))
      (defun color-theme-background-p (face &optional frame)
	"Return the background color name of face FACE, or nil if unspecified."
	(face-background face 'global))
      (if (fboundp 'custom-face-stipple); no longer in XEmacs 21.4.1
	  (defalias 'color-theme-stipple 'custom-face-stipple)
	(defalias 'color-theme-stipple 'custom-face-background-pixmap))
      (defalias 'color-theme-bold-p 'custom-face-bold)
      (defalias 'color-theme-italic-p 'custom-face-italic)
      (defalias 'color-theme-underline-p 'face-underline-p)
      (defun color-theme-default-foreground ()
	"Returns the default foreground color."
	(color-theme-foreground 'default))
      (defun color-theme-default-background ()
	"Returns the default background color."
	(color-theme-background 'default)))
  (defalias 'color-theme-reverse-p 'face-inverse-video-p)
  (defalias 'color-theme-foreground 'face-foreground)
  (defalias 'color-theme-background 'face-background)
  (defalias 'color-theme-foreground-p 'face-foreground)
  (defalias 'color-theme-background-p 'face-background)
  (defalias 'color-theme-stipple 'face-stipple)
  (defalias 'color-theme-bold-p 'face-bold-p)
  (defalias 'color-theme-italic-p 'face-italic-p)
  (defalias 'color-theme-underline-p 'face-underline-p)
  (defun color-theme-default-foreground ()
    "Returns the default foreground color."
    (cdr (assq 'foreground-color (frame-parameters))))
  (defun color-theme-default-background ()
    "Returns the default background color."
    (cdr (assq 'foreground-color (frame-parameters)))))

(defun color-theme-face-attr-construct (face)
  "Return a defface-style attribute list for FACE.
Ignores :inverse-video for XEmacs.

This is a bugfix and compatibility replacement for Emacs 20.6
`face-attr-construct'."
  (let (result)
    (if (and (color-theme-reverse-p face)
	     (null color-theme-xemacs-p)); don't switch in XEmacs
	(progn
	  (setq result (cons ':inverse-video (cons t result)))
	  (setq result (cons ':foreground
			     (cons (color-theme-background face) result)))
	  (setq result (cons ':background
			     (cons (color-theme-foreground face) result))))
      (if (color-theme-foreground-p face)
	  (setq result (cons ':foreground
			     (cons (color-theme-foreground face) result))))
      (if  (color-theme-background-p face)
	  (setq result (cons ':background
			     (cons (color-theme-background face) result)))))
    (if (color-theme-stipple face)
	(setq result (cons ':stipple
			   (cons (color-theme-stipple face) result))))
    (if (color-theme-bold-p face)
	(setq result (cons ':bold
			   (cons (color-theme-bold-p face) result))))
    (if (color-theme-italic-p face)
	(setq result (cons ':italic
			   (cons (color-theme-italic-p face) result))))
    (if (color-theme-underline-p face)
	(setq result (cons ':underline
			   (cons (color-theme-underline-p face) result))))
    result))

(defun color-theme-spec-match-p (face spec)
  "Return t if FACE, matches what SPEC says it should look like.
SPEC may only specify the display type t, eg. ((t (:foreground \"cyan4\"))).

This is a bugfix and compatibility replacement for Emacs 20.6
`face-spec-match-p'."
  (color-theme-attr-match-p face (nth 1 (car spec))))

(defun color-theme-attr-match-p (face attrs)
  "Return t if FACE matches attributes ATTRS.
If ATTRS contain :inverse-video, then foreground and background color
are swapped before they are compared to FACE.

This is a bugfix and compatibility replacement for Emacs 20.6
`face-attr-match-p'."
  (and (color-theme-attr-match-1 face attrs ':inverse-video 'color-theme-reverse-p)
       (if (color-theme-reverse-p face)
	   (and
	    (color-theme-attr-match-1 face attrs ':foreground 'color-theme-background)
	    (color-theme-attr-match-1 face attrs ':background 'color-theme-foreground))
	 (and
	  (color-theme-attr-match-1 face attrs ':foreground 'color-theme-foreground)
	  (color-theme-attr-match-1 face attrs ':background 'color-theme-background)))
       (color-theme-attr-match-1 face attrs ':stipple 'color-theme-stipple)
       (color-theme-attr-match-1 face attrs ':bold 'color-theme-bold-p)
       (color-theme-attr-match-1 face attrs ':italic 'color-theme-italic-p)
       (color-theme-attr-match-1 face attrs ':underline 'color-theme-underline-p)))

(defun color-theme-attr-match-1 (face plist property function)
  "This is a bugfix and compatibility replacement for Emacs 20.6
`face-attr-match-1'."
  (while (and plist (not (eq (car plist) property)))
    (setq plist (cdr (cdr plist))))
  (equal (funcall function face)
	 (nth 1 plist)))

(defun color-theme-alist (plist)
  "Transform PLIST into an alist if it is a plist and return it.
If the first element of PLIST is a cons cell, we just return PLIST,
assuming PLIST to be an alist.	If the first element of plist is not a
symbol, this is an error: We cannot distinguish a plist from an ordinary
list, but a list that doesn't start with a symbol is certainly no plist
and no alist.

This is used to make sure `default-frame-alist' really is an alist and not
a plist."
  (cond ((consp (car plist))
	 plist)
	((not (symbolp (car plist)))
	 (error "Wrong type argument: plist, %S" plist))
	(t
	 (plist-to-alist plist)))); XEmacs only

;; Customization

(defgroup color-theme nil
  "Color Themes for Emacs.
A color theme consists of frame parameter settings, variable settings,
and face definitions."
  :version "20.6"
  :group 'faces)

(defcustom color-theme-legal-frame-parameters "\\(color\\|mode\\)$"
  "Regexp that matches frame parameter names.
Only frame parameter names that match this regexp can be changed as part
of a color theme."
  :type '(choice (const :tag "Colors only" "\\(color\\|mode\\)$")
		 (const :tag "Colors, fonts, and size"
			"\\(color\\|mode\\|font\\|height\\|width\\)$")
		 (regexp :tag "Custom regexp"))
  :group 'color-theme
  :link '(info-link "(elisp)Window Frame Parameters"))

(defcustom color-theme-legal-variables "face$"
  "Regexp that matches variable names.
Only variables that match this regexp can be changed as part of a color
theme.  In addition to matching this name, the variables have to be user
variables (see function `user-variable-p')."
  :type 'regexp
  :group 'color-theme)

(defcustom color-theme-illegal-faces "^w3-"
  "Regexp that matches face names forbidden in themes.
The default setting \"^w3-\" excludes w3 faces since these
are created dynamically."
  :type 'regexp
  :group 'color-theme
  :link '(info-link "(elisp)Faces for Font Lock")
  :link '(info-link "(elisp)Standard Faces"))

(defcustom color-theme-is-global t
  "*Determines wether a color theme is installed on all frames or not.
If non-nil, color themes will be installed for all frames.
If nil, color themes will be installed for the selected frame only.

A possible use for this variable is dynamic binding. Here is a larger
example to put in your ~/.emacs; it will make the Blue Sea color theme
the default used for the first frame, and it will create two additional
frames with different color themes.

setup:
    \(require 'color-theme)
    ;; set default color theme
    \(color-theme-blue-sea)
    ;; create some frames with different color themes
    \(let ((color-theme-is-global nil))
      \(select-frame (make-frame))
      \(color-theme-gnome2)
      \(select-frame (make-frame))
      \(color-theme-standard))

Please note that using XEmacs and and a nil value for
color-theme-is-global will ignore any variable settings for the color
theme, since XEmacs doesn't have frame-local variable bindings.

Also note that using Emacs and a non-nil value for color-theme-is-global
will install a new color theme for all frames.	Using XEmacs and a
non-nil value for color-theme-is-global will install a new color theme
only on those frames that are not using a local color theme."
  :type 'boolean
  :group 'color-theme)

(defcustom color-theme-is-cumulative t
  "*Determines wether new color themes are installed on top of each other.
If non-nil, installing a color theme will undo all settings made by
previous color themes."
  :type 'boolean
  :group 'color-theme)

(defcustom color-theme-mode-hook nil
  "Hook for color-theme-mode."
  :type 'hook
  :group 'color-theme)

(defvar color-theme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'color-theme-install-at-point)
    (define-key map (kbd "c") 'list-color-display)
    (define-key map (kbd "d") 'color-theme-describe)
    (define-key map (kbd "f") 'list-faces-display)
    (define-key map (kbd "i") 'color-theme-install-at-point)
    (define-key map (kbd "l") 'color-theme-install-at-point-for-current-frame)
    (define-key map (kbd "p") 'color-theme-print)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "?") 'color-theme-describe)
    (if color-theme-xemacs-p
	(define-key map (kbd "<button2>") 'color-theme-install-at-mouse)
      (define-key map (kbd "<mouse-2>") 'color-theme-install-at-mouse))
    map)
  "Mode map used for the buffer created by `color-theme-select'.")

(defvar color-theme-buffer-name "*Color Theme Selection*"
  "Name of the color theme selection buffer.")

(defvar color-theme-original-frame-alist nil
  "nil until one of the color themes has been installed.")



;; List of color themes used to create the *Color Theme Selection*
;; buffer.

(defvar color-themes
  '((color-theme-aalto-dark "Aalto Dark" "Jari Aalto <jari.aalto@poboxes.com>")
    (color-theme-aalto-light "Aalto Light" "Jari Aalto <jari.aalto@poboxes.com>")
    (color-theme-bharadwaj "Bharadwaj" "Girish Bharadwaj <girishb@mvps.org>")
    (color-theme-billw "Billw" "Bill White <billw@wolfram.com>")
    (color-theme-blippblopp "Blipp Blopp" "Thomas Sicheritz-Ponten<thomas@biopython.org>")
    (color-theme-simple-1 "Black" "Jonadab <jonadab@bright.net>")
    (color-theme-blue-gnus "Blue Gnus" "Alex Schroeder <alex@gnu.org>")
    (color-theme-blue-sea "Blue Sea" "Alex Schroeder <alex@gnu.org>")
    (color-theme-goldenrod "Cheap Goldenrod" "Alex Schroeder <alex@gnu.org>")
    (color-theme-classic "Classic" "Frederic Giroud <postcard@worldonline.fr>")
    (color-theme-dark-laptop "Dark Laptop" "Laurent Michel <ldm@cs.brown.edu>")
    (color-theme-digital-ofs1 "Digital OFS1" "Gareth Owen <gowen@gwowen.freeserve.co.uk>")
    (color-theme-jsc-light "Cooper Light" "John S Cooper <John.Cooper@eu.citrix.com>")
    (color-theme-jsc-dark "Cooper Dark" "John S Cooper <John.Cooper@eu.citrix.com>")
    (color-theme-fischmeister "Fischmeister"
			      "Sebastian Fischmeister <sfischme@nexus.lzk.tuwien.ac.at>")
    (color-theme-gnome "Gnome" "Jonadab <jonadab@bright.net>")
    (color-theme-gnome2 "Gnome 2" "Alex Schroeder <alex@gnu.org>")
    (color-theme-greiner "Greiner" "Kevin Greiner <kgreiner@mapquest.com>")
    (color-theme-gtk-ide "GTK IDE" "Gordon Messmer <yinyang@eburg.com>")
    (color-theme-high-contrast "High Contrast" "Alex Schroeder <alex@gnu.org>")
    (color-theme-hober "Hober" "Edward O'Connor <ted@oconnor.cx>")
    (color-theme-infodoc "Infodoc" "Frederic Giroud <postcard@worldonline.fr>")
    (color-theme-jb-simple "JB Simple" "jeff@dvns.com")
    (color-theme-jedit-grey "Jedit Grey" "Gordon Messmer <yinyang@eburg.com>")
    (color-theme-jonadabian "Jonadab" "Jonadab <jonadab@bright.net>")
    (color-theme-loyola "Loyola" "Nelson Loyola <Loyola@sedonanetworks.com>")
    (color-theme-marine "Marine" "Girish Bharadwaj <girishb@mvps.org>")
    (color-theme-marquardt "Marquardt" "Colin Marquardt <colin@marquardt-home.de>")
    (color-theme-midnight "Midnight" "Gordon Messmer <yinyang@eburg.com>")
    (color-theme-mistyday "Misty Day" "Hari Kumar <Hari.Kumar@mtm.kuleuven.ac.be>")
    (color-theme-montz "Montz" "Brady Montz <bradym@becomm.com>")
    (color-theme-oswald "Oswald" "Tom Oswald <toswald@sharplabs.com>")
    (color-theme-parus "Parus" "Jon K Hellan <hellan@acm.org>")
    (color-theme-pierson "Pierson" "Dan L. Pierson <dan@sol.control.com>")
    (color-theme-ramangalahy "Ramangalahy" "Solofo Ramangalahy <solofo@irisa.fr>")
    (color-theme-raspopovic "Raspopovic" "Pedja Raspopovic <pedja@lsil.com>")
    (color-theme-retro-green "Retro Green" "Alex Schroeder <alex@gnu.org>")
    (color-theme-retro-orange "Retro Orange" "Alex Schroeder <alex@gnu.org>")
    (color-theme-robin-hood "Robin Hood" "Alex Schroeder <alex@gnu.org>")
    (color-theme-rotor "Rotor" "Jinwei Shen <shenjw@wam.umd.edu>")
    (color-theme-ryerson "Ryerson" "Luis Fernandes <elf@ee.ryerson.ca>")
    (color-theme-scintilla "Scintilla" "Gordon Messmer <yinyang@eburg.com>")
    (color-theme-sitaramv-nt "Sitaram NT"
			     "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")
    (color-theme-sitaramv-solaris "Sitaram Solaris"
				  "Sitaram Venkatraman <sitaramv@loc251.tandem.com>")
    (color-theme-snow "Snow" "Nicolas Rist <Nicolas.Rist@alcatel.de>")
    (color-theme-snowish "Snowish" "Girish Bharadwaj <girishb@mvps.org>")
    (color-theme-standard "Standard Emacs" "Emacs Team, added by Alex Schroeder <alex@gnu.org>")
    (color-theme-subtle-hacker "Subtle Hacker" "Colin Walters <levanti@verbum.org>")
    (color-theme-taming-mr-arneson "Taming Mr Arneson" "Erik Arneson <erik@aarg.net>")
    (color-theme-taylor "Taylor" "Art Taylor <reeses@hemisphere.org>")
    (color-theme-wheat "Wheat" "Alex Schroeder <alex@gnu.org>")
    (color-theme-pok-wob "White On Black" "S. Pokrovsky <pok@nbsp.nsk.su>")
    (color-theme-pok-wog "White On Grey" "S. Pokrovsky <pok@nbsp.nsk.su>")
    (color-theme-xemacs "Standard XEmacs" "XEmacs Team, added by Alex Schroeder <alex@gnu.org>"))
  "List of color themes.

Each THEME is itself a three element list (FUNC NAME MAINTAINER).

FUNC is a color theme function which does the setup.  The function
FUNC may call `color-theme-install'.  The color theme function may be
interactive.

NAME is the name of the theme and MAINTAINER is the name and/or email of
the maintainer of the theme.

If you defined your own color theme and want to add it to this list,
use something like this:

  (add-to-list 'color-themes '(color-theme-gnome2 \"Gnome2\" \"Alex\"))")

;;; Functions

(defun color-theme-backup-original-values ()
  "Back up the original `default-frame-alist'.
The values are stored in `color-theme-original-frame-alist' on
startup."
  (if (null color-theme-original-frame-alist)
      (setq color-theme-original-frame-alist
	    (color-theme-filter (frame-parameters (selected-frame))
				color-theme-legal-frame-parameters))))
(add-hook 'after-init-hook 'color-theme-backup-original-values)

(defun color-theme-select ()
  "Displays a special buffer for selecting and installing a color theme."
  (interactive)
  (switch-to-buffer (get-buffer-create color-theme-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  ;; recreate the snapshot if necessary
  (when (or (not (assq 'color-theme-snapshot color-themes))
	    (not (commandp 'color-theme-snapshot)))
    (fset 'color-theme-snapshot (color-theme-make-snapshot))
    (setq color-themes (delq (assq 'color-theme-snapshot color-themes)
			     color-themes)
	  color-themes (append '((color-theme-snapshot
				  "[Reset]" "Undo changes, if possible.")
				 (bury-buffer
				  "[Quit]" "Bury this buffer."))
			     color-themes)))
  (let ((themes color-themes))
    (while themes
      (let* ((theme (car themes))
	     (func (nth 0 theme))
	     (name (nth 1 theme))
	     (author (nth 2 theme))
	     (desc))
	(setq desc (format "%-23s %s" name author))
	(put-text-property 0 (length desc) 'color-theme func desc)
	(put-text-property 0 (length name) 'face 'bold desc)
	(put-text-property 0 (length name) 'mouse-face 'highlight desc)
	(insert desc)
	(newline))
      (setq themes (cdr themes))))
  (beginning-of-buffer)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (color-theme-mode))

(require 'easymenu)
(easy-menu-add-item nil '("tools") "--")
(easy-menu-add-item  nil '("tools")
  ["Color Themes" color-theme-select t])

(defun color-theme-mode ()
  "Major mode to select and install color themes.

Use \\[color-theme-install-at-point] to install a color theme on all frames.
Use \\[color-theme-install-at-point-for-current-frame] to install a color theme for the current frame only.

The changes are applied on top of your current setup.  This is a
feature.

Some of the themes should be considered extensions to the standard color
theme: they modify only a limited number of faces and variables.  To
verify the final look of a color theme, install the standard color
theme, then install the other color theme.  This is a feature. It allows
you to mix several color themes.

Use \\[color-theme-describe] to read more about the color theme function at point.
If you want to install the color theme permanently, put the call to the
color theme function into your ~/.emacs:

    \(require 'color-theme)
    \(color-theme-gnome2)

If you worry about the size of color-theme.el: You are right.  Use
\\[color-theme-print] to print the current color theme and save the resulting buffer
as ~/.emacs-color-theme.  Now you can install only this specific color
theme in your .emacs:

    \(load-file \"~/.emacs-color-theme\")
    \(my-color-theme)

The Emacs menu is not affected by color themes within Emacs.  Depending
on the toolkit you used to compile Emacs, you might have to set specific
X ressources.  See the info manual for more information.  Here is an
example ~/.Xdefaults fragment:

    emacs*Background: DarkSlateGray
    emacs*Foreground: wheat

\\{color-theme-mode-map}

The color themes are listed in `color-themes', which see."
  (kill-all-local-variables)
  (setq major-mode 'color-theme-mode)
  (setq mode-name "Color Themes")
  (use-local-map color-theme-mode-map)
  (when (functionp 'goto-address); Emacs
    (goto-address))
  (run-hooks 'color-theme-mode-hook))

;;; Commands in Color Theme Selection mode

(defun color-theme-describe ()
  "Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'."
  (interactive)
  (describe-function (get-text-property (point) 'color-theme)))

(defun color-theme-install-at-mouse (event)
  "Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (color-theme-install-at-point)))

(defun color-theme-install-at-point ()
  "Install color theme at point.
This calls the value of the text-property `color-theme' at point.
The text-property `color-theme' should be a color theme function.
See `color-themes'."
  (interactive)
  (let ((func (get-text-property (point) 'color-theme)))
    ;; install theme
    (if func
	(funcall func))
    ;; If goto-address is being used, remove all overlays in the current
    ;; buffer and run it again.	 The face used for the mail addresses in
    ;; the the color theme selection buffer is based on the variable
    ;; goto-address-mail-face.	Changes in that variable will not affect
    ;; existing overlays, however, thereby confusing users.
    (when (functionp 'goto-address); Emacs
      (let* ((them (overlays-in (point-min) (point-max))))
	(while them
	  (delete-overlay (car them))
	  (setq them (cdr them)))
	(goto-address)))))

(defun color-theme-install-at-point-for-current-frame ()
  "Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'."
  (interactive)
  (let ((color-theme-is-global nil))
    (color-theme-install-at-point)))



;; Taking a snapshot of the current color theme and pretty printing it.

(defun color-theme-filter (old-list regexp &optional exclude)
  "Filter OLD-LIST.
The resulting list will be newly allocated and contains only elements
with names matching REGEXP.  OLD-LIST may be a list or an alist.  If you
want to filter a plist, use `color-theme-alist' to convert your plist to
an alist, first.

If the optional argument EXCLUDE is non-nil, then the sense is
reversed: only non-matching elements will be retained."
  (let (elem new-list)
    (while old-list
      (setq elem (car old-list))
      (setq name (symbol-name (if (listp elem) (car elem) elem)))
      (when (or (and (not exclude)
		     (string-match regexp name))
		(and exclude
		     (not (string-match regexp name))))
	(add-to-list 'new-list elem))
      (setq old-list (cdr old-list)))
    new-list))

(defun color-theme-spec (face)
  "Return a list for FACE which has the form (FACE SPEC).
See `defface' for the format of SPEC.  In this case we use only one
DISPLAY, t, and determine ATTS using `color-theme-face-attr-construct'.
If ATTS is nil, (nil) is used  instead."
  (let ((atts (color-theme-face-attr-construct face)))
    (if atts
	`(,face ((t ,atts)))
      `(,face ((t (nil)))))))

(defun color-theme-get-params (&optional master-params)
  "Return a list of frame parameter settings usable in a color theme.
Such an alist may be installed by `color-theme-install-frame-params'.  The
frame parameters returned must match `color-theme-legal-frame-parameters'.

If the optional argument MASTER-PARAMS is provided, then the alist returned
will only contain frame parameters with settings differing from MASTER-PARAMS."
  (let ((params (color-theme-filter (frame-parameters (selected-frame))
				    color-theme-legal-frame-parameters))
	(param)
	(new-params))
    (while params
      (setq param (car params))
      (setq params (cdr params))
      (unless (member param master-params)
	(add-to-list 'new-params param)))
    ;; For XEmacs, take the default and text-cursor face into account.
    (when color-theme-xemacs-p
      (add-to-list 'new-params (cons 'foreground-color (color-theme-default-foreground)))
      (add-to-list 'new-params (cons 'background-color (color-theme-default-background)))
      (add-to-list 'new-params (cons 'cursor-color (color-theme-background 'text-cursor))))
    (sort new-params (lambda (a b) (string< (symbol-name (car a))
					    (symbol-name (car b)))))))

(defun color-theme-get-vars (&optional master-vars)
  "Return a list of variable settings usable in a color theme.
Such an alist may be installed by `color-theme-install-variables'.
The variable names must match `color-theme-legal-variables', and the
variable must be a user variable according to `user-variable-p'.

If the optional argument MASTER-VARS is provided, then the alist returned
will only contain variables with settings differing from MASTER-VARS."
  (let ((vars)
	(val))
    (mapatoms (lambda (v)
		(and (boundp v)
		     (user-variable-p v)
		     (string-match color-theme-legal-variables
				   (symbol-name v))
		     (setq val (eval v))
		     (unless (member (cons v val) master-vars)
		       (add-to-list 'vars (cons v val))))))
    (sort vars (lambda (a b) (string< (car a) (car b))))))

(defun color-theme-print-alist (func &rest args)
  "Print an alist returned by function FUNC.
Possible functions might be `color-theme-get-vars' or
`color-theme-get-params'.  The optional arguments ARGS will
be passed along to the function."
  (let ((alist (apply func args))
	(elem))
    (insert "\n	    " (if alist "(" "nil"))
    (while alist
      (setq elem (car alist))
      (setq alist (cdr alist))
      (when (= (preceding-char) ?\)) (insert "\n      "))
      (prin1 elem (current-buffer)))
    (when (= (preceding-char) ?\)) (insert ")"))))

(defun color-theme-get-faces (&optional master-faces)
  "Return a list of faces usable in a color theme.
Such an alist may be installed by `color-theme-install-faces'.	The
faces returned must not match `color-theme-illegal-faces'.

If the optional argument MASTER-FACES is provided, then the list returned
will only contain faces with settings differing from MASTER-FACES."
  (let ((faces (color-theme-filter (face-list) color-theme-illegal-faces t))
	(new-faces) face)
    ;; Put default face first, therefore default must be the last face
    ;; added to the front of the list.
    (while faces
      (setq face (car faces))
      (setq faces (cdr faces))
      (unless (or (eq face 'default)
		  (and master-faces
		       (color-theme-spec-match-p face (cadr (assoc face master-faces)))))
	(add-to-list 'new-faces face)))
    (setq new-faces (sort new-faces 'string-lessp))
    (unless (and master-faces
		 (color-theme-spec-match-p 'default (cadr (assoc 'default master-faces))))
      (add-to-list 'new-faces 'default))
    new-faces))

(defun color-theme-get-face-definitions (&optional master-faces)
  "Return face settings usable in a color-theme.
Optional argument MASTER-FACES is passed along to  `color-theme-get-faces'."
  (let ((faces (color-theme-get-faces master-faces))
	face result)
    (while faces
      (setq face (car faces))
      (setq faces (cdr faces))
      (setq result (cons (color-theme-spec face) result)))
    result))

(defun color-theme-print-faces (&optional master-faces)
  "Print face settings for all faces returned by `color-theme-get-faces'.
Optional argument MASTER-FACES is passed along to  `color-theme-get-faces'."
  (let ((faces (color-theme-get-faces master-faces))
	(face))
    (when faces (insert "\n    "))
    (while faces
      (setq face (car faces))
      (setq faces (cdr faces))
      (when (= (preceding-char) ?\)) (insert "\n    "))
      (prin1 (color-theme-spec face) (current-buffer)))))

(defun color-theme-reset-faces ()
  "Reset face settings for all faces returned by `color-theme-get-faces'."
  (let ((faces (color-theme-get-faces nil))
	(face) (spec) (entry)
	(frame (if color-theme-is-global nil (selected-frame))))
    (while faces
      (setq entry (color-theme-spec (car faces)))
      (setq face (nth 0 entry))
      (setq spec '((t (nil))))
      (setq faces (cdr faces))
      (if (functionp 'face-spec-reset-face)
	  (face-spec-reset-face face frame)
	(face-spec-set face spec frame)
	(if color-theme-is-global
	    (put face 'face-defface-spec spec))))))

(defun color-theme-print (&optional arg buf)
  "Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    \(require 'color-theme)
    \(defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      \(interactive)
      \(color-theme-install
       '(...
	 ...
	 ...)))
    \(my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    \(require 'color-theme)
    \(color-theme-gnome2)

If called with a prefix argument, this function tries to create a diff
color theme compared to the last color theme installed.	 The last color
theme installed is stored in the variable `color-theme-installed'.  The
default behaviour is a full dump of all relevant settings.  If you
already have two color themes and want to make one of them the parent of
the other, be sure to use a numeric prefix when you install the second
color theme.  See `color-theme-install' for more information."
  (interactive "P")
  (message "Pretty printing current color theme function...")
  (switch-to-buffer (if buf
			buf
		      (get-buffer-create "*Color Theme*")))
  (unless buf
    (setq buffer-read-only nil)
    (erase-buffer))
  (let ((master-name)
	(master-theme))
    ;; Set master theme, making sure that the color theme function is
    ;; actually the one mentioned in the theme itself.
    (when (and arg
	       color-theme-installed
	       (equal color-theme-installed
		      (car (get 'color-theme-installed 'color-theme))))
      (setq master-name (symbol-name color-theme-installed)
	    master-theme (get 'color-theme-installed 'color-theme)))
    ;; insert defun
    (insert "(defun my-color-theme ()\n"
	    "  \"Color theme by "
	    (if (string= "" user-full-name)
		(user-login-name)
	      user-full-name)
	    ", created " (format-time-string "%Y-%m-%d") "."
	    (if master-name (format "\nBased on %s." master-name) "")
	    "\"\n"
	    "  (interactive)\n"
	    (if master-name (format "  (%s)\n  (let ((color-theme-cumulative t))\n" master-name) "")
	    "  (color-theme-install\n"
	    "	'(my-color-theme")
    ;; alist of frame parameters
    (color-theme-print-alist 'color-theme-get-params (nth 1 master-theme))
    ;; alist of variables
    (color-theme-print-alist 'color-theme-get-vars (nth 2 master-theme))
    ;; remaining elements of snapshot: face specs
    (color-theme-print-faces (cdr (cddr master-theme)))
    (insert ")))")
    (if master-name (insert ")")))
  (unless buf
    (emacs-lisp-mode))
  (goto-char (point-min))
  (message "Pretty printing current color theme function... done"))

;;; Creating a snapshot of the current color theme

(defun color-theme-snapshot nil)

(defun color-theme-make-snapshot ()
  "Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment."
  (eval `(lambda ()
	   "The color theme in use when the selection buffer was created.
\\[color-theme-select] creates the color theme selection buffer.  At the
same time, this snapshot is created as a very simple undo mechanism.
The snapshot is created via `color-theme-snapshot'."
	   (interactive)
	   (color-theme-install
	    '(color-theme-snapshot
	      ;; alist of frame parameters
	      ,(color-theme-get-params)
	      ;; alist of variables
	      ,(color-theme-get-vars)
	      ;; remaining elements of snapshot: face specs
	      ,@(color-theme-get-face-definitions))))))



;;; Installation of a color theme

(defun color-theme-install-frame-params (params)
  "Change frame parameters using alist PARAMETERS.

If `color-theme-is-global' is non-nil, all frames are modified using
`modify-frame-parameters' and the PARAMETERS are prepended to
`default-frame-alist'.	The value of `initial-frame-alist' is not
modified.  If `color-theme-is-global' is nil, only the selected frame is
modified.  If `color-theme-is-cumulative' is nil, the frame parameters
are restored from `color-theme-original-frame-alist'.

Called from `color-theme-install'."
  (let ((params (color-theme-filter
		 params color-theme-legal-frame-parameters)))
    ;; We have a new list in params now, therefore we may use
    ;; destructive nconc.
    (if color-theme-is-global
	(let ((frames (frame-list))
	      (frame))
	  (if (or color-theme-is-cumulative
		  (null color-theme-original-frame-alist))
	      (setq default-frame-alist
		    (nconc params (color-theme-alist default-frame-alist)))
	    (setq default-frame-alist
		  (nconc params color-theme-original-frame-alist)))
	  (while frames
	    (setq frame (car frames))
	    (setq frames (cdr frames))
	    (modify-frame-parameters frame default-frame-alist)))
       (modify-frame-parameters (selected-frame) params))
    ;; Some frame-parameters affect the default and text-cursor face in XEmacs.
    (if color-theme-xemacs-p
      (let ((fg (cdr (assq 'foreground-color (frame-parameters))))
	    (bg (cdr (assq 'background-color (frame-parameters))))
	    (cc (cdr (assq 'cursor-color (frame-parameters))))
	    (frame (if color-theme-is-global nil (selected-frame))))
	(if fg (set-face-foreground 'default fg frame))
	(if bg (set-face-background 'default bg frame))
	;; The cursor color is the background; in order to simulate
	;; inverse-video, the background is used as the foreground --
	;; unless the background is the same as the cursor, in which
	;; case we use the foreground.
	(when cc
	  (set-face-background 'text-cursor cc frame)
	  (set-face-foreground 'text-cursor
			       (if (equal bg cc) fg bg) frame))))))

(defun color-theme-install-variables (vars)
  "Change variables using alist VARS.
All variables matching `color-theme-legal-variables' are set.

If `color-theme-is-global' and `color-theme-xemacs-p' are nil, variables
are made frame-local before setting them.  Variables are set using `set'
in either case.	 This may lead to problems if changing the variable
requires the usage of the function specified with the :set tag in
defcustom declarations.

Called from `color-theme-install'."
  (let ((vars (color-theme-filter vars color-theme-legal-variables))
	(var))
    (while vars
      (setq var (car vars))
      (setq vars (cdr vars))
      (if (or color-theme-is-global color-theme-xemacs-p)
	  (set (car var) (cdr var))
	(make-variable-frame-local (car var))
	(modify-frame-parameters (selected-frame) (list var))))))

(defun color-theme-install-faces (faces)
  "Change faces using FACES.

Change faces for all frames and create any faces listed in FACES which
don't exist.  The modified faces will be marked as \"unchanged from
its standard setting\".	 This is OK, since the changes made by
installing a color theme should never by saved in .emacs by
customization code.

FACES should be a list where each entry has the form:

  (FACE SPEC)

See `defface' for the format of SPEC.

If `color-theme-is-global' is non-nil, faces are modified on all frames
using `face-spec-set'.	If `color-theme-is-global' is nil, faces are
only modified on the selected frame.  Non-existing faces are created
using `make-empty-face' in either case.	 If `color-theme-is-cumulative'
is nil, all faces are reset before installing the new faces.

Called from `color-theme-install'."
  ;; clear all previous faces
  (when (not color-theme-is-cumulative)
    (color-theme-reset-faces))
  ;; install new faces
  (let ((faces (color-theme-filter faces color-theme-illegal-faces t))
	(frame (if color-theme-is-global nil (selected-frame))))
    (while faces
      (let* ((entry (car faces))
	     (face (nth 0 entry))
	     (spec (nth 1 entry)))
	(setq faces (cdr faces))
	;; Create and specify face unless running XEmacs and dealing
	;; with the default or the text-cursor face.  These faces are
	;; changed by frame parameters.
	(unless (and color-theme-xemacs-p
		     (memq face '(default text-cursor)))
	  (or (facep face)
	      (make-empty-face face))
	  ;; using a spec of ((t (nil))) to reset a face doesn't work
	  ;; in Emacs 21, we use the new function face-spec-reset-face
	  ;; instead
	  (if (and (functionp 'face-spec-reset-face)
		   (equal spec '((t (nil)))))
		(face-spec-reset-face face frame)
	    (face-spec-set face spec frame)
	    (if color-theme-is-global
		(put face 'face-defface-spec spec))))))))

;; `custom-set-faces' is unusable here because it doesn't allow to set
;; the faces for one frame only.

;; Emacs `face-spec-set': If FRAME is nil, the face is created and
;; marked as a customized face.	 This is achieved by setting the
;; `face-defface-spec' property.  If we don't, new frames will not be
;; created using the face we installed because `face-spec-set' is
;; broken: If given a FRAME of nil, it will not set the default faces;
;; instead it will walk through all the frames and set modify the faces.
;; If we do set a property (`saved-face' or `face-defface-spec'),
;; `make-frame' will correctly use the faces we defined with our color
;; theme.  If we used the property `saved-face',
;; `customize-save-customized' will save all the faces installed as part
;; of a color-theme in .emacs.	That's why we use the
;; `face-defface-spec' property.

(defvar color-theme-installed nil
  "Color theme function used to install the current color theme.
Calling this function should reset the current color theme in case
it has been modified.

The entire theme definition is stored in the property `color-theme' of
this variable.	The theme definition is the same one that was used in
the call to `color-theme-install'.")

(defun color-theme-install (theme)
  "Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
The function is stored in the variable `color-theme-installed'.	 The
entire THEME is stored in property `color-theme' of that variable.  See
below for information on how to prevent this, and why you would want to
do this.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.

VARIABLE-DEFINITIONS is an alist of variable settings.	These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes.

If you want to factor two color themes, you can use a prefix argument
for the second color theme function.  If a color theme function is
called with a prefix argument, then the installation of that color theme
function is not recorded in the variable `color-theme-installed'.
Example: You have two huge color theme functions A and B, and you want
to rewrite B such that it calls A and then installs the differences
between A and B.  This is how to proceed: Start emacs -q, load
color-theme, install A, install B with a numeric prefix, print it with a
numeric prefix:

\\[load-library] RET ~/elisp/color-theme.el RET
\\[color-theme-gnome2] RET
\\[universal-argument] \\[color-theme-subtle-hacker] RET
\\[universal-argument] \\[color-theme-print] RET

Happy hacking."
  ;; optional color theme function (for backwards compatibility)
  (when (functionp (car theme))
    (when (null current-prefix-arg)
      (setq color-theme-installed (car theme))
      (put 'color-theme-installed 'color-theme theme))
    (setq theme (cdr theme)))
  ;; frame parameters
  (color-theme-install-frame-params (car theme))
  (setq theme (cdr theme))
  ;; optional variable defintions (for backwards compatibility)
  (when (listp (caar theme))
    (color-theme-install-variables (car theme))
    (setq theme (cdr theme)))
  ;; face definitions
  (color-theme-install-faces theme))



;; Sharing your stuff

(defun color-theme-submit ()
  "Submit your color-theme to the maintainer."
  (interactive)
  (require 'reporter)
  (let ((reporter-eval-buffer (current-buffer))
	final-resting-place
	after-sep-pos
	(reporter-status-message "Formatting buffer...")
	(reporter-status-count 0)
	(problem "Yet another color-theme")
	(agent (reporter-compose-outgoing))
	(mailbuf (current-buffer))
	hookvar)
    ;; do the work
    (require 'sendmail)
    ;; If mailbuf did not get made visible before, make it visible now.
    (let (same-window-buffer-names same-window-regexps)
      (pop-to-buffer mailbuf)
      ;; Just in case the original buffer is not visible now, bring it
      ;; back somewhere
      (and pop-up-windows (display-buffer reporter-eval-buffer)))
    (goto-char (point-min))
    (mail-position-on-field "to")
    (insert color-theme-maintainer-address)
    (mail-position-on-field "subject")
    (insert problem)
    ;; move point to the body of the message
    (mail-text)
    (setq after-sep-pos (point))
    (unwind-protect
	(progn
	  (setq final-resting-place (point-marker))
	  (goto-char final-resting-place))
      (color-theme-print nil (current-buffer))
      (goto-char final-resting-place)
      (insert "\n\n")
      (goto-char final-resting-place)
      (insert "Hello there!\n\nHere's my color theme named: ")
      (set-marker final-resting-place nil))
    ;; compose the minibuf message and display this.
    (let* ((sendkey-whereis (where-is-internal
			     (get agent 'sendfunc) nil t))
	   (abortkey-whereis (where-is-internal
			      (get agent 'abortfunc) nil t))
	   (sendkey (if sendkey-whereis
			(key-description sendkey-whereis)
		      "C-c C-c")); TBD: BOGUS hardcode
	   (abortkey (if abortkey-whereis
			 (key-description abortkey-whereis)
		       "M-x kill-buffer"))); TBD: BOGUS hardcode
      (message "Enter a message and type %s to send or %s to abort."
	       sendkey abortkey))))



;;; The color theme functions

(autoload 'color-theme-gnome "color-theme-gnome" nil t)
(autoload 'color-theme-blue-gnus "color-theme-blue-gnus" nil t)
(autoload 'color-theme-gnome2 "color-theme-gnome2" nil t)
(autoload 'color-theme-simple-1 "color-theme-simple-1" nil t)
(autoload 'color-theme-jonadabian "color-theme-jonadabian" nil t)
(autoload 'color-theme-ryerson "color-theme-ryerson" nil t)
(autoload 'color-theme-wheat "color-theme-wheat" nil t)
(autoload 'color-theme-standard "color-theme-standard" nil t)
(autoload 'color-theme-fischmeister "color-theme-fischmeister" nil t)
(autoload 'color-theme-sitaramv-solaris "color-theme-sitaramv-solaris" nil t)
(autoload 'color-theme-sitaramv-nt "color-theme-sitaramv-nt" nil t)
(autoload 'color-theme-billw "color-theme-billw" nil t)
(autoload 'color-theme-retro-green "color-theme-retro-green" nil t)
(autoload 'color-theme-retro-orange "color-theme-retro-orange" nil t)
(autoload 'color-theme-subtle-hacker "color-theme-subtle-hacker" nil t)
(autoload 'color-theme-pok-wog "color-theme-pok-wog" nil t)
(autoload 'color-theme-pok-wob "color-theme-pok-wob" nil t)
(autoload 'color-theme-blue-sea "color-theme-blue-sea" nil t)
(autoload 'color-theme-rotor "color-theme-rotor" nil t)
(autoload 'color-theme-pierson "color-theme-pierson" nil t)
(autoload 'color-theme-xemacs "color-theme-xemacs" nil t)
(autoload 'color-theme-jsc-light "color-theme-jsc-light" nil t)
(autoload 'color-theme-jsc-dark "color-theme-jsc-dark" nil t)
(autoload 'color-theme-greiner "color-theme-greiner" nil t)
(autoload 'color-theme-jb-simple "color-theme-jb-simple" nil t)
(autoload 'color-theme-goldenrod "color-theme-goldenrod" nil t)
(autoload 'color-theme-ramangalahy "color-theme-ramangalahy" nil t)
(autoload 'color-theme-raspopovic "color-theme-raspopovic" nil t)
(autoload 'color-theme-taylor "color-theme-taylor" nil t)
(autoload 'color-theme-marquardt "color-theme-marquardt" nil t)
(autoload 'color-theme-parus "color-theme-parus" nil t)
(autoload 'color-theme-high-contrast "color-theme-high-contrast" nil t)
(autoload 'color-theme-loyola "color-theme-loyola" nil t)
(autoload 'color-theme-infodoc "color-theme-infodoc" nil t)
(autoload 'color-theme-classic "color-theme-classic" nil t)
(autoload 'color-theme-scintilla "color-theme-scintilla" nil t)
(autoload 'color-theme-gtk-ide "color-theme-gtk-ide" nil t)
(autoload 'color-theme-midnight "color-theme-midnight" nil t)
(autoload 'color-theme-jedit-grey "color-theme-jedit-grey" nil t)
(autoload 'color-theme-snow "color-theme-snow" nil t)
(autoload 'color-theme-montz "color-theme-montz" nil t)
(autoload 'color-theme-aalto-light "color-theme-aalto-light" nil t)
(autoload 'color-theme-aalto-dark "color-theme-aalto-dark" nil t)
(autoload 'color-theme-blippblopp "color-theme-blippblopp" nil t)
(autoload 'color-theme-hober "color-theme-hober" nil t)
(autoload 'color-theme-bharadwaj "color-theme-bharadwaj" nil t)
(autoload 'color-theme-oswald "color-theme-oswald" nil t)
(autoload 'color-theme-robin-hood "color-theme-robin-hood" nil t)
(autoload 'color-theme-snowish "color-theme-snowish" nil t)
(autoload 'color-theme-dark-laptop "color-theme-dark-laptop" nil t)
(autoload 'color-theme-taming-mr-arneson "color-theme-taming-mr-arneson" nil t)
(autoload 'color-theme-digital-ofs1 "color-theme-digital-ofs1" nil t)
(autoload 'color-theme-mistyday "color-theme-mistyday" nil t)
(autoload 'color-theme-marine "color-theme-marine" nil t)
(provide 'color-theme)

;;; color-theme.el ends here
