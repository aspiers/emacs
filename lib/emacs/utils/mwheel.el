;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@cs.indiana.edu>
;; Keywords: mouse

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; 2000-07-28  Adam Spiers  <adam@spiers.net>
;;
;;         * enhanced to support scrolling with Control depressed

;;; Code:

(require 'custom)
(require 'cl)

(defconst mwheel-running-xemacs (string-match "XEmacs" (emacs-version)))

(defcustom mwheel-scroll-amount '(5 1 nil)
  "Amount to scroll windows by when spinning the mouse wheel.  This is
actually a list, where the first item is the amount to scroll on a
normal wheel event, the second is the amount to scroll when the wheel
is moved with the shift key depressed, and the second is the amount to
scroll when the wheel is moved with the shift key depressed.  This
should be the number of lines to scroll, or `nil' for near full
screen.  A near full screen is `next-screen-context-lines' less than a
full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "With Shift"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "With Control"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))))

(defcustom mwheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(if (not (fboundp 'event-button))
    (defun mwheel-event-button (event)
      (let ((x (symbol-name (event-basic-type event))))
	(if (not (string-match "^mouse-\\([0-9]+\\)" x))
	    (error "Not a button event: %S" event))
	(string-to-int (substring x (match-beginning 1) (match-end 1)))))
  (fset 'mwheel-event-button 'event-button))

(if (not (fboundp 'event-window))
    (defun mwheel-event-window (event)
      (posn-window (event-start event)))
  (fset 'mwheel-event-window 'event-window))

(defun mwheel-scroll (event)
  (interactive "e")
  (let ((curwin (if mwheel-follow-mouse
		    (prog1
			(selected-window)
		      (select-window (mwheel-event-window event)))))
	(amt (cond ((memq 'shift (event-modifiers event))
                    (nth 1 mwheel-scroll-amount))
                   ((memq 'control (event-modifiers event))
                    (nth 2 mwheel-scroll-amount))
                   (t (car mwheel-scroll-amount)))))
    (case (mwheel-event-button event)
      (4 (scroll-down amt))
      (5 (scroll-up amt))
      (otherwise (error "Bad binding in mwheel-scroll")))
    (if curwin (select-window curwin))))

(define-key global-map (if mwheel-running-xemacs 'button4 [mouse-4])
  'mwheel-scroll)

(define-key global-map (if mwheel-running-xemacs [(shift button4)] [S-mouse-4])
  'mwheel-scroll)

(define-key global-map (if mwheel-running-xemacs [(control button4)] [C-mouse-4])
  'mwheel-scroll)

(define-key global-map (if mwheel-running-xemacs 'button5 [mouse-5])
  'mwheel-scroll)

(define-key global-map (if mwheel-running-xemacs [(shift button5)] [S-mouse-5])
  'mwheel-scroll)

(define-key global-map (if mwheel-running-xemacs [(control button5)] [C-mouse-5])
  'mwheel-scroll)

(provide 'mwheel)
