;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: mouse

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code will enable the use of the infamous 'wheel' on the new
;; crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
;; events are sent as button4/button5 events.

;; I for one would prefer some way of converting the button4/button5
;; events into different event types, like 'mwheel-up' or
;; 'mwheel-down', but I cannot find a way to do this very easily (or
;; portably), so for now I just live with it.

;; To enable this code, simply put this at the top of your .emacs
;; file:
;;
;; (mwheel-install)

;; 2000-07-28  Adam Spiers  <adam@spiers.net>
;;
;;         * enhanced to support scrolling with Control depressed

;;; Code:

(require 'custom)

(defconst mwheel-running-xemacs (string-match "XEmacs" (emacs-version)))

;; Setter function for mouse-button user-options.  Switch Mouse Wheel
;; mode off and on again so that the old button is unbound and
;; new button is bound to mwheel-scroll.

(defun mouse-wheel-change-button (var button)
  (set-default var button)
  (when mouse-wheel-mode
    (mouse-wheel-mode 0)
    (mouse-wheel-mode 1)))

(defcustom mouse-wheel-down-button 4
  "Mouse button number for scrolling down."
  :group 'mouse
  :type 'integer
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-up-button 5
  "Mouse button number for scrolling up."
  :group 'mouse
  :type 'integer
  :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-scroll-amount '(5 1 nil)
  "Amount to scroll windows by when spinning the mouse wheel.  This is
actually a list, where the first item is the amount to scroll on a
normal wheel event, the second is the amount to scroll when the wheel
is moved with the shift key depressed, and the third is the amount to
scroll when the wheel is moved with the control key depressed.

Each item should be the number of lines to scroll, or `nil' for near full
screen.
A near full screen is `next-screen-context-lines' less than a
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

(defcustom mouse-wheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(eval-and-compile
  (if (not (fboundp 'event-button))
      (defun mwheel-event-button (event)
        (let ((x (symbol-name (event-basic-type event))))
          (if (not (string-match "^mouse-\\([0-9]+\\)" x))
              (error "Not a button event: %S" event))
          (string-to-int (substring x (match-beginning 1) (match-end 1)))))
    (fset 'mwheel-event-button 'event-button)))

(eval-and-compile
  (if (not (fboundp 'event-window))
      (defun mwheel-event-window (event)
        (posn-window (event-start event)))
    (fset 'mwheel-event-window 'event-window)))

(defun mwheel-scroll (event)
  (interactive "e")
  (let ((curwin (if mouse-wheel-follow-mouse
		    (prog1
			(selected-window)
		      (select-window (mwheel-event-window event)))))
	(amt (cond ((memq 'shift (event-modifiers event))
                    (nth 1 mouse-wheel-scroll-amount))
                   ((memq 'control (event-modifiers event))
                    (nth 2 mouse-wheel-scroll-amount))
                   (t (car mouse-wheel-scroll-amount)))))
    (unwind-protect
	(let ((button (mwheel-event-button event)))
	  (cond ((= button mouse-wheel-down-button) (scroll-down amt))
		((= button mouse-wheel-up-button) (scroll-up amt))
		(t (error "Bad binding in mwheel-scroll"))))
      (if curwin (select-window curwin)))))


;;; Note this definition must be at the end of the file, because
;;; `define-minor-mode' actually calls the mode-function if the
;;; associated variable is non-nil, which requires that all needed
;;; functions be already defined.
;;;###autoload
(define-minor-mode mouse-wheel-mode
  "Toggle mouse wheel support.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'mouse
  ;; In the latest versions of XEmacs, we could just use
  ;; (S-)*mouse-[45], since those are aliases for the button
  ;; equivalents in XEmacs, but I want this to work in as many
  ;; versions of XEmacs as it can.
  (let ((keys
	 (if (featurep 'xemacs)
	     (let ((down (intern (format "button%d" mouse-wheel-down-button)))
		   (up (intern (format "button%d" mouse-wheel-up-button))))
	       `(,down [(shift ,down)] [(control ,down)]
                 ,up   [(shift ,up)]   [(control ,up)]))
	   (let ((down (intern (format "mouse-%d" mouse-wheel-down-button)))
		 (s-down (intern (format "S-mouse-%d" mouse-wheel-down-button)))
		 (c-down (intern (format "C-mouse-%d" mouse-wheel-down-button)))
		 (up (intern (format "mouse-%d" mouse-wheel-up-button)))
		 (s-up (intern (format "S-mouse-%d" mouse-wheel-up-button)))
		 (c-up (intern (format "C-mouse-%d" mouse-wheel-up-button))))
	     `([,down] [,s-down] [,c-down] [,up] [,s-up] [,c-up])))))
    ;; This condition-case is here because Emacs 19 will throw an error
    ;; if you try to define a key that it does not know about.  I for one
    ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
    ;; that if the wheeled-mouse is there, it just works, and this way it
    ;; doesn't yell at me if I'm on my laptop or another machine, etc.
    (condition-case ()
	(dolist (key keys)
	  (cond (mouse-wheel-mode
		 (define-key global-map key 'mwheel-scroll))
		((eq (lookup-key global-map key) 'mwheel-scroll)
		 (define-key global-map key nil))))
      (error nil))))

;;; Compatibility entry point
;;;###autoload
(defun mwheel-install (&optional uninstall)
  "Enable mouse wheel support."
  (mouse-wheel-mode t))

(provide 'mwheel)

;;; mwheel.el ends here
