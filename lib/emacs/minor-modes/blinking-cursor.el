;;; Blinking cursor mode for GNU Emacs
;;; Copyright (C) 1997 Kyle E. Jones
;;; Customizability added by Adam Spiers <adam@spiers.net>, 23/1/2001
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle_jones@wonderworks.com)
;;; or from the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;
;;; Send bug reports to kyle_jones@wonderworks.com

;; To install, put this code into a file called blinking-cursor.el in
;; one of the directories in your Emacs load-path and byte-compile,
;; i.e. M-x byte-compile-file .  Then put
;;
;; (require 'blinking-cursor)
;; (blinking-cursor-mode 1)
;;
;; in your .emacs file.

(provide 'blinking-cursor)

(require 'timer)
(require 'custom)

(defconst blinking-cursor-version "1.01")

(defgroup blinking-cursor nil
  "Group for customizing Blinking Cursor mode."
  :prefix "blinking-cursor-"
  :group 'display)

(defcustom blinking-cursor-mode nil
  "Non-nil value means Blinking Cursor mode is active."
  :type  'boolean
  :group 'blinking-cursor)

(defcustom blinking-cursor-states '(("blue" "box" 0.5) ("gold" "box" 0.5))
  "List of states that will be cycled through to blink the cursor.

The list should contain at least two states.  Each state compromises
of a color, a cursor type, and corresponding to the `cursor-type'
window frame parameter.

The cursor will not cycle states unless Emacs is idle."
  :type  '(repeat
           (list :tag "Cursor state" :offset 4
                 (color :tag "color")
                 (radio :tag "shape"
                        (const "box")
                        (radio :tag "bar"
                               (const :tag "normal" "bar")
                               (integer :tag "custom width, pixels")))
                 (number :tag "duration, seconds")))
  :group 'blinking-cursor)

(defun blinking-cursor-mode (&optional arg)
  "Toggle Blinking Cursor mode.
With arg, turn Blinking Cursor mode on iff arg is positive.
When Blinking Cursor mode is enabled, the cursor blinks when Emacs is idle."
  (interactive "P")
  (setq blinking-cursor-mode (or (and arg (> (prefix-numeric-value arg) 0))
				 (and (null arg) (null blinking-cursor-mode))))
  (cond ((not blinking-cursor-mode)
         (cancel-function-timers 'blinking-cursor-start-blinking)
	 (blinking-cursor-stop-blinking))
	(t
         (run-with-idle-timer 0 t 'blinking-cursor-start-blinking))))

(defun blinking-cursor-start-blinking ()
  "Make the cursor start blinking."
  (add-hook 'pre-command-hook 'blinking-cursor-stop-blinking)
  (setq blinking-cursor-tick 0)
  (blinking-cursor-blink))

(defun blinking-cursor-stop-blinking ()
  "Stop the cursor's blinking.
Returns the cursor's color to the first color in the blinking-cursor-colors
array."
  (cancel-function-timers 'blinking-cursor-blink)
  (remove-hook 'pre-command-hook 'blinking-cursor-stop-blinking)
  (setq blinking-cursor-tick 0)
  (set-all-cursor-states (blinking-cursor-current-state)))

(defun blinking-cursor-blink (&rest ignored)
  "Changes the cursor color and shape, and sets a timer to do it again soon.

Uses colors, shapes, and timer durations from blinking-cursor-states."
  (condition-case err-data
      (progn
        (set-all-cursor-states (blinking-cursor-current-state))
        (blinking-cursor-tick-advance)
        (let ((timeout (blinking-cursor-next-timeout)))
          (run-with-timer timeout nil 'blinking-cursor-blink)))
    ;; if Emacs can't get a color don't throw an error.
    (error
     (message "blinking-cursor-blink signaled: %S" err-data))))

(defvar blinking-cursor-tick 0
  "Blinking cursor's internal tick clock.
Internal variable, do not set this.")

(defun blinking-cursor-tick-advance ()
  "Advances the blinking cursor's internal tick clock."
  (setq blinking-cursor-tick (1+ blinking-cursor-tick)))

(defun blinking-cursor-current-state ()
  "Returns the blinking cursor's current state."
  (nth
   (% blinking-cursor-tick (length blinking-cursor-states))
   blinking-cursor-states))

(defun blinking-cursor-next-timeout ()
  "Returns how long in seconds the current cursor state lasts."
  (nth 2 (blinking-cursor-current-state)))

(defun set-all-cursor-states (state)
  "Sets the blinking cursor color and shape in all frames."
  ;; there's probably a nicer, lispier way of doing this assignment
  (let ((color-name (car state))
        (shape (cadr state)))
    (set-cursor-state (selected-frame) color-name shape)))
;;      (mapcar (lambda (frame)
;;                (set-cursor-state frame color-name shape))
;;              (frame-list))))

(defun set-cursor-state (frame color-name shape)
  "Sets the blinking cursor color and shape for the given frame."
  ;; code fragments borrowed from frame.el
  (modify-frame-parameters
   frame
   (list (cons 'cursor-color color-name)
         (cons 'cursor-type
               (cond ((stringp shape) shape)
                     ((integerp shape) (cons 'bar shape)))))))
