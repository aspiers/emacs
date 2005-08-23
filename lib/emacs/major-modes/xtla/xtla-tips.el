;;; xtla-tips.el --- "Tip of the day" feature for Xtla.

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To raise the learning curve for xtla.el users. Some commands can
;; (optionaly) pop-up a buffer with a tip. Currently, `tla-commit'
;; does this.


;;; History:
;;
;; Created on October 2004 by Matthieu MOY

;;; Code:

(eval-and-compile
  (if (featurep 'xemacs)
      (require 'xtla-xemacs)
    (require 'xtla-emacs)))

(eval-and-compile
  (require 'xtla-defs)
  (require 'xtla))

(defconst tla-tips-array
  [
"Welcome to Xtla. I'm the tip buffer. I will appear from time to time
to show you interesting features that you may have missed! Disable me
by setting the variable `tla-tips-enabled' to nil.

Press q to exit, n to view next tip, p to view previous tip."
   "For the available tla commands Xtla provides a corresponding interactive
function.
Some examples:

M-x tla-inventory   ... runs tla inventory
M-x tla-undo        ... runs tla undo
M-x tla-changes     ... runs tla changes

Xtla prompts for the needed parameters."
   "Most interesting commands are available through a global keyboard
shortcut. Try \"C-x T C-h\" to get a list"
   "Xtla provides several major modes for different buffers. Each mode
has its own keybindings. Get help with \"C-h m\""
   "When Xtla is loaded, C-M-h in a minibuffer prompt gives you help
about the command being run."
   "When you are prompted for an archive, category, branch, version or
revision name, lots of keybindings are available. Get a list with \"C-h\"."
   "Xtla allows you to manage a list of bookmarks. Try \"C-x T b\" and add
bookmarks from the menu.  You may also add bookmarks from an archives,
category, version or revision buffer as well as from the tla-browse
buffer."
   "From the bookmark buffer, you can select some bookmarks and make
them partners with M-p. Afterwards, pressing 'M m' on a bookmark will
show you the missing patches from his partners."
   "You can add changelog style comments to your commit log by \"C-x T a\"."
   "You can enable ispell, flyspell or other useful mode for editing
log files by \"M-x customize-variable RET tla-log-edit-mode-hook RET\"."
   "By default, Xtla caches any log file you retrieve with
`tla-cat-log' or `tla-cat-archive-log' in ~/.arch-log-library. This
speeds up many Xtla operations.

You can disable this by setting `tla-log-library-greedy' to nil."
   "Xtla is highly customizable.
Start it by \"M-x customize-group RET xtla RET\"."
   "In an *tla-changes* buffer you can quickly jump to the source file by
\"RET\", or view the source file in another window by \"v\", or start
an ediff session by \"e\" to inspect/reject parts of the changes."
   "In a *tla-changes* buffer, you can quickly jump from the list of
files to the corresponding patch hunk, and come back with \"j\""
   "After committing, you can review the last committed patch with
\"M-x tla-changes-last-revision RET\".

Usefull to review and fix a patch you've just merged without mixing
manual modifications and merge in the same patch."
   "After a merge, typing \"C-c m\" in the log buffer will generate
for you a summary line, keyword and body. This is highly
customizable."
   "Report bugs using M-x tla-submit-bug-report RET, or using the bug
tracker at https://gna.org/bugs/?group=xtla-el"
   "You've got a nice, graphical, archive browser one M-x tla-browse
RET away."
   "In the bookmark buffer, pressing \"C-x C-f\" starts with the local
tree of the bookmark at point for the default directory."
   "SMerge mode is an Emacs minor mode usefull to resolve conflicts
after a --three-way merge. Xtla will enter this mode automatically
when you open a file with conflicts. Type M-x tla-conflicts-finish RET
to exit smerge mode and delete the corresponding .rej file."
   "\"C-x T e\" in a source file will open an ediff session with the
unmodified version of the file. From here, you can undo patch hunks
one by one with the key \"b\""
   "In the *tree-lint* buffer, with your cursor on a message, most
commands will apply to all the files listed under this message."
   ]
  "List of tips. Add more !")

(defvar tla-tips-number 0
  "Number of the last tip viewed.
Will be saved in state.el")

(defun tla-tips-message-number (number)
  "Return the message number NUMBER, as a string."
  (let ((number (mod number (length tla-tips-array))))
    (aref tla-tips-array number)))

(define-derived-mode tla-tips-mode fundamental-mode "tla-tips"
  "Major mode for buffers displaying tip of the day.

Commands:
\\{tla-tips-mode-map}"
  (toggle-read-only 1))


(defun tla-tips-popup-number (number &optional noswitch)
  "Pops up tip number NUMBER."
  (let ((message (tla-tips-message-number number)))
    (switch-to-buffer (get-buffer-create "*tla-tip*"))
    (tla-tips-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (tla--face-add
               "*************************   Did you know?   *************************"
               'tla-messages)
              "\n\n")
      (insert message)
      (newline 2)
      (insert (tla--face-add
               "*********************************************************************"
               'tla-messages))
      (goto-char (point-min))
      )
    (when (and (not noswitch) (eq tla-switch-to-buffer-mode 'single-window))
      ;; If mode is single-window, switch to another window (and if
      ;; necessary, split the frame) anyway.
      (when (= (length (window-list)) 1)
	(split-window-vertically))
      (other-window 1))))

;;;###autoload
(defun tla-tips-popup-maybe ()
  "Pop up a buffer with a tip if tips are enabled.

see `tla-tips-enabled'."
  (when tla-tips-enabled
    (tla-tips-popup)))

;;;###autoload
(defun tla-tips-popup (&optional direction noswitch)
  "Pop up a buffer with a tip message.

Don't use this function from Xtla. Use `tla-tips-popup-maybe'
instead."
  (interactive)
  (tla-load-state)
  (tla-tips-popup-number tla-tips-number noswitch)
  (setq tla-tips-number
        (mod (+ tla-tips-number (or direction 1)) (length tla-tips-array)))
  (tla-save-state))

(defun tla-tips-next-tip ()
  "Show next tip."
  (interactive)
  (tla-tips-popup 1 t))

(defun tla-tips-previous-tip ()
  "Show previous tip."
  (interactive)
  (tla-tips-popup -1 t))

(defun tla-tips-customize ()
  "Run customize group for tla-tips."
  (interactive)
  (customize-group 'tla-tips))

(provide 'xtla-tips)
;;; xtla-tips.el ends here
;; arch-tag: 782d1e21-d7b5-4d1a-ac64-1fc41f1c13fe
