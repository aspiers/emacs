;;; planner-gnats.el --- GNATS integration for the Emacs Planner

;; Copyright (C) 2005 Jeremy Cowgar (jeremy AT cowgar DOT com)

;; Author: Jeremy Cowgar (jeremy AT cowgar DOT com)
;; Version: 2005.08.20-17.59-stable
;; Keywords: planner, gnats
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php
;; URL: http://www.gnu.org/software/gnats/

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

;; GNATS is the GNU problem report management system (central database)
;; GNATS is a bug-tracking tool designed for use at a central "Support
;; Site".  Users who experience problems use electronic mail to
;; communicate these problems to "maintainers" at that Support Site.
;;
;; Add
;;
;;   (require 'planner-gnats)
;;
;; to your .emacs. You will then be able to call M-x
;; planner-create-task-from-buffer from Gnats edit or view buffers
;; with the correct annotation.

;; To add keybindings to Gnats, call (from .emacs)
;;
;;   (planner-gnats-insinuate)
;;

;; URLs are of the form gnats:pr-number

;;; Todo::

;; 1. The URL should really be something like
;; gnats:database/pr-number however, I'm not yet certian how to
;; handle all the possible variations to setup the correct server, for
;; instance, port number, user name, password. The password could then
;; get tricky, because what if the text file is stored in an unsecure
;; manner?
;;
;; So, right now this assumes your gnats is already setup for the
;; correct server, which will be the case for probably 90% of the
;; users.

;;; Code:

(require 'planner)
(require 'gnats)

;;;###autoload
(defun planner-gnats-annotation-from-gnats ()
  "If called from gnats-edit or gnats-view buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (string-match "^\\*gnats-\\(edit\\|\\view\\)-\\(.+\\)\\*"
					  (buffer-name))
    (let ((pr-num (match-string 2 (buffer-name))))
      (planner-make-link
       (concat "gnats:" pr-num)
       (concat "Gnats Bug: " pr-num)
       t))))

;;;###autoload
(defun planner-gnats-browse-url (url)
  "If this is a Gnats URL, view the pr (view-pr)."
  (when (string-match "^gnats:/*\\(.+\\)" url)
    (let ((pr-num (match-string 1 url)))
      (view-pr (string-to-number pr-num)))
    t))

(planner-add-protocol "gnats" 'planner-gnats-browse-url nil)
(custom-add-option 'planner-annotation-functions
                   'planner-gnats-annotation-from-gnats)
(add-hook 'planner-annotation-functions 'planner-gnats-annotation-from-gnats)

(provide 'planner-gnats)

;;; planner-gnats.el ends here
