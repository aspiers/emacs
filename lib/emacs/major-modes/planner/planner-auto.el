;;; planner-auto.el --- Automatic loading for Planner

;;; Commentary:
;;
;;;_* Commentary

;;;_ + Package description

;; Copyright (C) 2004 Sandra Jean Chua <sacha@free.net.ph>

;; Emacs Lisp Archive Entry
;; Filename: planner.el
;; Version: 2005.08.20-17.59-stable
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Description: Use Emacs for life planning
;; URL: http://sacha.free.net.ph/notebook/emacs/planner/planner.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/planner/ChangeLog
;; Compatibility: Emacs20, Emacs21

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Usage:

;; Add (require 'planner-auto) to your ~/.emacs.

;;; Code:

(mapcar
 (lambda (item)
   (eval-after-load (car item)
     `(require (quote ,(cdr item)))))
 '((erc . planner-erc)
   (bbdb . planner-bbdb)
   (gnus . planner-gnus)
   (wl . planner-wl)
   (ledger . planner-ledger)
   (mhe . planner-mhe)
   (rmail . planner-unix-mail)
   (timeclock . planner-timeclock)
   (vm . planner-vm)
   (w3m . planner-w3m)
   (wl . planner-wl)
   (bibtex . planner-bibtex)
   (bookmark . planner-bookmark)))
              
(autoload 'planner-mode "planner" nil t)
(autoload 'plan "planner" nil t)
(autoload 'planner-create-task "planner" nil t)
(autoload 'planner-create-task-from-buffer "planner" nil t)
(autoload 'planner-goto "planner" nil t)
(autoload 'planner-goto-plan-page "planner" nil t)
(autoload 'planner-show "planner" nil t)
(autoload 'planner-goto-today "planner" nil t)
(autoload 'planner-goto-most-recent "planner" nil t)
(autoload 'planner-calendar-goto "planner" nil t)
(autoload 'planner-calendar-show "planner" nil t)
(autoload 'planner-bbdb-annotation-from-bbdb "planner" nil t)
(autoload 'planner-bbdb-browse-url "planner" nil t)
(autoload 'planner-bbdb-resolve-url "planner" nil t)
(autoload 'planner-wl-insinuate "planner" nil t)
(autoload 'planner-wl-annotation-from-wl "planner-wl" nil t)
(autoload 'planner-wl-browse-url "planner-wl" nil t)
(autoload 'planner-vm-annotation-from-mail "planner-vm" nil t)
(autoload 'planner-vm-browse-url "planner-vm")  nil t
(autoload 'planner-unix-mail-annotation-from-mail "planner-unix-mail")  nil t
(autoload 'planner-unix-mail-browse-url "planner-unix-mail" nil t)
(autoload 'planner-rmail-annotation-from-mail "planner-rmail" nil t)
(autoload 'planner-rmail-browse-url "planner-rmail" nil t)
(autoload 'planner-mhe-annotation "planner-mhe" nil t)
(autoload 'planner-mhe-browse-url "planner-mhe" nil t)
(autoload 'planner-lisp-browse-url "planner-lisp" nil t)
(autoload 'planner-erc-annotation-from-erc "planner-erc" nil t)
(autoload 'planner-erc-browse-url "planner-erc" nil t)
(autoload 'planner-gnus-insinuate "planner-gnus" nil t)
(autoload 'planner-gnus-annotation "planner-gnus" nil t)
(autoload 'planner-gnus-browse-url "planner-gnus" nil t)
(autoload 'planner-accomplishments-insinuate "planner-accomplishments" nil t)
(autoload 'planner-accomplishments-update "planner-accomplishments" nil t)
(autoload 'planner-accomplishments-show "planner-accomplishments" nil t)
(autoload 'planner-bibtex-annotation "planner-bibtex" nil t)
(autoload 'planner-bibtex-browse-url "planner-bibtex" nil t)

(provide 'planner-auto)

;;; planner-auto.el ends here
