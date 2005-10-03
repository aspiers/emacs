;;; emacs-wiki-id.el --- emacs-wiki.el extension for global IDs

;; Copyright (C) 2003, 2004 Sacha Chua

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki.el
;; Keywords: hypermedia
;; Author: Sacha Chua (sacha AT free DOT net DOT ph)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain Emacs-friendly Wikis in a local directory
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: Emacs20, Emacs21, XEmacs21

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs-wiki identifiers provide globally-unique identifiers for
;; wiki pages. They also allow quick linking. They are of the form
;; {{Number:Page,Page,Page,Page,...}}.

(require 'emacs-wiki)

(defcustom emacs-wiki-id-tracking-file "~/.emacs-wiki-id"
  "File that stores the next emacs-wiki-id."
  :type 'file
  :group 'emacs-wiki)

(defvar emacs-wiki-id-regexp "{{\\([0-9]+\\):?\\([^}\n]*\\)}}"
  "Regexp that matches IDs.")

(defvar emacs-wiki-id-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'emacs-wiki-id-follow-id-at-point)
    (define-key map [(control ?m)] 'emacs-wiki-id-follow-id-at-point)
    (define-key map [(shift return)]
      'emacs-wiki-id-follow-previous-id-at-point)
    (if (featurep 'xemacs)
        (progn
          (define-key map [(button2)]
            'emacs-wiki-id-follow-id-at-mouse)
          (define-key map [(shift button2)]
            'emacs-wiki-id-follow-previous-id-at-mouse))
      (define-key map [(mouse-2)] 'emacs-wiki-id-follow-id-at-mouse)
      (define-key map [(shift mouse-2)]
        'emacs-wiki-id-follow-id-at-mouse))
    map)
  "Local keymap used by emacs-wiki when on an ID.")

(defun emacs-wiki-id-highlight ()
  "Highlight IDs as unobstrusive, clickable text."
  ;; FIXME: Code duplicated from planner-markup-region should be moved
  ;; into emacs-wiki.el eventually, I guess
  (let ((properties (list 'keymap emacs-wiki-id-keymap)))
    (if (and (fboundp 'overlay-put) (fboundp 'remove-overlays))
        (progn
          (remove-overlays (match-beginning 0) (match-end 0)
                           'emacs-wiki-id t)
          (let ((overlay (make-overlay (match-beginning 0)
                                       (match-end 0))))
            (overlay-put overlay 'emacs-wiki-id t)
            (overlay-put overlay 'priority 65)
            (while properties
              (overlay-put overlay (car properties) (cadr properties))
              (setq properties (cddr properties)))))
      (add-text-properties (match-beginning 0) (match-end 0)
                           properties))))

(defun emacs-wiki-id-markup ()
  "Replace emacs-wiki IDs with an ID and a link to the next one."
  ;; FIXME: Code duplicated from planner-markup-region should be moved
  ;; into emacs-wiki.el eventually, I guess
  (let* ((id (save-match-data (emacs-wiki-id-parse (match-string 0))))
         (next (emacs-wiki-id-next-page (emacs-wiki-page-name) id)))
    (concat "<a id=\"id" (number-to-string (car id)) "\" href=\""
            (emacs-wiki-transform-name next)
            "\">" (emacs-wiki-escape-html-string next) "</a>")))

;;;_* Internal function.

(defun emacs-wiki-id-next-value ()
  "Return the next ID value, updating `emacs-wiki-id-tracking-file'."
  (save-excursion
    (save-window-excursion
      (find-file emacs-wiki-id-tracking-file)
      (let ((value 1))
        ;; Load data from the file
        (condition-case err
            (setq value (string-to-number (buffer-string)))
          (error
           (progn
             (message
              "Error reading number from %s. Setting value to 1."
              emacs-wiki-id-tracking-file)
             (setq value 1))))
        ;; Save data to file
        (erase-buffer)
        (insert (number-to-string (1+ value)))
        (save-buffer)
        value))))

(defun emacs-wiki-id-make-id (&rest pages)
  "Return a globally unique ID as {{number[:page,page]}}.
If the first element of PAGES is a number, it is used as the ID
value.  Otherwise, the next ID value is taken from
`emacs-wiki-id-next-value'."
  (if pages
      (if (numberp (car pages))
          (concat "{{" (number-to-string (car pages)) ":"
                  (mapconcat 'identity (cdr pages) ",") "}}")
        (concat "{{" (number-to-string (emacs-wiki-id-next-value)) ":"
                (mapconcat 'identity pages ",") "}}"))
    (concat "{{" (number-to-string (emacs-wiki-id-next-value)) "}}")))

(defun emacs-wiki-id-parse (identifier)
  "Parse IDENTIFIER and return a list of the form (ID PAGE PAGE PAGE...)."
  (when (string-match emacs-wiki-id-regexp identifier)
    (append
     (list (string-to-number (emacs-wiki-match-string-no-properties
                              1 identifier)))
     (split-string (emacs-wiki-match-string-no-properties 2 identifier) ","))))

(defsubst emacs-wiki-id-add-page (identifier page)
  "Parse IDENTIFIER and append PAGE to it.
Return the new identifier."
  (emacs-wiki-id-make-id (append (emacs-wiki-id-parse identifier)
                                 page)))

(defsubst emacs-wiki-id-remove-page (identifier page)
  "Parse IDENTIFIER and remove PAGE to it.  Return the new identifier."
  (emacs-wiki-id-make-id (delete page
                                 (emacs-wiki-id-parse identifier))))

(defun emacs-wiki-id-next-page (current-page parsed-id)
  "Return the page following CURRENT-PAGE in PARSED-ID,
or nil if not found."
  (let ((tail (member current-page parsed-id)))
    (when tail
      (if (= (length tail) 1)
          (cadr parsed-id)
        (cadr tail)))))

(defun emacs-wiki-id-previous-page (current-page parsed-id)
  "Return the page before CURRENT-PAGE in PARSED-ID,
or nil if not found."
  (let ((pages (cdr parsed-id))
        last found)
    (while (and (not found) pages)
      (if (equal (car pages) current-page)
          (setq found (or last (car (last pages))))
        (setq last (car pages))
        (setq pages (cdr pages))))
    found))

(defun emacs-wiki-id-visit (page parsed-id &optional reverse)
  "Jump to PAGE and search for PARSED-ID.
If REVERSE is non-nil, search backward."
  (emacs-wiki-visit-link page)
  (goto-char (if reverse (point-max) (point-min)))
  (if (funcall
       (if reverse
           're-search-backward
         're-search-forward)
       (concat "{{"
               (number-to-string
                (car parsed-id)) ":?[^}\n]*}}") nil t)
      (goto-char (match-beginning 0))
    (message "Not found.")
    nil))

(defun emacs-wiki-id-visit-next-page (current-page parsed-id)
  "Jump to the ID in the page that follows CURRENT-PAGE based on
PARSED-ID."
  (forward-char 1)
  (if (re-search-forward (concat "{{"
                                 (number-to-string (car parsed-id))
                                 ":?[^}]*}}") nil t)
      (goto-char (match-beginning 0))
    (emacs-wiki-id-visit (emacs-wiki-id-next-page current-page
                                                  parsed-id)
                         parsed-id)))

(defun emacs-wiki-id-visit-previous-page (current-page parsed-id)
  "Jump to the ID in the page before CURRENT-PAGE based on PARSED-ID."
  (if (re-search-backward (concat "{{"
                                  (number-to-string (car parsed-id))
                                  ":?[^}]*}}") nil t)
      (goto-char (match-beginning 0))
    (emacs-wiki-id-visit (emacs-wiki-id-previous-page current-page
                                                      parsed-id)
                         parsed-id t)))

(defun emacs-wiki-id-at-point (&optional pos)
  "Return non-nil if an ID is at point or POS."
  (if (or (null pos)
          (and (char-after pos)
               (not (eq (char-syntax (char-after pos)) ? ))))
      (let ((case-fold-search nil)
            (here (or pos (point))))
        (save-excursion
          (goto-char here)
          (when
              (and (eq (char-after here) ?{)
                   (> here (point-min))
                   (eq (char-after (1- here)) ?{))
            (goto-char (1- here)))
          (and
           (or
            (looking-at "{{")
            (search-backward "{{" (emacs-wiki-line-beginning-position) t))
           (looking-at emacs-wiki-id-regexp)
           (<= here (match-end 0)))))))

(defun emacs-wiki-id-follow-id-at-point (&optional reverse)
  "Visit the next page linked to by this ID."
  (interactive "P")
  (if (emacs-wiki-id-at-point)
      (funcall
       (if reverse 'emacs-wiki-id-visit-previous-page
         'emacs-wiki-id-visit-next-page)
       (emacs-wiki-page-name)
       (emacs-wiki-id-parse
        (emacs-wiki-match-string-no-properties 0)))
    (error "There is no valid ID at point")))

(defun emacs-wiki-id-follow-previous-id-at-point ()
  "Visit the next page linked to by this ID."
  (interactive)
  (if (emacs-wiki-id-at-point)
      (emacs-wiki-id-visit-previous-page
       (emacs-wiki-page-name)
       (emacs-wiki-id-parse
        (emacs-wiki-match-string-no-properties 0)))
    (error "There is no valid ID at point")))

(defun emacs-wiki-id-follow-id-at-mouse (event)
  "Jump to the next page linked to by the ID at the position in EVENT."
  (interactive "eN")
  (save-excursion
    (cond ((fboundp 'event-window)      ; XEmacs
           (set-buffer (window-buffer (event-window event)))
           (and (event-point event) (goto-char (event-point event))))
          ((fboundp 'posn-window)       ; Emacs
           (set-buffer (window-buffer
                        (posn-window (event-start event))))
           (goto-char (posn-point (event-start event)))))
    (emacs-wiki-id-follow-id-at-point)))

(defvar emacs-wiki-id-prepare-for-update-hook nil
  "Hook run in preparation for ID updates.")

(defvar emacs-wiki-id-delete-functions nil
  "Hook run to delete an ID.")

(defvar emacs-wiki-id-update-functions nil
  "Hook run to update an ID.")

(defvar emacs-wiki-id-create-functions nil
  "Hook run to create an ID.")

(defun emacs-wiki-id-insert-at-point (&optional other-page)
  "Insert an ID at point.
OTHER-PAGE, if specified, is added to the list of pages linked."
  (interactive)
  (insert
   (if other-page
       (emacs-wiki-id-make-id (emacs-wiki-page-name) other-page)
     (emacs-wiki-id-make-id (emacs-wiki-page-name)))))

(defun emacs-wiki-id-edit-id-at-point ()
  "Edit the current ID."
  (interactive)
  (if (emacs-wiki-id-at-point)
      (let ((new-pages (read-string "Edit pages: "
                                   (emacs-wiki-match-string-no-properties 2)))
            (old-data (emacs-wiki-id-parse
                       (emacs-wiki-match-string-no-properties 0)))
            new-id)
        (let ((data (run-hook-with-args-until-success
                     'emacs-wiki-id-prepare-for-update-hook)))
          (setq new-id (emacs-wiki-id-make-id
                        (list (car old-data)
                              (split-string new-pages ","))))
          ;; Delete/replace it from all the old pages.
          (mapc (lambda (item)
                  (when (emacs-wiki-id-visit item new-id)
                    (if (member item new-id)
                        (run-hook-with-args-until-success
                         'emacs-wiki-id-update-functions data)
                      (run-hook-with-args-until-success
                       'emacs-wiki-id-delete-functions data))))
                (cdr old-data))
          ;; Add it to any new ones
          (mapc (lambda (item)
                  (unless (member item old-data)
                    (if (emacs-wiki-id-visit item new-id)
                        (run-hook-with-args-until-success
                         'emacs-wiki-id-update-functions data)
                      (run-hook-with-args-until-success
                       'emacs-wiki-id-create-functions data))))
                (cdr new-id))))
    (error "No ID at point")))

(add-to-list 'emacs-wiki-publishing-markup
             '[emacs-wiki-id-regexp 0 emacs-wiki-id-markup])

(add-to-list 'emacs-wiki-highlight-markup
             '(emacs-wiki-id-regexp 0 emacs-wiki-id-highlight))

(provide 'emacs-wiki-id)

;;; emacs-wiki-id.el ends here
