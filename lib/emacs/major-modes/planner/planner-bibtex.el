;; planner-bibtex.el -- BibTeX support for Planner
;; Copyright (c) 2004 James Clarke <james@jamesclarke.info>
;; Parts copyright (c) 2004 Jody Klymak <jklymak@ucsd.edu>

;;; Copyright

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

;;; Commentary:
;; 
;; Provides bibtex annotations for Planner mode.  Handles the two
;; versions of bibtex.el I have encountered, those bundled with
;; emacs-21.3 and emacs-21.2.

(require 'planner)
(require 'bibtex)

;; This function is not present in old bibtex.els so I've added it
;; here as I cannot find alternative in old bibtex.els.

(defvar planner-bibtex-separator ":" "Character separating filename and label.")
(defvar planner-bibtex-regexp
  (concat "^bibtex:\\([^"
          planner-bibtex-separator "]+\\)"
          planner-bibtex-separator "\\(.+\\)") "Regexp matching URLs.")

;;; Code:
(defun planner-bibtex-key-in-head (&optional empty)
  "Extract BibTeX key in head. Return optional arg EMPTY if key is empty."
  (if (fboundp 'bibtex-key-in-head)
      (bibtex-key-in-head empty)
    (if (match-beginning bibtex-key-in-head)
        (buffer-substring-no-properties (match-beginning bibtex-key-in-head)
                                        (match-end bibtex-key-in-head))
      empty)))

;; makes a nice bibtex url with the title upto a terminator for the
;; link name.  For emacs 21.3 (updated bibtex.el)
;;;###autoload
(defun planner-bibtex-annotation-new ()
  "Return an annotation for the current bibtex entry."
  (when (planner-derived-mode-p 'bibtex-mode)
    (let ((titlestring
           (bibtex-autokey-get-field "title"
                                     bibtex-autokey-titleword-change-strings)))
      (if (string-match "[\.:!\?;]" titlestring)
          (setq titlestring (substring titlestring 0 (match-beginning 0))))
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head nil t)
      (planner-make-link
       (concat "bibtex:" (buffer-file-name) planner-bibtex-separator
               (planner-bibtex-key-in-head))
       titlestring
       t))))

;; makes a nice bibtex url with the title up to a terminator for the
;; link name.  For emacs 21.2 (older bibtex)
;;;###autoload
(defun planner-bibtex-annotation-old ()
  "Return the filename on the current line in dired."
  (when (planner-derived-mode-p 'bibtex-mode)
      (let* ((min (bibtex-beginning-of-entry))
            (max (bibtex-end-of-entry))
            (titlestr (planner-replace-regexp-in-string
                       "[ \t\n]+" " "
                       (bibtex-autokey-get-titlestring min max))))
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head nil t)
        (planner-make-link
         (concat "bibtex:" (buffer-file-name) planner-bibtex-separator
                 (planner-bibtex-key-in-head))
         titlestr
         t))))

;;decide which version of the function to use
(if (fboundp 'bibtex-autokey-get-field)
    (defalias 'planner-bibtex-annotation
      'planner-bibtex-annotation-new)
  (defalias 'planner-bibtex-annotation
    'planner-bibtex-annotation-old))

;;;###autoload
(defun planner-bibtex-browse-url (url)
  "If this is a Bibtex URL, jump to it."
  (when (string-match planner-bibtex-regexp url)
    (let ((file (match-string 1 url))
	  (label (match-string 2 url)))
      (find-file file)
      (widen)
      (goto-char (point-min))
      (search-forward (concat "{"  label ",")))))

(planner-add-protocol "bibtex" 'planner-bibtex-browse-url nil)
(add-hook 'planner-browse-url-functions 'planner-bibtex-browse-url)
(add-hook 'planner-annotation-functions 'planner-bibtex-annotation)

(planner-update-wiki-project)

(provide 'planner-bibtex)

;;; planner-bibtex.el ends here
