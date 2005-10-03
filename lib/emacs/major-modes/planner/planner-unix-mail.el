;;; planner-unix-mail.el --- Unix mailbox support for Planner

;; Copyright (C) 2004  Frederik Fouvry <fouvry@coli.uni-saarland.de>

;; Author: Frederik Fouvry <fouvry@coli.uni-saarland.de>
;; Keywords:

;; This file is not part of GNU Emacs.

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

;;;_ + Usage
;;
;; Place planner-unix-mail.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-unix-mail)
;;
;; Unix mailbox URLs are of the form
;;
;; mail://PATH/TO/INBOX/message-id
;;
;; Annotations will be of the form
;; [[mail://PATH/TO/INBOX/E1AyTpt-0000JR-LU%40sacha.ateneo.edu][E-mail from Sacha Chua]]

;;; Code:

(require 'planner)

;;; Code:

;; This is (kind of) a combination of the "mid" and "file" (RFC 1738)
;; protocol.
(defconst planner-unix-mail-protocol-name "mail")

(defun planner-unix-mail-narrow-to-message ()
  (save-match-data
    (let ((b (or (and (looking-at "^From ") (point))
		 (re-search-backward "^From ")))
	  (e (progn
	       (re-search-forward "\n\n\\(?:From \\|\\'\\)" nil 'point)
	       (forward-line 0)
	       (point))))
    (narrow-to-region b e))))


;; Encoding according to RFC 1738.
(defun planner-url-encode (string &optional reserved)
  (save-match-data
    (let ((chars (split-string string ""))
	  (newchars)
	  (not-to-encode (concat "\\(?:[0-9A-Za-z]\\|[$_.+!*'(),-]"
				 (if (and (stringp reserved)
					  (not (string= reserved "")))
				     (concat "\\|" reserved)
				   "")
				 "\\)")))
      (while chars
	(setq newchars
	      (cons (if (string-match not-to-encode (car chars))
			(car chars)
		      (format "%%%x" (string-to-char (car chars))))
		    newchars)
	      chars (cdr chars)))
      (eval (cons 'concat (nreverse newchars))))))

;; Decoding according to RFC 1738
(defun planner-url-decode (string)
  (save-match-data
    (let* ((parts (split-string string "%"))
	   (newparts)
	   (ignore))
      (unless (string-match "^%" string)
	(setq ignore (car parts)
	      parts (cdr parts)))
      (while parts
	(when (string-match "^\\(..\\)" (car parts))
	  (setq newparts (cons (replace-match
				(char-to-string
				 (string-to-number (match-string 1 (car parts)) 16))
				nil t (car parts) 1)
			       newparts)))
	(setq parts (cdr parts)))
      (eval (cons 'concat (append (when ignore (list ignore))
				  (nreverse newparts)))))))


;;;###autoload
(defun planner-unix-mail-annotation-from-mail ()
  "Return an annotation for the current message.
This function can be added to `planner-annotation-functions'."
  (save-match-data
    ;; This test replaces the major-mode test.
    (when (save-excursion
	    (save-restriction
	      (widen)
	      (and (goto-char (point-min))
		   (re-search-forward "\\`From " nil t)
		   (goto-char (- (point-max) 3))
		   (re-search-forward "\n\n\\'" nil t))))
      (save-excursion
	(save-restriction
	  (planner-unix-mail-narrow-to-message)
	  (planner-make-link
	   (concat planner-unix-mail-protocol-name "://"
		   (buffer-file-name) "/"
		   ;; Format is defined on RFC 2111 ("/" is reserved, but should not be used because of the presence of the file path)
		   (let* ((mid (mail-fetch-field "message-id")))
		     (if (stringp mid)
			 (if (string-match "^<\\(.+\\)>$" mid)
			     (planner-url-encode (match-string 1 mid) "[/]")
			   (error "Mal-formed Message-Id header field encountered"))
		       ;; From_ header could be used as a backup
		       (error "No Message-Id header field found in this message"))))
	   (if (and planner-ignored-from-addresses
		    (string-match planner-ignored-from-addresses
				  (mail-fetch-field "from"))
		    (mail-fetch-field "to")) ; May be missing
	       (concat "E-mail to " (planner-get-name-from-address
				     (mail-fetch-field "to")))
	     (concat "E-mail from " (planner-get-name-from-address
				     (mail-fetch-field "from"))))
	   t))))))


;;;###autoload
(defun planner-unix-mail-browse-url (url)
  "If this is an UNIX-MAIL URL, jump to it."
  (save-match-data
    (when (string-match (concat "^" planner-unix-mail-protocol-name
				"://\\(.+\\)/\\(.+?\\)$") url)
      (let* ((message-id (planner-url-decode (match-string 2 url)))
	     (file (match-string 1 url))
	     (point (save-excursion
		      (save-window-excursion
			(find-file file)
			(save-restriction
			  (widen)
			  (goto-char (point-max))
			  (re-search-backward
			   (concat "^Message-Id:[[:space:]]+<"
				   (regexp-quote message-id) ">")
			   nil t))))))
	(if point
	    (progn
	      (find-file file)
	      (goto-char point)
	      (re-search-backward "^From ")
	      (recenter 0))
	  (error "Message not found"))))))

(planner-add-protocol 'planner-unix-mail-protocol-name
		      'planner-unix-mail-browse-url nil)
(add-hook 'planner-annotation-functions 'planner-unix-mail-annotation-from-mail)
(custom-add-option 'planner-annotation-functions
                   'planner-unix-mail-annotation-from-rmail)
(planner-update-wiki-project)
(provide 'planner-unix-mail)

