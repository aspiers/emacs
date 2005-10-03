;;; planner-gnus.el --- Gnus integration for the Emacs Planner

;; Copyright (C) 2003, 2004 Sandra Jean Chua (sacha AT free.net.ph)
;; Copyright (C) 2001 John Wiegley (johnw AT gnu.org)
;; Parts copyright (C) 2004 Sven Kloppenburg (kloppenburg AT informatik.tu-darmstadt.de)
;; Parts copyright (C) 2004 Magnus Henoch (mange AT freemail.hu)
;; Parts copyright (C) 2004 Mario Domgörgen (kanaldrache AT gmx.de)

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Version: 2005.08.20-17.59-stable
;; Keywords: planner, gnus
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php#gnus

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

;; This file adds annotations for Gnus messages. ;You will then be
;; able to use M-x planner-create-task-from-buffer to create tasks
;; from Gnus summary or message buffers with the correct annotation.
;; If you add
;;
;;     (planner-gnus-insinuate)
;;
;; to your .emacs, you can also use 'C-c C-t' to create a task from a buffer.

;; Mario Domgörgen (kanaldrache AT gmx DOT de) got this to work nicely
;; with multiple marked messages in Gnus summary buffers.

;;; Code:

(require 'planner)
(require 'gnus)

;;;###autoload
(defun planner-gnus-insinuate ()
  "Hook Planner into Gnus.

Adds special planner keybindings to the variable
`gnus-summary-article-map'. From a summary or article buffer, you
can type C-c C-t to call planner-create-task-from-buffer."
  (eval-after-load 'gnus-sum
    `(define-key gnus-summary-article-map ,(kbd "C-c C-t")
       'planner-create-task-from-buffer))
  (eval-after-load 'gnus
    `(define-key gnus-article-mode-map ,(kbd "C-c C-t")
       'planner-create-task-from-buffer)))

(require 'gnus-sum)

(defun planner-gnus-get-message-id (&optional article-number)
  "Return the message-id of the current message."
  (save-excursion
    (if (equal major-mode 'gnus-summary-mode)
        (mail-header-message-id (gnus-data-header
                                 (assq (or article-number
					   (gnus-summary-article-number))
                                       gnus-newsgroup-data)))
      ;; Refer to the article buffer
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward "^Message-ID:\\s-*\\(.+\\)"
                                   (point-max) t)
            (match-string 1)))))))

(defun planner-gnus-get-address (&optional header)
  "Return the address of the sender of the current message.
If HEADER is \"To\", return the recipient instead."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (concat "^" (or header "From") ":\\s-*\\(.+\\)")
             (point-max) t)
        (planner-match-string-no-properties 1)))))

(defun planner-gnus-annotation-from-summary ()
  "If called from a Gnus summary buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (equal major-mode 'gnus-summary-mode)
    (let ((articles (gnus-summary-work-articles nil)))
      (planner-make-link
       (concat "gnus://" gnus-newsgroup-name "/"
               (mapconcat (lambda (article-number)
			    (planner-gnus-get-message-id article-number))
			  (gnus-summary-work-articles nil) "\\|"))
       (if (= 1 (length articles))
	   (let ((headers (gnus-data-header (assq (car articles)
						  gnus-newsgroup-data))))
	     (if (gnus-news-group-p gnus-newsgroup-name)
		 (concat "Post "
			 (if (and planner-ignored-from-addresses
				  (string-match
				   planner-ignored-from-addresses
                                   (mail-header-from headers)))
			     ""
			   (concat "from "
				   (planner-get-name-from-address
                                    (mail-header-from headers))
				   " "))
			 "on "
			 gnus-newsgroup-name)
	       (concat "E-Mail "
		       (if (and planner-ignored-from-addresses
				(mail-header-from headers)
				(string-match planner-ignored-from-addresses
					      (mail-header-from headers))
				(assq 'To
				      (mail-header-extra headers)))
			   ;; Mail from me, so use the To: instead
			   (concat "to " (planner-get-name-from-address
					  (cdr (assq 'To
						     (mail-header-extra headers)))))
			 ;; Mail to me, so use the From:
			 (concat "from " (planner-get-name-from-address
					  (mail-header-from headers)))))))
	 (concat (number-to-string (length articles))
		 " E-Mails from folder " gnus-newsgroup-name))
       t))))

(defun planner-gnus-annotation-from-message ()
  "If called from a Gnus article, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (or (equal major-mode 'gnus-article-mode)
            (equal major-mode 'gnus-original-article-mode))
    (gnus-copy-article-buffer)
    (with-current-buffer gnus-article-copy
      (let ((from (planner-gnus-get-address "From"))
	    (newsgroups (planner-gnus-get-address "Newsgroups")))
        (planner-make-link
         (concat "gnus://" gnus-newsgroup-name "/"
                 (planner-gnus-get-message-id))
	 (if newsgroups
	     (concat "Post "
		     (if (and planner-ignored-from-addresses
			      (string-match
			       planner-ignored-from-addresses from))
			 ""
		       (concat "from "
			       (planner-get-name-from-address from)
			       " "))
		     "on "
		     newsgroups)
	   (concat "E-Mail "
		   (let ((to-addr
			  (planner-gnus-get-address "To")))
		     (if (and planner-ignored-from-addresses
			      from
			      to-addr
			      (string-match
			       planner-ignored-from-addresses from))
			 (concat "to " (planner-get-name-from-address
					to-addr))
		       (concat "from " (planner-get-name-from-address
					from))))))
         t)))))

;;;###autoload
(defun planner-gnus-annotation ()
  "Return an annotation from a Gnus summary or message buffer.
Suitable for use in `planner-annotation-functions'. If you
include this, you can omit `planner-gnus-annotation-from-summary'
and `planner-gnus-annotation-from-message'."
  (or (planner-gnus-annotation-from-summary)
      (planner-gnus-annotation-from-message)))

(defvar planner-gnus-group-threshold 10 "Number of messages to retrieve from groups.
Raise this if you have problems browsing gnus URLs.")

;;;###autoload
(defun planner-gnus-browse-url (url)
  "If this is a Gnus URL, jump to it."
  (when (string-match "^gnus://\\(.+\\)/\\(.+\\)" url)
    (let ((group (match-string 1 url))
          (articles (match-string 2 url)))
      ;; (gnus-fetch-group group 1) ;; always gives us problems
      (when (featurep 'gnus-registry)
        (let ((reg-group (gnus-registry-fetch-group articles)))
          (when reg-group 
            (if gnus-registry-use-long-group-names
                (setq group reg-group)
              (when (cadr (split-string group ":")) ;; group contains a :
                (setq group (concat (car (split-string group ":")) ":"
                                    reg-group)))))))
      (gnus-fetch-group group planner-gnus-group-threshold)
      (mapcar
       (lambda (article-id)
         (gnus-summary-goto-article article-id nil t))
       (split-string articles "\\\\|"))
      (let ((articles (gnus-find-matching-articles "message-id" articles)))
	(gnus-summary-limit articles)
	(gnus-summary-select-article))
      t)))

(fset 'planner-get-from 'planner-gnus-get-address)
(fset 'planner-get-message-id 'planner-gnus-get-message-id)
(custom-add-option 'planner-annotation-functions
                   'planner-gnus-annotation)
(add-hook 'planner-annotation-functions 'planner-gnus-annotation)
(planner-add-protocol "gnus" 'planner-gnus-browse-url nil)

(provide 'planner-gnus)

;;; planner-gnus.el ends here
