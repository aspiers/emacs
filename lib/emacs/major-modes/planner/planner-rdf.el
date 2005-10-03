;;; planner-rdf.el --- RDF export for Emacs PlannerMode

;; Copyright 2004 Rainer Volz
;;
;; Author: Rainer Volz, http://www.rainervolz.de
;; Version: 0.1
;; Keywords: PlannerMode OWL RDF
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; planner-rdf is an add-on to GNU Emacs PlannerMode. It provides functions to
;; publish Planner information in RDF format, so that the Planner information
;; can be processed in an automated way by RDF-aware tools. To do this
;; planner-rdf adds itself to the hook emacs-wiki-after-file-publish-hook,
;; which is run whenever Planner pages are being published.

;; The source distribution and the HTML edition of the documentation is
;; available at http://www.rainervolz.de/planner-rdf/

;; To use planner-rdf put this file into your load-path and the following into
;; your ~/.emacs file:
;;   (require 'planner-rdf)
;;   (add-hook 'emacs-wiki-after-file-publish-hook 'planner-rdf-publish-file)
;;   (add-hook 'emacs-wiki-after-wiki-publish-hook 'planner-rdf-publish-index)

;; To minimise interference with the publication of other (non-Planner)
;; emacs-wiki projects, planner-rdf-publish-file checks whether the current
;; major mode is planner-mode. If yes, RDF publication is started, if not, the
;; file is ignored.

;; planner-rdf creates two files for every published Planner file: a .rdf and
;; a .owl file. The .owl file contains 
;; - tasks 
;; - notes 
;; of the respective Planner page in OWL format. The OWL schema used is
;; documented in planner-rdf.owl, which is part of this distribution.
;; For a detailed explanation of the format please look at the planner-rdf
;; documentation included.

;; The .rdf file contains Dublin Core metadata about the source file, the HTML
;; file, and the OWL file produced. It is mainly used to provide information
;; about the relation of the different page versions to various tools.

;; Please note: Currently the .rdf/.owl files are meant for local processing,
;; not Internet publishing. Therefore they contain file-URIs pointing to the
;; local file system.

;; Tested with GNU Emacs 21.3.50...

;; Customisation

;; By default the RDF files are stored in the
;; planner-publishing-directory. For a different location change variable
;; planner-rdf-directory.


(eval-when-compile
  (require 'cl))
(require 'planner)

(defvar planner-rdf-version "0.1"
  "Module Version")

(defvar planner-rdf-directory planner-publishing-directory
  "Directory where the RDF files are stored. Defaults to planner-publishing-directory.")

(defvar planner-rdf-schema-planner
  "http://www.rainervolz.de/schema/2004/11/planner-rdf#"
  "The Planner schema.")

(defvar planner-rdf-base
  "http://localhost/planner/"
  "The base URI for Planner info")
(defvar planner-rdf-task-prefix (concat planner-rdf-base "task-"))
(defvar planner-rdf-note-prefix (concat planner-rdf-base "note-"))
(defvar planner-rdf-tag-prefix  (concat planner-rdf-base "tag-"))
(defvar planner-rdf-link-prefix (concat planner-rdf-base "link-"))
(defvar planner-rdf-page-prefix (concat planner-rdf-base "page-"))
(defvar planner-rdf-pagetype-prefix (concat planner-rdf-base "page-type-"))
(defvar planner-rdf-pagetype-day (concat planner-rdf-base "day-page"))
(defvar planner-rdf-pagetype-project (concat planner-rdf-base "topic-page"))
(defvar planner-rdf-pagetype-other (concat planner-rdf-base "other-page"))

;; common match strings - tasks
(defvar planner-rdf-matchstr-ptaskids 
  "{{Tasks:\\([0-9]+\\)}}" 
  "Finds task ids of the form {{Tasks:<Number>}} in Planner tasks.")
(defvar planner-rdf-matchstr-ptask
  "^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oX>CP]\\)\\s-*\\(.+\\)$"
  "Finds a Planner task in a buffer.")

;; common match strings - notes
(defvar planner-rdf-matchstr-pnote
  "^.#\\([0-9]+\\)\\s-+\\(.+\\)"
  "Finds the beginning of a Planner note in a buffer. Matches id.")
(defvar planner-rdf-matchstr-pnote1 
  "^\\(.+\\)\\s-+[0-9][0-9]:[0-9][0-9]\\s-+(\\[\\[[0-9]+.[0-9]+.[0-9]+#[0-9]+\\]\\])$"
  "Note title with time and day page link. Match text.")
(defvar planner-rdf-matchstr-pnote2
  "^\\(.+\\)\\s-+[0-9][0-9]:[0-9][0-9]\\s-+(\\[\\[.+#[0-9]+\\]\\])$"
  "Note title with time and project page link. Match text.")
(defvar planner-rdf-matchstr-pnote3 
  "^\\(.+\\)\\s-+[0-9][0-9]:[0-9][0-9]$"
  "Note title with time, no page link. Match text.")
(defvar planner-rdf-matchstr-pnote4
  "^.+\\s-+[0-9][0-9]:[0-9][0-9]\\s-+(\\[\\[\\([0-9]+.[0-9]+.[0-9]+#[0-9]+\\)\\]\\])$"
  "Note title with time and day page link. Match page link.")
(defvar planner-rdf-matchstr-pnote5
  "^.+\\s-+[0-9][0-9]:[0-9][0-9]\\s-+(\\[\\[\\(.+#[0-9]+\\)\\]\\])$"
  "Note title with time and project page link. Match page link.")

;; common match strings - links
(defvar planner-rdf-matchstr-plink1
  "\\[\\[\\([^[]+?\\)\\]\\[\\(.+?\\)\\]\\]"
  "Link like [[<link>][<text>]]. Match link and text.")
(defvar planner-rdf-matchstr-plink2
  "\\[\\[\\([^[]+?\\)\\]\\]"
  "Link like [[<link>]]. Match link.")

(defvar planner-rdf-matchstr-pagenotelink
  "\\(.+\\)#[0-9]+"
  "Matches the page part of a page/note reference.")

(defvar planner-rdf-matchstr-pagenotelink1
  "\\(.+\\)#note[0-9]+"
  "Matches the page part of a page/note reference.")

;; common match strings - tags
(defvar planner-rdf-matchstr-ptag
  "{{\\(.+?\\)}}"
  "Tag like {{<type>:<id>}}. Match everything.")

;; hooks
(defvar planner-rdf-analysis-note-functions nil 
  "A hook called when the standard note content has been saved.
Hook functions must take one parameter, the vector with the note content. 
Their return value is ignored. Output must be to the current buffer, and in 
Turtle syntax.")

(defvar planner-rdf-analysis-task-functions nil 
  "A hook called when the standard task content has been saved.
Hook functions must take one parameter, the vector with the task content. 
Their return value is ignored. Output must be to the current buffer, and in 
Turtle syntax.")

(defvar planner-rdf-prolog-hook nil
  "A hook called when the standard turtle file prolog has been saved.
Output must be to the current buffer, and in Turtle syntax.")

(defvar planner-rdf-epilog-hook nil
  "A hook called when all standard content has been saved.
Output must be to the current buffer, and in Turtle syntax.")


(defun planner-rdf-curtz ()
  "Return a string representation of the current time zone."
  (let* ((tz (current-time-zone))
			(secs (car tz))
			(mins (/ (abs secs) 60))
			(tzstr "")
			(hours (/ mins 60))
			(rmins (% mins 60)))
	 (if (< secs 0)
		  (setq tzstr "-")
		(setq tzstr "+"))
	 (format "%s%02d:%02d" tzstr hours rmins)))
		
	 

(defun planner-rdf-replace-sth (input replace with)
  "Replace <replace> with <with> in <input>."
  (let ((input2 input)
		  (start 0))
	 (while (not (null (string-match (regexp-opt (list replace)) input2 start)))
		(setq input2 (replace-match with t t input2))
		(setq start (match-end 0)))
	 input2))

(defun planner-rdf-get-taskid (taskdesc)
  "Retrieve or generate a task id from the description."
  (if (string-match planner-rdf-matchstr-ptaskids taskdesc)
		(planner-match-string-no-properties 1 taskdesc)
	 (concat "-" (number-to-string(abs (sxhash taskdesc))))))

(defun planner-rdf-get-task-info ()
  "Retrieve the task information from the current buffer."
  (let ((results '()))
	 (goto-char (point-min))
	 (while (not (eobp))
		(when (looking-at planner-rdf-matchstr-ptask)
		  (let ((info (planner-task-info-from-string 
							(buffer-name) 
							(match-string 0))))
			 (add-to-list
			  'results
			  (vector (buffer-name)
                                  (planner-task-priority info)
                                  (planner-task-status info)
                                  (planner-task-date info)
                                  (planner-task-plan info)
                                  (planner-task-description info)
                                  (planner-task-number info)
                                  (planner-rdf-get-taskid (planner-task-description info))
                                  ))))
		(forward-line))
	 results))

(defun planner-rdf-get-note-info ()
  "Retrieve the note information from the current buffer."
  (interactive)
  (let ((results '())(in-notes 'nil) 
		  (curr-note-id 0) 
		  (curr-note-headline "") 
		  (curr-note-txt ""))
	 (goto-char (point-min))
	 (while (not (eobp))
		(if (looking-at planner-rdf-matchstr-pnote)
			 (progn
				(if (null in-notes)
					 (setq in-notes 't)
				  (progn ; save previous note
					 (add-to-list
						'results
						(vector (buffer-name)
								  curr-note-id
								  curr-note-headline
								  curr-note-txt))
					 ;; (planner-rdf-log (concat "Note " curr-note-id " " curr-note-headline))
					 (setq curr-note-txt "")
					 (setq curr-note-id 0)))
				(setq curr-note-id (planner-match-string-no-properties 1))
				(setq curr-note-headline (planner-match-string-no-properties 2))
				;; zur nächsten - sonst wird der Titel als Text mitgenommen
				(forward-line)))
		(if in-notes
			 (setq curr-note-txt (concat curr-note-txt (thing-at-point 'line))))
		(forward-line))
	 ; save the last note if there
	 (if (and in-notes (> (string-to-number curr-note-id) 0))
		  (progn
			 (add-to-list
			  'results
			  (vector (buffer-name)
						 curr-note-id
						 curr-note-headline
						 curr-note-txt))
			 ;; (planner-rdf-log (concat "Note " curr-note-id " " curr-note-headline))
			 ))
	 results))

(defun planner-rdf-insert-prolog ()
  "Insert a file prolog, necessary schema references etc.
Runs the hook planner-rdf-prolog-hook at the end to allow 
custom content."

  (insert "<rdf:RDF\n"
    "\txmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n"
    "\txmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"\n"
    "\txmlns:planner=\"http://www.rainervolz.de/schema/2004/11/planner-rdf#\"\n"
    "\txmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"\n"
    "\txmlns=\"http://localhost/planner/\"\n"
    "\txmlns:owl=\"http://www.w3.org/2002/07/owl#\"\n"
	 "\txml:base=\"http://localhost/planner/\"\n"
	 ">\n")
  (run-hooks 'planner-rdf-prolog-hook))

(defun planner-rdf-insert-dcprolog ()
  "Insert a file prolog, necessary schema references etc."
  (insert 
	(concat "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n")
   (concat "\txmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n")
	">\n"))

(defun planner-rdf-insert-epilog ()
  "Insert a file epilog.
Runs the hook planner-rdf-epilog-hook at the end to allow 
custom content."
  (run-hooks 'planner-rdf-epilog-hook)
  (insert "</rdf:RDF>\n"))


(defun planner-rdf-retrieve-links (id ctr input parent)
  "Collect Planner-style links, [[link][text]] or [...]."
  (let ((start 0)(results '()))
	 (while (not (null (string-match planner-rdf-matchstr-plink1 input start)))
		(push (list 				 
				 (match-string 1 input) 
				 (match-string 2 input)
				 (concat id "-" (number-to-string ctr))
				 parent) 
				results)
		(setq ctr (incf ctr))
		(setq start (match-end 0)))
	 (setq start 0)
	 (while (not (null (string-match planner-rdf-matchstr-plink2 input start)))
		(push (list				 
				 (match-string 1 input)
				 nil
				 (concat id "-" (number-to-string ctr))
				 parent)
				results)
		(setq ctr (incf ctr))
		(setq start (match-end 0)))
	 results))

(defun planner-rdf-retrieve-tags (id ctr input parent)
  "Collect Planner-style tags, {{text:id}}."
  (let ((start 0)(results '()))
	 (while (not (null (string-match planner-rdf-matchstr-ptag input start)))
		(push (list
				 (match-string 1 input)
				 (concat id "-" (number-to-string ctr))
				 parent)
				results)
		(setq ctr (incf ctr))
		(setq start (match-end 0)))
	 results))

(defun planner-rdf-replace-links (input)
  "replace Planner-style links, [[link][text]] or [...], with text."
  (while (not (null (string-match planner-rdf-matchstr-plink1 input 0)))
	 (setq input (replace-match (planner-match-string-no-properties 2 input) t t input)))
  (while (not (null (string-match planner-rdf-matchstr-plink2 input 0)))
	 (setq input (replace-match "*link*" t t input)))
  input)

(defun planner-rdf-link-type (uri)
  "Retrieve the link protocol."
  (if (string-match "\\(\\w+\\):.+" uri)
		(match-string 1 uri)
	 "unknown"))

(defun planner-rdf-tag-id (tag)
  "Retrieve the id part of a tag."
  (if (string-match "\\w+:\\(.+\\)" tag)
		(match-string 1 tag)
	 "unknown"))

(defun planner-rdf-escape-uri (uri)
  "Apply various escapes to the URI."
  (let* ((uri2 (planner-rdf-replace-sth uri "<" "\\u003c"))
			(uri3 (planner-rdf-replace-sth uri2 ">" "\\u003e"))
			(uri4 (planner-rdf-replace-sth uri3 "&" "&amp;")))
	 uri4))

(defun planner-rdf-escape-string (input)
  "Apply various escapes to strings."
  (let* ((input2 (planner-rdf-replace-sth input "\"" "\\u0022"))
			(input3 (planner-rdf-replace-sth input2 "&" "&amp;"))
			(input4 (planner-rdf-replace-sth input3 "<" "&lt;"))
			(input5 (planner-rdf-replace-sth input4 ">" "&gt;"))
			)
	 input5))

(defun planner-rdf-print-link (link)
  "Output a RDF representation of a link.
If the link has no name, insert the defined unknown string."
  (let ((lt (planner-rdf-link-type (elt link 0)))
		  (uri (elt link 0)))
	 (insert "<planner:Link rdf:about=\"" planner-rdf-link-prefix (elt link 2) "\">\n")
	 (if (not (null (elt link 1)))
		  (insert "\t<link-text>" (planner-rdf-escape-string(elt link 1)) "</link-text>\n")
		(insert "\t<link-text>" 
				  (planner-rdf-escape-string 
					(if (string= lt "unknown")
						 (substring uri 0 10)
					  (substring uri (+ (length lt) 3) 10)))
				  "</link-text>\n"))
	 (insert "\t<link-type>" lt "</link-type>\n")
	 (insert "\t<link-uri>"(planner-rdf-escape-uri uri) "</link-uri>\n")
	 (insert "</planner:Link>\n")))

(defun planner-rdf-print-link2 (link)
  "Output a resource statement for a link."
  (insert "\t<link rdf:resource=\"" planner-rdf-link-prefix (elt link 2) "\"/>\n"))

(defun planner-rdf-print-tag (tag)
  "Output a RDF representation of a tag."
  (insert "<planner:Tag rdf:about=\"" planner-rdf-tag-prefix (elt tag 1) "\">\n")
  (insert "\t<tag-type>" (planner-rdf-link-type (elt tag 0)) "</tag-type>\n")
  (insert "\t<tag-id>" (planner-rdf-escape-uri(planner-rdf-tag-id (elt tag 0))) "</tag-id>\n")
  (insert "</planner:Tag>\n"))

(defun planner-rdf-print-tag2 (tag)
  "Output a resource statement for a tag."
  (insert "\t<tag rdf:resource=\"" planner-rdf-tag-prefix (elt tag 1) "\"/>\n"))

(defun planner-rdf-task-get-nice-description (input)
  "Extract a nice, readable description.
  Remove the task ids e.g., {{Tasks:XX}}."
  (let ((pos (string-match planner-rdf-matchstr-ptaskids input))
		  (title ""))
	 (when (numberp pos)
		  (setq input (substring input 0 (decf pos))))
	 (setq title (planner-rdf-replace-links input))
	 (planner-rdf-replace-sth title "<nop>" "")
	 ))


(defun planner-rdf-task (taskvector)
  "Transform task information to turtle syntax.
Run the hook 'planner-rdf-task-functions' at the end 
of the standard output to provide customised content."
  ;; Generate a task-id
  (let ((task-id)
		  (task-unique-id)
		  (linkes '())
		  (tags '()))
	 (setq task-id (elt taskvector 7))
;;	  (setq task-unique-id (concat (elt taskvector 0) "-" task-id))
;; 	 (setq task-unique-id (concat 
;; 								  (elt taskvector 0) 
;; 								  "#" 
;; 								  (elt taskvector 1)
;; 								  (elt taskvector 6)))
	 (setq task-unique-id (concat 
 								  (elt taskvector 0) 
 								  "-" 
 								  task-id))
	 (setq linkes (planner-rdf-retrieve-links 
						task-unique-id 
						0 
						(elt taskvector 5) 
						(concat planner-rdf-task-prefix task-unique-id)))
	 (mapcar 'planner-rdf-print-link linkes)
	 (setq tags (planner-rdf-retrieve-tags 
					 task-unique-id 
					 0 
					 (elt taskvector 5)
					 (concat planner-rdf-task-prefix task-unique-id)))
	 (mapcar 'planner-rdf-print-tag tags)
	 (insert "<planner:Task rdf:about=\"" planner-rdf-task-prefix task-unique-id "\">\n")
	 (insert "\t<task-priority>" (elt taskvector 1) "</task-priority>\n")
	 (insert "\t<task-number>" (elt taskvector 6) "</task-number>\n")
	 (insert "\t<task-status>" (elt taskvector 2) "</task-status>\n")
	 (insert "\t<date>" (planner-rdf-replace-sth (elt taskvector 3) "." "-") "</date>\n")
	 (if (not (null (elt taskvector 4)))
		  (insert "\t<alias rdf:resource=\"" planner-rdf-task-prefix (elt taskvector 4) "-" task-id "\" />\n"))
	 (insert "\t<description>" 
				(planner-rdf-escape-string 
				 (planner-rdf-task-get-nice-description
				  (elt taskvector 5)))
				"</description>\n")
	 ;; if the task-id is negative, it is generated, not good for linking
	 (when (> (string-to-number task-id) -1)
		(insert "\t<task-id>" task-id "</task-id>\n"))
	 (mapcar 'planner-rdf-print-link2 linkes)
	 (mapcar 'planner-rdf-print-tag2 tags)
	 (run-hook-with-args 'planner-rdf-analysis-task-functions taskvector)
	 (insert "</planner:Task>\n")))

(defun planner-rdf-note-get-nice-description (input)
  "Extract a nice description, text only if possible."
  (let ((title ""))
	 (cond
	  ;; title with time and day page link
	  ((string-match planner-rdf-matchstr-pnote1 input)
		(setq title (planner-rdf-escape-string(planner-match-string-no-properties 1 input))))
	  ;; title with time and project page link
	  ((string-match planner-rdf-matchstr-pnote2 input)
		(setq title (planner-rdf-escape-string(planner-match-string-no-properties 1 input))))
	  ;; title with time, no page link
	  ((string-match planner-rdf-matchstr-pnote3 input)
		(setq title (planner-rdf-escape-string (planner-match-string-no-properties 1 input))))
	  ;; ok, take evrything
	  (t (setq title (planner-rdf-escape-string input))))
	 (setq title (planner-rdf-replace-links title))
	 (planner-rdf-replace-sth title "<nop>" "")
	 ))

(defun planner-rdf-note-get-alias (input)
  "Extract the alias page link from title string."
  (let ((alias ""))
	 (cond
	  ;; title with time and day page link
	  ((string-match planner-rdf-matchstr-pnote4 input)
		(setq alias (planner-rdf-escape-string(planner-match-string-no-properties 1 input))))
	  ;; title with time and project page link
	  ((string-match planner-rdf-matchstr-pnote5 input)
		(setq alias (planner-rdf-escape-string(planner-match-string-no-properties 1 input))))
	  ;; ok, nothing there
	  (t (setq alias "")))
	 (planner-rdf-replace-sth alias "#" "-")))

(defun planner-rdf-note-get-plan (page input)
  "Extract the alias page link from title string."
  (let ((plan "")
		  (alias (planner-rdf-note-get-alias input)))	 
	 (cond
	  ;; there is something
	  ((string-match planner-rdf-matchstr-pagenotelink alias)
		(setq plan (planner-rdf-escape-string(planner-match-string-no-properties 1 alias))))
	  ((string-match planner-rdf-matchstr-pagenotelink1 alias)
		(setq plan (planner-rdf-escape-string(planner-match-string-no-properties 1 alias))))
	  ;; ok, nothing there
	  (t (setq plan page)))
	 plan))

(defun planner-rdf-note (notevector)
  "Transform note information to RDF. 
Run the hook 'planner-rdf-note-functions' at the end 
of the standard output to provide customised content."
  ;; Generate a note-id
  (let ((note-id (concat (elt notevector 0) "-" (elt notevector 1)))
		  (title (planner-rdf-note-get-nice-description (elt notevector 2)))
		  (alias (planner-rdf-note-get-alias (elt notevector 2)))
		  (linksall '())
		  (tagsall '()))
		;; links from title and body
		(let* ((linkst (planner-rdf-retrieve-links 
							 note-id 
							 0 
							 title 
							 (concat planner-rdf-note-prefix note-id)))
				 (linksb (planner-rdf-retrieve-links 
							 note-id 
							 (length linkst) 
							 (elt notevector 3)
							 (concat planner-rdf-note-prefix note-id))))
		  (setq linksall (append linkst linksb))
		  (mapcar 'planner-rdf-print-link linksall))
		(let* ((tagst (planner-rdf-retrieve-tags 
							note-id 
							0 
							title 
							(concat planner-rdf-note-prefix note-id)))
				 (tagsb (planner-rdf-retrieve-tags 
							note-id 
							(length tagst) 
							(elt notevector 3) 
							(concat planner-rdf-note-prefix note-id))))
		  (setq tagsall (append tagst tagsb))
		  (mapcar 'planner-rdf-print-tag tagsall))
	 (insert "<planner:Note rdf:about=\"" planner-rdf-note-prefix note-id "\">\n")
	 (insert "\t<note-id>" note-id "</note-id>\n")
	 ; find a date - first try buffer name
	 (if (string-match "^\\([0-9]+.[0-9]+.[0-9]+\\)$" (elt notevector 0))
		  (insert "\t<date>" (planner-rdf-replace-sth (elt notevector 0) "." "-") "</date>\n")
		(progn
		  ; no luck, try title line
		  (if (string-match "(\\[\\[\\([0-9]+.[0-9]+.[0-9]+\\)#[0-9]+\\]\\])" (elt notevector 2))
				(insert 
				 "\t<date>" 
				 (planner-rdf-replace-sth 
				  (planner-match-string-no-properties 1 (elt notevector 2)) "." "-") 
				 "</date>\n"))))

	 ;; find time
	 (if (string-match "\\s-+\\([0-9][0-9]:[0-9][0-9]\\)\\s-+\\|\\s-+\\([0-9][0-9]:[0-9][0-9]\\)$" 
							 (elt notevector 2))
		  (insert 
			"\t<note-time>" 
			(if (planner-match-string-no-properties 1 (elt notevector 2))
				 (planner-match-string-no-properties 1 (elt notevector 2))
			  (planner-match-string-no-properties 2 (elt notevector 2)))
			":00</note-time>\n"))
	 (insert "\t<description>" title "</description>\n")	 
	 (when (not (string= "" alias))
		(insert "\t<alias rdf:resource=\"" planner-rdf-note-prefix alias "\" />\n"))
	 (mapcar 'planner-rdf-print-link2 linksall)
	 (mapcar 'planner-rdf-print-tag2 tagsall)
	 (run-hook-with-args 'planner-rdf-analysis-note2turtle-functions notevector)
	 (insert "</planner:Note>\n")))

(defun planner-rdf-insert-page-info (name tasks notes)
  "Create a page object."
  (insert "<planner:Page rdf:about=\"" planner-rdf-page-prefix name "\">\n")
  (let ((type (if (string-match "^[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.[0-9]\\{2\\}" name)
				  planner-rdf-pagetype-day
				planner-rdf-pagetype-project)))
	 (insert "\t<page-type rdf:resource=\"" type "\"/>\n")
	 )
  (mapcar 
	(lambda (task)
	  (insert "\t<task rdf:resource=\"" 
				 planner-rdf-task-prefix 
				 (elt task 0)
				 "-"
				 (elt task 7) 
				 "\"/>\n"))
	tasks)
  (mapcar 
	(lambda (note)
	  (insert "\t<note rdf:resource=\"" 
				 planner-rdf-note-prefix 
				 (concat (elt note 0) "-" (elt note 1))
				 "\"/>\n"))
	notes)  
  (insert "</planner:Page>\n"))

(defun planner-rdf-metadata (file)
  "Create Dublin Core metadata for the varous files involved."
  ;; The source file
  (let ((name (file-name-nondirectory file))
		  (rdfdir (file-name-as-directory planner-rdf-directory)))
	 (insert "<rdf:Description rdf:about=\"file:" 
				(expand-file-name file)
				"\">\n")
	 (insert "\t<dc:Title>" (file-name-nondirectory file) "</dc:Title>\n")
	 (insert "\t<dc:Identifier>" planner-rdf-base "page-" name "</dc:Identifier>\n")
	 (insert "\t<dc:Format>text</dc:Format>\n")
	 (if (string-match "^[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.[0-9]\\{2\\}" name)
		  (progn
			 (insert "\t<dc:Type>" planner-rdf-pagetype-day "</dc:Type>\n")
			 (insert 
			  "\t<dc:Coverage>" 
			  planner-rdf-base 
			  "date/" 
			  (planner-rdf-replace-sth name "." "-") 
			  "</dc:Coverage>\n"))
		(progn
		  (insert "\t<dc:Type>" planner-rdf-pagetype-project "</dc:Type>\n")
		  (insert "\t<dc:Coverage>"
					 planner-rdf-base
					 "topic/"
					 name 
					 "</dc:Coverage>\n")))
	 (insert "</rdf:Description>\n")
	 ;; The HTML file
	 (insert "<rdf:Description rdf:about=\"file:" 
				(expand-file-name (concat rdfdir name	".html"))
				"\">\n")
	 (insert "\t<dc:Title>" name "</dc:Title>\n")
	 (insert "\t<dc:Source>file:" file	"</dc:Source>\n")
	 (insert "\t<dc:Format>text/html</dc:Format>\n")
	 (insert "</rdf:Description>\n")
	 ;; The OWL file  
	 (insert "<rdf:Description rdf:about=\"file:"
				(expand-file-name (concat rdfdir name	".owl"))
				"\">\n")
	 (insert "\t<dc:Title>" name "</dc:Title>\n")
	 (insert "\t<dc:Source>file:" file	"</dc:Source>\n")
	 (insert "\t<dc:Format>application/rdf+xml</dc:Format>\n")
	 (insert "</rdf:Description>\n")
  )
)

;;;###autoload
(defun planner-rdf-publish-file (file)
  "Publish the file in RDF format, if called by PlannerMode.
Designed to be called via the emacs-wiki-after-file-publis-hook.
Non-Planner files, matching emacs-wiki-image-regexp will be treated 
differently. Currently they are simply ignored."
  (interactive "f")
  (when (eq major-mode 'planner-mode)
	 (if (string-match emacs-wiki-image-regexp file)
			(message (concat "planner-rdf: Ignored file " file))
		(let ((tasks nil)
				(notes nil))
		  (let ((mybuff (get-file-buffer file))
				  (opened nil))
			 (when (not mybuff) 
				(setq mybuff (find-file file))
				(setq opened t))
			 (with-current-buffer mybuff
				(setq tasks (planner-rdf-get-task-info)))
			 (with-current-buffer mybuff
				(setq notes (planner-rdf-get-note-info)))
			 (if (and (not (buffer-modified-p mybuff)) opened) 
				  (kill-buffer mybuff)))
		  (let ((mybuff2 (find-file 
								(expand-file-name 
								 (concat (file-name-as-directory planner-rdf-directory) 
											(file-name-nondirectory file) 
											".owl")))))
			 (with-current-buffer mybuff2
				(erase-buffer)
				(planner-rdf-insert-prolog)
				(mapcar 'planner-rdf-task tasks)
				(mapcar 'planner-rdf-note notes)
				(planner-rdf-insert-page-info (file-name-nondirectory file) tasks notes)
				(planner-rdf-insert-epilog)
				(let ((backup-inhibited t))
				  (save-buffer)))
			 (kill-buffer mybuff2))
		  (let ((mybuff3 (find-file 
								(expand-file-name 
								 (concat (file-name-as-directory planner-rdf-directory) 
											(file-name-nondirectory file) 
											".rdf")))))
			 (with-current-buffer mybuff3
				(erase-buffer)
				(planner-rdf-insert-dcprolog)
				(planner-rdf-metadata file)
				(planner-rdf-insert-epilog)
				(let ((backup-inhibited t))
				  (save-buffer)))
			 (kill-buffer mybuff3))
		  ))))

;;;###autoload
(defun planner-rdf-publish-index ()
  "Create an index for the .rdf files.
Will be called via the emacs-wiki-after-wiki-publish-hook.
Creates index.rdf, a rdf:bag, with all existing .rdf files as
items."
  (interactive)
  (let ((rdf-index (expand-file-name 
						  (concat 
							(file-name-as-directory planner-rdf-directory) 
							"index.rdf"))))
	 (when (file-exists-p rdf-index)
		(delete-file rdf-index))	 
	 (with-temp-buffer
		(erase-buffer)
		(message "Creating RDF index...")
		(insert "<?xml version=\"1.0\"?>\n")
		(insert "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n")
		(insert "\t<rdf:Bag>\n")
		(let ((rdf-files (directory-files 
								(file-name-as-directory planner-rdf-directory)
								t
								"\\.rdf$"
								nil)))
		  (insert 
			(mapconcat 
			 (lambda (name)
				(concat 
				 "\t\t<rdf:li rdf:resource=\"file:"
				 name
				 "\"/>"))
			 rdf-files
			 "\n")))
		(insert "\n\t</rdf:Bag>\n")
		(insert "</rdf:RDF>\n")
		(let ((backup-inhibited t))
		  (write-file rdf-index	nil)))))


(provide 'planner-rdf)
