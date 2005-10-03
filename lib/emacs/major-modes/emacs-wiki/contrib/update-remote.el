;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update-remote.el --- 
;; 
;; Copyright (C) 2004, 2005 Yu Li
;;
;; Filename: update-remote.el
;; Description: Incremental updating remote directory in background.
;; Author: Yu Li
;; Maintainer:
;; Created: Wed Dec 22 17:10:59 2004
;; Version: $Id$
;; Last-Updated: Wed Jan 12 07:25:02 2005
;;           By: Yu Li
;; Compatibility: Emacs21
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; ----------
;; Motivation
;; ----------
;;
;; In current version EmacsWikiMode, when you press the C-c C-p to
;; publish your page to remote servers, emacs will be blocked and you
;; can do nothing but wait, especially when your connection is slow. I
;; think a much better way is allow the updating work be done
;; background. It should be done in these steps
;;
;; 1. Configure EmacsWikiMode to publish all pages in local directory, eg.
;;    ~/homepage/publish/
;; 2. When need publish pages onto remote server, I invoke a function
;;    give it the local publish directory and remote directory, it will find
;;    out all new pages.
;; 3. Then generate a shell script contains all commands to transfer new pages
;;    to remote server, and then invoke this shell script asynchronously.
;; 4. Get down to other works while update.
;;
;; This package is written for updating remote directory in
;; background.
;;
;; -----------------
;; How does it work?
;; -----------------
;;
;; update-remote.el do its work in these steps
;;
;; 1. Recursively scan all files of the publish directory(e.g.
;;    ~/homepage/publish/) whose name do not match `update-remote-regexp'
;;    and generate a file-list.(`update-remote-directory-files')
;; 2. If file in file-list have been changed, it will have a time stamp
;;    bigger than the past. Chech every file in file-list and put them into
;;    file-need-update-list. This is done by `update-remote-scan-local'.
;;    It will generate a file `.update-remote' in the root directory recoding
;;    time stamps for next time scaning.
;; 3. Now generate a shell script. For each file in file-need-update-list, 
;;    there will be line in form
;;
;;    `update-remote-cmd-prefix' remote-dir local-file-full-name
;;    e.g. `ncftpput -u xxx -p yyy ftp.host.com / ~/homepage/publish/foo.html'
;;
;;    The generated shell script will be named `update-remote-batch-file' and
;;    stored in the root directory.
;; 4. update-remote.el will execute the shell script asynchronously and you
;;    can get down to other work while waiting.
;;    
;; -----
;; Usage
;; -----
;;
;; Place below line into your .emacs
;;
;;     (require 'update-remote)
;;
;; and setting proper variables. Here is a example
;;
;;     (setq update-remote-cmd-name "ncftpput")
;;     (setq update-remote-cmd-prefix "-u xxx -p yyy ftp.host.com")
;;     (defun update-my-homepage ()
;;       (interactive)
;;       (update-remote "/" "~/homepage/publish"))
;; 
;; then M-x update-my-homepage to invoke it.
;;
;; The first time you use update-remote.el absolute will upload all
;; your files, for there are no .update-remote existing. It probably
;; is not necessary. To avoid this, do M-x eval-expression and input
;;
;;     (update-remote-generate "/" "~/homepage/publish")
;;
;; It will only generate script but not execute it.:)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log$
;; RCS Revision 1.1  2005/10/03 22:32:51  adam
;; RCS planner-mode and related imports
;; RCS
;; RCS Revision 1.2  2005/01/12 07:34:13  liyu
;; RCS * turn some defvars to defcustoms
;; RCS
;; RCS * allow shell script pre-actions and post-actions, see
;; RCS   `update-remote-shell-script-pre' and
;; RCS   `update-remote-shell-script-post'
;; RCS
;; RCS * add a feature: allow using white-list to specify files should be
;; RCS   scaned but do not match `update-remote-regexp', see
;; RCS   `update-remote-white-list-file'
;; RCS
;; RCS Revision 1.1  2005/01/12 04:46:38  liyu
;; RCS Initial revision
;; RCS
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup update-remote nil
  "Options controlling the behaviour of update-remote, which is a
package for incremental updating remote directory in background."
  :group 'emacs-wiki)

(defcustom update-remote-regexp "^[^\\.#]\\(.*\\)"
  "Name that match `update-remote-regexp' will be added into the
result of `update-remote-scan-local'. This also is used to valid
directory name. In other word, these files and directories do not
matching `update-remote-regexp' will be ignored."
  :type 'regexp
  :group 'update-remote)

(defcustom update-remote-white-list-file ".update-remote-white"
  "The white list while scaning files. Any name in this file but
do not match `update-remote-regexp' will survive. The content of
this file is a setq sexp associate a list to `white-list' and
here is a example

         (setq white-list (list
           \".emacs.el.html\"
           \".emacs-wiki.el.html\"
         ))

The file listed in white list can not have path prefix, which
means that `update-remote-scan-local' only try to find them in
the same directories. If you have many directories, you can place
in each of them a white list.
"
  :type 'string
  :group 'update-remote)

(defcustom update-remote-cmd-name "ncftpput"
  "A program that can do single FTP PUT operation. As I known
'ncftpput' is a good choice, but you can have yours favorites. So
configure it here."
  :type 'string
  :group 'update-remote)

(defcustom update-remote-shell-script-pre ""
  "Pre-actions indicated by this string will be added in the head
of generated shell script. You can configurate it to do some
preparing work."
  :type 'string
  :group 'update-remote)

(defcustom update-remote-shell-script-post ""
  "Post-actions indicated by this string will be added in the
footer of generated shell script. You can configurate it to do
some finalizing work."
  :type 'string
  :group 'update-remote)

(defcustom update-remote-cmd-prefix ""
  "update-remote-cmd-name usually need to known something about
you, such as username, password and server address. If you use
'ncftpput' as the program, you can configure this string like
this

\(setq update-remote-cmd-prefix
      \(concat \"-u \" emacs-wiki-server-usr \" \"
	      \"-p \" emacs-wiki-server-passwd \" \"
	      emacs-wiki-server\)\)

emacs-wiki-server, emacs-wiki-server-usr and
emacs-wiki-server-passwd are variables defined in other place, so
you can protect them."
  :type 'string
  :group 'update-remote)

(defcustom update-remote-batch-file "#update.bat"
  "The shell script's name which will be generated in root
directory of that you want to scan. In different shell
environment, this can be something different, and make sure you
make this name match update-remote-regexp to ignore it in scaning
process."
  :type 'string
  :group 'update-remote)

(defcustom update-remote-meta-file ".update-remote"
  "The name of meta file storing scaned files' time stamps. This
file usually do not need configuration. And if you mean to do
this, be sure that you have read description in
`update-remote-scan-local'. YOU ARE WARNED!"
  :type 'string
  :group 'update-remote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun update-remote-directory-files (dir)
  "Recursively scan directory for files but not that match
update-remote-regexp."
  (defun update-remote-directory-files-internal (dir root-dir)
    (let* ((list (directory-files dir nil))
	   (file-list nil)
	   (white-list nil))
      (if (file-readable-p (concat dir "/" update-remote-white-list-file))
	  (load-file (concat dir "/" update-remote-white-list-file)))
      (while list
	(let ((name (car list)))
	  (if (file-directory-p (concat dir "/" name))
	      (if (string-match update-remote-regexp name)
		  (let ((inner-file-list (update-remote-directory-files-internal
					  (concat dir "/" name)
					  root-dir)))
		    (dolist (inner-file inner-file-list)
		      (setq file-list
			    (cons inner-file file-list)))))
	    (if (or (string-match update-remote-regexp name)
		    (find name white-list :test 'string=))
	      (setq file-list
		    (cons (substring (expand-file-name name dir)
				     (length root-dir))
			  file-list)))))
	(setq list (cdr list)))
      file-list))
  (update-remote-directory-files-internal dir dir))

(defun update-remote-scan-local (local-dir)
  "Scan local-dir for all files whose name do not match
update-remote-regexp and add new files decided by time stamp into
file-need-update-alist. After storing the files and their time
stamps in a file '.update-remote' located in root directory of
local-dir, return file-need-update-alist."
  (interactive)
  (let* ((file-need-update-alist nil)
	 (local-file-list (update-remote-directory-files local-dir))
         (local-file-alist nil)
	 (local-file-alist2 nil)
         (old-file-alist nil))

    (dolist (file local-file-list)
      (setq local-file-alist
	    (cons
	     (cons file (nth 5 (file-attributes (concat local-dir "/" file))))
	     local-file-alist)))
    (setq local-file-alist2 (copy-alist local-file-alist))
    (setq local-file-alist (sort local-file-alist
				 (lambda (e1 e2) (string< (car e2) (car e1)))))
    (if (file-readable-p (concat local-dir "/" update-remote-meta-file))
	(load-file (concat local-dir "/" update-remote-meta-file)))
    (if old-file-alist
      (while local-file-alist
	(let* ((local-file-entry (car local-file-alist))
	       (old-file-entry (car old-file-alist)))
	  (if (string= (car local-file-entry) (car old-file-entry))
	      (progn (if (time-less-p (cdr old-file-entry)
				      (cdr local-file-entry))
			 (setq file-need-update-alist
			       (cons local-file-entry file-need-update-alist)))
		     (setq local-file-alist (cdr local-file-alist))
		     (setq old-file-alist (cdr old-file-alist)))
	    (let ((find-result nil))
	      (dolist (entry old-file-alist)
		(if (string= (car local-file-entry) (car entry))
		    (setq find-result t)))
	      (if (not find-result)
		  (setq file-need-update-alist
			(cons local-file-entry file-need-update-alist)))
	      (setq local-file-alist (cdr local-file-alist))))))
      (setq file-need-update-alist local-file-alist))

    (update-remote-update-scan-file local-file-alist2 local-dir)
    file-need-update-alist
    ))

(defun update-remote-update-scan-file
  (local-file-alist local-dir)
  (setq local-file-alist (sort local-file-alist
			       (lambda (e1 e2) (string< (car e2) (car e1)))))
  (with-temp-buffer
    (insert "(setq old-file-alist '(\n")
    (dolist (entry local-file-alist)
      (insert (concat "(\"" (format "%s" (car entry)) "\""
		      " " (format "%s" (cadr entry))
		      " " (format "%s" (car (cddr entry))) ")\n")))
    (insert "))")
    (write-file (concat local-dir "/" update-remote-meta-file))))

(defun update-remote-generate-shell-script
  (file-need-update-alist local-dir remote-dir)
  "Generate a shell script for updating according to update-remote-cmd-name,
update-remote-cmd-prefix and update-remote-batch-file."
  (defun expand-remote-directory (remote-dir file)
    (let* ((start (- (length file) 1))
	   (p 0))
      (while (not (<= start 0))
	(if (string= (char-to-string (elt file start)) "/")
	    (progn (setq p start)
		   (setq start -1)) ;; cause a break by hand:)
	  (setq start (- start 1))))
      (if (= p 0) "/"
	(concat remote-dir (substring file 0 p)))))

  (with-temp-buffer
    (and (not (string= update-remote-shell-script-pre ""))
	 (insert (concat update-remote-shell-script-pre "\n")))
    (dolist (file-entry file-need-update-alist)
      (insert
       (concat update-remote-cmd-name " "
	       update-remote-cmd-prefix
	       " " (expand-remote-directory (if (string= remote-dir "/")
						"" remote-dir)
					    (car file-entry))
	       " " (concat local-dir (car file-entry))
	       "\n")))
    (and (not (string= update-remote-shell-script-post ""))
	 (insert (concat update-remote-shell-script-post "\n")))
    (write-file (concat local-dir "/" update-remote-batch-file))))
    
(defun update-remote-invoke-shell-script (local-dir)
  "Invoke the shell script generated by
update-remote-generate-shell-script in background."
  (interactive)
  (compile (expand-file-name (concat (expand-file-name local-dir) "/"
				     update-remote-batch-file))))

(defun update-remote (remote-dir local-dir)
  "Update remote-dir according to local-dir. Files in local-dir
will be scaned to find new files, and then generate a shell
script to do updating work in background."
  (interactive)
  (let ((update-alist nil))
    (setq update-alist (update-remote-scan-local local-dir))
    (update-remote-generate-shell-script update-alist local-dir remote-dir)
    (update-remote-invoke-shell-script local-dir)))

(defun update-remote-generate (remote-dir local-dir)
  "This function is similar to update-remote and the difference
is that it do not updating after generating shell script."
  (interactive)
  (let ((update-alist nil))
    (setq update-alist (update-remote-scan-local local-dir))
    (update-remote-generate-shell-script update-alist local-dir remote-dir)))

(provide 'update-remote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update-remote.el ends here
