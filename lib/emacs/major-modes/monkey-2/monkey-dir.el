;;; monkey-dir.el -	a monkey mode for directory editting.
;;;			See also "./monkey.el".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001, 2002 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-local variable declarations
;;;

(defvar monkey-local-variables '())
(establish-local-variables 'monkey-dir-local-variables
			   (append '((case-fold-search nil)
				     (buffer-read-only nil)
				     (selective-display t)
				     (default-directory (eval default-directory))
				     (major-mode monkey-mode)
				     (mode-name "Monkey")
				     (mode-line-buffer-identification ("Monkey: %17b"))
				     (monkey-directory-files-function ())
				     (monkey-file-list ())
				     (monkey-menu-override ())
				     (monkey-file-to-token-list ())
				     (monkey-not-a-file-list ())
				     )
				   monkey-local-variables))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file utilities
;;;

(defun monkey-canonicalize-dirname (name)
  (let ((full-name (expand-file-name name)))
    (if (string= "/" (substring full-name -1))
	(setq full-name (substring full-name 0 -1)))
    full-name))

(defun monkey-delete-directory (file)
  "This little lose of a function should be in C.
And should do error checking."
  (call-process "rmdir" nil nil nil file))

(defun monkey-delete-file-properly (file &optional ok-if-directory)
  "Delete FILE.  If FILE is a nonempty directory, signal an error.  If FILE
is an empty directory, the course of action depends on the optional 
parameter OK-IF-DIRECTORY.  If nil, an error is raised, if numeric, 
the user is asked for permission to delete it, otherwise, the file is 
silently deleted."
  (interactive "fDelete File: \np")
  (cond ((not (file-attributes file))
	 (error "You don't have access to %s." file))
	((not (eq t (car (file-attributes file))))
	 (delete-file file))
	((not ok-if-directory)
	 (error "%s is a directory." file))
	((not (eq (length (directory-files file)) 2))
	 (error "%s is not an empty directory." file))
	((or (not (numberp ok-if-directory)) 
	     (y-or-n-p (format "Delete directory %s? " file)))
	 (monkey-delete-directory file))
	(t (error "%s not deleted."))))
      
(defun monkey-copy-file-properly (file destination &optional ok-if-already-exists)
  "Copy FILE to DESTINATION. If DESTINATION is a directory, 
then copy FILE into DESTINATION."
  (interactive "fCopy file: \nfCopy to: \np")
  (let ((real-destination
	 (if (and (not (file-directory-p file))
		  (file-directory-p destination))
	     (concat (file-name-as-directory destination)
		     (file-name-nondirectory file))
	   destination)))
    (copy-file file real-destination ok-if-already-exists)
    real-destination))

(defun monkey-symlink-file-properly (file destination &optional ok-if-already-exists)
  "Symlink FILE to DESTINATION. If DESTINATION is a directory, 
then symlink FILE into DESTINATION."
  (interactive "fSymlink file: \nfSymlink to: \np")
  (let ((real-destination
	 (if (and (not (file-directory-p file))
		  (file-directory-p destination))
	     (concat (file-name-as-directory destination)
		     (file-name-nondirectory file))
	   destination)))
    (make-symbolic-link file real-destination ok-if-already-exists)
    real-destination))

(defun monkey-rename-file-properly (file newname &optional ok-if-already-exists)
  "Rename FILE as NEWNAME.  If NEWNAME is the name of a directory,
then move FILE to that directory. See rename-file for more."
  (interactive "fRename File: \nFRename to: \np")
  (let ((real-destination
	 (if (file-directory-p newname)
	     (concat
	      (file-name-as-directory newname)
	      (file-name-nondirectory file))
	   newname)))
    (rename-file file real-destination ok-if-already-exists)
    real-destination))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mapping files to tokens
;;;


(defun monkey-file-token (&optional important)
  (let ((tk (monkey-token important))
	(l monkey-file-to-token-list))
    (catch 'it
      (while l
	(if (string= tk (car (cdr (car l))))
	    (throw 'it (car (car l))))
	(setq l (cdr l)))
      tk)))

(defvar monkey-be-fast nil
  "When true, monkey avoids invoking the stat system call.")

(defun monkey-toggle-quietness ()
  "Turn file stating on or off"
  (interactive)
  (if (setq monkey-be-fast (not monkey-be-fast))
      (message "monkey: file stating turned off")
    (message "monkey: file stating turned on")))

(defun monkey-stat-marked ()
  "Fill in the type field for the marked files."
  (interactive)
  (let ((monkey-be-fast nil)
	(p (point)))
    (monkey-temp-unhide)
    (monkey-for-each-marked
     '(lambda ()
	(let ((name (monkey-file-token)))
	  (monkey-delete-node)
	  (monkey-distribute-file-name
	   (concat default-directory name)))) t)
    (monkey-un-temp-unhide)
    (goto-char p)
    (nice-monkey)))

(defun monkey-insert-file-name-args (file-name-passed &optional top-level)
  (let* ((fill-prefix	"")
	 (mark-field	" ")
	 (pretty-file-name (if top-level
			       file-name-passed
			     (monkey-tail (expand-file-name default-directory)
					  (if (eq ?~ (string-to-char file-name-passed))
					      (expand-file-name file-name-passed)
					    file-name-passed))))

	 (specialp (assoc file-name-passed monkey-file-to-token-list))

	 (pretty-name (cond
		       (specialp 	(car (cdr specialp)))
		       (t		pretty-file-name)))

	 (not-a-filep (member file-name-passed monkey-not-a-file-list))

	 (abs-file-name (and (not not-a-filep)
			     (expand-file-name file-name-passed default-directory)))

	 (stat (cond (not-a-filep 		? )
		     (monkey-be-fast		??)
		     ((file-directory-p name)	?/)
		     ((file-symlink-p name)	?@)
		     ((file-readable-p name)	" ")
		     (t				??)))

	 (indent-spaces
	  (if (or top-level (eq ?/ (string-to-char pretty-name)))
	      1
	    (+ 1 (* monkey-insert-distance
		    (monkey-occurences ?/ pretty-name)))))

	 (expand-field (or (and (not not-a-filep)
				(not monkey-be-fast)
				(file-directory-p abs-file-name)
				(not 
				 (string-match "^\\.\\.?$" (file-name-nondirectory abs-file-name)))
				"^")
			   " ")))

    (list fill-prefix
	  mark-field
	  stat
	  expand-field
	  (make-string indent-spaces 32)
	  pretty-name)))

(defun monkey-insert-file-name (name &optional top-level)
  (apply 'monkey-insert-node (monkey-insert-file-name-args name top-level)))

(defun monkey-sorted-insert-file-name (name)
  (apply 'monkey-sorted-insert-node 
	 'string<
	 (monkey-insert-file-name-args name ())))

(defconst monkey-insert-distance 2
  "*Number of spaces to indent for each level of subdirectoriness. This 
can be any number greater than 0.")

(defun monkey-occurences (char string)
  "Return the number of occurences of CHAR in STRING."
  (let ((len (length string))
	(x 0)
	(total 0))
    (while (< x len)
      (if (eq char (aref string x))
 	  (setq total (1+ total)))
      (setq x (1+ x)))
    total))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a mapping of buffers to the directories "watched" there
;;;

(defvar monkey-visible-directories ()
  "List of directories watched in a monkey-mode directory.")

(make-variable-buffer-local 'monkey-visible-directories)

;; Delcare that DIR is watched in the current buffer.
;;
(defun monkey-visible-directory (dir)
  (setq monkey-visible-directories
	(cons (list (monkey-canonicalize-dirname dir))
	      monkey-visible-directories)))

;; Delcare that DIR is no longer watched in the current buffer.
;;
(defun monkey-invisible-directory (dir)
  (setq monkey-visible-directories
	(monkey-rem-visable (monkey-canonicalize-dirname dir)
			    monkey-visible-directories)))

(defun monkey-rem-visable (dir lst)
  (cond ((null lst) nil)
	((string= dir (car (car lst))) (cdr lst))
	(t (cons (car lst) (monkey-rem-visable dir (cdr lst))))))

;; Return t if DIR is watched in the current directory.
;;
(defun monkey-is-visible-p (dir)
  (not (not (assoc (monkey-canonicalize-dirname dir) 
		   monkey-visible-directories))))




;; Insert filename NAME in all monkey buffers watching its directory.
;;
(defun monkey-distribute-file-name (name)
  "Put a file-name in all monkey-buffers that should know about it."
  (let* ((buffers (buffer-list))
	(full-name (expand-file-name name))
	(directory (file-name-directory full-name)))
    (while buffers
      (if (string= (buffer-mode-name (car buffers)) "Monkey")
	  (save-excursion
	    (set-buffer (car buffers))
	    (if (monkey-is-visible-p directory)
		(monkey-sorted-insert-file-name (monkey-tail (file-name-as-directory
							      (expand-file-name default-directory))
							     full-name)))))
      (setq buffers (cdr buffers)))))

(defun buffer-mode-name (&optional b)
  "Return the mode of a buffer"
  (save-excursion
    (set-buffer (or b (current-buffer)))
    mode-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; listing directories into monkey-dir buffers
;;; 

(defvar monkey-always-hide-regexp nil
  "*Regexp matching those files which should be hidden after a 
directory is listed in a minkey buffer.")

(defun monkey-computed-file-list (dir fullp &optional dir-spec-opt)
  (if monkey-directory-files-function
      (apply monkey-directory-files-function dir directory dir-spec-opt)
    (directory-files dir (not top-level))))
  
(defun monkey-list-directory (&optional directory donthide)
  "Insert a directory listing of the default directory or optionally of
DIRECTORY."
  (let* ((top-level (not directory))
	 (dir (file-name-as-directory (or directory default-directory)))
	 (file-list (cond

		     (monkey-menu-override
		      (monkey-computed-file-list dir directory))

		     ((file-exists-p (concat dir "=menu.el"))
		      (if (or (not dir) (equal dir default-directory))
			  (read-file-menu)
			(mapcar '(lambda (x) (concat dir x)) (read-file-menu dir))))

		     (t		(monkey-computed-file-list dir directory)))))
    (mapcar '(lambda (x) (monkey-insert-file-name x top-level)) file-list)
    (if (and (not donthide) monkey-always-hide-regexp)
	(monkey-for-each-matches 'monkey-hide-line
			    monkey-always-hide-regexp))))

(defun monkey-expand-subdirectory-interactive (&optional un-exp)
  "Expand in-place the contents of a subdirectory."
  (modify-monkey-buffer
   (let* ((name (monkey-file-token t))
	  (base-name (file-name-nondirectory name)))
     (if (not (file-directory-p name))
	 (error "%s is not a directory." name))
     (if (or (string= "." base-name)
	     (string= ".." base-name))
	 (error "Why would you expand `%s'?" name))
    
     (if (monkey-expanded-p)
	 (if un-exp
	     (progn
	       (monkey-unexpand-subdirectory)
	       (monkey-invisible-directory name))
	   (error "%s has already been expanded." name))
       (monkey-set-expand-field ?>))
    
     (save-excursion (monkey-list-directory name t))
     (monkey-visible-directory name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; monkey commands on files
;;;

(defun monkey-list-long ()
  "Show the output of ls -l'ing the marked files."
  (interactive)
  (with-output-to-temp-buffer "*monkey-ls-output*"
    (monkey-for-each-marked
     '(lambda ()
	(call-process "ls" nil "*monkey-ls-output*" t "-ld" 
		      (monkey-file-token))) t)))

(defun monkey-list-this-long ()
  "Show the output of ls -l'ing the marked files."
  (interactive)
  (save-window-excursion
    (with-output-to-temp-buffer "*monkey-short-ls-output*"
      (call-process "ls" nil "*monkey-short-ls-output*" t "-ld" 
		    (monkey-file-token)))
    (set-buffer "*monkey-short-ls-output*")
    (goto-char (monkey-point-min))
    (message (buffer-substring (monkey-bol-point) (monkey-eol-point)))))
  
(defun monkey-delete-marked ()
  "Delete all the marked files."
  (interactive)
  (save-window-excursion
    (let (deletion-list)
      (monkey-for-each-marked
       '(lambda ()
	  (setq deletion-list
		(cons (monkey-file-token t)
		      deletion-list)))
       t)
      (if (not (monkey-confirm-deletions deletion-list))
	  (message "Nothing Deleted.")
	(monkey-for-each-marked
	 '(lambda ()
	    (monkey-delete-file-properly (monkey-file-token t) 1)
	    (monkey-delete-node)))))))

(defun monkey-delete-this ()
  "Delete all the current file."
  (interactive)
  (save-window-excursion
    (let ((deletion-list (list (monkey-file-token t))))
      (if (not (monkey-confirm-deletions deletion-list))
	  (message "Nothing Deleted.")
	(monkey-delete-file-properly (car deletion-list) 1)
	(monkey-delete-node)
	(if (not (= (point-min) (monkey-bol-point)))
	    (monkey-next-line 1)
	  (nice-monkey))))))

(defun monkey-confirm-deletions (deletion-list)
  "Make sure that DELETION-LIST meets with the users approval."
  (if (= (length deletion-list) 1)
      (y-or-n-p (format "Delete %s ?" (car deletion-list)))
    (save-excursion
      (with-output-to-temp-buffer "*Deletions*"
	(set-buffer "*Deletions*")
	(while deletion-list
	  (insert (car deletion-list) "\n")
	  (setq deletion-list (cdr deletion-list)))
	(display-buffer "*Deletions*")
	(yes-or-no-p "Delete these files?")))))

(defun monkey-copy-file (destination &optional unhidden)
  "Copy the current file to DESTINATION."
  (monkey-distribute-file-name 
   (monkey-copy-file-properly (monkey-file-token t) destination 1)))

(defun monkey-symlink-file (destination &optional unhidden)
  "Symlink the current file to DESTINATION."
  (monkey-distribute-file-name 
   (monkey-symlink-file-properly (monkey-file-token t) destination 1)))

(defun monkey-copy-this (destination)
  "Copy the current file to DESTINATION."
  (interactive "FCopy to: ")
  (monkey-copy-file destination)
  (nice-monkey))

(defun monkey-symlink-this (destination)
  "Symlink the current file to DESTINATION."
  (interactive "FSymlink to: ")
  (monkey-symlink-file destination)
  (nice-monkey))

(defun monkey-copy-marked (destination)
  "Copy the marked files to DESTINATION."
  (interactive "FCopy to: ")
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-for-each-marked
       '(lambda () (monkey-copy-file destination t)) t)
    (monkey-un-temp-unhide)))

(defun monkey-symlink-marked (destination)
  "Symlink the marked files to DESTINATION."
  (interactive "FSymlink to: ")
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-for-each-marked
       '(lambda () (monkey-symlink-file destination t)) t)
    (monkey-un-temp-unhide)))

(defun monkey-rename-file (destination &optional unhidden)
  "Rename the current file to DESTINATION."
  (save-excursion
    (monkey-distribute-file-name
     (monkey-rename-file-properly (monkey-file-token t) destination 1)))
  (monkey-delete-node))

(defun monkey-rename-this (destination)
  "Rename the current file to DESTINATION."
  (interactive "FRename to: ")
  (monkey-rename-file destination)
  (nice-monkey))

(defun monkey-rename-marked (destination)
  "Rename the marked files to DESTINATION."
  (interactive "FRename to: ")
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-for-each-marked
       '(lambda ()
	  (monkey-rename-file destination t)) t)
    (monkey-un-temp-unhide)))

(defun monkey-edit-file ()
  "If the current file is a directory, create a monkey-buffer for it.  
Otherwise, find-file it, but don't switch to the new buffer.  
Someday, this should offer to execute executables.  Returns the 
new buffer, if any."
  (save-window-excursion
    (save-excursion
      (let ((name (monkey-file-token t)))
	(if (file-directory-p name)
	    (monkey-directory (file-name-as-directory name))
	  (find-file name))
	(current-buffer)))))

(defun monkey-edit-marked ()
  "Edit all the marked files.  Switch to the edit buffer for the first one
in the list."
  (interactive)
  (let ((destination-buffer nil))
    (monkey-for-each-marked
     '(lambda ()
	(message "finding %s..." (monkey-file-token))
	(let ((x (monkey-edit-file)))
	  (setq destination-buffer 
		(if destination-buffer 'dont-switch x)))) t)
    (message "done.")
    (or (eq destination-buffer 'dont-switch)
	(switch-to-buffer destination-buffer))))

(defun monkey-edit-this ()
  "Edit this file."
  (interactive)
  (switch-to-buffer (monkey-edit-file)))

(defun monkey-edit-marked-other-window ()
  "Edit all the marked files.  Switch to the edit buffer for the first
one in the other window."
  (interactive)
  (let ((destination-buffer nil))
    (monkey-for-each-marked
     '(lambda ()
	(message "finding %s..." (monkey-file-token))
	(let ((x (monkey-edit-file)))
	  (setq destination-buffer (or destination-buffer x)))) t)
    (switch-to-buffer-other-window destination-buffer)))

(defun monkey-edit-this-other-window ()
  "Edit this file in the other window."
  (interactive)
  (switch-to-buffer-other-window (monkey-edit-file)))

(defun monkey-edit-marked-and-trash-this-buffer ()
  "Edit all the marked files and trash this buffer."
  (interactive)
  (save-excursion
    (monkey-edit-marked))
  (kill-buffer (current-buffer)))

(defun monkey-edit-this-and-trash-this-buffer ()
  "Edit all the marked files and trash this buffer."
  (interactive)
  (save-excursion
    (monkey-edit-this))
  (kill-buffer (current-buffer)))

(defun monkey-copy-by-regexp (copy-by-regexp-target) 
  "Using the regexp used last for monkey-mark-by-regexp, match each marked
file-name, then copy it."
  (interactive "sCopy to: ")
  (if (null monkey-last-mark-regexp)
      (error "You must mark files using a regexp first."))
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-for-each-marked
       '(lambda ()
	  (goto-char (monkey-token-beginning))
	  (if (not (looking-at monkey-last-mark-regexp))
	      (error "%s does not match the source regexp." 
		     (monkey-file-token)))
	  (modify-monkey-buffer
	   (let ((old-name (monkey-file-token)))
	     (re-search-forward monkey-last-mark-regexp (monkey-point-max) nil)
	     (replace-match copy-by-regexp-target nil nil)
	     (let ((new-name (monkey-file-token)))
	       (delete-region
		(monkey-token-beginning)
		(monkey-token-end))
	       (insert old-name)
	       (monkey-copy-file new-name t)))))
       t)
       (monkey-un-temp-unhide)))

(defun monkey-rename-by-regexp (rename-by-regexp-target)
  "Using the regexp used last for monkey-mark-by-regexp, match each marked
file-name, then rename it."
  (interactive "sRename to: ")
  (or monkey-last-mark-regexp
      (error "You must mark files using a regexp first."))
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-for-each-marked
       '(lambda ()
	  (goto-char (monkey-token-beginning))
	  (if (not (looking-at monkey-last-mark-regexp))
	      (error "%s does not match the source regexp." 
		     (monkey-file-token)))
	  (modify-monkey-buffer
	   (let ((old-name (monkey-file-token)))
	     (re-search-forward monkey-last-mark-regexp (monkey-point-max) nil)
	     (replace-match rename-by-regexp-target nil nil)
	     (let ((new-name (monkey-file-token)))
	       (delete-region
		(monkey-token-beginning)
		(monkey-token-end))
	       (insert old-name)
	       (monkey-rename-file new-name t)))))
       t)
       (monkey-un-temp-unhide)))

(defun monkey-mkdir (directory)
  "Make DIRECTORY by forking mkdir."
  (interactive "FMake directory: ")
  (and (file-exists-p directory)
       (error "directory `%s' already exists" directory))
  (call-process "mkdir" nil nil nil (expand-file-name directory))
  (or (file-exists-p directory)
      (error "can't make directory `%s'" directory))
  (monkey-distribute-file-name directory))

(defun monkey-mark-auto-save-files ()
  "Mark all the autosave files.  Unmarks them with a prefix."
  (interactive)
  (monkey-mark-by-regexp (concat "#" monkey-token-field-regexp "#")))

(defun monkey-mark-backup-files ()
  "Mark all the backup files.  Unmarks them with a prefix."
  (interactive)
  (monkey-mark-by-regexp (concat monkey-token-field-regexp "~")))

(defun monkey-mark-dotfiles ()
  "Mark all files beginning with a `.'."
  (interactive)
  (monkey-mark-by-regexp (concat "\\." monkey-token-field-regexp)))

(defun monkey-mark-by-extension (regexp)
  "Mark all nodes with tokens that end with .REGEXP"
  (interactive "sRegexp for extension: ")
  (monkey-mark-by-regexp (concat monkey-token-field-regexp "\\." regexp)))

(defun monkey-mark-directories ()
  "Mark all the directories."
  (interactive)
  (monkey-mark-by-type "/"))

(defun monkey-mark-links ()
  "Mark all the symbolic links."
  (interactive)
  (monkey-mark-by-type "@"))

(defun monkey-mark-executables ()
  "Mark all the executable nodes."
  (interactive)
  (monkey-mark-by-type "*"))  

(defun monkey-mark-subdirectory ()
  "Mark the current subdirectory."
  (interactive)
  (save-excursion
    (or (monkey-expanded-p)
	(monkey-directory-heading 1))
    (let ((dirname (monkey-file-token)))
      (monkey-for-each-matches
       (if current-prefix-arg
	   'monkey-unmark
	 'monkey-mark)
       (concat (regexp-quote (concat dirname "/")) ".+")))))

(defun monkey-unexpand-subdirectory-interactive ()
  (or (monkey-expanded-p)
      (monkey-directory-heading 1))
  (monkey-unexpand-subdirectory)
  (monkey-invisible-directory (monkey-canonicalize-dirname (monkey-file-token))))

(defun monkey-mung-marked-subdirectories ()
  "Expand or collapse marked subdirectories.  Prefix makes a difference as
to which happens."
  (interactive)
  (save-excursion
    (monkey-for-each-marked 'monkey-mung-this-subdirectory))
  (nice-monkey))

(defun monkey-mung-this-subdirectory ()
  "Expand or collapse a subdirectory. "
  (interactive)
  (save-excursion
    (if (monkey-expanded-p)
	(monkey-unexpand-subdirectory-interactive)
      (monkey-expand-subdirectory-interactive t)))
  (nice-monkey))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; =menu.el
;;;
;;; A file name "=menu.el" can be used to change the order in which
;;; the files of a given directory are displayed, as well as to 
;;; restrict the list of files displayed (by default) in a directory.
;;;

(defun read-file-menu (&optional dir)
  (let* ((buf (generate-new-buffer ",scratch"))
	 (menu (save-excursion	(set-buffer buf)
				(insert-file-contents (concat (file-name-as-directory (or dir "."))
							      "=menu.el"))
				(goto-char (point-min))
				(read (current-buffer))))
	 (offp (and menu (eq 'off (car menu))))
	 (menu (if offp (cdr menu) menu)))

    (kill-buffer buf)
    menu))

(defun monkey-toggle-menu-mode ()
  (interactive)
  (setq monkey-menu-override (not monkey-menu-override))
  (and (not monkey-menu-override)
       (file-exists-p "=menu.el")
       (setq monkey-file-list (read-file-menu)))
  (monkey-gee))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History
;;;
;;; A list of files visited by monkey commands is maintained
;;; and can be searched interactively.  I'm skeptical that this
;;; is useful.
;;;

(setq monkey-global-backward-list ())
(setq monkey-global-forward-list ())
(setq monkey-global-history-bound 100)

(defun monkey-computed-history-position ()
  (cond
   ((buffer-file-name)				(list 'file (buffer-file-name)))
   ((and (or (eq major-mode 'monkey-mode)
	     (eq major-mode 'dired)))		(list 'file default-directory))
   (t						(list 'buffer (buffer-name)))))

(defun monkey-prune-history ()
  (let ((bl nil)
	(fl nil))
    (while (and monkey-global-history-bound
		(<= monkey-global-history-bound
		    (+ (setq bl (length monkey-global-backward-list))
		       (setq fl (length monkey-global-forward-list)))))
      (if (> bl 1)
	  (setq monkey-global-backward-list (reverse (cdr (reverse monkey-global-backward-list))))
	(setq monkey-global-forward-list (reverse (cdr (reverse monkey-global-forward-list))))))))
	  
    

(defun monkey-history-record (&optional dest)
  (monkey-prune-history)
  (let ((f (if dest
	       (list 'file (expand-file-name dest))
	     (monkey-computed-history-position))))
    (if (and (not (and monkey-global-backward-list
		  (equal f (car monkey-global-backward-list))))
	     (not (and monkey-global-forward-list
		       (equal f (car monkey-global-forward-list)))))
	(setq monkey-global-backward-list
	      (cons f monkey-global-backward-list)))))

(defun monkey-global-history-backward ()
  "Move to the previous file or buffer in the global history list."
  (interactive)
  (if (or (not monkey-global-backward-list)
	  (and (not (cdr monkey-global-backward-list))
	       (equal (monkey-computed-history-position) (car monkey-global-backward-list))))
      (error "beginning of global history list"))
  (monkey-history-record)
  (setq monkey-global-forward-list
	(cons (car monkey-global-backward-list) monkey-global-forward-list))
  (setq monkey-global-backward-list (cdr monkey-global-backward-list))
  (monkey-history-goto (car monkey-global-backward-list)))

(defun monkey-global-history-forward ()
  "Move to the next file or buffer in the global history list."
  (interactive)
  (if (not monkey-global-forward-list)
      (error "end of global history list"))
  (monkey-history-goto (car monkey-global-forward-list))
  (setq monkey-global-backward-list
	(cons (car monkey-global-forward-list) monkey-global-backward-list))
  (setq monkey-global-forward-list (cdr monkey-global-forward-list)))

(defun monkey-history-reset ()
  "Move to the last (most recent) file or buffer in the global history list."
  (interactive)
  (setq monkey-global-backward-list
	(append (reverse monkey-global-forward-list) monkey-global-backward-list))
  (setq monkey-global-forward-list ())
  (and monkey-global-backward-list
       (monkey-history-goto (car monkey-global-backward-list))))

(defun monkey-history-rewind ()
  "Move to the first (oldest) file or buffer in the global history list."
  (interactive)
  (setq monkey-global-forward-list
	(append (reverse monkey-global-backward-list) monkey-global-forward-list))
  (setq monkey-global-backward-list ())
  (and monkey-global-forward-list
       (monkey-history-goto (car monkey-global-forward-list))))

(defun monkey-history-goto (h)
  (cond
   ((eq (car h) 'file)				(monkey-file (car (cdr h))))
   ((eq (car h) 'directory)			(let* ((n (car (cdr h)))
						       (b (get-buffer n)))
						  (if b
						      (switch-to-buffer b)
						    (or (y-or-n-p
							 (concat "Buffer "
								 n
								 " no longer exists.  Skip it? "))
							(switch-to-buffer
							 (get-buffer-create n))))))))
						   
					  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Favorites
;;;
;;; Lists of favorite files can be maintained and searched interactively.  
;;; I'm skeptical that this is useful.
;;;


(setq monkey-favorites-backward-list ())
(setq monkey-favorites-forward-list ())
(setq monkey-favorites ())

(defun monkey-computed-favorites-position ()
  (cond
   ((buffer-file-name)				(list 'file (buffer-file-name)))
   ((and (or (eq major-mode 'monkey-mode)
	     (eq major-mode 'dired)))		(list 'file default-directory))
   (t						(list 'buffer (buffer-name)))))

(defun monkey-favorites-record (&optional dest)
  "Record the current buffer or file as `a favorite'."
  (interactive)
  (let ((f (if dest
	       (list 'file (expand-file-name dest))
	     (monkey-computed-favorites-position))))
    (if (and (not (and monkey-favorites-backward-list
		  (equal f (car monkey-favorites-backward-list))))
	     (not (and monkey-favorites-forward-list
		       (equal f (car monkey-favorites-forward-list)))))
	(setq monkey-favorites-backward-list
	      (cons f monkey-favorites-backward-list)))))

(defun monkey-favorites-erase (&optional dest)
  "Unrecord the current buffer or file as `a favorite'."
  (interactive)
  (let ((f (if dest
	       (list 'file (expand-file-name dest))
	     (monkey-computed-favorites-position))))
    (setq monkey-favorites-backward-list (delete f monkey-favorites-backward-list))
    (setq monkey-favorites-forward-list (delete f monkey-favorites-forward-list))))

(defun name-monkey-favorites (name)
  "Assign a name to the current set of favorite files."
  (interactive "SName: ")
  (define-monkey-favorites name monkey-favorites-backward-list monkey-favorites-forward-list))

;; Use this in your .emacs file.
;;
(defun define-monkey-favorites (name back forward)
  (let* ((k (assq name monkey-favorites))
	 (kv (or k (car (setq monkey-favorites (cons (list name back forward) monkey-favorites))))))
    (setcdr kv (list back forward))))

(defun select-monkey-favorites (name)
  "Choose a set of favorite files by name."
  (interactive "SName: ")
  (let ((k (assq name monkey-favorites)))
    (if (not k)
	(error "no such favorites list has been defined"))
    (setq monkey-favorites-backward-list (car (cdr k)))
    (setq monkey-favorites-forward-list (car (cdr (cdr k))))
    (and monkey-favorites-backward-list
	 (monkey-favorites-goto (car monkey-favorites-backward-list)))))

(defun monkey-favorites-backward ()
  "Move to the previous file or buffer in the favorite file list."
  (interactive)
  (if (or (not monkey-favorites-backward-list)
	  (and (not (cdr monkey-favorites-backward-list))
	       (equal (monkey-computed-favorites-position) (car monkey-favorites-backward-list))))
      (error "beginning of favorites favorites list"))
  (monkey-favorites-record)
  (setq monkey-favorites-forward-list
	(cons (car monkey-favorites-backward-list) monkey-favorites-forward-list))
  (setq monkey-favorites-backward-list (cdr monkey-favorites-backward-list))
  (monkey-favorites-goto (car monkey-favorites-backward-list)))

(defun monkey-favorites-forward ()
  "Move to the next file or buffer in the favorite file list."
  (interactive)
  (if (not monkey-favorites-forward-list)
      (error "end of favorites favorites list"))
  (monkey-favorites-goto (car monkey-favorites-forward-list))
  (setq monkey-favorites-backward-list
	(cons (car monkey-favorites-forward-list) monkey-favorites-backward-list))
  (setq monkey-favorites-forward-list (cdr monkey-favorites-forward-list)))

(defun monkey-favorites-reset ()
  "Move to the last (most recent) file or buffer in the favorite file list."
  (interactive)
  (setq monkey-favorites-backward-list
	(append (reverse monkey-favorites-forward-list) monkey-favorites-backward-list))
  (setq monkey-favorites-forward-list ())
  (and monkey-favorites-backward-list
       (monkey-favorites-goto (car monkey-favorites-backward-list))))

(defun monkey-favorites-rewind ()
  "Move to the first (oldest) file or buffer in the favorite file list."
  (interactive)
  (setq monkey-favorites-forward-list
	(append (reverse monkey-favorites-backward-list) monkey-favorites-forward-list))
  (setq monkey-favorites-backward-list ())
  (and monkey-favorites-forward-list
       (monkey-favorites-goto (car monkey-favorites-forward-list))))

(defun monkey-favorites-goto (h)
  (cond
   ((eq (car h) 'file)				(monkey-file (car (cdr h))))
   ((eq (car h) 'directory)			(let* ((n (car (cdr h)))
						       (b (get-buffer n)))
						  (if b
						      (switch-to-buffer b)
						    (or (y-or-n-p
							 (concat "Buffer "
								 n
								 " no longer exists.  Skip it? "))
							(switch-to-buffer
							 (get-buffer-create n))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ways To Create New Monkey Buffers For Files
;;;


(defun monkey-directory (directory)
  "Make a buffer for directory and monkey around with it."
  (interactive "DDirectory: ")
  (monkey-history-record)
  (monkey-history-record directory)
  (switch-to-buffer (monkey-directory-noselect directory)))
  
(defun monkey-directory-noselect (directory)
  "Make a buffer for DIRECTORY, but don't select it.
The buffer is returned."
  (let* ((name (file-name-as-directory (expand-file-name directory)))
	 (buffer (get-buffer-create name)))
    (save-excursion
      (set-buffer buffer)
      (and (eq (buffer-size) 0)
	   (progn (setq buffer-read-only t)
		  (setq default-directory name)
		  (set-buffer-modified-p nil)
		  (setq monkey-menu-override (file-exists-p ".monkey-menu-override"))
		  (monkey-list-directory)
		  (goto-char (monkey-point-min))
		  (goto-char (or (monkey-token-beginning) (point)))
		  (monkey-mode)
		  (setq monkey-menu-override (file-exists-p ".monkey-menu-override"))))
      buffer)))

(defun monkey-gee ()
  "Empty the buffer, and re-list the directory. Reconstruct
its state wrt. expanded, marked, and hidden files."
  (interactive)
  (let ((marked-files (monkey-map-marked (function (lambda () (monkey-token))) t nil t))
	(expanded-files (monkey-map-expanded (function (lambda () (monkey-token))) ">"))
	(hidden-files (monkey-map-hidden (function (lambda () (monkey-token))) "&")))
    (modify-monkey-buffer
     (delete-region (monkey-point-min) (monkey-point-max))
     (goto-char (monkey-point-min))
     (monkey-list-directory)
     (goto-char (monkey-point-min))
     (monkey-unhide-all)
     (while expanded-files
       (monkey-for-each-nodes
	(function (lambda ()
		    (let ((tk (monkey-token)))
		      (and tk
			   (progn (if (member tk expanded-files)
				      (progn
					(monkey-expand-subdirectory-interactive)
					(setq expanded-files (delete tk expanded-files)))))))))))
     (monkey-for-each-nodes (function
			     (lambda ()
			       (let ((tk (monkey-token)))
				 (and tk
				      (progn
					(if (member tk marked-files) (monkey-mark))
					(if (member tk hidden-files) (monkey-hide-line))))))))
     )
    (nice-monkey)))

(defun monkey-reset ()
  "Empty the buffer, and re-list the directory. Start from 
scratch."
  (interactive)
  (modify-monkey-buffer
   (delete-region (monkey-point-min) (monkey-point-max))
   (goto-char (monkey-point-min))
   (monkey-list-directory)
   (goto-char (monkey-point-min)))
  (nice-monkey))

(defun monkey-parent ()
  "Edit the directory `..'."
  (interactive)
  (monkey-directory ".."))

(defun file-name-at-point ()
  (condition-case nil
      (save-excursion
	(and (not (looking-at "\""))
	     (eq ?\" (char-after (max (point-min) (- (point) 1))))
	     (goto-char (max (point-min) (- (point) 1))))
	(or (looking-at "\"/")
	    (looking-at "\"./")
	    (looking-at "\"../")
	    (search-backward "\"" (max (point-min) (- (point) 2048)) t))
	(and (or (looking-at "\"/")
		 (looking-at "\"./")
		 (looking-at "\"../"))
	     (read (current-buffer))))
    (error	nil)))

(defun monkey-file-name-at-point () (or (file-name-at-point) "."))

(setq monkey-link-re
      "\"\\(/\\|~\\([^\"\\\\]\\|\\\\.\\)*\\|\\./\\|\\.\\./\\)\\([^\"\\\\]\\|\\\\.\\)*\"")

(defun monkey-next-link ()
  (interactive)
  (if (eq major-mode 'monkey-mode)
      (monkey-next-line)
    (and (file-name-at-point)
	 (progn
	   (and (or (looking-at "\"/")
		    (looking-at "\"./")
		    (looking-at "\"../"))
		(forward-char 1))
	   (search-forward "\"" nil t)))
    (goto-char (or (and (re-search-forward monkey-link-re nil t)
			(match-beginning 0))
		   (point-max)))))

(defun monkey-prev-link ()
  (interactive)
  (if (eq major-mode 'monkey-mode)
      (monkey-previous-line)
    (and (file-name-at-point) (search-backward "\"" nil t))
    (goto-char (or (and (re-search-backward monkey-link-re nil t)
			(match-beginning 0))
		   (point-min)))))

(defun monkey-follow-link ()
  (interactive)
  (if (eq major-mode 'monkey-mode)
      (monkey-edit-this)
    (monkey-file (monkey-file-name-at-point))))


(defun monkey-containing-directory ()
  (interactive)
  (if (eq major-mode 'monkey-mode)
      (monkey-file "..")
    (monkey-current-directory)))

(defun monkey-file (file)
  (interactive (let ((dflt (monkey-file-name-at-point)))
		 (list (read-file-name (concat "[" dflt "] File: ") default-directory dflt))))
  (if (file-directory-p file)
      (monkey-directory file)
    (progn
      (monkey-history-record)
      (monkey-history-record file)
      (find-file file))))

(defun monkey-alternate-file (file)
  (interactive (let ((dflt (monkey-file-name-at-point)))
		 (list (read-file-name (concat "[" dflt "] Alternate File: ")
				       default-directory dflt))))
  (kill-buffer (current-buffer))
  (monkey-file file))

(defun monkey-file-other-window (file)
  (interactive (let ((dflt (monkey-file-name-at-point)))
		 (list (read-file-name (concat "[" dflt "] File other window: ")
				       default-directory dflt))))
  (if (not (file-directory-p file))
      (find-file-other-window file)
    (let ((pop-up-windows t))
      (pop-to-buffer (monkey-directory-noselect file)))))

(defun monkey-current-directory ()
  (interactive)
  (monkey-directory default-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings / Keymaps
;;;

(defun add-monkey-mode-file-bindings (keymap)
  (define-key keymap "\C-cc" 'monkey-copy-by-regexp)
  (define-key keymap "\C-cr" 'monkey-rename-by-regexp)
  (define-key keymap "#" 'monkey-mark-auto-save-files)
  (define-key keymap "*" 'monkey-mark-executables)
  (define-key keymap "." 'monkey-mark-dotfiles)
  (define-key keymap "/" 'monkey-mark-directories)
  (define-key keymap ">" 'monkey-symlink-this)
  (define-key keymap "}" 'monkey-symlink-marked)
  (define-key keymap "@" 'monkey-mark-links)
  (define-key keymap "^" 'monkey-parent)
  (define-key keymap "`" 'monkey-parent)
  (define-key keymap "!" 'monkey-shell-command)
  (define-key keymap "&" 'monkey-background)
  (define-key keymap "C" 'monkey-copy-marked)
  (define-key keymap "c" 'monkey-copy-this)
  (define-key keymap "D" 'monkey-delete-marked)
  (define-key keymap "d" 'monkey-delete-this)
  (define-key keymap "E" 'monkey-edit-marked)
  (define-key keymap "e" 'monkey-edit-this)
  (define-key keymap "F" 'monkey-edit-marked)
  (define-key keymap "f" 'monkey-edit-this)
  (define-key keymap "\C-m" 'monkey-mark-this)
  (define-key keymap "g" 'monkey-gee)
  (define-key keymap "G" 'monkey-reset)
  (define-key keymap "j" 'monkey-edit-this-and-trash-this-buffer)
  (define-key keymap "l" 'monkey-list-this-long)
  (define-key keymap "L" 'monkey-list-long)
  (define-key keymap "M" 'monkey-toggle-menu-mode)
  (define-key keymap "o" 'monkey-edit-this-other-window)
  (define-key keymap "O" 'monkey-edit-marked-other-window)
  (define-key keymap "q" 'monkey-toggle-quietness)
  (define-key keymap "Q" 'monkey-stat-marked)
  (define-key keymap "r" 'monkey-rename-this)
  (define-key keymap "R" 'monkey-rename-marked)
  (define-key keymap "s" 'monkey-mung-this-subdirectory)
  (define-key keymap "\M-s" 'monkey-unmung-this-subdirectory)
  (define-key keymap "S" 'monkey-mung-marked-subdirectories)
  (define-key keymap "V" 'monkey-edit-marked-and-trash-this-buffer)
  (define-key keymap "v" 'monkey-edit-this-and-trash-this-buffer)
  (define-key keymap "w" 'monkey-copy-this-file-name)
  (define-key keymap "W" 'monkey-copy-marked-file-names)
  (define-key keymap "x" 'monkey-mark-by-extension)
  (define-key keymap "~" 'monkey-mark-backup-files)
  (define-key keymap "\M-/" 'monkey-mkdir))

(defvar monkey-mode-map nil "Local keymap for monkey-mode buffers.")
(put 'monkey-mode 'mode-class 'special)
(setq monkey-mode-map (make-keymap))
(suppress-keymap monkey-mode-map)
(defun add-monkey-mode-bindings (keymap))
(add-monkey-mode-bindings monkey-mode-map)
(add-monkey-mode-file-bindings monkey-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; monkey-mode
;;;

(defun monkey-mode ()
  "monkey-mode is for \"editing\" directory listings.

When use use a command like `\\[monkey-file]' to edit a directory,
a buffer is created containing a listing of that directory and the
major mode of that buffer is set to `monkey-mode'.

A monkey-mode directory listing looks like this:

/^  .
/^  ..
    =Contents
    =Readme
    credits
/^  design-notes
    index.html

The characters in the left-most colums are type indicators.  In this
case, they indicate that `.', `..', and `design-notes' are directories.

You can move around in monkey mode using the usual cursor motion
commands.  If the default bindings are in place, you can also use
short-cuts like `n' and `p'.

You can mark files using `\\[monkey-mark-this]' and unmark them using
`\\[monkey-unmark-this]' or `\\[monkey-unmark-all]' to unmark all files.  

Most function in monkey come in two flavors.  One flavor is for
operating on one file at a time.  The other flavor is for operating on
a set of several file -- the \"marked files\".

In the default bindings, the two flavors of command are usually bound
to the lower and uppercase of some character.  For example,
`\\[monkey-edit-this]' loads one file for editting, while
`\\[monkey-edit-marked]' loads all of the marked files for editting.

The format of lines in a monkey buffer tells you about the file named
on that line.  It consists of three fields:

        <typefield><markfield> <file-name>.

The markfield is empty for unmarked files, and is a `+' for marked
files.

The typefield contains a character describing the type of the file:
`/' for directories, `@' for symbolic links, `*' for executables, `,'
for character devies, and `$' for block devices.

For example, a marked directory named `foo' would look like

/^+ foo

while an unmarked directory named `bar' would look like:

/^  bar

Subdirectories can be expanded and unexpanded using the command
`\\[monkey-mung-this-subdirectory]'.  An expanded subdirectory looks like this:


/>  design-notes
/^    design-notes/.
/^    design-notes/..
      design-notes/comparisons-scorecard
      design-notes/failure-modes
      design-notes/glossary

The monkey-mode commands are summarized below.  

Basic movement commands:
========================

move down one line		`\\[monkey-next-line]'
move up one line		`\\[monkey-previous-line]'	


Basic marking commands:
=======================

Note: with a prefix arg, all marking commands become
unmarking commands, and vice versa.  

mark this file				`\\[monkey-mark-this]'
unmark this file			`\\[monkey-unmark-this]'
unmark and move backwards		`\\[monkey-unmark-this-back]'
toggle this mark			`\\[monkey-toggle-this]'
mark everything				`\\[monkey-mark-all]'
unmark everything			`\\[monkey-unmark-all]'
toggle all mark				`\\[monkey-toggle-all]'
mark all backup files			`\\[monkey-mark-backup-files]'
mark all check point files		`\\[monkey-mark-auto-save-files]'
mark all `dot' files			`\\[monkey-mark-dotfiles]'
mark all symbolic links			`\\[monkey-mark-links]'
mark all executables			`\\[monkey-mark-executables]'
mark all directories			`\\[monkey-mark-directories]'
mark files by typefield			`\\[monkey-mark-by-type]'
mark files by extension			`\\[monkey-mark-by-extension]'


Basic Operations.
=================

edit this file				`\\[monkey-edit-this]'
edit marked files			`\\[monkey-edit-marked]'
edit this file and kill this buffer	`\\[monkey-edit-this-and-trash-this-buffer]'
edit maked files and kill this buffer	`\\[monkey-edit-marked-and-trash-this-buffer]'
edit this file in the other window	`\\[monkey-edit-this-other-window]'
edit marked files in the other windows	`\\[monkey-edit-marked-other-window]'
long listing for the this file		`\\[monkey-list-this-long]'
long listing for the marked files	`\\[monkey-list-long]'
copy					`\\[monkey-copy-this]'
copy marked files			`\\[monkey-copy-marked]'
rename					`\\[monkey-rename-this]'
rename marked files			`\\[monkey-rename-marked]'
delete					`\\[monkey-delete-this]'
delete named files			`\\[monkey-delete-marked]'
copy this file-name to the kill ring	`\\[monkey-copy-this-file-names]'
copy marked file-names to the kill ring	`\\[monkey-copy-marked-file-names]'
copy marked file-names to a scratch buffer	`\\[monkey-shove]'
run a shell command on marked files		`\\[monkey-shell-command]'
run a background command on marked files	`\\[monkey-background]'


Subdirectory commands:
======================

  These two commands expand unexpanded subdirectories and 
  unexpand expanded subdirectories.

mung the current subdirectory		`\\[monkey-mung-subdirectory]'
mung the marked subdirectories		`\\[monkey-mung-marked-subdirectories]'

  With a prefix, this becomes an unmarking command:

mark this subdirectory			`\\[monkey-mark-subdirectory]'

  These traverse subdirectories, outline style.

move past this subdirectory			`\\[monkey-past-subdirectory]'
move before this subdirectory			`\\[monkey-before-subdirectory]'
move forward skipping subdirectories		`\\[monkey-next-same-level]'
move backward skipping subdirectories		`\\[monkey-previous-same-level]'
move to the next directory			`\\[monkey-next-directory]'
move to the previous directory			`\\[monkey-previous-directory]'
move to the directory line for this subdir	`\\[monkey-directory-heading]'


Hiding commands:
================

  With a prefix arg, these unhide rather than hide.

hide this file					`\\[monkey-hide-this]'
hide marked files				`\\[monkey-hide-marked]'

unhide all hidden files				`\\[monkey-unhide-all]'


Regexp commands:
================

mark files matching a regexp			`\\[monkey-mark-by-regexp]'
unmark files matching a regexp			`\\[monkey-unmark-by-regexp]'
copy by regexp (see below)			`\\[monkey-copy-by-regexp]'
rename by regexp				`\\[monkey-rename-by-regexp]'
mark by extension				`\\[monkey-mark-by-extension]'

The destination file-names given to the commands
monkey-rename-by-regexp (`\\[monkey-rename-by-regexp]') and
monkey-copy-by-regexp (`\\[monkey-copy-by-regexp]') can include
backreferences to the regexp last given to the command 
monkey-mark-by-regexp (`\\[monkey-mark-by-regexp]').

For example, suppose the goal is to rename:

	m-i586.c
	m-mips.c
	m-sparc.c
	...

to a subdirectory, removing \"m-\" from the name:

	machines/i586.c
	machines/mips.c
	machines/sparc.c
	...

This can be done using monkey-mark-by-regexp and
monkey-rename-by-regexp:

1) Mark the files using `\\[monkey-mark-by-regexp] m-\\(.*\\)'
2) Rename them using `\\[monkey-rename-by-regexp] \\1'


Commands For Outline Processing
===============================

If a directory contains a file \"=menu.el\", that file should contain an
s-expression which defines an optional file list (and file list order)
for monkey to display.  This behavior can be toggled:

toggle menu mode		`\\[monkey-toggle-menu-mode]'

Menu mode is \"on\" by default, but can be suppressed for a particular
directory by creating a file \".monkey-menu-override\".

To see this feature in action, examine the files in \"=t-elisp-how-to\"
that you should have received along with the monkey source code.

Misc commands:
==============

toggle file stating.		`\\[monkey-toggle-quietness]'

  When file stating is off, monkey is very fast, 
  but all type fields show up as '?'.

rebuild directory		`\\[monkey-gee]'
edit ..				`\\[monkey-parent]'

"

  (kill-all-local-variables)    
  (setq major-mode 'monkey-mode)
  (setq mode-name "Monkey")
  (setq mode-line-buffer-identification '("Monkey: %17b"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (setq selective-display t)
  (use-local-map monkey-mode-map)
  (monkey-visible-directory default-directory)
  (add-hook 'post-command-buffer-hook 'nice-monkey-2 t)
  (run-hooks 'monkey-mode-hook))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Editors Embedded In Larger Buffers
;;;

(defun enter-monkey-dir-region ()
  (enter-modal-region (monkey-modal-overlay (point))
		      monkey-dir-local-variables
		      monkey-mode-map))

(defun monkey-region-crossing (old new)
  (if (not (eq monkey-restriction (monkey-modal-overlay new)))
      (cond
       ((not monkey-restriction)		(enter-modal-region (monkey-modal-overlay new)
								    monkey-dir-local-variables
								    monkey-mode-map))
       ((not (monkey-modal-overlay new))	(leave-modal-region monkey-restriction
								    monkey-dir-local-variables))
       (t					(leave-modal-region monkey-restriction
								    monkey-dir-local-variables)
						(enter-modal-region (monkey-modal-overlay new)
								    monkey-dir-local-variables
								    monkey-mode-map)))))
(defun soft-monkey-mode (start end map)
  (let ((o (make-monkey-overlay start end)))
    (put-text-property start end 'point-entered 'monkey-region-crossing)
    (put-text-property start end 'point-left 'monkey-region-crossing)
    (goto-char start)
    (enter-monkey-dir-region)
    (add-hook 'post-command-buffer-hook 'nice-monkey-2 t)
    (setq monkey-restriction o)
    (use-local-map map)))



(provide 'monkey-dir)
