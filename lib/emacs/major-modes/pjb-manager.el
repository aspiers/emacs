;; Simple emacs interface to pjb command line utility.
;; Author: Toni Tammisalo <ttammisa@cc.hut.fi>
;;
;; See http://www.kolumbus.fi/toni.tammisalo/emacs-pjb-manager.html for
;; usage instructions.
;; 
;;   C-c r  Load TOC from pjb and show it in editable buffer
;;   C-c w  Write TOC from buffer to PJB. Set/Disk/Track reordering, 
;;          renaming and removing works by manually editing the TOC.
;;          This command also uploads any added files to PJB
;;   C-c a  Add new track(s) to TOC.  Single track is added by selecting
;;          single mp3 file. Multiple files are added by selecting
;;          directory containing them. In such case tracks are added to
;;          disc named after the directory. If there are subdirectories,
;;          a set is created which will wuther contain all the discs
;;          made out of those directories.
;;   C-c v  Verify edited TOC without commiting it.
;;   C-c i  Print statistics
;;
;; This requires modified pjb commandline utility 'pjb' which can upload 
;; new tracks, handle cue files and print id3 tags.
;;
;; Customization:
;;  pjb-mode-hook       - can be used to start font-lock mode by default with
;;                        (add-hook 'pjb-mode-hook 'turn-on-font-lock)
;;  pjb-program-name    - path to pjb command line utility.
;;  pjb-ls-options      - options for 'pjb ls' command. Use atleast '-all'.
;;  pjb-backup-options  - additional options for 'pjb backup' command. 
;;  pjb-external-player - external player type. supported: mpg123
;;  pjb-artist-in-discname - Set to nil if you don't want artist in discnames.
;;

(defgroup pjb nil
  "Mode for managing PJB TOC files in intelligent manner."
  :prefix "pjb-"
  :group 'pjb)

(defcustom pjb-mode-hook nil
  "If non-nil, its value is called on entry to pjb-manager mode.
One useful value to include is `turn-on-font-lock' to highlight the pieces."
  :type 'hook
  :group 'pjb)

(defvar pjb-version-string "1.6" "Version string")

;; Use these to customize paths for 'pjb'. 
(defvar pjb-program-name "pjb" 
  "*Path to pjb commandline utility for pjb-manager")
(defvar pjb-ls-options "-all" 
  "*Options string for pjb commandline utility. Use atleast \"-all\" to ensure compatibility with 'writetoc' command")
(defvar pjb-use-id3tags t)
(defvar pjb-backup-options (list "-skip" "-add-extension" "-filter=?!*\"\\/")
  "*Options string for backup command.")
(defvar pjb-external-player 'mpg123)
(defvar pjb-artist-in-discname t
  "*Set to nil if you don't want artist in discnames")

(defvar pjb-process-handle nil)

(defun reset-pjb-mode ()
  (setq pjb-process-handle nil)
  (setq major-mode 'pjb-manager)
  (setq mode-name "PJB Manager")
  (use-local-map pjb-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pjb-font-lock-keywords t)))

(defun get-error-line ()
  (save-excursion
    (set-buffer "*pjb-toc-output*")
    (goto-char (point-min))
    (if (re-search-forward "error: line \\([0-9]+\\):" nil t)
	(string-to-number (match-string 1))
	nil)))

(defun clear-output-buffer ()
  (save-excursion
    (set-buffer "*pjb-toc-output*")
    (erase-buffer)))

(defun pjb-reload-toc ()
  "Load TOC from PJB and show it in buffer."
  (interactive)
  (erase-buffer)
  (shell-command (concat pjb-program-name " ls " pjb-ls-options) 
		 "*pjb-toc-buffer*")
  (goto-char (point-min)))

(defun pjb-print-info ()
  (interactive)
  (clear-output-buffer)
  (shell-command (concat pjb-program-name " info") "*pjb-toc-output*"))

(defun pjb-verify-sentinel (process event)  
  (setq pjb-process-handle nil)
  (save-excursion
    (set-buffer "*pjb-toc-output*")
    (insert "\n" event "\n"))
  (save-excursion
    (set-buffer "*pjb-toc-buffer*")
    (let* ((err (get-error-line)))
      (if (numberp err)
	  (goto-line err)))))

(defun pjb-verify-toc ()
  (interactive)
  (if (processp pjb-process-handle)
      (if (y-or-n-p "TOC commit in progress, do you want to abort it? ")
	  (progn
	    (setq pjb-process-handle nil)
	    (delete-process "pjb-process-name")))
    (let* ((cmdname pjb-program-name)
	   (pname "pjb-process-name")
	   (process-connection-type nil))
      (clear-output-buffer)
      (setq pjb-process-handle
	    (start-process pname
			   "*pjb-toc-output*"
			   cmdname 
			   "writetoc" "stdin" "-quiet" "-verify"))
      (if (fboundp 'set-process-coding-system)	  
	  (set-process-coding-system pjb-process-handle
				     'iso-latin-1 'iso-latin-1))
      (set-process-sentinel pjb-process-handle 
			    'pjb-verify-sentinel)
      (process-send-region pname (point-min) (point-max))
      (if (equal (process-status pname) 'run)
	  (process-send-eof pname)))))

(defun pjb-commit-sentinel (process event)
  (delete-process process)
  (setq pjb-process-handle nil)
  (set-buffer "*pjb-toc-buffer*")
  (let* ((err (get-error-line)))
    (if (numberp err)
	(goto-line err))))

(defun pjb-commit-toc ()
  "Commit TOC to PJB, possibly uploading any added new tracks.
If commit was succesful, TOC will be automatically reloaded."
  (interactive)
  (if (processp pjb-process-handle)
      (if (y-or-n-p "TOC commit in progress, do you want to abort it? ")
	  (progn
	    (setq pjb-process-handle nil)
	    (delete-process "pjb-process-name")))
    (if (y-or-n-p "Are you sure you want to commit TOC? ")
	(let* ((cmdname pjb-program-name)
	       (process-connection-type nil))
	  (clear-output-buffer)
	  (setq pjb-process-handle
		(start-process "pjb-process-name" 
			       "*pjb-toc-output*"
			       cmdname "writetoc" "stdin" "-quiet"))
	  (set-process-sentinel pjb-process-handle 'pjb-commit-sentinel)
	  (if (fboundp 'set-process-coding-system)
	      (set-process-coding-system pjb-process-handle
					 'iso-latin-1 'iso-latin-1))
	  (process-send-region "pjb-process-name" 
			       (point-min) (point-max))
	  (process-send-eof "pjb-process-name")))))


(defun pjb-backup-process-sentinel (process event)
  (delete-process process)
  (setq pjb-process-handle nil))

(defun pjb-backup-process (args)
  (if (processp pjb-process-handle)
      (if (y-or-n-p "PJB command in progress, do you want to abort it? ")
	  (progn
	    (setq pjb-process-handle nil)
	    (delete-process "pjb-process-name")))
    (let* ((cmdname pjb-program-name)
	   (process-connection-type nil))
      (clear-output-buffer)
      (setq pjb-process-handle
	    (apply 'start-process 
		   (append (list "pjb-process-name" 
				 "*pjb-toc-output*"
				 cmdname "backup")
			   pjb-backup-options
			   args)))
      (process-send-eof "pjb-process-name")
      (set-process-sentinel pjb-process-handle 
			    'pjb-backup-process-sentinel))))


(defun print-help-text ()
  (insert 
   "Interactive PJB management mode\n"
   "Author: Toni Tammisalo <ttammisa@cc.hut.fi>\n"
   "Version " pjb-version-string "\n"
   "Available key bindings:\n"
   "  C-c r       - Read TOC from PJB.  This discards the currently\n"
   "                loaded (and possibly modified) TOC.\n"
   "  C-c w       - Write TOC to PJB.  This commits all ordering and\n"
   "                naming changes, removes deleted tracks and \n"
   "                automatically uploads newly added files. If commit is\n"
   "                already in progress, it will be aborted instead.\n"
   "  C-c a       - Add new track to current cursor position.\n"
   "  C-c s       - Sort current set or disc (based on cursor position) in\n"
   "                alphabetical, reverse alphabetical or random order.\n"
   "  C-c p       - Play selected track by downloading it to external\n"
   "                command. Stops playing when used without track.\n"
   "  C-c C-p     - Prettify track names inside a region.\n"
   "                Currently just replaces '_' with ' '.\n"
   "  C-c i       - Retrieve information from PJB.\n"
   "  C-c C-i     - Get statistics on current (based on cursor position)\n"
   "                disc or set.\n"      
   "  C-c c       - Count total file size for new (added) tracks.\n"
   "  C-c v       - Verify edited TOC but don't commit it.\n"
   "  C-c C-b     - Backup current set, disc or track.\n"
   "  C-c C-c C-b - Backup whole PJB contents.\n"
   "  C-c C-t     - Toggle id3tag reading on and off.\n"
   "  C-c h       - Show this help text.\n\n"
   "  Shift up/down jump between sets and control up/down between discs.\n"
   "  Shift right/left increase and decrease hiding level.\n"
   "\n"))

(defun pjb-print-help-text ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*pjb-toc-output*"))
    (erase-buffer)
    (print-help-text)))

(defun pjb-get-current-context ()
  (save-excursion
    (let* ((end nil)
	   (start nil)
	   (type nil)
	   (sname "")
	   (dname "")
	   (tname ""))
      (end-of-line)
      (setq end (point-marker))
      (beginning-of-line)    
      (setq start (point-marker))
      (narrow-to-region start end)
      (cond ((re-search-forward "Track: \\([^\n\r]*\\)[ \t]*$" nil t)
	     (setq type 'track)
	     (setq tname (match-string 1))
	     (widen)
	     (setq dname (if (re-search-backward "Disc: \\([^\n\r]*\\)[ \t]*$" 
						 nil t)
			     (match-string 1)
			   ""))
	     (setq sname (if (re-search-backward "Set: \\([^\n\r]*\\)[ \t]*$" 
						 nil t)
			     (match-string 1)
			   "")))
	    ((re-search-forward "Disc: \\([^\n\r]*\\)[ \t]*$" nil t)
	     (setq type 'disc)
	     (setq dname (match-string 1))
	     (widen)
	     (setq sname (if (re-search-backward "Set: \\([^\n\r]*\\)[ \t]*$" 
						 nil t)
			     (match-string 1)
			   "")))
	    ((re-search-forward "Set: \\([^\n\r]*\\)[ \t]*$" nil t)
	     (setq type 'set)
	     (setq sname (match-string 1))
	     (widen))
	    (t (widen)))
      (cond ((equal type 'track)
	     (list type sname dname tname))
	    ((equal type 'disc)
	     (list type sname dname))
	    ((equal type 'set)
	     (list type sname))
	    (t
	     nil)))))
(defun pjb-get-track-name (ctx)
  (if ctx (nth 3 ctx) nil))
(defun pjb-get-disc-name (ctx)
  (if ctx (nth 2 ctx) nil))
(defun pjb-get-set-name (ctx)
  (if ctx (nth 1 ctx) nil))

(defun pjb-print-current-context-info ()
  (interactive)
  (save-excursion
    (let* ((ctx (pjb-get-current-context)))
      (clear-output-buffer)
      (cond ((equal ctx nil)
	     (message (concat "First select set, disc or track by "
			      "placing cursor on top of it")))
	    ((equal (car ctx) 'track)
	     (save-excursion
	       (set-buffer "*pjb-toc-output*")	       
	       (insert "Info on track '" (pjb-get-track-name ctx) "'\n\n"))
	     (shell-command (concat pjb-program-name " info "
				    "-set=\"" (pjb-get-set-name ctx) "\" "
				    "-disc=\"" (pjb-get-disc-name ctx) "\" "
				    "-track=\"" (pjb-get-track-name ctx) "\" ")
			    "*pjb-toc-output*"))
	    ((equal (car ctx) 'disc)
	     (save-excursion
	       (set-buffer "*pjb-toc-output*")
	       (insert "Info on disc '" (pjb-get-disc-name ctx) "'\n\n"))
	     (shell-command (concat pjb-program-name " info "
				    "-set=\"" (pjb-get-set-name ctx) "\" "
				    "-disc=\"" (pjb-get-disc-name ctx) "\" ")
			    "*pjb-toc-output*"))
	    ((equal (car ctx) 'set)
	     (save-excursion
	       (set-buffer "*pjb-toc-output*")
	       (insert "Info on set '" (pjb-get-set-name ctx) "'\n\n"))
	     (shell-command (concat pjb-program-name " info "
				    "-set=\"" (pjb-get-set-name ctx) "\" ")
			    "*pjb-toc-output*"))))))

(defun pjb-backup ()
  "Download whole PJB contents."
  (interactive)
  (save-excursion
    (if (processp pjb-process-handle)
	(if (y-or-n-p "PJB command in progress, do you want to abort it? ")
	    (progn
	      (setq pjb-process-handle nil)
	      (delete-process "pjb-process-name")))
      (let ((dir (expand-file-name 
		  (read-file-name "Whole PJB backup to directory: " 
				  nil nil nil))))
	(if (file-directory-p dir)
	    (pjb-backup-process (list (concat "-dir=" filename)))
	  (message "Destination must be a directory"))))))
  
(defun pjb-backup-current-context ()
  "Download either single track or entire disc or set structure from PJB."
  (interactive)
  (if (processp pjb-process-handle)
      (if (y-or-n-p "PJB command in progress, do you want to abort it? ")
	  (progn
	    (setq pjb-process-handle nil)
	    (delete-process "pjb-process-name")))
    (save-excursion
      (let* ((ctx (pjb-get-current-context)))
	(clear-output-buffer)
	(cond ((equal ctx nil)
	       (message (concat "First select set, disc or track by "
				"placing cursor on top of it")))
	      ((equal (car ctx) 'track)
	       (let ((dir (expand-file-name 
			   (read-file-name "Backup directory (or file): " 
					   nil nil nil))))
		 (pjb-backup-process 
		  (list (concat "-set=" (pjb-get-set-name ctx))
			(concat "-disc=" (pjb-get-disc-name ctx))
			(concat "-track=" (pjb-get-track-name ctx))
			(if (file-directory-p dir)
			    (concat "-dir=" dir)
			  (concat "-file=" dir))))))
	      ((equal (car ctx) 'disc)
	       (let ((dir (expand-file-name 
			   (read-file-name "Backup directory: " nil nil nil))))
		 (if (file-directory-p dir)
		     (pjb-backup-process 
		      (list (concat "-dir=" dir)
			    (concat "-set=" (pjb-get-set-name ctx))
			    (concat "-disc=" (pjb-get-disc-name ctx))))
		   (message "Destination must be a directory"))))
	      ((equal (car ctx) 'set)
	       (let ((dir (expand-file-name 
			   (read-file-name "Backup directory: " nil nil nil))))
		 (if (file-directory-p dir)
		     (pjb-backup-process 
		      (list (concat "-dir=" dir)
			    (concat "-set=" (pjb-get-set-name ctx))))
		   (message "Destination must be a directory")))))))))

(defun pjb-play-track-process-sentinel (process event)
  (delete-process process)
  (setq pjb-process-handle nil))

(defun pjb-play-track-process (set disc track)
  (let* ((cmdname pjb-program-name)
	 (ext pjb-external-player)
	 (process-connection-type nil))
    (clear-output-buffer)
    (setq pjb-process-handle
	  (cond 
	   ((equal ext 'mpg123)
	    (apply 'start-process 
		   (list "pjb-process-name" 
			 "*pjb-toc-output*"
			 "sh" "-c" 
			 (concat cmdname " backup -set=\"" set "\" "
				 "-disc=\"" disc "\" -track=\"" track 
				 "\" -file=stdout | mpg123 -b 128 -"))))
	   (t
	    (message "error; unknown external player.")
	    nil)))
    (process-send-eof "pjb-process-name")
    (set-process-sentinel pjb-process-handle 
			  'pjb-backup-process-sentinel)))

(defun pjb-play-current-track ()
  "Play currently selected track by downloading it to external application."
  (interactive)
  (if (processp pjb-process-handle)
      (progn
	(setq pjb-process-handle nil)
	(delete-process "pjb-process-name")))
  (save-excursion
    (let* ((ctx (pjb-get-current-context)))
      (clear-output-buffer)
      (cond ((equal ctx nil)
	     (message (concat "First select set, disc or track by "
			      "placing cursor on top of it")))
	    ((equal (car ctx) 'track)
	     (pjb-play-track-process (pjb-get-set-name ctx) 
				     (pjb-get-disc-name ctx) 
				     (pjb-get-track-name ctx)))
	    (t (message "Only single tracks can be played."))))))

(defun get-last-path-component (path)
  (if (string-match "^.*/\\([^/]+\\)/?$" path)
      (substring path (match-beginning 1) (match-end 1))
    path))

(defun replace-char (str chr rpl)
  (while (string-match chr str)
    (setq str (replace-match rpl t t str)))
  str)

(defun trim-whitespace (str)
  (if (string-match "^[ \t]*\\(\\w\\|\\w.*\\w\\)[ \t]*$" str)
      (substring str (match-beginning 1) (match-end 1))
    str))

(defun us-to-sp (str)
  (replace-char (trim-whitespace str) "_" " "))

(defun get-mp3-info (path)
  (if pjb-use-id3tags
      (condition-case err
	  (save-excursion
	    (set-buffer (get-buffer-create "*pjb-toc-output*"))
	    (erase-buffer)
	    (shell-command (concat pjb-program-name " id3tags \"" path "\"")
			   "*pjb-toc-output*")
	    (goto-line 0)
	    (let* ((sr1 (re-search-forward 
			 "Artist: \\(.*\\) *$" nil t))
		   (artist (match-string 1))
		   (sr2 (re-search-forward 
			 "Album: \\(.*\\) *$" nil t))
		   (album (match-string 1))
		   (sr3 (re-search-forward 
			 "Title: \\(.*\\) *$" nil t))
		   (track (match-string 1)))
	      (erase-buffer)
	      (if sr1 (list track album artist)))))
    nil))

(defun get-track-name (info)
  (nth 0 info))
(defun get-album-name (info)
  (nth 1 info))
(defun get-artist-name (info)
  (nth 2 info))

(defun insert-new-track (fname)
  (let* ((info (get-mp3-info fname))
	 (tname (if info
		    (get-track-name info)
		  (us-to-sp (file-name-sans-extension 
			     (file-name-nondirectory fname))))))
    (insert "        Track: " tname "\n"
	    "               Filename=\"" fname "\"\n"
	    "\n")))


;; This function will create a disk 'dirname' and add all mp3
;; files in the directory to that disk. If the directory contains
;; further subdirectories, a set is created and all subdirectories 
;; are handled in the same way.
(defun handle-directory (setname dirname level first)
  (let* ((lst (directory-files dirname t))
	 (entries lst)
	 (count 0)
	 (artist setname))
    ;; First loop the contents for all .mp3's.
    (while (stringp (car entries))
      (if (and (not (file-directory-p (car entries)))
	       (string-match "\\.[mM][pP]3$" (car entries)))
	  (progn
	    (if (= count 0)
		(let* ((info (get-mp3-info (car entries)))
		       (discname (us-to-sp (get-last-path-component dirname))))
		  (if (and first 
			   (stringp setname))
		      (insert "Set: " setname "\n"))
		  (if info
		      (progn
			(setq artist (get-artist-name info))
			(setq discname (get-album-name info))))
		  (if (and (stringp artist)
			   pjb-artist-in-discname)
		      (insert "    Disc: " artist " / " discname "\n")
		    (insert "    Disc: " discname "\n"))))
	    (insert-new-track (car entries))
	    (setq count (+ count 1))))
      (setq entries (cdr entries)))
    ;; Then rescan for any subdirectories.
    (if (> level 1)
	nil
      (setq count 0)
      (setq entries lst)
      (while (stringp (car entries))
	(if (file-directory-p (car entries))
	    (let* ((set (file-name-nondirectory (car entries))))
	      (if (or (string= set ".") (string= set ".."))
		  nil
		(handle-directory (us-to-sp (get-last-path-component dirname))
				  (car entries) (+ level 1) (= count 0))
		(setq count (+ count 1)))))
	(setq entries (cdr entries))))))


(defun pjb-add-files ()
  "Add new MP3 files as tracks to current location in TOC."
  (interactive)
  (save-excursion
    (let* ((filename (expand-file-name 
		      (read-file-name "Add new track: " nil nil nil)))
	   (track (file-name-sans-extension 
		   (file-name-nondirectory filename)))
	   (search-result (re-search-forward "\\(Track:\\|Set:\\|Disc:\\)" 
					     nil t)))
      (if search-result
	  (beginning-of-line)
	(goto-char (point-max)))
      (if (file-name-directory filename)
	  (setq default-directory (file-name-directory filename)))
      (if (file-directory-p filename)
	  (handle-directory nil filename 0 t)
	(if (or (not (file-exists-p filename)) (string= "" track))
	    (message "Can't find file")
	  (insert-new-track filename))))))

(defun insert-cue-file (filename)
  (save-excursion
    (let* ((output (shell-command-to-string (concat pjb-program-name 
						    " convert-cue -cuefile=\""
						    filename "\""))))
      (insert output))))

(defun pjb-add-cue-file ()
  "Add CUE file (and the MP3 it refers) as a disc in current location in TOC."
  (interactive)
  (save-excursion
    (let* ((filename (expand-file-name 
		      (read-file-name "Add cue file: " nil nil nil)))
	   (track (file-name-sans-extension 
		   (file-name-nondirectory filename)))
	   (search-result (re-search-forward "\\(Track:\\|Set:\\|Disc:\\)" 
					     nil t)))
      (if search-result
	  (beginning-of-line)
	(goto-char (point-max)))
      (if (file-name-directory filename)
	  (setq default-directory (file-name-directory filename)))
      (if (or (not (file-exists-p filename)) (string= "" track))
	  (message "Can't find file")
	(insert-cue-file filename)))))

(defun pjb-count-file-sizes ()
  "Count total size of new files to be added."
  (defun fsize (fname)
    (let ((attr (file-attributes fname)))
      (if attr (/ (+ (nth 7 attr) 1023) 1024) 0)))
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((kbytes 0)
	  (files 0))
      (while (progn
	       (let* ((search-result 
		       (re-search-forward "Filename=\"\\(.*\\)\"" nil t))
		      (name (match-string 1)))
		 (if search-result
		     (progn 
		       (setq kbytes (+ kbytes (fsize name)))
		       (setq files (+ files 1))
		       t)
		   nil))))
      (message "Total file size for new tracks is %d Mb in %d files." 
	       (/ kbytes 1024) files))))

(defun pjb-sort-set ()
  "Sort set of discs alphabetically."
  (interactive)
  (save-excursion
    (let* ((search-result (re-search-backward 
			   "^[ \t]*Set: \\([^\n\r]*\\)[ \t]*$"
			   nil t))
	   (setname (match-string 1)))
      (if (and search-result
	       (y-or-n-p (concat "Sort set '" setname "' alphabetically? ")))
	  (progn
	    (shell-command (concat pjb-program-name " sort -set=\"" 
				   setname "\"")
			   "*pjb-toc-buffer*")
	    (erase-buffer)
	    (shell-command (concat pjb-program-name " ls " pjb-ls-options)
			   "*pjb-toc-buffer*"))))))
      
(defun pjb-prettify-track-names (point mark)
  (defun prettify ()
    (beginning-of-line)
    (let* ((search-result (re-search-forward 
			   "\\(Track|Disc\\): \\([^\n\r]*\\)[ \t]*$"
			   nil t))
	   (start (match-beginning 0))
	   (end (match-end 0))
	   (label (match-string 1))
	   (name (match-string 2)))
      (if search-result
	  (let* ((newname (replace-char name "_" " ")))
	    (delete-region start end)
	    (insert label ": " newname)
	    t)
	nil)))
  (interactive "r")
  (save-excursion
    (narrow-to-region point mark)
    (goto-char point)
    (while (prettify)
      (forward-line 1))
    (widen)))

(defun pjb-sort-blocks (startexp stopexp recstart reverse random-order)
  (if (not stopexp)
      (setq stopexp startexp))
  (save-excursion
    (save-restriction
      (defun narrow-to-block ()   
	(let* ((sr1 (re-search-backward startexp nil t))
	       (start (match-beginning 0)))
	  (if sr1
	      (progn
		(forward-line 1)
		(let* ((sr2 (re-search-forward stopexp nil t))
		       (end (if sr2
				(match-beginning 0)
			      (point-max))))
		  (narrow-to-region start end))))))
      (defun next-record ()
	(let* ((sr1 (re-search-forward recstart nil t))
	       (start (match-beginning 0)))
	  (if sr1
	      (goto-char start)
	    (goto-char (point-max)))))
      (defun end-of-record ()
	(let* ((sr1 (re-search-forward recstart nil t))
	       (start (match-beginning 0)))
	  (if sr1
	      (progn
		(goto-char start)
		(forward-line -1)
		(end-of-line))
	    (goto-char (point-max)))))
      (defun sort-key ()
	(let* ((sr (re-search-forward "\\(Disc\\|Track\\): \\(.*\\)$" nil t)))
	  (if sr
	      (match-string 2)
	    "z")))
      (let ((alst '()))
	(defun random-key ()
	  (let* ((sr (re-search-forward "Start=\\([0-9\\.]+\\) " nil t)))
	    (if sr
		(let* ((string (match-string 1))
		       (value (assoc string alst)))
		  (if value
		      (cdr value)
		    (let ((rnd (random)))
		      (setq alst (cons (cons string rnd) alst))
		      rnd)))		  
	      "z")))
	(narrow-to-block)
	(goto-char (point-min))
	(next-record)
	(sort-subr reverse 'next-record 'end-of-record 
		   (if random-order
		       'random-key
		     'sort-key))))))
(defun pjb-sort-discs (&optional reverse random-order)
  (interactive)
  (pjb-sort-blocks "^[ \t]*Set:" nil "^[ \t]*Disc:" reverse random-order))
(defun pjb-sort-discs-reverse ()
  (interactive)
  (pjb-sort-discs t nil))
(defun pjb-sort-discs-random ()
  (interactive)
  (pjb-sort-discs nil t))
(defun pjb-sort-tracks (&optional reverse random-order)
  (interactive)
  (pjb-sort-blocks "^[ \t]*Disc:" "^[ \t]*\\(Set\\|Disc\\):" "^[ \t]*Track:" 
		   reverse random-order))
(defun pjb-sort-tracks-reverse ()
  (interactive)
  (pjb-sort-tracks t nil))
(defun pjb-sort-tracks-random ()
  (interactive)
  (pjb-sort-tracks nil t))
(defun pjb-sort-context-internal (ask &optional reverse random)
  (beginning-of-line)
  (let* ((sr (re-search-forward "^.*$" nil t))
	 (line (match-string 0))
	 (sort-set nil))
    (cond ((string-match "Set:" line)
	   (setq sort-set t)))
    (defun get-sort-order ()      
      (let ((answer (completing-read
		     (concat "Sort " 
			     (if sort-set "discs" "tracks")
			     " in alphabetical, reverse or random order? ")
		     '(("alpha" . 1) ("reverse" . 2) ("random" . 3))
		     nil t "alpha" nil "alpha")))
	(cond ((string= answer "alpha")
	       t)
	      ((string= answer "reverse") 
	       (setq reverse t))
	      ((string= answer "random") 
	       (setq random t))
	      (t (get-sort-order)))))
    (if ask
	(get-sort-order))
    (if sort-set
	(pjb-sort-discs reverse random)
      (pjb-sort-tracks reverse random))))
(defun pjb-sort-context ()
  (interactive)
  (pjb-sort-context-internal t nil nil))
(defun pjb-sort-context-alpha ()
  (interactive)
  (pjb-sort-context-internal nil nil nil))
(defun pjb-sort-context-reverse ()
  (interactive)
  (pjb-sort-context-internal nil t nil))
(defun pjb-sort-context-random ()
  (interactive)
  (pjb-sort-context-internal nil nil t))


(defun pjb-toggle-id3tags ()
  (interactive)
  (setq pjb-use-id3tags (not pjb-use-id3tags))
  (message (if pjb-use-id3tags "ID3 tags enabled" "ID3 tags disabled")))

(defun pjb-forward-rx (pattern)
  (forward-line 1)
  (let* ((search-result (re-search-forward pattern nil t))
	 (start (match-beginning 0)))
    (if search-result
	(goto-char start)
      (forward-line -1))))
(defun pjb-backward-rx (pattern)
  (forward-line -1)
  (let* ((search-result (re-search-backward pattern nil t))
	 (start (match-beginning 0)))
    (if search-result
	(goto-char start)
      (forward-line 1))))

(defun pjb-forward-set ()
  (interactive)
  (pjb-forward-rx "^[ \t]*Set: "))
(defun pjb-backward-set ()
  (interactive)
  (pjb-backward-rx "^[ \t]*Set: "))
(defun pjb-forward-disc ()
  (interactive)
  (pjb-forward-rx "^[ \t]*Disc: "))
(defun pjb-backward-disc ()
  (interactive)
  (pjb-backward-rx "^[ \t]*Disc: "))
(defun pjb-forward-track ()
  (interactive)
  (pjb-forward-rx "^[ \t]*Track: "))
(defun pjb-backward-track ()
  (interactive)
  (pjb-backward-rx "^[ \t]*Track: "))

(defvar pjb-hiding-level nil)
(defun pjb-inc-hiding-level ()
  (interactive)
  (cond ((equal pjb-hiding-level nil)
	 (setq pjb-hiding-level 9))
	((equal pjb-hiding-level 9)
	 (setq pjb-hiding-level 6))
	(t 
	 (setq pjb-hiding-level 3)))
  (setq selective-display pjb-hiding-level)
  (redraw-display))
(defun pjb-dec-hiding-level ()
  (interactive)
  (cond ((equal pjb-hiding-level 3)
	 (setq pjb-hiding-level 6))
	((equal pjb-hiding-level 6)
	 (setq pjb-hiding-level 9))
	(t 
	 (setq pjb-hiding-level nil)))
  (setq selective-display pjb-hiding-level)
  (redraw-display))
  

(defvar pjb-mode-map nil "*Local keymap for PJB mode listings.")
(setq pjb-mode-map (make-keymap))

(define-key pjb-mode-map "\C-cr" 'pjb-reload-toc)
(define-key pjb-mode-map "\C-ca" 'pjb-add-files)
(define-key pjb-mode-map "\C-c\C-a" 'pjb-add-cue-file)
(define-key pjb-mode-map "\C-cw" 'pjb-commit-toc)
;(define-key pjb-mode-map "\C-cs" 'pjb-sort-set)
(define-key pjb-mode-map "\C-cs" 'pjb-sort-context)
(define-key pjb-mode-map "\C-ci" 'pjb-print-info)
(define-key pjb-mode-map "\C-c\C-i" 'pjb-print-current-context-info)
(define-key pjb-mode-map "\C-c\C-c\C-b" 'pjb-backup)
(define-key pjb-mode-map "\C-c\C-b" 'pjb-backup-current-context)
(define-key pjb-mode-map "\C-cv" 'pjb-verify-toc)
(define-key pjb-mode-map "\C-c\C-p" 'pjb-prettify-track-names)
(define-key pjb-mode-map "\C-ch" 'pjb-print-help-text)
(define-key pjb-mode-map "\C-c\C-t" 'pjb-toggle-id3tags)
(define-key pjb-mode-map "\C-cc" 'pjb-count-file-sizes)
(define-key pjb-mode-map "\C-cp" 'pjb-play-current-track)

(define-key pjb-mode-map (kbd "<S-down>") 'pjb-forward-set)
(define-key pjb-mode-map (kbd "<S-up>") 'pjb-backward-set)
(define-key pjb-mode-map (kbd "<C-down>") 'pjb-forward-disc)
(define-key pjb-mode-map (kbd "<C-up>") 'pjb-backward-disc)
(define-key pjb-mode-map (kbd "<M-down>") 'pjb-forward-track)
(define-key pjb-mode-map (kbd "<M-up>") 'pjb-backward-track)

(define-key pjb-mode-map (kbd "<S-left>") 'pjb-inc-hiding-level)
(define-key pjb-mode-map (kbd "<S-right>") 'pjb-dec-hiding-level)

;; Menus for those who like them.
(defvar pjb-menu-map (make-sparse-keymap "PJB Manager"))
(define-key pjb-menu-map [commit] '("Commit TOC" . pjb-commit-toc))
(define-key pjb-menu-map [separator2] '("--"))
(define-key pjb-menu-map [playtr] '("Play Track" . pjb-play-current-track))
(define-key pjb-menu-map [info] '("Info" . pjb-print-info))
(define-key pjb-menu-map [help] '("Help" . pjb-print-help-text))
(define-key pjb-menu-map [separator1] '("--"))
(define-key pjb-menu-map [id3tags] '("Toggle ID3 Tags" . pjb-toggle-id3tags))

(define-key pjb-menu-map [separator3] '("--"))
(define-key pjb-menu-map [backup] '("Backup..." . pjb-backup))
(define-key pjb-menu-map [separator4] '("--"))

(define-key pjb-menu-map [sort] (cons "Sort" (make-sparse-keymap "Sort")))
(define-key pjb-menu-map [sort sortrnd] 
  '("Random Order" . pjb-sort-context-random))
(define-key pjb-menu-map [sort sortrev] 
  '("Reverse Order" . pjb-sort-context-reverse))
(define-key pjb-menu-map [sort sortalp] 
  '("Alphabetic Order" . pjb-sort-context-alpha))

(define-key pjb-menu-map [addnew] '("Add New Files" . pjb-add-files))
(define-key pjb-menu-map [addcue] '("Add Cue File" . pjb-add-cue-file))
(define-key pjb-menu-map [fsizes] '("Count File Sizes" . pjb-count-file-sizes))
(define-key pjb-menu-map [reload] '("Reload TOC" . pjb-reload-toc))

(define-key pjb-mode-map [menu-bar pjb] (cons "PJB Manager" pjb-menu-map))


(defface pjb-font-lock-trackname-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:bold t)))
  "*Face to use for PJB tracknames."
  :group 'pjb)
(defface pjb-font-lock-keyword-face
  '((((class color) (background light)) (:foreground "DarkGreen" :bold f))
    (((class color) (background dark)) (:foreground "LightGreen" :bold f))
    (t (:underline t)))
  "*Face to use for PJB keywords."
  :group 'pjb)
(defface pjb-font-lock-discname-face
  '((((class color) (background light)) (:foreground "Blue" :bold t))
    (((class color) (background dark)) (:foreground "LightBlue" :bold t))
    (t (:bold t)))
  "*Face to use for PJB discnames."
  :group 'pjb)
(defvar pjb-font-lock-keyword-face 'pjb-font-lock-keyword-face)
(defvar pjb-font-lock-trackname-face 'pjb-font-lock-trackname-face)
(defvar pjb-font-lock-discname-face 'pjb-font-lock-discname-face)
(defvar pjb-font-lock-keywords  
  '(("\\(Track\\|Disc\\|Set\\):" . pjb-font-lock-keyword-face)
    ("Track: \\(.*\\)$" 1 pjb-font-lock-trackname-face)
    ("\\(Disc\\|Set\\): \\(.*\\)$" 2 pjb-font-lock-discname-face))
  "Expressions to highlight in pjb-manager mode.")

(defun pjb-manager-mode ()
  "Major mode for interactive PJB TOC editing. "
  (interactive)
  (setq major-mode 'pjb-mode
        mode-name "PJB Manager")
  (pop-to-buffer "*pjb-toc-output*")
  (pop-to-buffer "*pjb-toc-buffer*")
  (print-help-text)
  (setq coding-system-for-read 'iso-latin-1)
  (setq paragraph-start "^[ \t]*$")
  (setq paragraph-separate "^[ \t]*$")
  (setq sentence-end "EncType=[A-Za-z0-9]*")
  (setq page-delimiter "^[ \t]*Set:")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pjb-font-lock-keywords t))
  (reset-pjb-mode)
  (random t)
  (run-hooks 'pjb-mode-hook))

(defun pjb-manager ()
  "Start PJB Manager mode."
  (interactive)
  (let ((buff (get-buffer "*pjb-toc-buffer*")))
    (if buff
        (switch-to-buffer buff)  
      (pjb-manager-mode))))

(provide 'pjb-manager)
(add-hook 'pjb-mode-hook 'turn-on-font-lock)

