;;; background.el - 	A collection of nice interfaces for running 
;;;			subprocesses, both interactive and for lisp 
;;;			programs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;


(require 'shell)
(require 'modal)
(require 'comint)
(require 'nice)


;;; {Parameters}
;;;

(setq background-minibuffer-map (make-keymap))

(put 'background-minibuffer-map 'variable-documentation
     "A keymap to use when prompting the user for a shell command.")

(setq background-history-list '())

(establish-local-variables 'background-variables

			   '(;; a parameter to the callback.
			     ;;
			     (background-closure		())
			     
			     (background-callback		())
			     (background-silentp		())))




;;; {Background Jobs}
;;;

(defun interactive-background-command (command showp)
  "Run COMMAND in the background in an interactive buffer.
A message is displayed when the job starts and finishes,
or otherwise changes state.  The process buffer is displayed
but not selected."
  (interactive (list (read-from-minibuffer "% " nil nil nil 'background-history-list)
		     (not current-prefix-arg)))
  (background command showp))

(defun interactive-shell-command (command select)
  "Run COMMAND in the in an interactive buffer and select that buffer.
A message is displayed when the job starts and finishes,
or otherwise changes state."
  (interactive (list (read-from-minibuffer "% " nil nil nil 'background-history-list)
		     (cond
		      ((or (consp current-prefix-arg)
			   (and (numberp current-prefix-arg)
				(>= current-prefix-arg 0)))	'insert)
		      ((or (eq ?- current-prefix-arg)
			   (and (numberp current-prefix-arg)
				(< current-prefix-arg 0)))	'other)
		      (t					t))))
  (if (eq 'insert select)
      (shell-command command t)
    (background command nil select)))


(defun background (command &optional showp selectp silentp callback closure name)
  (background-exec (list shell-file-name "-c" command)
		   showp selectp silentp callback closure name))

(defun background-exec (command-line &optional showp selectp silentp callback closure name)
  (let* ((job-number 1)
	 (job-name (or name
		       (let ((name "%1"))
			 (while (get-process name)
			   (setq job-number (1+ job-number))
			   (setq name (concat "%" job-number)))
			 name)))
	 (buffer (get-buffer-create job-name))
	 (process (save-excursion
		    (let ((dir (if (not (string-match "^[\t ]*cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*"
						      (car command-line)))
				   default-directory
				 (prog1
				     (file-name-as-directory 
				      (expand-file-name 
				       (substring (car command-line)
						  (match-beginning 1)
						  (match-end 1))))
				   (setq command-line (cons (substring (car command-line)
								       (match-end 0))
							    (cdr command-line))))))
			  (cmd (mapconcat (lambda (x) x) command-line " ")))
		      (set-buffer buffer)
		      (erase-buffer)
		      (setq default-directory dir)
		      (or silentp
			  (insert "*** Start \"" cmd "\" in " default-directory " at "
				  (substring (current-time-string) 11 19) ?\n))
		      (let* ((buffer (comint-exec (current-buffer) 
						  job-name
						  (car command-line) () (cdr command-line)))
			     (p (get-buffer-process buffer)))
			(set-marker (process-mark p) (point))
			(add-stable-sentinel (process-name p) 'background-sentinel)
			(message "[%d] %d" job-number (process-id p))
			(shell-mode)
			(setq mode-name "Background")
			(setq background-callback callback)
			(setq background-closure closure)
			(setq background-silentp silentp)
			(stable-hook-check)
			p)))))

    (cond
     ((and showp (not selectp))		(let ((win (selected-window)))
					  (unwind-protect 
					      (pop-to-buffer 
					       (buffer-name (process-buffer process)))
					    (and (window-live-p win)
						 (select-window win)))))

     ((eq 'other selectp)		(pop-to-buffer (buffer-name (process-buffer process)))
      					(goto-char (point-max)))

     (selectp 				(set-window-buffer (selected-window) 
							   (process-buffer process))))
    process))

(defun background-sentinel (process msg)
  (message "[%s] %s %s"
	   (substring (process-name process) 1)
	   (setq msg
		 (cond
		  ((eq t msg) "Starting")
		  ((string-match "finished" msg) "Done")
		  ((string-match "exited ab" msg) (concat "Exit " (substring msg 28 -1)))
		  (t (concat (upcase (substring msg 0 1)) (substring msg 1 -1)))))
	   (nth 2 (process-command process)))
  (let ((status (process-status process)))
    (if (not (memq status '(run stop open)))
	(remove-stable-sentinel (process-name process) 'background-sentinel))
    (let ((buffer (process-buffer process)))
      (if (buffer-name buffer)
	  (save-excursion
	    (set-buffer buffer)
	    (and (memq status '(exit signal closed))
		 (not background-silentp)
		 (save-excursion
		   (goto-char (point-max))
		   (insert "*** " msg " at " (substring (current-time-string) 11 19) ?\n))
		 (set-buffer-modified-p nil)
		 (undo-boundary))
	    (if background-callback
		(funcall background-callback background-closure process)))))))

(defun get-next-background-buffer ()
  (let* ((job-number 1)
	 (job-name (let ((name "%1"))
		     (while (get-process name)
		       (setq job-number (1+ job-number))
		       (setq name (concat "%" job-number)))
		     name))
	 (buffer (get-buffer-create job-name)))
    buffer))



;; {Missing From Shell-Mode}
;;

(defun kill-shell-subjob ()
  "Send continue signal to this shell's current subjob."
  (interactive)
  (kill-process nil t))

(defun continue-shell-subjob ()
  "Send continue signal to this shell's current subjob."
  (interactive)
  (continue-process nil t))

(defun stop-shell-subjob ()
  "Send stop signal to this shell's current subjob."
  (interactive)
  (stop-process nil t))

(provide 'background)
