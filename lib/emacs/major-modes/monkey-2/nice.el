;;; nice.el - 	cleaned up versions of various interfaces in emacs.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001, 2002 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;


;;; {match-beginning/end convenience functions}
;;;

(defun match-substring (s n) 
  (and (match-beginning n)
       (substring s (match-beginning n) (match-end n))))


;;; {canonical symbolic event types: the comment}
;;;
;;; There are a zillion ways to represent a keysequence in emacs,
;;; none of which matches the print format of keysequences used
;;; by where-is.  Sucky.
;;;
;;; The following functions implement a new canonicalizingnaming system
;;; for event types.
;;;
;;; In this system, every event type corresponds to a symbol and every
;;; symbol is a valid event name.  The syntax of an event symbol name 
;;; indicates whether there are any modifiers to the event.
;;;
;;; If the name of a symbol looks like the name of a character,
;;; the event type is a character:
;;;
;;;		'A  	-- aka "A", ?A, 65
;;;		'C-x	-- aka "\C-x", ?\C-x, 24
;;;		'C-M-x	-- aka "\C-x", ?\C-x, -134217704
;;;
;;; Modifiers may occur in any order, but there may be no more
;;; than three.  The system always generates event names modifiers in the
;;; canonical (and alphabetical) order C-M-S-
;;;
;;; Some event types have only one representation:
;;;
;;;		'f1	-- known only as 'f1
;;;
;;; Others, with modifiers, have more than one:
;;;
;;;		'M-C-f1   -- aka 'C-M-f1
;;;
;;; Every symbol is a valid event name.
;;;
;;; Aside from reordering modifiers, there is only one way to write a 
;;; keysequence in this new syntax: as a vector of event symbols...
;;;
;;;		[C-x C-f]
;;;		[C-x home]
;;;		[insert]
;;;
;;; This syntax is easy to read and looks nice in help messages. 
;;;
;;; symbol-to-key translates from this new syntax into event 
;;; representations that emacs likes (integers for simple characters
;;; and so on).
;;;
;;; key-to-symbol translates from (a substantial subset of) the 
;;; emacs event type representations into the canonical symbol
;;; understood by symbol-to-key.
;;;
;;; These functions are inverses, modulo canonicalization.
;;;
;;; key-sequence-to-symbol-sequence and symbol-sequence-to-key-sequence
;;; are trivial generalizations.
;;;
;;; Obscure modifiers (hype, super) are not handled.
;;;


;;; {canonical symbolic event types: the functions}
;;;
;;;


;; Translate a uniform key symbol into a representation
;; the emacs innards likes.
;;
(defun symbol-to-key (sym)
  (let* ((nam (symbol-name sym)))
    (cond
     ((= 1 (length nam))						(aref nam 0))
     
     ((string-match
       "^\\(\\([MCSmcs]\\)-\\)\\(\\([MCSmcs]\\)-\\)?\\(\\([MCSmcs]\\)-\\)?\\(.*\\)$"
       nam)
      
      (let* ((p0 (match-substring nam 2))
	     (p1 (match-substring nam 4))
	     (p2 (match-substring nam 6))
	     (c-raw (intern (match-substring nam 7)))
	     (c (symbol-to-key c-raw))
	     (mods (sort (apply 'append `(,(and p0 (list p0))
					  ,(and p1 (list p1))
					  ,(and p2 (list p2))))
			 'string<)))

	(cond
	 ((symbolp c)		    	(intern (mapconcat '(lambda (x) x)
							   (append mods (list (symbol-name c)))
							   "-")))

	 ((null mods)			c)

	 (t				(read (concat "?\\"
						      (mapconcat '(lambda (x) x) mods "-\\")
						      "-"
						      (symbol-name c-raw)))))))

     (t					sym))))



;; Translate an event type that emacs likes, either a symbol
;; or an integer, into an event symbol.
;;
(defun key-to-symbol (key)
  (cond

   ((symbolp key)			(let ((k2 (symbol-to-key key)))
					  (if (eq k2 key)
					      k2
					    (key-to-symbol k2))))

   (t
    (let ((verbose-modifers 		(syntactic-event-modifiers key))
	  (e 				(syntactic-event-basic-type key)))

      (if (and (integerp e)
	       (memq 'shift verbose-modifers)
	       (not (= (upcase e) e)))
	  (setq e (upcase e)
		verbose-modifers (delq 'shift verbose-modifers)))
      
      (if (integerp e)
	  (setq e (char-to-string e))
	(setq e (symbol-name e)))

      (intern (concat (if (memq 'control verbose-modifers) "C-" "")
		      (if (memq 'meta verbose-modifers) "M-" "")
		      (if (memq 'shift verbose-modifers) "S-" "")
		      e))))))


(defun symbol-sequence-to-key-sequence (seq0)
  (let ((l (length seq0))
	(seq1 (copy-sequence seq0))
	(x 0))
    (while (< x l)
      (let* ((sym (aref seq1 x))
	     (key (symbol-to-key sym)))
	(aset seq1 x key)
	(setq x (1+ x))))
    seq1))

(defun key-sequence-to-symbol-sequence (seq0)
  (let ((l (length seq0))
	(seq1 (copy-sequence seq0))
	(x 0))
    (while (< x l)
      (let* ((sym (aref seq1 x))
	     (key (key-to-symbol sym)))
	(aset seq1 x key)
	(setq x (1+ x))))
    seq1))

(defun syntactic-event-basic-type (e)
  (if (integerp e)
      (event-basic-type e)
    (let ((n (symbol-name e)))
      (string-match "\\([MCS]-\\)*\\(.+\\)" n)
      (match-substring n 2))))


(defun syntactic-event-modifiers (e)
  (if (integerp e)
      (event-modifiers e)
    (let ((n (symbol-name e)))
      (string-match "\\([MCSmcs]-\\)*\\(.+\\)" n)
      (let ((raw-mods (match-substring n 1)))
	(append (if (string-match "[Cc]" raw-mods) '(control) ())
		(if (string-match "[Mm]" raw-mods) '(meta) ())
		(if (string-match "[Ss]" raw-mods) '(shift) ()))))))



;;; {canonical symbolic event types in customization files}
;;;
;;; Forget global-set-key and define-key in emacs lisp source code.
;;; global-set-key and local-set-key are fine for interactive
;;; use, but for customization files, use customize-key which uses
;;; the aforementioned sequence syntax.
;;;

(defun customize-key (&rest args)
  (cond 
   ((= (length args) 2)		(setq args (cons global-map args)))
   ((= (length args) 3)		())
   (t				(error "wrong number of parameters to customize-key")))

  (let ((map (car args))
	(seq0 (car (cdr args)))
	(command (car (cdr (cdr args)))))
    (if (not (vectorp seq0))
	;; Deliberately novice-aimed error message.
	;;
	(error "keysequences must be written as arrays [key1 key2 ...]"))

    (define-key map (symbol-sequence-to-key-sequence seq0) command)))

  
;;; {canonical symbolic event types in help messages}
;;;

(defun symbolic-key-sequence-description (seq)
  (prin1-to-string (key-sequence-to-symbol-sequence seq)))

(defun binding-description (cmd where-it-is)
  (if (null where-it-is)
      (concat "M-x " (symbol-name cmd))

    (let* ((possibilities (mapcar 'symbolic-key-sequence-description where-it-is))
	   (seitilibissop (reverse possibilities))
	   (but1-possibilities (reverse (cdr seitilibissop))))
      (cond
       ((null but1-possibilities)		(concat (symbol-name cmd)
							" (type: "
							(car possibilities)
							")"))

       ((null (cdr but1-possibilities))	(concat (symbol-name cmd)
						" (type: "
						(car but1-possibilities)
						" or "
						(car seitilibissop)
						")"))
       (t 					(concat (symbol-name cmd)
							" (type: "
							(mapconcat '(lambda (x) x)
								   but1-possibilities
								   " or ")
							(car seitilibissop)
							")"))))))

;;; {stable process sentinels and filters}
;;;

(establish-local-variables 'stable-process-hooks
			   '(;; ((name sentinel) ...)
			     ;; 
			     (local-stable-process-sentinels ())

			     ;; ((name filter) ...)
			     ;; 
			     (local-stable-process-filters ())))

(setq stable-process-sentinels ())
(setq stable-process-filters ())

(defun process-sentinel-list (l proc string)
  (mapcar '(lambda (w) 
	     (if (equal (car w) (process-name proc))
		 (funcall (car (cdr w)) proc string)))
	  l))

(defun sentinel-fanout (proc string)
  (process-sentinel-list stable-process-sentinels proc string)
  (mapcar '(lambda (b)
	     (process-sentinel-list local-stable-process-sentinels proc string))
	  (buffer-list))
  )

(defun process-filter-list (l proc string)
  (mapcar '(lambda (w) 
	     (if (equal (car w) (process-name proc))
		 (funcall (car (cdr w)) proc string)))
	  l))

(defun filter-fanout (string)
  (let ((proc (get-buffer-process (current-buffer))))
    (process-filter-list stable-process-filters proc string)
    (save-excursion
      (mapcar '(lambda (b)
		 (set-buffer b)
		 (process-filter-list local-stable-process-filters proc string))
	      (buffer-list)))))

(defun add-stable-sentinel (name hook)
  (let ((a (assoc-2 name hook stable-process-sentinels))
	(p (get-process name)))
    (setq stable-process-sentinels
	  (cons (list name hook) (delq a stable-process-sentinels)))
    (if (and p (not (process-sentinel p)))
	(set-process-sentinel p 'sentinel-fanout))
    (if (and p (eq 'sentinel-fanout (process-sentinel p)))
	(funcall hook p t))))

(defun remove-stable-sentinel (name hook)
  (let ((a (assoc-2 name hook stable-process-sentinels))
	(p (get-process name)))
    (setq stable-process-sentinels (delq a stable-process-sentinels))
    (if (and a p (eq 'sentinel-fanout (process-sentinel p)))
	(funcall hook p t))))

(defun add-stable-filter (name hook)
  (let ((a (assoc-2 name hook stable-process-filters))
	(p (get-process name)))
    (setq stable-process-filters
	  (cons (list name hook) (delq a stable-process-filters)))
    (if (and p (process-buffer p) (buffer-name (process-buffer p)))
	(save-excursion
	  (set-buffer (process-buffer p))
	  (if (not (memq 'filter-fanout comint-output-filter-functions))
	      (setq comint-output-filter-functions 
		    (cons 'filter-fanout comint-output-filter-functions)))
	  (if (memq 'filter-fanout comint-output-filter-functions)
	      (funcall hook p t))))))

(defun remove-stable-filter (name hook)
  (let ((a (assoc-2 name hook stable-process-filters))
	(p (get-process name)))
    (setq stable-process-filters (delq a stable-process-filters))
    (if (and a (and p (process-buffer p) (buffer-name (process-buffer p))))
	(save-excursion
	  (set-buffer (process-buffer p))
	  (if (memq 'filter-fanout comint-output-filter-functions)
	      (funcall hook p ()))))))


(defun add-local-stable-sentinel (name hook)
  (let ((a (assoc-2 name hook local-stable-process-sentinels))
	(p (get-process name)))
    (setq local-stable-process-sentinels
	  (cons (list name hook) (delq a local-stable-process-sentinels)))
    (if (and p (not (process-sentinel p)))
	(set-process-sentinel p 'sentinel-fanout))
    (if (and p (eq 'sentinel-fanout (process-sentinel p)))
	(funcall hook p t))))

(defun remove-local-stable-sentinel (name hook)
  (let ((a (assoc-2 name hook local-stable-process-sentinels))
	(p (get-process name)))
    (setq local-stable-process-sentinels (delq a local-stable-process-sentinels))
    (if (and a p (eq 'sentinel-fanout (process-sentinel p)))
	(funcall hook p t))))

(defun add-local-stable-filter (name hook)
  (let ((a (assoc name local-stable-process-filters))
	(p (get-process name)))
    (setq local-stable-process-filters
	  (cons (list name hook) (delq a local-stable-process-filters)))
    (if (and p (process-buffer p) (buffer-name (process-buffer p)))
	(save-excursion
	  (let ((orig-buffer (current-buffer)))
	    (set-buffer (process-buffer p))
	    (if (not (memq 'filter-fanout comint-output-filter-functions))
		(setq comint-output-filter-functions
		      (cons 'filter-fanout comint-output-filter-functions)))
	    (if (memq 'filter-fanout comint-output-filter-functions)
		(progn
		  (set-buffer orig-buffer)
		  (funcall hook p t))))))))

(defun remove-local-stable-filter (name hook)
  (let ((a (assoc name hook local-stable-process-filters))
	(p (get-process name)))
    (setq local-stable-process-filters (delq a local-stable-process-filters))
    (if (and a (and p (process-buffer p) (buffer-name (process-buffer p))))
	(save-excursion
	  (let ((orig-buffer (current-buffer)))
	    (set-buffer (process-buffer p))
	    (if (memq 'filter-fanout comint-output-filter-functions)
		(progn
		  (set-buffer orig-buffer)
		  (funcall hook p ()))))))))


(defun assoc-2 (k k2 l)
  (catch 'it
    (while l
      (if (and (equal k (car (car l)))
	       (equal k2 (car (cdr (car l)))))
	  (throw 'it (car l))
	(setq l (cdr l))))))

;; The stable sentinel check installs stable sentinels.
;; Only processes started with background are supported.
;;
(defun stable-hook-check ()
  (check-stable-sentinel-list stable-process-sentinels)
  (check-stable-filter-list stable-process-filters)
  (save-excursion
    (mapcar '(lambda (b)
	       (set-buffer b)
	       (check-stable-sentinel-list local-stable-process-sentinels)
	       (check-stable-filter-list local-stable-process-filters))
	    (buffer-list))))

(defun check-stable-sentinel-list (sents)
  ;; Scan for any new processes with names that interest us.
  ;;
  (mapcar '(lambda (p)
	     (mapcar '(lambda (w)
			(if (string= (car w) (process-name p))
			    (progn
			      (and (not (process-sentinel p))
				   (set-process-sentinel p 'sentinel-fanout))
			      (if (eq 'sentinel-fanout (process-sentinel p))
				  (funcall (car (cdr w)) p t)))))
		     sents))
	  (process-list)))

(defun check-stable-filter-list (filts)
  ;; Scan for any new processes with names that interest us.
  ;;
  (save-excursion
    (mapcar '(lambda (p)
	       (if (and (assoc (process-name p) filts)
			(process-buffer p)
			(buffer-name (process-buffer p)))
		   (let ((orig-buffer (current-buffer)))
		     (set-buffer (process-buffer p))
		     (if (not (memq 'filter-fanout comint-output-filter-functions))
			 (progn
			   (setq comint-output-filter-functions 
				 (cons 'filter-fanout comint-output-filter-functions))
			   (set-buffer orig-buffer)
			   (process-filter-list filts p t))))))
	    (process-list))))




(defun centered-insert-file (f)
  (insert "\n")
  (let* ((p (point))
	 (amt (car (cdr (insert-file-contents f)))))
    (center-region p (+ p amt))))


(defun current-date-string ()
  (let ((ts (current-time-string)))
    (concat (substring  ts 0 10) ", " (substring ts -4))))



(provide 'nice)

