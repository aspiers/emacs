;;; monkey.el - a mode for tree exploration and editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001, 2002 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;


;;; Monkey mode is for exploring and editting trees, such as 
;;; file system directory trees.
;;; 



(require 'modal)
(require 'fields)


;;; {local variables}
;;; 

(establish-local-variables
 'monkey-local-variables
 '(
   ;; Every line in a monkey buffer describes one node of 
   ;; the tree. 
   ;;
   ;; These regexps control the syntax of those lines.
   ;;
   ;; Each line is a sequence of fields, each field is 
   ;; characterized by a regexp.
   ;;
   ;; The regexps can be redefined, but care should be taken
   ;; not to ambiguate the parsing of lines.
   ;;

   ;; Stuff to ignore at the beginning of a line.
   ;;
   (monkey-fill-prefix-regexp "")

   ;; The mark field indicates whether and how the field is 
   ;; selected.
   ;; 
   (monkey-mark-field-regexp "[ +&]")
   ;;
   ;; A mark field filled in this way indicates a marked node:
   ;;
   (monkey-marked-node-regexp "\\+")
   ;;
   ;; When monkey itself marks a node, it uses this function to
   ;; map the mark field before marking, to the mark field after.
   ;;
   (monkey-mark-field-xformer (lambda (before) "+"))
   ;;
   ;; When monkey itself unmarks a node, it uses this function to
   ;; map the mark field before unmarking, to the mark field after.
   ;;
   (monkey-unmark-field-xformer (lambda (before) " "))

   ;; The type field contains an arbitrary classification
   ;; of the node.
   ;;
   (monkey-type-field-regexp ".")

   ;; The hide field contains an & if the file is a hidden file.
   ;;
   (monkey-hide-field-regexp "&?")

   ;; The expanded field indicates whether the 
   ;; of the line can be or has been expanded.
   ;;
   (monkey-expand-field-regexp ".")
   ;;
   ;; If the expanded field matches this pattern, then
   ;; monkey considers the node to be expanded.
   ;;
   (monkey-expanded-field-regexp ">")
	
   ;; The indentation is arbitrary filler.
   ;;
   (monkey-indentation-field-regexp " +")
	
   ;; Each line holds a token that uniquely (within the 
   ;; buffer) identifies the node.
   ;;
   (monkey-token-field-regexp "[^\n\r]*")

   ;; Hidden files can be made temporarily visible.
   ;;
   (monkey-hidden-files-visible		nil)
	

   ;; Suppose that node A is a child of node B.
   ;; Given the token for node A, it must be possible to 
   ;; compute the token for node B.  This hook makes
   ;; that computation:
   ;;
   (monkey-token-to-heading-function monkey-file-name-to-heading)

   ;; Suppose that node A is an internal node with children
   ;; nodes displayed in the same buffer.
   ;;
   ;; Given the token for node A, it must be possible to 
   ;; compute a regular expression that will match the token
   ;; name of all children nodes of A displayed in the same buffer.
   ;; This hook makes that computation:
   ;;
   (monkey-subdir-regexp-function monkey-file-name-subdir-regexp)

   ;; Monkey is designed to be capable of operating as a "modal-region"
   ;; major mode.  That means it will confine its activities to one
   ;; region of a buffer, only installing its keymap and local variables
   ;; when the point is in that region.
   ;;
   ;; When operating modally, this variable holds an overlay, created
   ;; for the private use of monkey, whose extents define the monkey's
   ;; region of operation.
   ;;
   ;; There may be more than one of these overlays in a buffer
   ;; so long as they do not overlap. This variable points to the
   ;; particular overlay containing the cursor, if there is one.
   ;;
   (monkey-restriction			())


   (modified-monkey-hooks		())

   ;; Computed locals:
   ;;
   ;; These are computed in a fixed way from the preceding parameters.
   ;;

   (monkey-line-description (eval 
			     (list (list
				    ;; This field must come first.
				    ;;
				    'optional-hide-field monkey-hide-field-regexp)

				   ;; Other fields may be rearranged freely.
				   ;;
				   (list 'type-field monkey-type-field-regexp)
				   (list 'expand-field monkey-expand-field-regexp)
				   (list 'mark-field monkey-mark-field-regexp)
				   (list 'fill-prefix monkey-fill-prefix-regexp)
				   (list 'indentation-field monkey-indentation-field-regexp)
				   (list 'token-field monkey-token-field-regexp))))


   (monkey-pre-token-regexp (eval
			     (let* ((r (reverse monkey-line-description))
				    (f (memq (assq 'token-field r) r))
				    (fields (cdr f))
				    (regexps (mapcar '(lambda (x) (car (cdr x))) 
						     fields)))
			       (apply 'concat "^" (reverse regexps)))))

   (monkey-token-regexp (eval (concat monkey-pre-token-regexp
				      "\\("
				      monkey-token-field-regexp
				      "\\)")))

   (monkey-token-regexp-index (eval
			       (let ((s monkey-pre-token-regexp)
				     (n 1))
				 (while (string-match "^\\(\\|.*[^\\\\]\\)\\\\(" s)
				   (setq s (substring s (match-end 0)))
				   (setq n (1+ n)))
				 n)))

   ))


;;; {monkey buffer format munging: the ends of the world for monkey}
;;;
;;;

(defun monkey-point-min ()
  (if monkey-restriction
      (1+ (overlay-start monkey-restriction))
    (point-min)))

(defun monkey-point-max ()
  (if monkey-restriction
      (1- (overlay-end monkey-restriction))
    (point-max)))

(defun monkey-bobp () (eq (point) (monkey-point-min)))

(defun monkey-eobp () (eq (point) (monkey-point-max)))

(defun make-monkey-overlay (beg end)
  (make-modal-overlay beg end 'monkey-region))

(defun monkey-modal-overlay (point)
  (modal-overlay-at point 'monkey-region))



(setq modify-monkey-buffer-active nil)

(defmacro modify-monkey-buffer (&rest code)
  `(progn (let* ((modify-monkey-buffer-was-active modify-monkey-buffer-active)
		 (modify-monkey-buffer-active t)
		 (buffer-read-only nil))
	    (progn ,@code)
	    (if (and (not modify-monkey-buffer-was-active)
		     modified-monkey-hooks)
		(mapcar 'funcall modified-monkey-hooks)))))


;;; {monkey buffer format munging: basic insert and remove}
;;; 

;; Insert a line for NAME in the current buffer before
;; the current line (or at the end of the buffer).
;;
(defun monkey-insert-node (fill-prefix mark type expand indentation name)
  (modify-monkey-buffer
   (beginning-of-line)
   (cond
    ((not (or (= (point) (point-max))
	      (eq (char-after (point)) ?\n)))
     (end-of-line)
     (insert-and-inherit ?\n)))
   (insert-fields-and-inherit monkey-line-description
			      (list (list 'fill-prefix fill-prefix)
				    (list 'mark-field mark)
				    (list 'type-field type)
				    (list 'expand-field expand)
				    (list 'indentation-field indentation)
				    (list 'token-field name)))))


;; Move to the correct place, and insert NAME.
;;
(defun monkey-sorted-insert-node (lessp fill-prefix mark stat expand indentation name)
  (modify-monkey-buffer
   (save-excursion
     (goto-char (catch 'FOUND-POS
		  (progn
		    (monkey-for-each-nodes
		     '(lambda ()
			(if (funcall lessp (monkey-token) name)
			    nil
			  (beginning-of-line)
			  (save-excursion (insert-and-inherit ?\n))
			  (throw 'FOUND-POS (point)))))
		    ;; This is reached if file-name should
		    ;; be added at the bottom.
		    (goto-char (point-max))
		    (insert-and-inherit "\n")
		    (throw 'FOUND-POS (point)))))
     (monkey-insert-node fill-prefix mark stat expand indentation name))))

;; Remove the current line from a monkey buffer.
;;
(defun monkey-delete-node ()
  (modify-monkey-buffer
   (if (monkey-expanded-p)
       (monkey-unexpand-subdirectory))
   (delete-region (monkey-bol-point) (monkey-eol-point))
   (or (monkey-bobp) (backward-delete-char 1))
   (and (monkey-bobp) (delete-char 1))))


;;; {monkey line details: the mark field}
;;;

(defun monkey-mark-field ()
  (save-excursion
    (beginning-of-line)
    (get-field monkey-line-description 'mark-field)))

(defun monkey-set-mark-field (s)
  (modify-monkey-buffer
   (save-excursion
     (beginning-of-line)
     (set-field monkey-line-description 'mark-field s))))

(defun monkey-mark ()
  (monkey-token t)			; make sure there is a token
  (monkey-set-mark-field (funcall monkey-mark-field-xformer (monkey-mark-field))))

(defun monkey-unmark ()
  (monkey-token t)			; make sure there is a token
  (monkey-set-mark-field (funcall monkey-unmark-field-xformer (monkey-mark-field))))

(defun monkey-marked-p ()
  (string-match monkey-marked-node-regexp (monkey-mark-field)))


;;; {monkey line details: the hidden field}
;;;
(defun monkey-hidden-field ()
  (save-excursion
    (beginning-of-line)
    (get-field monkey-line-description 'optional-hide-field)))

;;; {monkey line details: the type field}
;;;

(defun monkey-type-field ()
  (save-excursion
    (beginning-of-line)
    (get-field monkey-line-description 'type-field)))

(defun monkey-set-type-field (s)
  (modify-monkey-buffer
   (save-excursion
     (beginning-of-line)
     (set-field monkey-line-description 'type-field s))))


;;; {monkey line details: the expand field}
;;;
(defun monkey-expand-field ()
  (save-excursion
    (beginning-of-line)
    (get-field monkey-line-description 'expand-field)))

(defun monkey-set-expand-field (s)
  (modify-monkey-buffer
   (save-excursion
     (beginning-of-line)
     (set-field monkey-line-description 'expand-field s))))

(defun monkey-expanded-p ()
  (string-match monkey-expanded-field-regexp (monkey-expand-field)))


;;; {monkey line details: the token field}
;;;

(defun monkey-token-field ()
  (save-excursion
    (beginning-of-line)
    (get-field monkey-line-description 'token-field)))

(defun monkey-set-token-field (s)
  (modify-monkey-buffer
   (save-excursion
     (beginning-of-line)
     (set-field monkey-line-description 'token-field s))))

;; Return the point position of the first char of the token on the 
;; current line.  If the optional parameter IMPORTANT is
;; non-nil, then signal an error if there is no token on this line.
;; Otherwise, returns nil if no token is found.
;;
(defun monkey-token-beginning (&optional important)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-token-regexp) (match-beginning monkey-token-regexp-index))
	  (important (error "There is no token on this line."))
	  (t nil))))

;; Return the point position of the end of the token on the 
;; current monkeybuffer line.  If the optional parameter IMPORTANT is
;; non-nil, then signal an error if there is no token on this line.
;; Otherwise, returns nil if no token is found.
;;
(defun monkey-token-end (&optional important)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-token-regexp) (match-end monkey-token-regexp-index))
	  (important (error "There is no token on this line."))
	  (t nil))))

;; Return the token on the current line.  If the optional parameter
;; IMPORTANT is nil, then signal an error. Otherwise, return nil if no
;; token is found.
;;
(defun monkey-token (&optional important)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-token-regexp)
	   (buffer-substring (match-beginning monkey-token-regexp-index)
			     (match-end monkey-token-regexp-index)))
	  (important (error "There is no token on this line."))
	  (t nil))))


;;; {monkey line details: parsing tokens}
;;;

;; Given a token, return the token for its parent directory.
;;
;; e.g. (using file-name syntax)
;;
;;    'a/b => a
;;    'a/b/c => a/b
;;    'a => nil
;;
(defun monkey-token-to-heading (f)
  (funcall monkey-token-to-heading-function f))

;; The default value for monkey-token-to-heading-function:
;;
(defun monkey-file-name-to-heading (f) 
  (let ((f (file-name-directory f)))
    (and f
	 (if (string= "/" (substring f -1))
	     (substring f 0 -1)))))

;; Given a token, return a regexp that matches
;; files in that directory.
;;
;; e.g. (using file-name syntax)
;;
;;    'a => "a/.*"
;;
(defun monkey-subdir-regexp (f) (funcall monkey-subdir-regexp-function name))

;; The default value for monkey-subdir-regexp-function:
;;
(defun monkey-file-name-subdir-regexp (f)
  (concat (regexp-quote (concat f "/")) ".*"))


;;; {removing expanded directories}
;;;
;;; Removing expanded subdirectories can be done generically with
;;; the help of a few buffer-specific regexps.  There is nothing special
;;; about inserting expanded subdirectories: just insert them one token
;;; at a time using monkey-sorted-insert-node (or monkey-insert-node
;;; for better performance if you can deal with sorting tokens yourself).
;;; 
;;;

;; Unexpand the contents of a subdirectory.
;;
(defun monkey-unexpand-subdirectory ()
  (modify-monkey-buffer
   (let* ((name (monkey-token t))
	  (subdir-regexp (monkey-subdir-regexp name)))
     (monkey-set-expand-field ?^)
     (save-excursion
       (monkey-for-each-matches
	'(lambda ()
	   (monkey-unhide-line t))
	subdir-regexp))
     (monkey-delete-matching-lines subdir-regexp)
     (goto-char (monkey-point-max))
     (backward-char 1)
     (if (looking-at "\n")
	 (delete-char 1)))))

;; Delete all the lines that match regexp.  This effects 
;; hidden as well as visible lines.
;;
(defun monkey-delete-matching-lines (regexp)
  (save-excursion
    (goto-char (monkey-point-min))
    (monkey-replace-regexp
     (concat monkey-pre-token-regexp regexp "\\([/\n\r]\\|$\\)")
     "")
    (goto-char (monkey-point-min))
    (monkey-replace-regexp
     (concat "\r" monkey-pre-token-regexp regexp "\\([/\n\r]\\|$\\)")
     "")
    (goto-char (monkey-point-min))
    (monkey-replace-regexp "\n\n" "\n")
    (goto-char (monkey-point-min))))


;;; {marked lines}
;;;

;; Change the mark status of the current line.
;;
(defun monkey-toggle ()
  (if (monkey-marked-p)
      (monkey-unmark)
    (monkey-mark)))

;; Provide the apply-and-move semantics of the -this functions.
;; FUNCTION is the function to apply to each line, COUNT is a repitition count
;; (may be nil) and the optional MOVE-FIRST, if non-nil, means move before 
;; applying the function.
;;
(defun monkey-action-and-move (function &optional count move-first)
  (let ((direction (monkey-signum (or count 1)))
	(count (monkey-abs (or count 1))))
    (while (> count 0)
      (if move-first
	  (forward-line direction))
      (apply function ())
      (if (not move-first)
	  (forward-line direction))
      (setq count (1- count)))
    (nice-monkey)))

(defun monkey-mark-this (&optional count)
  "Mark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-mark count))

(defun monkey-unmark-this (&optional count)
  "Unmark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-unmark count))

(defun monkey-toggle-this (&optional count)
  "Unmark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-toggle count))

(defun monkey-unmark-this-back (&optional count)
  "Unmark this line and move backwards one line."
  (interactive "p")
  (monkey-action-and-move 'monkey-unmark (- (or count 1)) t))

(defun monkey-mark-all ()
  "Mark all tokens. With a prefix arg, unmarks all tokens."
  (interactive)
  (monkey-for-each-nodes
   (if current-prefix-arg
       'monkey-unmark
     'monkey-mark)))

(defun monkey-mark-region (start end)
  "Mark all tokens in the region. With a prefix arg, unmarks all tokens."
  (interactive "r")
  (monkey-for-each-nodes-region
   start end
   (if current-prefix-arg
       'monkey-unmark
     'monkey-mark)))

(defun monkey-unmark-all ()
  "Unmark all tokens. With a prefix arg, marks all tokens."
  (interactive)
  (monkey-for-each-nodes
   (if current-prefix-arg
       'monkey-mark
     'monkey-unmark)))

(defun monkey-toggle-all ()
  "Exchange the set of marked tokens with the set of unmarked tokens."
  (interactive)
  (monkey-for-each-nodes 'monkey-toggle))

(defvar monkey-last-mark-regexp nil
  "The last regexp used to mark tokens in a monkey buffer.")

(make-variable-buffer-local 'monkey-last-mark-regexp)

(defun monkey-mark-by-regexp (regexp)
  "Mark all tokens matching REGEXP.  Unmarks with a prefix arg."
  (interactive "sRegexp: ")
  (save-excursion
    (monkey-for-each-matches
     (if current-prefix-arg
	 'monkey-unmark
       'monkey-mark)
     regexp)
    (setq monkey-last-mark-regexp regexp)))

(defun monkey-unmark-by-regexp (regexp)
  "Unmark all tokens matching REGEXP.  Marks with a prefix arg."
  (interactive "sRegexp: ")
  (save-excursion
    (monkey-for-each-matches
     (if current-prefix-arg
	 'monkey-mark
       'monkey-unmark)
     regexp)
    (setq monkey-last-mark-regexp regexp)))

(defun monkey-toggle-marked-by-regexp (regexp)
  "Exchange the set of marked tokens that match REGEXP with the set of 
unmarked tokens that match."
  (interactive "sRegexp: ")
  (monkey-for-each-matches 'monkey-toggle regexp))

(defun monkey-mark-by-type (type)
  "Mark all the tokens of type TYPE."
  (interactive "sType: ")
  (monkey-for-each-type
   (if current-prefix-arg
       'monkey-unmark
     'monkey-mark)
   type))


;;; {mapping functions}

;; Apply FUNCTION to each token in the buffer.
;;
(defun monkey-for-each-nodes (fun)
  (save-excursion
    (goto-char (monkey-point-min))
    (while (not (monkey-eobp))
      (let ((tk (monkey-token)))
	(and tk
	     (progn (apply fun ())
		    (if (equal tk (monkey-token))
			(forward-line))))))))

(defun monkey-map-nodes (fun)
  (let ((answer ()))
    (save-excursion
      (goto-char (monkey-point-min))
      (while (not (monkey-eobp))
	(let ((tk (monkey-token)))
	  (and tk
	       (let ((elt (apply fun ())))
		 (if (equal tk (monkey-token))
		     (forward-line))
		 (setq answer (append answer (list elt))))))))
    answer))

;; In the region START END, apply FUNCTION to each token.
;;
(defun monkey-for-each-nodes-region (start end fun)
  (save-excursion
    (goto-char start)
    (while (not (> (point) end))
      (and (monkey-token) (apply fun ()))
      (forward-line))))

(defun monkey-for-each-marked (function &optional dont-unmark call-on-any-line)
  (let ((were-any nil))
    (monkey-for-each-nodes
     '(lambda ()
	(and (monkey-marked-p)
	     (progn (setq were-any t)
		    (or dont-unmark (monkey-unmark))
		    (apply function ())))))
    (or were-any
	(and (not call-on-any-line)
	     (not (monkey-token t)))
	(apply function ()))))

(defun monkey-map-marked (function &optional dont-unmark call-on-any-line not-implicitly)
  (let ((answer ()))
    (monkey-for-each-nodes
     '(lambda ()
	(and (monkey-marked-p)
	     (progn (setq were-any t)
		    (or dont-unmark (monkey-unmark))
		    (setq answer (append answer (list (apply function ()))))))))
    (if not-implicitly
	answer
      (or answer
	  (and (not call-on-any-line)
	       (not (monkey-token t)))
	  (list (apply function ()))))))

(defun monkey-for-each-type (function typefield)
  (monkey-for-each-nodes
   '(lambda ()
      (and (string= (monkey-type-field) typefield)
	   (apply function ())))))

(defun monkey-map-type (function typefield)
  (apply 'append
	 (monkey-map-nodes
	  '(lambda () 
	     (and (string= (monkey-type-field) typefield)
		  (list (apply function ())))))))

(defun monkey-map-hidden (function field)
  (let ((were-visible monkey-hidden-files-visible))
    (unwind-protect (progn
		      (if (not were-visible)
			  (monkey-temp-unhide))
		      (apply 'append
			     (monkey-map-nodes
			      '(lambda () 
				 (and (string= (monkey-hidden-field) field)
				      (list (apply function ())))))))
      (if (not were-visible) (monkey-un-temp-unhide)))))

(defun monkey-for-each-expanded (function expfield)
  (monkey-for-each-nodes
   '(lambda ()
      (and (equal (monkey-expand-field) expfield)
	   (apply function ())))))

(defun monkey-map-expanded (function expfield)
  (apply 'append
	 (monkey-map-nodes
	  '(lambda ()
	     (and (equal (monkey-expand-field) expfield)
		  (list (apply function ())))))))
  
;; Apply FUNCTION to each token line matching REGEXP.
;; The REGEXP must match the entire token name.
;; Don't use .* in REGEXP.  Instead, use monkey-token-field-regexp.
;;
(defun monkey-for-each-matches (function regexp)
  (save-excursion
    (let ((srchfor (concat monkey-pre-token-regexp "\\(" regexp "\\)"
			   "\\([\r]\\|$\\)")))
      (goto-char (monkey-point-min))
      (while (re-search-forward srchfor (monkey-point-max) t)
	(and (not (eq (point) (point-min)))
	     (eq ?\r (char-after (- (point) 1)))
	     (forward-char -1))
	(apply function ())))))


;;; {hidden lines}
;;;

;; Hide the current line.  If the token is the name of an expanded 
;; subdir, then hide the entire subdir.
;;
(defun monkey-hide-line ()
  (modify-monkey-buffer
   (let* ((expanded (monkey-expanded-p))
	  (name	(and expanded (monkey-token))))
     (save-excursion
       (monkey-unmark)
       (beginning-of-line)
       (or (monkey-bobp)
	   (backward-delete-char 1))
       (insert-and-inherit "\r&")
       (and expanded
	    (monkey-for-each-matches 'monkey-hide-line
				     (concat (regexp-quote (concat name "/")) ".*")))))))

;; Unhide all hidden lines temporarily.
;;
(defun monkey-temp-unhide ()
  (modify-monkey-buffer
   (save-excursion
     (goto-char (monkey-point-min))
     (monkey-replace-string "\r&" "\n&"))))

;;
(defun monkey-un-temp-unhide ()
  (modify-monkey-buffer
   (save-excursion
     (goto-char (monkey-point-min))
     (monkey-replace-string "\n&" "\r&"))))

(defun monkey-bounded-replace-string (from to start end)
  (save-excursion
    (goto-char start)
    (while (search-forward from end t)
      (replace-match to t nil))))

;; Unhide tokens hidden on this line.
;;
(defun monkey-unhide-line (&optional fail-silently)
  (modify-monkey-buffer
   (unwind-protect
       (save-excursion
	 (beginning-of-line)
	 (or (search-forward "\r" (monkey-eol-point) t)
	     fail-silently
	     (error "Nothing is hidden here!"))
	 (beginning-of-line)
	 (and (search-forward "\r&" (monkey-eol-point) t)
	      (replace-match "\n" t nil))
	 (beginning-of-line)
	 (while (eq ?\n (char-after (point)))
	   (delete-char 1))))))


(defun monkey-hide-marked ()
  "Hide all marked tokens.  With a prefix, unhide near marked lines."
  (interactive)
  (monkey-for-each-marked
   (if current-prefix-arg
       'monkey-unhide-line
     'monkey-hide-line)
   nil t))

(defun monkey-hide-this ()
  "Hide this token.  With a prefix, unhide near this line."
  (interactive)
  (if current-prefix-arg
      (monkey-unhide-line)
    (monkey-hide-line))
  (monkey-next-line))

(defun monkey-unhide-all ()
  "Unhide all tokens."
  (interactive)
  (modify-monkey-buffer
   (save-excursion
     (unwind-protect
	 (progn (goto-char (monkey-point-min))
		(monkey-replace-string "\r&" "\n")
		(goto-char (monkey-point-min))
		(monkey-replace-string "\n\n" "\n")
		(goto-char (monkey-point-min))
		(and (= (following-char) ?\n)
		     (delete-char 1))))))
  (nice-monkey))



;;; {cursor motion}
;;;

;; Put the cursor in a nice place.
;;
(defun nice-monkey ()
  (and (save-excursion
	 (re-search-backward
	  "\r"
	  (save-excursion (beginning-of-line) (point))
	  t))
       (forward-line))
  (goto-char (min (monkey-point-max)
		  (max (monkey-point-min)
		       (or (monkey-token-beginning)
			   (point))))))

(defun nice-monkey-2 () t)

(defun monkey-next-line (&optional prefix)
  "Move to the next line of a monkey buffer."
  (interactive "p")
  (let ((i (monkey-point-min))
	(a (monkey-point-max)))
    (forward-line prefix)
    (if (< (point) i) (goto-char i))
    (if (>= (point) a) (goto-char (1- a)))
    (nice-monkey)))

(defun monkey-previous-line (&optional prefix)
  "Move to the previous line of a monkey buffer."
  (interactive "p")
  (monkey-next-line (- (or prefix 1))))

(defun monkey-beginning-of-buffer ()
  "Move to the beginning of a monkey buffer."
  (interactive)
  (push-mark)
  (goto-char (monkey-point-min))
  (nice-monkey))

(defun monkey-end-of-buffer ()
  "Move to the bottom of a monkey buffer."
  (interactive)
  (push-mark)
  (goto-char (monkey-point-max))
  (nice-monkey))

(defun monkey-scroll-up (&optional prefix)
  "Scroll up nicely in a monkey-buffer"
  (interactive "p")
  (scroll-up (and current-prefix-arg prefix))
  (nice-monkey))

(defun monkey-scroll-down (&optional prefix)
  "Scroll down nicely in a monkey-buffer"
  (interactive "p")
  (scroll-down (and current-prefix-arg prefix))
  (nice-monkey))

(defun monkey-directory-heading (count)
  "Move to the line containing the directory name for the current
subdirectory."
  (interactive "p")
  (let ((count (monkey-abs (or count 1))))
    (while (> count 0)
      (let ((target-name (monkey-token-to-heading (monkey-token))))
	(or target-name (error "You are not in a subdirectory."))
	(goto-char (monkey-point-min))
	(re-search-forward (concat monkey-pre-token-regexp (regexp-quote target-name))
			   (monkey-point-max)))
      (beginning-of-line)
      (setq count (1- count))))
  (nice-monkey))

(defun monkey-next-directory-heading (count)
  "Move to the next line at the same level as the directory for the current
subdirectory."
  (interactive "p")
  (monkey-directory-heading 1)
  (monkey-next-same-level count))

(defun monkey-previous-directory-heading (count)
  "Move to the next line at the same level as the directory for the current
subdirectory."
  (interactive "p")
  (monkey-directory-heading 1)
  (monkey-previous-same-level (1- count)))

(defun monkey-next-same-level (&optional count)
  "Move to the next line which is at the same or lower depth of nesting."
  (interactive "p")
  (let ((depth (monkey-depth))
	(count (monkey-abs (or count 1)))
	(direction (monkey-signum (or count 1))))
    (while (> count 0)
      (forward-line direction)
      (while (not (or (monkey-bobp) (monkey-eobp) (<= (monkey-depth) depth)))
	(forward-line direction))
      (setq count (1- count))))
  (nice-monkey))

(defun monkey-previous-same-level (&optional count)
  "Move to the previous line which is at the same depth or lower of nesting."
  (interactive "p")
  (monkey-next-same-level (- (or count 1))))

;; Return an integer describing the level of subdirectory nesting.
;; This integer is NOT the level of subdirectory nesting, but can be used
;; to decide which of two tokens is nested deeper.
;;
(defun monkey-depth ()
  (- (monkey-token-beginning t) (monkey-bol-point)))



;; {cutting and copying tokens}
;;
;;
(defun monkey-copy-this-token ()
  "Copy the current token into the kill ring"
  (interactive)
  (copy-region-as-kill (monkey-token-beginning) (monkey-token-end)))

(defun monkey-copy-marked-tokens (&optional flag)
  "Copy all of the marked tokens into the kill ring 
seperated by a space.  Prefix arg causes file-names to be 
sperated by a newline."
  (interactive "P")
  (save-excursion
    (let ((list-buffer (get-buffer-create "*Cut Token List*"))
	  (sep (if flag "\n" " ")))
      (save-excursion
	(set-buffer list-buffer)
	(erase-buffer))
      (monkey-for-each-marked
       '(lambda ()
	  (let ((name (monkey-token)))
	    (save-excursion
	      (set-buffer list-buffer)
	      (goto-char (monkey-point-max))
	      (insert name sep)))) t)
      (set-buffer list-buffer)
      (copy-region-as-kill (monkey-point-min) (monkey-point-max)))))

(defun monkey-shove ()
  "Put the names of the marked tokens into a scratch buffer."
  (interactive)
  (let ((list-buffer (get-buffer-create "*Token List*")))
    (save-excursion
      (set-buffer list-buffer)
      (erase-buffer))
    (monkey-for-each-marked
     '(lambda ()
	(let ((name (monkey-token)))
	  (save-excursion
	    (set-buffer list-buffer)
	    (goto-char (monkey-point-max))
	    (insert name "\n")))) t)
    (switch-to-buffer-other-window list-buffer)))

(defun monkey-shell-command ()
  "Prompt for a shell command using the marked tokens as
a default command string."
  (interactive)
  (monkey-copy-marked-tokens)
  (shell-command-on-region (point) (point) 
			   (read-string "Shell command: " 
					(car kill-ring)) nil))

(defun monkey-background (s)
  "Invoke a background process on the marked tokens."
  (interactive (progn
		 (monkey-copy-marked-tokens)
		 (list (read-from-minibuffer "% "
					     (car kill-ring)
					     nil
					     nil
					     'background-history-list))))
  (background s t))


;;; {miscellaneous utility functions}
;;;  
;;;

;; s1 and s2 are absolute paths.
;;
;; if s1 is a prefix of s2, return the suffix of s2 that follows s1
;; if s1 is /, return s2
;; else tmp = monkey-tail  file-name-directory directory-file-name s1   s2
;;      if tmp[0] == / then tmp else ../tmp
;;

(defun monkey-tail (p1 p2)
  (let ((n (length p1))
	(y (length p2)))
    (setq fuck p1)
    (setq shit p2)
    (cond
     ((and (>= y n) (string= p1 (substring p2 0 n)))		(substring p2 n y))
     ((string= "/" p1)						p2)
     (t (let ((answer (monkey-tail (file-name-directory (directory-file-name p1)) p2)))
	  (cond
	   ((string= answer p2)				answer)
	   (t						(concat "../" answer))))))))

(defun monkey-abs (x)
  (cond	((< x 0) (- x))
	(t x)))

(defun monkey-signum (x)
  (if (< x 0)
      -1
    1))

(defun monkey-replace-string (orig subst)
  (while (search-forward orig (monkey-point-max) t)
    (replace-match subst t nil)))

(defun monkey-replace-regexp (pattern subst)
  (while (re-search-forward pattern (monkey-point-max) t)
    (replace-match subst t nil)))

;; Return the point at the end of the current line.
;;
(defun monkey-eol-point (&optional count)
  (save-excursion
    (end-of-line count)
    (point)))

;; Return the point at the beginning of the current line.
;;
(defun monkey-bol-point (&optional count)
  (save-excursion
    (beginning-of-line count)
    (point)))


(defun add-monkey-mode-bindings (keymap)
  (define-key keymap "\C-c\C-m" 'monkey-mark-by-regexp)
  (define-key keymap "\C-c+" 'monkey-mark-by-regexp)
  (define-key keymap "\C-c=" 'monkey-mark-by-regexp)
  (define-key keymap "\C-cm" 'monkey-mark-by-regexp)
  (define-key keymap "\C-m" 'monkey-mark-this)
  (define-key keymap "\M-r" 'monkey-mark-region)
  (define-key keymap "\M-m" 'monkey-mark-all)
  (define-key keymap "\M-+" 'monkey-mark-all)
  (define-key keymap "\M-=" 'monkey-mark-all)
  (define-key keymap "\M-\C-m" 'monkey-mark-all)
  (define-key keymap "+" 'monkey-mark-this)
  (define-key keymap "=" 'monkey-mark-this)
  (define-key keymap "\C-c\C-u" 'monkey-unmark-by-regexp)
  (define-key keymap "\C-cu" 'monkey-unmark-by-regexp)
  (define-key keymap "\M-u" 'monkey-unmark-all)
  (define-key keymap "u" 'monkey-unmark-this)
  (define-key keymap "\C-?" 'monkey-unmark-this-back)
  (define-key keymap "t" 'monkey-toggle-this)
  (define-key keymap "\C-ct" 'monkey-toggle-marked-by-regexp)
  (define-key keymap "\C-c\C-t" 'monkey-toggle-marked-by-regexp)
  (define-key keymap "\M-t" 'monkey-toggle-all)

  (define-key keymap "\C-n" 'monkey-next-line)
  (define-key keymap " "  'monkey-next-line)
  (define-key keymap "n" 'monkey-next-line)

  (define-key keymap "\C-p" 'monkey-previous-line)
  (define-key keymap "p" 'monkey-previous-line)

  (define-key keymap "\\" 'monkey-directory-heading)

  (define-key keymap "N" 'monkey-next-same-level)
  (define-key keymap "P" 'monkey-previous-same-level)

  (define-key keymap "\M-p" 'monkey-previous-directory-heading)
  (define-key keymap "\M-n" 'monkey-next-directory-heading)

  (define-key keymap "\M-<" 'monkey-beginning-of-buffer)
  (define-key keymap "\M->" 'monkey-end-of-buffer)

  (define-key keymap "\M-v" 'monkey-scroll-down)
  (define-key keymap "\C-v" 'monkey-scroll-up)

  (define-key keymap "\C-c\C-s" 'monkey-mark-subdirectory)
  (define-key keymap "\C-cs" 'monkey-mark-subdirectory)
  (define-key keymap "\M-h" 'monkey-unhide-all)

  (define-key keymap "h" 'monkey-hide-this)
  (define-key keymap "H" 'monkey-hide-marked)
  (define-key keymap "k" 'monkey-mark-by-type)
  (define-key keymap "m" 'monkey-mark-this)
  (define-key keymap "]" 'monkey-shove)
  )

(require 'background)
(require 'monkey-dir)
(provide 'monkey)
