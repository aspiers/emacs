;;; modal.el - 	major modes peacefully coexisting in one buffer.
;;;		This file defines programming conventions that
;;;		apply to the entire library.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



;;; {Local Variables}
;;;
;;;
;;; WHY DEFVAR IS YUCKY
;;;
;;; The original reason for defvar seems to have been that a user could
;;; set a variable in ".emacs" using setq, then load a file containing 
;;; a defvar.  defvar semantics specify that the user's setq definition
;;; overrides the binding in defvar.  That interacts well enough
;;; with the autoloader: you can set up parameters when emacs 
;;; starts up, but load the definitions for those parameters later,
;;; using the autoloader. So, using "defvar" was supposed
;;; to make emacs more easily customizable.
;;;
;;; But for developers, the appearence of "defvar" in emacs lisp
;;; source code is a pain.  For example, "eval-defun" of a defvar only
;;; sets the default value of the variable the first time.  Subsequent
;;; calls to eval-defun for that defvar have no effect other than
;;; changing the documentation string.  If you are hacking the source
;;; code in which a variable is introduced, that's not what you want.
;;;
;;; DON'T USE DEFVAR
;;; 
;;; Don't use "defvar".  It is a pain for emacs lisp hackers
;;; and more importantly it doesn't help with the hacks contained
;;; within for context switching between multiple major modes in a 
;;; single buffer.
;;;
;;; Here are some conventions that work better.
;;;
;;; To introduce a new global variable, use:
;;;
;;; 	(setq new-variable initial-value)
;;;
;;; To set documentation, use:
;;;
;;;	(set-variable-documentation 'symbol "doc")
;;;
;;; Important! Only set documentation for variables that users who
;;; are not programmers may want to change.  Users who are programmers,
;;; and who are interested in more obscure variables, should read
;;; source code comments instead of doc strings.  This saves space in
;;; emacs and encourages programmers to read the source code.  Using
;;; the tags package is at least as convenient as using describe-variable.
;;;
;;; For a typical mode, defined in one source file, do the following
;;; to establish buffer-local variables.
;;;
;;; Near the top of the file, put:
;;;
;;;	(require 'modal)
;;;	(establish-local-variables 'variable-name-list
;;;				   '((variable-name  initial-value)
;;;				     ...))
;;;
;;; List all local variables there.  
;;;
;;; The initial value can be a constant expression or a form:
;;;
;;;	(eval exp)
;;;					
;;; establish-local-variables is defined below.  It makes a list of
;;; variables buffer local and establishesa default values for them
;;; according to a specification.
;;;
;;; The specification is given a name which is added to the
;;; list of variables made local.  The specification variable
;;; is bound to the specification (extended to include a 
;;; specification for the specification variable itself).
;;;
;;; The function establish-local-variables will not change the default
;;; value of variables in the list system-local-variables.
;;;
;;; The specification variable is useful with enter-scope and
;;; leave-scope, defined below.
;;;

(defun establish-local-variables (name spec)
  (setq spec (cons (list name spec) spec))
  (rplaca (cdr (car spec)) spec)
  (while spec
    (if (not (memq (car (car spec)) system-local-variables))
	(progn
	  (make-variable-buffer-local (car (car spec)))
	  (if (cdr (car spec))
	      (set-default (car (car spec))
			   (evaluate-default-value-spec (car (cdr (car spec))))))))
    (setq spec (cdr spec))))


;; The function establish-local-variables will not change the default
;; value of variables in this list.
;;
(setq system-local-variables
      '(case-fold-search
	buffer-read-only
	selective-display
	default-directory
	major-mode
	mode-name
	mode-line-buffer-identification))

(defun set-variable-documentation (s d) 
  (put s 'variable-documentation d))

;; Like establish-local-variables but don't 
;; call make-variable-buffer-local and don't change
;; the default of any variable except those specified 
;; to have an "eval" init.
;;
(defun recompute-local-variables (spec)
  (while spec
    (if (and (cdr (car spec))
	     (listp (car (cdr (car spec))))
	     (consp (car (cdr (car spec))))
	     (eq 'eval (car (car (cdr (car spec))))))
	(set (car (car spec))
	     (eval (car (cdr (car (cdr (car spec))))))))
    (setq spec (cdr spec))))


;; A default value spec for establish-local-variables is either
;; a constant expression or an expression of the form:
;;
;;	(eval exp)
;;
;; This evaluates such a value spec:
;;
(defun evaluate-default-value-spec (val)
  (if (and (listp val) (consp val) (eq 'eval (car val)))
      (eval (car (cdr val)))
    val))


;;; {Scopes}
;;;
;;; Swap values of many local variables at once.
;;; This is useful for "context switching" between major
;;; modes in a single buffer, assuming that you have handy
;;; lists of the buffer local variables used by each mode.
;;;
;;;
;;; (enter-scope saved-values-list previously-saved-values):
;;;
;;;  SAVED-VALUES-LIST: An association list mapping local variables
;;;  to value specifications.  ((var spec) ...)
;;;  Each specification is either a constant value or (eval some-exp).
;;;  This is the same format of list used by establish-local-variables.
;;;
;;;  PREVIOUSLY-SAVED-VALUES: An association list mapping local
;;;  variables to values. ((var val) ...)
;;;
;;;  Atomicly:
;;;
;;;  1. Save the values of the variables in SAVED-VALUES-LIST in a new
;;;     assoc list.
;;;  2. Install any bindings for those variables recorded in 
;;;	PREVIOUSLY-SAVED-VALUES.
;;;  3. Install any remaining bindings for those variables
;;;     from the specifications in SAVED-VALUES-LIST.
;;;  4. Return the assoc list from step 1.
;;;
;;; (leave-scope saved-values-list previously-saved-values):
;;;
;;;  Atomicly:
;;;
;;;  1. Save the values of the variables in SAVED-VALUES-LIST in a new
;;;     assoc list.
;;;  2. Install any bindings for those variables recorded in 
;;;	PREVIOUSLY-SAVED-VALUES.
;;;  3. Return the assoc list from step 1.
;;;

(defun enter-scope (saved-values-list previously-saved-values)
  (let ((inhibit-quit t))
    (let ((values-on-entry (mapcar '(lambda (s) (list s (symbol-value s)))
				   (mapcar 'car saved-values-list))))
      (mapcar '(lambda (vl)
		 (set (car vl)
		      (let ((a (assq (car vl) previously-saved-values)))
			(or (and a (car (cdr a)))
			    (condition-case nil
				(let ((inhibit-quit nil))
				  (evaluate-default-value-spec (car (cdr vl))))
			      (error '<<<error>>>)
			      (quit '<<<quit>>>))))))
	      saved-values-list)
      values-on-entry)))

(defun leave-scope (saved-values-list previously-saved-values)
  (let ((inhibit-quit t))
    (let ((values-on-exit 
	   (mapcar '(lambda (s) (list s (symbol-value s)))
		   (mapcar 'car saved-values-list))))
      (mapcar '(lambda (vl) (set (car vl) (car (cdr vl))))
	      previously-saved-values)
      values-on-exit)))


;;; {Modal Regions}
;;;
;;; A modal region is defined by a tagged overlay and has
;;; an associated list of local variables and a keymap.
;;; It is used to implement regions of a buffer which are
;;; editted in a different major mode from other regions of
;;; the same buffer.
;;;

(defun make-modal-overlay (beg end tag)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'modal-region tag)
    o))

(defun modal-overlay-at (point tag)
  (catch 'x
    (let ((overlays (overlays-at point)))
      (while overlays
	(if (eq tag (overlay-get (car overlays) 'modal-region))
	    (throw 'x (car overlays))
	  (setq overlays (cdr overlays))))
      ())))

(defun enter-modal-region (overlay saved-values-list map)
  (let* ((inhibit-quit t)
	 (previously-saved-values (overlay-get overlay 'saved-values)))

    (overlay-put overlay 'values-on-entry
		 (enter-scope saved-values-list previously-saved-values))

    (overlay-put overlay 'map-on-entry (current-local-map))
    (use-local-map (or (overlay-get overlay 'saved-map) map))
    (force-mode-line-update)))

(defun leave-modal-region (overlay saved-values-list)
  (let ((previously-saved-values (and overlay (overlay-get overlay 'values-on-entry))))

    (overlay-put overlay 'saved-values (leave-scope saved-values-list previously-saved-values))
    (overlay-put overlay 'saved-map (current-local-map))
    (use-local-map (overlay-get overlay 'map-on-entry))
    (force-mode-line-update)))


(provide 'modal)


