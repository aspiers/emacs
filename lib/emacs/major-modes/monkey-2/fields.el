;;; fields.el - 	parsing buffers into fields defined by regexps.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



;; desc is 
;;	((tag regexp) ...)
;;
(defun move-to-field (desc tag)
  (while desc
    (if (not (looking-at (car (cdr (car desc)))))
	(error "parse error parsing %s with \"%s\"" tag (car (cdr (car desc)))))
    (goto-char (match-end 0))
    (if (eq tag (car (car desc)))
	(setq desc ())
      (setq desc (cdr desc))
      (if (null desc)
	  (error "no such tag %s" tag)))))

(defun get-field (desc tag)
  (save-excursion 
    (move-to-field desc tag)
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun set-field (desc tag val)
  (save-excursion 
    (move-to-field desc tag)
    (delete-region (match-beginning 0) (match-end 0))
    (insert val)))

(defun set-field-and-inherit (desc tag val)
  (save-excursion 
    (move-to-field desc tag)
    (delete-region (match-beginning 0) (match-end 0))
    (insert-and-inherit val)))

;; VALUES is an assoc like DESC, but not necessarily in the same
;; order
;;
(defun insert-fields (desc values &optional defaults)
  (while desc
    (let* ((x (car (car desc)))
	   (v (or (assq x values) 
		  (and defaults (assq x defaults)))))
      (and v (insert (car (cdr v)))))
    (setq desc (cdr desc))))

(defun insert-fields-and-inherit (desc values &optional defaults)
  (while desc
    (let* ((x (car (car desc)))
	   (v (or (assq x values) 
		  (and defaults (assq x defaults)))))
      (and v (insert-and-inherit (car (cdr v)))))
    (setq desc (cdr desc))))


(provide 'fields)
