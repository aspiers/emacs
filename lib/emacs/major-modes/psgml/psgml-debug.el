;;;;\filename psgml-debug.el
;;;\Last edited: 2000-06-07 07:29:59 lenst
;;;\RCS $Id$
;;;\author {Lennart Staflin}
;;;\maketitle

;;\begin{codeseg}
(provide 'psgml-debug)
(require 'psgml)
(require 'psgml-parse)
(require 'psgml-edit)
(require 'psgml-dtd)
(autoload 'sgml-translate-model "psgml-dtd" "" nil)

;;;; Debugging

(define-key sgml-mode-map "\C-c," 'sgml-goto-cache)
(define-key sgml-mode-map "\C-c\C-x" 'sgml-dump-tree)
(define-key sgml-mode-map "\C-c."   'sgml-shortref-identify)

(defun sgml-this-element ()
  (interactive)
  (let ((tree (sgml-find-element-of (point))))
    (sgml-dump-rec tree)))

(defun sgml-goto-cache ()
  (interactive)
  (setq sgml-dtd-info (sgml-pstate-dtd sgml-buffer-parse-state)
	sgml-top-tree (sgml-pstate-top-tree sgml-buffer-parse-state))
  (sgml-find-start-point (point))
  (message "%s" (sgml-dump-node sgml-current-tree)))

(defun sgml-dump-tree (arg)
  (interactive "P")
  (when arg
    (sgml-parse-to-here))
  (with-output-to-temp-buffer "*Dump*"
    (sgml-dump-rec (sgml-pstate-top-tree sgml-buffer-parse-state))))

(defun sgml-auto-dump ()
  (let ((standard-output (get-buffer-create "*Dump*"))
	(cb (current-buffer)))

    (when sgml-buffer-parse-state
      (unwind-protect
	  (progn (set-buffer standard-output)
		 (erase-buffer))
	(set-buffer cb))
    
      (sgml-dump-rec (sgml-pstate-top-tree sgml-buffer-parse-state))

      ))
  )

(defun sgml-start-auto-dump ()
  (interactive)
  (add-hook 'post-command-hook
	    (function sgml-auto-dump)
	    'append))

(defun sgml-comepos (epos)
  (if (sgml-strict-epos-p epos)
      (format "%s:%s"
	      (sgml-entity-name (sgml-eref-entity (sgml-epos-eref epos)))
	      (sgml-epos-pos epos))
    (format "%s" epos)))

(defun sgml-dump-node (u)
  (format
   "%s%s start:%s(%s) end:%s(%s) epos:%s/%s net:%s\n"
   (make-string (sgml-tree-level u) ?. )
   (sgml-element-gi u)
   (sgml-element-start u) (sgml-tree-stag-len u)
   (if (sgml-tree-etag-epos u) (sgml-tree-end u)) (sgml-tree-etag-len u)
   (sgml-comepos (sgml-tree-stag-epos u))
   (sgml-comepos (sgml-tree-etag-epos u))
   (sgml-tree-net-enabled u)))

(defun sgml-dump-rec (u)
  (while u
    (princ (sgml-dump-node u))
    (sgml-dump-rec (sgml-tree-content u))
    (setq u (sgml-tree-next u))))

(defun sgml-shortref-identify ()
  (interactive)
  (sgml-find-context-of (point))
  (let* ((nobol (eq (point) sgml-rs-ignore-pos))
	 (tem (sgml-deref-shortmap sgml-current-shortmap nobol)))
    (message "%s (%s)" tem nobol)))

(defun sgml-lookup-shortref-name (table map)
  (car (rassq map (cdr table))))

(defun sgml-show-current-map ()
  (interactive)
  (sgml-find-context-of (point))
  (let ((name (sgml-lookup-shortref-name
	       (sgml-dtd-shortmaps sgml-dtd-info)
	       sgml-current-shortmap)))
    (message "Current map: %s"
	     (or name "#EMPTY"))))

;;;; For edebug

;;(put 'when 'edebug-form-hook t)
;;(put 'unless 'edebug-form-hook t)
;;(put 'push 'edebug-form-hook '(form sexp))
;;(put 'setf 'edebug-form-hook '(sexp form))

(setq edebug-print-level 3
      edebug-print-length 5
      edebug-print-circle nil
)

(eval-when (load)
  (unless sgml-running-lucid
    (def-edebug-spec sgml-with-parser-syntax (&rest form))
    (def-edebug-spec sgml-with-parser-syntax-ro (&rest form))
    (def-edebug-spec sgml-skip-upto (sexp))
    (def-edebug-spec sgml-check-delim (sexp &optional sexp))
    (def-edebug-spec sgml-parse-delim (sexp &optional sexp))
    (def-edebug-spec sgml-is-delim (sexp &optional sexp sexp sexp))))

;;;; dump

(defun sgml-dump-dtd (&optional dtd)
  (interactive )
  (unless dtd
    (setq dtd (sgml-pstate-dtd sgml-buffer-parse-state)))
  (with-output-to-temp-buffer "*DTD dump*"
    (princ (format "Dependencies: %S\n"
		   (sgml-dtd-dependencies dtd)))
    (loop for et being the symbols of (sgml-dtd-eltypes dtd)
	  do (sgml-dp-element et))))

(defun sgml-dump-element (el-name)
  (interactive
   (list (completing-read "Element: "
			  (sgml-dtd-eltypes
			   (sgml-pstate-dtd sgml-buffer-parse-state))
			  nil t)))
  (with-output-to-temp-buffer "*Element dump*"
    (sgml-dp-element (sgml-lookup-eltype el-name))))

(defun sgml-dp-element (el)
  (cond
   ((sgml-eltype-defined el)
    (princ (format "Element %s %s %s%s:\n"
		   (sgml-eltype-name el)
		   (if (sgml-eltype-stag-optional el) "O" "-")
		   (if (sgml-eltype-etag-optional el) "O" "-")
		   (if (sgml-eltype-mixed el) " mixed" "")))
    (cond
     ((sgml-model-group-p (sgml-eltype-model el))
      (sgml-dp-model (sgml-eltype-model el)))
     (t
      (prin1 (sgml-eltype-model el))
      (terpri)))
    (princ (format "Exeptions: +%S -%S\n"
		   (sgml-eltype-includes el)
		   (sgml-eltype-excludes el)))
    (princ (format "Attlist: %S\n" (sgml-eltype-attlist el)))
    (princ (format "Plist: %S\n" (symbol-plist el))))
   (t
    (princ (format "Undefined element %s\n" (sgml-eltype-name el)))))
  (terpri))


(defun sgml-dp-model (model &optional indent)
  (or indent (setq indent 0))
  (let ((sgml-code-xlate (sgml-translate-model model)))
    (loop
     for i from 0
     for x in sgml-code-xlate do
     (cond ((sgml-normal-state-p (car x))
	    (princ (format "%s%d: opts=%s reqs=%s\n"
			   (make-string indent ? ) i
			   (sgml-untangel-moves (sgml-state-opts (car x)))
			   (sgml-untangel-moves (sgml-state-reqs (car x))))))
	   (t				; and-node
	    (princ (format "%s%d: and-node next=%d\n"
			   (make-string indent ? ) i
			   (sgml-code-xlate (sgml-and-node-next (car x)))))
	    (loop for m in (sgml-and-node-dfas (car x))
		  do (sgml-dp-model m (+ indent 2))))))))

(defun sgml-untangel-moves (moves)
  (loop for m in moves
	collect (list (sgml-move-token m)
		      (sgml-code-xlate (sgml-move-dest m)))))


;;;; Dump state

(defun sgml-dump-state ()
  (interactive)
  (with-output-to-temp-buffer "*State dump*"
    (sgml-dp-state sgml-current-state)))

(defun sgml-dp-state (state &optional indent)
  (or indent (setq indent 0))
  (cond
   ((sgml-normal-state-p state)
    (sgml-dp-model state indent))
   (t
    (princ (format "%sand-state\n" (make-string indent ? )))
    (sgml-dp-state (sgml-and-state-substate state) (+ 2 indent))
    (princ (format "%s--next\n" (make-string indent ? )))    
    (sgml-dp-state (sgml-and-state-next state)     (+ 2 indent))
    (princ (format "%s--dfas\n" (make-string indent ? )))        
    (loop for m in (sgml-and-state-dfas state)
	  do (sgml-dp-model m (+ indent 2))
	  (princ (format "%s--\n" (make-string indent ? )))))))


;;;; Build autoloads for all interactive functions in psgml-parse

(defun sgml-build-autoloads ()
  (interactive)
  (with-output-to-temp-buffer "*autoload*"
    (loop
     for file in '("psgml-parse" "psgml-edit" "psgml-dtd"
		   "psgml-info" "psgml-charent")
     do
     (set-buffer (find-file-noselect (concat file ".el")))
     (goto-char (point-min))
     (while (and
	     (not (eobp))
	     (re-search-forward "^(defun +\\([^ ]+\\)" nil t))
       (let ((name (buffer-substring (match-beginning 1)
				     (match-end 1)))
	     doc)
	 (forward-sexp 1)		; skip argument list
	 (skip-chars-forward " \n\t")
	 (when (eq ?\" (following-char)) ; doc string
	       (setq doc (buffer-substring (point)
					   (progn (forward-sexp 1)
						  (point)))))
	 (skip-chars-forward " \n\t")
	 (when (looking-at "(interactive")
	       (if (null doc)
		   (message "No doc for %s" name))
	       (princ (format
		       "(autoload '%s \"%s\" %s t)\n"
		       name file doc))))))))

;;;; Test psgml with sgmls test cases

(defun test-sgml (start)
  (interactive "p")
  (let (file
	(sgml-show-warnings t))
    (with-output-to-temp-buffer "*Testing psgml*"
      (while
	  (progn
	    (setq file (format "/ni/src/sgmls-1.1/test/test%03d.sgm"
			       start))
	    (file-exists-p file))
	(princ (format "*** File test%03d ***\n" start))
	(find-file file)
	(condition-case errcode
	    (progn
	      (sgml-parse-prolog)
	      ;;(sgml-next-trouble-spot)
	      (sgml-parse-until-end-of nil))
	  (error
	   (princ errcode)
	   (terpri)))
	(if (get-buffer sgml-log-buffer-name)
	    (princ (save-excursion
		     (set-buffer sgml-log-buffer-name)
		     (buffer-string))))
	(terpri)
	(terpri)
	(sit-for 0)
	(kill-buffer (current-buffer))
	(setq start (1+ start))))))


;;;; Profiling

(defun profile-sgml (&optional file)
  (interactive)
  (or file (setq file (expand-file-name "~/work/sigmalink/BBB/config/configspec.xml")))
  (find-file file)
  (sgml-need-dtd)
  (sgml-instrument-parser)
  (elp-reset-all)
  (dotimes (i 5)
    (garbage-collect)
    (sgml-reparse-buffer (function sgml-handle-shortref)))
  (elp-results))

(defun sgml-instrument-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  sgml-parse-to
	  sgml-parser-loop
	  sgml-parse-markup-declaration
	  sgml-do-processing-instruction
	  sgml-pop-entity
	  sgml-tree-net-enabled
	  sgml-do-end-tag
	  sgml-do-data
	  sgml-deref-shortmap
	  sgml-handle-shortref
	  sgml-do-start-tag
	  sgml-do-general-entity-ref
	  sgml-set-face-for
	  sgml-pcdata-move
	  sgml-shortmap-skipstring
	  ;;
	  sgml-parse-attribute-specification-list
	  sgml-check-tag-close
	  sgml-do-move
	  sgml-open-element
	  sgml-list-implications
	  sgml-move-current-state
          sgml-do-empty-start-tag
          sgml-lookup-eltype
          sgml-startnm-char-next
          sgml-eltype-defined
          sgml-execute-implied
          sgml-next-sub-and
          sgml-get-and-move
          format
	  ))
  (elp-instrument-list))


(defun sgml-instrument-dtd-parser ()
  (interactive)
  (require 'elp)
  (setq elp-function-list nil)
  (elp-restore-all)
  (setq elp-function-list
	'(
	  sgml-parse-prolog
	  sgml-skip-ds
	  sgml-parse-markup-declaration
	  sgml-check-doctype-body
	  ;;
	  sgml-check-dtd-subset
	  sgml-parse-ds
	  sgml-declare-attlist
	  sgml-declare-entity
	  sgml-declare-element
	  sgml-declare-shortref
	  ;;
	  sgml-parse-parameter-literal
	  sgml-check-element-type
	  sgml-check-primitive-content-token
	  sgml-check-model-group
	  ;; In sgml-check-model-group
	  sgml-parse-modifier
	  sgml-make-pcdata
	  sgml-skip-ts
	  sgml-make-opt
	  sgml-make-*
	  sgml-make-+
	  sgml-reduce-,
	  sgml-reduce-|
	  sgml-make-&
	  sgml-make-conc
	  sgml-copy-moves
	  ;; is ps*
	  sgml-do-parameter-entity-ref
	  ;; 
	  sgml-make-primitive-content-token
	  sgml-push-to-entity
	  sgml-lookup-entity
	  sgml-lookup-eltype
	  sgml-one-final-state
	  sgml-remove-redundant-states-1
	  ))
  (elp-instrument-list))

;;;; Structure Viewing and Navigating

(require 'psgml-api)

(defvar show-structure-buffer nil)
(defvar show-structure-positions nil)
(defvar show-structure-source-buffer nil)

(defun show-structure ()
  (interactive)
  (let* ((source (current-buffer))
         (result (get-buffer-create "*Struct*"))
         (show-structure-buffer result))
    (set-buffer result)
    (erase-buffer)
    (make-local-variable 'show-structure-positions)
    (setq show-structure-positions nil)
    (make-local-variable 'show-structure-source-buffer)
    (setq show-structure-source-buffer source)
    (use-local-map (make-sparse-keymap))
    (local-set-key "\C-c\C-c" 'show-structure-goto)
    (set-buffer source)
    (show-element (sgml-top-element))
    (display-buffer result)))


(defun show-structure-goto ()
  (interactive)
  (beginning-of-line)
  (let ((pos-pair (assoc (point) show-structure-positions)))
    (when pos-pair
      (select-window
       (display-buffer show-structure-source-buffer))
      (goto-char (cdr pos-pair)))))


(defun show-struct-element-p (element)
  (or (and (not (sgml-element-data-p element))
           (not (sgml-element-empty element)))
      (sgml-element-appdata element 'structure)))


(defun show-element (element)
  (cond ((show-struct-element-p element)
         (let ((gi (sgml-element-gi element))
               (level (sgml-element-level element)))
           (save-excursion
             (set-buffer show-structure-buffer)
             (if (not (bolp))
                 (insert "\n"))
             (push (cons (point) (sgml-element-start element))
                   show-structure-positions)
             (insert (format "%s[%15s] " (make-string (- level 1) ? ) gi))))
         (catch 'show-data-stop
             (show-element-data element))
         (sgml-map-content element #'show-element))))

(defun show-element-data (element)
  (sgml-map-content element #'show-element-data #'show-data)
  (throw 'show-data-stop nil))

(defun show-data (data)
  (save-excursion
    (set-buffer show-structure-buffer)
    (let ((start (point)))
      (insert data)
      (let ((end (point)))
        (subst-char-in-region start end ?\n ? )
        (when (> (current-column) fill-column)
          (move-to-column fill-column)
          (delete-region (point) end)
          (throw 'show-data-stop nil))))))

;;;; Show current element type
;; Candidate for C-c C-t

(autoload 'sgml-princ-names "psgml-info")

(define-key sgml-mode-map "\C-c\C-t" 'sgml-show-current-element-type)

(defun sgml-show-current-element-type ()
  (interactive)
  (let* ((el (sgml-find-context-of (point)))
         (et (sgml-element-eltype el)))
    (with-output-to-temp-buffer "*Current Element Type*"
      (princ (format "ELEMENT: %s%s\n" (sgml-eltype-name et)
                     (let ((help-text (sgml-eltype-appdata et 'help-text)))
                       (if help-text
                           (format " -- %s" help-text)
                           ""))))
      (when sgml-omittag
        (princ (format "\n Start-tag is %s.\n End-tag is %s.\n"
                       (if (sgml-eltype-stag-optional et)
                           "optional" "required")
                       (if (sgml-eltype-etag-optional et)
                           "optional" "required"))))
      ;; ----
      (princ "\nCONTENT: ")
      (cond ((symbolp (sgml-eltype-model et)) (princ (sgml-eltype-model et)))
	    (t
	     (princ (if (sgml-eltype-mixed et)
                        "mixed\n"
                      "element\n"))	     
             (sgml-print-position-in-model el et (point) sgml-current-state)
             (princ "\n\n")
	     (sgml-princ-names
	      (mapcar #'symbol-name (sgml-eltype-refrenced-elements et))
              "All: ")))
      (let ((incl (sgml-eltype-includes et))
            (excl (sgml-eltype-excludes et)))
        (when (or incl excl)
          (princ "\n\nEXCEPTIONS:"))
        (when incl
          (princ "\n + ")
          (sgml-princ-names (mapcar #'symbol-name incl)))
        (when excl
          (princ "\n - ")
          (sgml-princ-names (mapcar #'symbol-name excl))))
      ;; ----
      (princ "\n\nATTRIBUTES:\n")
      (sgml-print-attlist et)
      ;; ----
      (let ((s (sgml-eltype-shortmap et)))
	(when s
	  (princ (format "\nUSEMAP: %s\n" s))))
      ;; ----
      (princ "\nOCCURS IN:\n")
      (let ((occurs-in ()))
	(sgml-map-eltypes
	 (function (lambda (cand)
		     (when (memq et (sgml-eltype-refrenced-elements cand))
		       (push cand occurs-in))))
	 (sgml-pstate-dtd sgml-buffer-parse-state))
        (sgml-princ-names (mapcar 'sgml-eltype-name
                                  (sort occurs-in (function string-lessp))))))))

(defun sgml-print-attlist (et)
  (let ((ob (current-buffer)))
    (set-buffer standard-output)
    (unwind-protect
        (loop
         for attdecl in (sgml-eltype-attlist et) do
         (princ " ")
         (princ (sgml-attdecl-name attdecl))
         (let ((dval (sgml-attdecl-declared-value attdecl))
               (defl (sgml-attdecl-default-value attdecl)))
           (when (listp dval)
             (setq dval (concat (if (eq (first dval)
                                        'NOTATION)
                                    "#NOTATION (" "(")
                                (mapconcat (function identity)
                                           (second dval)
                                           "|")
                                ")")))
           (indent-to 15 1)
           (princ dval)
           (cond ((sgml-default-value-type-p 'FIXED defl)
                  (setq defl (format "#FIXED '%s'"
                                     (sgml-default-value-attval defl))))
                 ((symbolp defl)
                  (setq defl (upcase (format "#%s" defl))))
                 (t
                  (setq defl (format "'%s'"
                                     (sgml-default-value-attval defl)))))

           (indent-to 48 1)          
           (princ defl)
           (terpri)))
      (set-buffer ob))))


(defun sgml-print-position-in-model (element element-type buffer-pos parse-state)
  (let ((u (sgml-element-content element))
        (names nil))
    (while (and u (>= buffer-pos (sgml-element-end u)))
      (push (sgml-element-gi u) names)
      (setq u (sgml-element-next u)))
    (when names
      (sgml-princ-names (nreverse names) " " ", ")
      (princ "\n")))
  (princ " ->")
  (let* ((state parse-state)
         (required-seq                  ; the seq of req el following point
          (loop for required = (sgml-required-tokens state)
                while (and required (null (cdr required)))
                collect (sgml-eltype-name (car required))
                do (setq state (sgml-get-move state (car required)))))
         (last-alt
          (mapcar 'sgml-eltype-name
                  (append (sgml-optional-tokens state)
                          (sgml-required-tokens state)))))
    (cond
     (required-seq
      (when last-alt
        (nconc required-seq
               (list (concat "("
                             (mapconcat (lambda (x) x)
                                        last-alt " | ")
                             (if (sgml-final state)
                                 ")?" ")")))))
      (sgml-princ-names required-seq " " ", "))

     (last-alt
      (sgml-princ-names last-alt " (" " | ")
      (princ ")")
      (when (sgml-final state)
        (princ "?"))))))

;;;; Adding appdata to element types
;;; Candidate for PI PSGML processing

(defvar sgml-psgml-pi-enable-outside-dtd nil)

(defun sgml-eval-psgml-pi ()
  (interactive)
  (let ((sgml-psgml-pi-enable-outside-dtd t))
    (sgml-parse-to-here)))

(define-key sgml-mode-map "\e\C-x" 'sgml-eval-psgml-pi)

(defun sgml--pi-element-handler ()
  (sgml-skip-ps)
  (let ((eltype (sgml-lookup-eltype (sgml-parse-name)))
        name value)
    (sgml-skip-ps)
    (while (setq name (sgml-parse-name))
      ;; FIXME: check name not reserved
      (sgml-skip-ps)
      (cond ((sgml-parse-delim "VI")
             (sgml-skip-ps)
             (setq value
                   (if (looking-at "['\"]")
                       (sgml-parse-literal)
                     (read (current-buffer)))))
            (t
             (setq value t)))
      (message "%s = %S" name value)
      (setf (sgml-eltype-appdata eltype (intern (downcase name))) value)
      (sgml-skip-ps))))


(defun sgml-do-processing-instruction (in-declaration)
  (let ((start (point)))
    (when (and (or in-declaration
                   sgml-psgml-pi-enable-outside-dtd)
               (eq ?P (following-char))
	       (looking-at "PSGML +\\(\\sw+\\) *"))
      (let* ((command (format "%s" (downcase (match-string 1))))
             (flag-command (assoc command
                                  '(("nofill"      . nofill)
                                    ("breakafter"  . break-after-stag)
                                    ("breakbefore" . break-before-stag)
                                    ("structure"   . structure)))))
	(goto-char (match-end 0))
	(cond (flag-command
               (sgml-parse-set-appflag (cdr flag-command)))
              ((equal command "element")
               (sgml--pi-element-handler))
              (t
               (sgml-log-warning "Unknown processing instruction for PSGML: %s"
                                 command)))))
    (if sgml-xml-p
	(sgml-skip-upto "XML-PIC")
      (sgml-skip-upto "PIC"))
    (when sgml-pi-function
      (funcall sgml-pi-function
	       (buffer-substring-no-properties start (point)))))
  (if sgml-xml-p
      (sgml-check-delim "XML-PIC")
    (sgml-check-delim "PIC"))
  (unless in-declaration
    (sgml-set-markup-type 'pi))
  t)

;;;; Possible modification to allow setting face on content:
(defun sgml-set-face-for (start end type)
  (let ((face (cdr (assq type sgml-markup-faces))))
    ;;++
    (if (and (null type) sgml-current-tree) 
        (setq face (sgml-element-appdata sgml-current-tree 'face)))
    ;;--
    (cond
     (sgml-use-text-properties
      (let ((inhibit-read-only t)
	    (after-change-function nil)	; obsolete variable
	    (before-change-function nil) ; obsolete variable
	    (after-change-functions nil)
	    (before-change-functions nil))
	(put-text-property start end 'face face)
        (when (< start end)
          (put-text-property (1- end) end 'rear-nonsticky '(face)))))
     (t
      (let ((current (overlays-at start))
	    (pos start)
	    old-overlay)
	(while current
	  (cond ((and (null old-overlay)
                      type
		      (eq type (overlay-get (car current) 'sgml-type)))
		 (setq old-overlay (car current)))
		((overlay-get (car current) 'sgml-type)
		 ;;(message "delov: %s" (overlay-get (car current) 'sgml-type))
		 (delete-overlay (car current))))
	  (setq current (cdr current)))
	(while (< (setq pos (next-overlay-change pos))
		  end)
	  (setq current (overlays-at pos))
	  (while current
	    (when (overlay-get (car current) 'sgml-type)
	      (delete-overlay (car current)))
	    (setq current (cdr current))))
	(cond (old-overlay
	       (move-overlay old-overlay start end)
	       (if (null (overlay-get old-overlay 'face))
		   (overlay-put old-overlay 'face face)))
	      (face
	       (setq old-overlay (make-overlay start end))
	       (overlay-put old-overlay 'sgml-type type)
	       (overlay-put old-overlay 'face face))))))))

;¤¤\end{codeseg}
