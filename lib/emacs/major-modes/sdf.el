;;; 
;;; SDF minor mode
;;;

(eval-when-compile
  (require 'hilit19))

(cond (window-system
       (require 'hilit19)
       (hilit-set-mode-patterns
	'sdf-mode
	'(("\\s #.*$" nil comment)
	  ("^#.*$" nil comment)
	  ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
	  ("\\([HPALE][1-4]:\\)" nil label)
	  ("\\([HPALE][1-4]\\[\\)" "\\]" label)
	  ("NB:" "NE:" struct)
	  ("\{\{[ABEINU]:" "\}\}" define)
	  ("^R?E:.*$" nil struct)
	  ("GL:" nil defun)
	  ("^\!include.*$" nil include)
	  ("^\!block " "^\!endblock" define)
	  ("^\!define" "$" define)
	  ("\\[\\[" "\\]\\]" define)
	  ("^\!.*$" nil decl)
	  ("^\*+" nil rule)
	  ("^\\^+" nil rule)
	  ("^\++" nil rule)
	  
	  ;; when looking for matching closing brace sets, stop at the first set
	  ;; to avoid problems with two lots on the one line, 
	  ;; e.g. {{A:first}} some other text {{B:second}}
	  ("\{\{" "\}\}" keyword)
	  ))))

(defun sdf-mode ()
  "Major mode for SDF documentation"
  (interactive)
  (indented-text-mode)
;;  (auto-fill-mode)
  (setq major-mode 'sdf-mode)
  (setq mode-name "SDF")
)