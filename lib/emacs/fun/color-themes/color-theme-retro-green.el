(defun color-theme-retro-green (&optional color func)
  "Plain green on black faces for those longing for the good old days."
  (interactive)
  ;; Build a list of faces without parameters
  (let ((old-faces (face-list))
	(faces)
	(face)
	(foreground (or color "green")))
    (while old-faces
      (setq face (car old-faces)
	    old-faces (cdr old-faces))
      (cond ((memq face '(bold bold-italic))
	     (add-to-list 'faces `(,face (( t (:bold t))))))
	    ((memq face '(italic underline show-paren-mismatch-face))
	     (add-to-list 'faces `(,face (( t (:underline t))))))
	    ((memq face '(modeline modeline-buffer-id modeline-mousable
			  modeline-mousable-minor-mode highlight region
			  secondary-selection show-paren-match-face))
	     (add-to-list 'faces `(,face (( t (:foreground "black"
					       :background ,foreground
					       :inverse t))))))
	    (t
	     (add-to-list 'faces `(,face (( t (nil))))))))
    (color-theme-install
     (append
      (list (or func 'color-theme-retro-green)
	    (list (cons 'foreground-color foreground)
		  (cons 'background-color "black")
		  (cons 'mouse-color foreground)
		  (cons 'cursor-color foreground)
		  (cons 'border-color foreground)
		  (cons 'background-mode 'dark)))
      faces))))

(provide 'color-theme-retro-green)
