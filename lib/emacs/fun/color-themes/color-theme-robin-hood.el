(defun color-theme-robin-hood ()
  "`color-theme-gnome2' with navajo white on green."
  (interactive)
  (color-theme-gnome2)
  (let ((color-theme-cumulative t))
    (color-theme-install
     '(color-theme-robin-hood
       ((foreground-color . "navajo white")
	(background-color . "#304020"))))))

(provide 'color-theme-robin-hood)
