(defun color-theme-simple-1 ()
  "Black background.
Doesn't mess with most faces, but does turn on dark background mode."
  (interactive)
  (color-theme-install
   '(color-theme-simple-1
     ((foreground-color . "white")
      (background-color . "black")
      (cursor-color	. "indian red")
      (background-mode	. dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "white"))))
     (modeline-buffer-id ((t (:foreground "black" :background "white"))))
     (modeline-mousable ((t (:foreground "black" :background "white"))))
     (modeline-mousable-minor-mode ((t (:foreground "black" :background "white"))))
     (underline ((t (:underline t))))
     (region ((t (:background "grey")))))))

(provide 'color-theme-simple-1)
