(defun color-theme-ryerson ()
  "White on midnightblue scheme.
Used at Ryerson Polytechnic University in the Electronic Engineering department."
  (interactive)
  (color-theme-install
   '(color-theme-ryerson
     ((foreground-color . "white")
      (background-color . "midnightblue")
      (cursor-color	. "red")
      (background-mode	. dark))
     (default ((t (nil))))
     (modeline ((t (:foreground "black" :background "slategray3"))))
     (modeline-buffer-id ((t (:foreground "black" :background "slategray3"))))
     (modeline-mousable ((t (:foreground "black" :background "slategray3"))))
     (modeline-mousable-minor-mode ((t (:foreground "black" :background "slategray3"))))
     (underline ((t (:underline t))))
     (region ((t (:foreground "black" :background "slategray3")))))))

(provide 'color-theme-ryerson)
