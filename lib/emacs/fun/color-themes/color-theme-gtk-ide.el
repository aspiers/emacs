(defun color-theme-gtk-ide ()
  "Color theme by Gordon Messmer, created 2001-02-07.
Inspired by a GTK IDE whose name I've forgotten.

If you want to modify the font as well, you should customize variable
`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".
The default setting will prevent color themes from installing specific
fonts."
  ;; The light editor style doesn't seem to look right with
  ;; the same font that works in the dark editor style.
  ;; Dark letters on light background just isn't as visible.
  (interactive)
  (color-theme-install
   '(color-theme-gtk-ide
     ((font . "-monotype-courier new-medium-r-normal-*-*-120-*-*-m-*-iso8859-15")
      (width  . 95)
      (height . 45)
      (background-color . "white")
      (foreground-color . "black")
      (background-mode . light)
      (mouse-color . "grey15")
      (cursor-color . "grey15"))
     (default ((t nil)))
     (font-lock-comment-face ((t (:italic t :foreground "grey55"))))
     (font-lock-string-face ((t (:foreground "DarkRed"))))
     (font-lock-keyword-face ((t (:foreground "DarkBlue"))))
     (font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))
     (font-lock-constant-face ((t (:foreground "OliveDrab"))))
     (font-lock-type-face ((t (:foreground "SteelBlue4"))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-function-name-face ((t (:foreground "SlateBlue"))))
     (font-lock-builtin-face ((t (:foreground "ForestGreen"))))
     (highline-face ((t (:background "grey95"))))
     (show-paren-match-face ((t (:background "grey80"))))
     (region ((t (:background "grey80"))))
     (highlight ((t (:background "LightSkyBlue"))))
     (secondary-selection ((t (:background "grey55"))))
     (widget-field-face ((t (:background "navy"))))
     (widget-single-line-field-face ((t (:background "royalblue")))))) )

(provide 'color-theme-gtk-ide)
