(defun color-theme-scintilla ()
  "Color theme by Gordon Messmer, created 2001-02-07.
Based on the Scintilla editor.

If you want to modify the font as well, you should customize variable
`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".
The default setting will prevent color themes from installing specific
fonts."
  (interactive)
  (color-theme-install
   ;; The light editor style doesn't seem to look right with
   ;; the same font that works in the dark editor style.
   ;; Dark letters on light background just isn't as visible.
   '(color-theme-scintilla
     ((font . "-monotype-courier new-bold-r-normal-*-*-140-*-*-m-*-iso8859-1")
      (width  . 95)
      (height . 40)
      (background-color . "white")
      (foreground-color . "black")
      (background-mode . light)
      (mouse-color . "grey15")
      (cursor-color . "grey15"))
     (default ((t nil)))
     (font-lock-comment-face ((t (:italic t :foreground "ForestGreen"))))
     (font-lock-string-face ((t (:foreground "DarkMagenta"))))
     (font-lock-keyword-face ((t (:foreground "NavyBlue"))))
     (font-lock-warning-face ((t (:bold t :foreground "VioletRed"))))
     (font-lock-constant-face ((t (:foreground "Blue"))))
     (font-lock-type-face ((t (:foreground "NavyBlue"))))
     (font-lock-variable-name-face ((t (:foreground "DarkCyan"))))
     (font-lock-function-name-face ((t (:foreground "DarkCyan"))))
     (font-lock-builtin-face ((t (:foreground "NavyBlue"))))
     (highline-face ((t (:background "Grey95"))))
     (show-paren-match-face ((t (:background "Grey80"))))
     (region ((t (:background "Grey80"))))
     (highlight ((t (:foreground "ForestGreen"))))
     (secondary-selection ((t (:background "NavyBlue" :foreground "white"))))
     (widget-field-face ((t (:background "NavyBlue"))))
     (widget-single-line-field-face ((t (:background "RoyalBlue")))))) )

(provide 'color-theme-scintilla)
