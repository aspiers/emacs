(defun color-theme-jedit-grey ()
  "Color theme by Gordon Messmer, created 2001-02-07.
Based on a screenshot of jedit.

If you want to modify the font as well, you should customize variable
`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".
The default setting will prevent color themes from installing specific
fonts."
  (interactive)
  (color-theme-install
   '(color-theme-jedit-grey
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-color . "grey77")
      (foreground-color . "black")
      (background-mode . light)
      (mouse-color . "black")
      (cursor-color . "black"))
     (default ((t (nil))))
     (font-lock-comment-face ((t (:italic t :foreground "RoyalBlue4"))))
     (font-lock-string-face ((t (:foreground "Gold4"))))
     (font-lock-keyword-face ((t (:bold t :foreground "DarkRed"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "DarkCyan"))))
     (font-lock-type-face ((t (:foreground "DarkRed"))))
     (font-lock-function-name-face ((t (:foreground "Green4"))))
     (font-lock-builtin-face ((t (:bold t :foreground "DarkRed"))))
     (highline-face ((t (:background "grey84"))))
     (setnu-line-number-face ((t (:background "White" :foreground "MediumPurple3" :italic t))))
     (show-paren-match-face ((t (:background "grey60"))))
     (region ((t (:background "grey70"))))
     (highlight ((t (:background "grey90"))))
     (secondary-selection ((t (:background "white"))))
     (widget-field-face ((t (:background "royalblue"))))
     (widget-single-line-field-face ((t (:background "royalblue")))))) )

(provide 'color-theme-jedit-grey)
