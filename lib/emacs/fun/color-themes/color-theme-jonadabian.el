(defun color-theme-jonadabian ()
  "Dark blue background.
Supports standard faces, font-lock, highlight-changes, widget and
custom."
  (interactive)
  (color-theme-install
   '(color-theme-jonadabian
     ((foreground-color . "wheat")
      (cursor-color . "medium turquoise")
      (background-color . "#000055")
      (background-mode . dark))
     (default ((t (:foreground "#CCBB77" :background "#000055"))))
     (modeline ((t (:foreground "cyan" :background "#007080"))))
     (modeline-buffer-id ((t (:foreground "cyan" :background "#007080"))))
     (modeline-mousable ((t (:foreground "cyan" :background "#007080"))))
     (modeline-mousable-minor-mode ((t (:foreground "cyan" :background "#007080"))))
     (underline ((t (:underline t))))
     (region ((t (:background "#004080"))))
     (font-lock-keyword-face ((t (:foreground "#00BBBB"))))
     (font-lock-comment-face ((t (:foreground "grey50" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "#10D010"))))
     (font-lock-constant-face ((t (:foreground "indian red"))))
     (highlight-changes-face ((t (:background "navy"))))
     (highlight-changes-delete-face ((t (:foreground "red" :background "navy"))))
     (widget-field-face ((t (:foreground "black" :background "grey35"))))
     (widget-inactive-face ((t (:foreground "gray"))))
     (custom-button-face ((t (:foreground "yellow" :background "dark blue"))))
     (custom-state-face ((t (:foreground "mediumaquamarine"))))
     (custom-face-tag-face ((t (:foreground "goldenrod" :underline t))))
     (custom-documentation-face ((t (:foreground "#10D010"))))
     (custom-set-face ((t (:foreground "#2020D0")))))))

(provide 'color-theme-jonadabian)
