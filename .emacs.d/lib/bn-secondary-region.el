;; Ben North's secondary selection hacks

;;;###autoload
(defun bn-make-region-into-secondary (start end)
  "Turn the region into the secondary selection.
The secondary selection is enabled if required, and set equal to
the region.  The region is deactivated.  The buffer is not
altered at all."
  (interactive "r")
  (if mouse-secondary-overlay
      (move-overlay mouse-secondary-overlay start end (current-buffer))
    (setq mouse-secondary-overlay (make-overlay start end)))
  (overlay-put mouse-secondary-overlay 'face 'secondary-selection)
  (x-set-selection
   'SECONDARY
   (buffer-substring (overlay-start mouse-secondary-overlay)
                     (overlay-end mouse-secondary-overlay)))
  (deactivate-mark))

;;;###autoload
(defun bn-exchange-region-and-secondary (start end)
  "Interchange the region and the secondary selection.
The results are not well-defined if the region and the
secondary selection overlap."
  (interactive "r")
  (or mouse-secondary-overlay
      (error "The secondary selection is not active now"))
  (let ((sec-start (overlay-start mouse-secondary-overlay))
        (sec-end (overlay-end mouse-secondary-overlay)))
    (transpose-regions start end sec-start sec-end)
    (delete-overlay mouse-secondary-overlay)
    (setq mouse-secondary-overlay nil)))

;;;###autoload
(defun bn-keyboard-quit ()
  "Deactivate secondary region, deactivate region, or perform quit.
If the secondary region is active, then deactivate it.  If not, then if
the region is active, then deactivate it.  If not, then do
`keyboard-quit'."
  (interactive)
  (cond ((and (overlayp mouse-secondary-overlay)
              (overlay-buffer mouse-secondary-overlay))
         (delete-overlay mouse-secondary-overlay))
        ((and (boundp 'mark-active)
              mark-active)
         (deactivate-mark))
        (t
         (keyboard-quit))))

(provide 'bn-secondary-region)
