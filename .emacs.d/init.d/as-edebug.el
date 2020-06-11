;; Define edebug-view-point, which is like edebug-bounce-point but without
;; the bouncing.

(defun edebug-bounce-point (arg)
  "Bounce the point in the outside current buffer.
If prefix argument ARG is supplied, sit for that many seconds
before returning.  The default is one second."
  (interactive "p")
  (if (not edebug-active)
      (error "Edebug is not active"))
  (save-excursion
    ;; If the buffer's currently displayed, avoid set-window-configuration.
    (save-window-excursion
      (edebug-view-point-1)
      (message "Current buffer: %s Point: %s Mark: %s"
               (current-buffer) (point)
               (if (marker-buffer (edebug-mark-marker))
                   (marker-position (edebug-mark-marker)) "<not set>"))
      (sit-for arg)
      (edebug-pop-to-buffer edebug-buffer (car edebug-window-data)))))

(defun edebug-view-point ()
  "View the point in the outside current buffer.

If you want the outside current buffer to remain visible while
stepping through the code, use `edebug-toggle-save-windows` to
disable window saving.

Use `edebug-where` to return."
  (interactive)
  (if (not edebug-cactive)
      (error "Edebug is not active"))
  (edebug-view-point-1)

  ;; Failed attempt to make outside buffer "sticky" even when window saving
  ;; is enabled:
  ;;
  ;; (setq edebug-inside-windows
  ;;       (edebug-current-windows edebug-save-windows))
  ;; (edebug-set-windows edebug-outside-windows)
  (message "Current buffer: %s Point: %s Mark: %s  Return with %s"
           (current-buffer) (point)
           (if (marker-buffer (edebug-mark-marker))
               (marker-position (edebug-mark-marker)) "<not set>")
           (substitute-command-keys "\\<global-map>\\[edebug-where]")))

(defun edebug-view-point-1 ()
  "View the point in the outside current buffer."
  (edebug-pop-to-buffer edebug-outside-buffer)
  (goto-char edebug-outside-point))

;; edebug-view-point is already bound to both "v" and "P" which seems
;; excessive.  "P" is a natural mnemonic for the non-bouncing version of "p".
(define-key edebug-mode-map "P" 'edebug-view-point)
