;; See http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling
;; for more information on this.

(setq scroll-margin 0)

(defvar smooth-scroll-margin 10
  "Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'.")

(defvar smooth-scroll-strict-margins t
  "If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin.")

(defadvice previous-line (after smooth-scroll-down
                            (&optional arg try-vscroll)
                            activate)
  "Scroll down smoothly if cursor is within `smooth-scroll-margin'
lines of the top of the window."
  (and (> (window-start) (buffer-end -1))
       (let ((lines-from-window-start
              (apply (if smooth-scroll-strict-margins
                         'count-screen-lines
                       'count-lines)
                     (list (window-start) (point)))))
         (and (< lines-from-window-start smooth-scroll-margin)
              (< lines-from-window-start (/ (window-height) 2))))
       (save-excursion (scroll-down 1))))
                            
(defadvice next-line (after smooth-scroll-up
                            (&optional arg try-vscroll)
                            activate)
  "Scroll up smoothly if cursor is within `smooth-scroll-margin'
lines of the bottom of the window."
  (interactive)
  (and (< (window-end) (buffer-end 1))
       (let ((lines-from-window-bottom
              (apply (if smooth-scroll-strict-margins
                         'count-screen-lines
                       'count-lines)
                     (list (point) (window-end)))))
         (and (< lines-from-window-bottom smooth-scroll-margin)
              (< lines-from-window-bottom (/ (window-height) 2))))
       (save-excursion (scroll-up 1))))

(provide 'smooth-scrolling)
