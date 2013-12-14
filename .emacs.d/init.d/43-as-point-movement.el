;; Scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;; Show position in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Default right margin
(setq fill-column 70)

;; Stop down cursor adding newlines to end of buffer.
(setq next-line-add-newlines nil)

;; IntelliMouse
(cond ((and (< emacs-major-version 22) window-system)
       (load "mwheel" t)
       (mwheel-install)))

