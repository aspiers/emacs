(add-to-list 'load-path
             (concat (getenv "HOME")
                     "/.emacs.d/straight/repos/org/lisp"))

(require 'org)
(require 'ox-icalendar)

(load (concat (getenv "HOME") "/.as-custom.el"))

(let ((org-agenda-files argv)
      (org-icalendar-store-UID nil)
      (org-export-with-broken-links t))
  (org-icalendar-export-agenda-files)
  ;; (org-icalendar-combine-agenda-files)
  ;; (org-icalendar-export-to-ics)
  )
