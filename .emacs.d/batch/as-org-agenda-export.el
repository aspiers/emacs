(require 'ox-icalendar)

(let ((org-agenda-files
       '("~/blockchain/Toucan/TODO.org"
         "/home/adam/org/TODO.org"
         "/home/adam/Panther/TODO.org"))
      (org-icalendar-store-UID nil)
      (org-export-with-broken-links t))
  (org-icalendar-export-agenda-files))
