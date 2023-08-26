(with-packages org
  :custom
  (org-capture-templates
   '(
     ;; Effort estimate can be asked for via %^{Effort}p

     ;; add a new NEXT for various things
     ("n" "personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?%a" :prepend t :jump-to-captured t)

     ("c" "personal NEXT from clipboard" entry
      (file "~/org/TODO.org")
      "* NEXT %?
  %x" :prepend t
      :jump-to-captured t)

     ("i" "immediate personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?%a
  SCHEDULED: %T" :prepend t :jump-to-captured t :clock-in t
      :clock-keep t)

     ("m" "personal NEXT from mairix" entry
      (file "~/org/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)

     ;; topic-specific NEXTs
     ("o" "orgmode NEXT" entry
      (file+olp "~/org/TODO.org" "GTD" "orgmode")
      "* NEXT %?%a" :prepend t :jump-to-captured t)

     ("O" "OW2000 NEXT" entry
      (file+olp "~/org/TODO.org" "community" "OW2000")
      "* NEXT %?%a" :prepend t :jump-to-captured t)

     ("e" "emacs NEXT" entry
      (file+olp "" "computer and technology" "tool software development" "emacs")
      "* NEXT %?%a" :prepend t :jump-to-captured t)

     ("r" "rmx NEXT" entry
      (file+olp "" "welfare and self-development" "rmx")
      "* NEXT %?%a" :prepend t :jump-to-captured t
      :refile-targets ((nil :regexp . "rmx")))

     ("p" "personal project" entry
      (file "~/org/TODO.org")
      "* PROJECT %^{project title}
*** why
    - %?
    - 
    - 
    - 
    - 
*** outcome
*** brainstorming
***** Who?
***** What?
***** When?
***** Where?
***** Why?
***** How?" :prepend t :jump-to-captured t)

     ;; --------------------------------------------------
     ;; Logging stuff which already happened
     ("a" "personal diary entry from mairix" entry
      (file "~/org/diary.org")
      "* %^t %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)

     ("d" "personal task DONE" entry
      (file "~/org/DONE.org")
      "* DONE %?
  CLOSED: %U")

     ("x" "Procrastination" entry
      (file "~/org/notes/ProcrastinationLog.org")
      "* %T %^{activity}
  :PROPERTIES:
  :thoughts/feelings: %^{thoughts/feelings}
  :justification: %^{justification}
  :attempted solution: %^{attempted solution}
  :resultant thoughts/feelings: %^{resultant thoughts/feelings}
  :END:" :jump-to-captured t)

     ("X" "nuisance phone call" entry
      (file "~/org/notes/NuisanceCalls.org")
      "* %T %?" :jump-to-captured t)

     ("z" "property test" entry
      (file "~/org/TODO.org")
      "" :prepend t :jump-to-captured t)

     ("T" "Toucan capture templates")
     ("Tc" "Toucan NEXT from clipboard" entry
      (file "~/Toucan/TODO.org")
      "* NEXT %?
  %x" :prepend t
      :jump-to-captured t)
     ("Ts" "Toucan NEXT from Slack" entry
      (file "~/Toucan/TODO.org")
      "* NEXT %?
  %:annotation
  %i" :prepend t
      :jump-to-captured t)
     ("Tn" "Toucan NEXT" entry
      (file "~/Toucan/TODO.org")
      "* NEXT %?%a" :prepend t :jump-to-captured t)
     ("Ti" "Toucan immediate NEXT" entry
      (file "~/Toucan/TODO.org")
      "* NEXT %?%a
  SCHEDULED: %T" :prepend t :jump-to-captured t
      :clock-in t :clock-keep t)
     ("Tm" "Toucan NEXT from mairix" entry
      (file "~/Toucan/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t
      :jump-to-captured t)
     ("Tl" "Toucan learning material" entry
      (file+olp "~/Toucan/TODO.org" "learning")
      "* NEXT read%? %a" :prepend t :jump-to-captured t)
     ("Td" "Toucan DONE" entry
      (file "~/Toucan/DONE.org")
      "* DONE %?
  CLOSED: %U")

     ("P" "Panther capture templates")
     ("Pn" "Panther NEXT" entry
      (file "~/Panther/TODO.org")
      "* NEXT %?%a" :prepend t :jump-to-captured t)
     ("Pi" "Panther immediate NEXT" entry
      (file "~/Panther/TODO.org")
      "* NEXT %?%a
  SCHEDULED: %T" :prepend t :jump-to-captured t :clock-in t
      :clock-keep t)
     ("Pm" "Panther NEXT from mairix" entry
      (file "~/Panther/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
     ("Pl" "Panther learning material" entry
      (file+olp "~/Panther/TODO.org" "general learning")
      "* NEXT read%? %a" :prepend t :jump-to-captured t)
     ("Pd" "Panther DONE" entry
      (file "~/Panther/DONE.org")
      "* DONE %?
  CLOSED: %U"))))


(provide 'as-org-capture)
