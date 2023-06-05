(with-packages org
  :config
  (setq org-capture-templates
        '(
          ;; add a new NEXT for various things
          ("n" "personal NEXT" entry
           (file "~/org/TODO.org")
           "* NEXT %?%a%^{Effort}p" :prepend t :jump-to-captured t)

          ("i" "immediate personal NEXT" entry
           (file "~/org/TODO.org")
           "* NEXT %?%:annotation
  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)

          ("m" "NEXT from personal mail" entry
           (file "~/org/TODO.org")
           "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)

          ;; topic-specific NEXTs
          ("o" "orgmode NEXT" entry
           (file+olp "~/org/TODO.org" "GTD" "orgmode")
           "* NEXT %?%:annotation" :prepend t :jump-to-captured t)

          ("e" "emacs NEXT" entry
           (file+olp "" "computer and technology" "tool software development" "emacs")
           "* NEXT %?%a" :prepend t :jump-to-captured t)

          ("r" "rmx NEXT" entry
           (file+olp "" "welfare and self-development" "rmx")
           "* NEXT %?%a" :prepend t :jump-to-captured t
           :refile-targets ((nil :regexp . "rmx")))

          ("p" "project" entry
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
          ("a" "personal diary entry" entry
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
           "%^{Effort}p" :prepend t :jump-to-captured t)

          ("T" "Toucan capture templates")
          ("Tn" "Toucan NEXT" entry
           (file "~/Toucan/TODO.org")
           "* NEXT %?%:annotation" :prepend t :jump-to-captured t)
          ("Ti" "immediate Toucan NEXT" entry
           (file "~/Toucan/TODO.org")
           "* NEXT %?%:annotation
  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t
           :clock-in t)
          ("Tm" "NEXT from Toucan mail" entry
           (file "~/Toucan/TODO.org")
           "* NEXT %?%[~/.org-mairix-link]" :prepend t
           :jump-to-captured t)
          ("Tl" "Toucan learning material" entry
           (file+olp "~/Toucan/TODO.org" "general learning")
           "* NEXT read%? %a" :prepend t :jump-to-captured t)
          ("Td" "Toucan task DONE" entry
           (file "~/Toucan/DONE.org")
           "* DONE %?
  CLOSED: %U")

          ("P" "Panther capture templates")
          ("Pn" "Panther NEXT" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%:annotation" :prepend t :jump-to-captured t)
          ("Pi" "immediate Panther NEXT" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%:annotation
  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)
          ("Pm" "NEXT from Panther mail" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
          ("Pl" "Panther learning material" entry
           (file+olp "~/Panther/TODO.org" "general learning")
           "* NEXT read%? %a" :prepend t :jump-to-captured t)
          ("Pd" "Panther task DONE" entry
           (file "~/Panther/DONE.org")
           "* DONE %?
  CLOSED: %U"))))


(provide 'as-org-capture)
