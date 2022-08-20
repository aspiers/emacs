(with-packages org
  :config
  (setq org-capture-templates
        '(("c" "CO2ken NEXT" entry
           (file+olp "~/org/TODO.org" "community" "green" "CO2ken")
           "* NEXT %?%a" :prepend t :jump-to-captured t)
          ("o" "orgmode NEXT" entry
           (file+olp "~/org/TODO.org" "GTD" "orgmode")
           "* NEXT %?%:annotation" :prepend t :jump-to-captured t)
          ("z" "property test" entry
           (file "~/org/TODO.org")
           "%^{Effort}p" :prepend t :jump-to-captured t)
          ("i" "immediate personal NEXT" entry
           (file "~/org/TODO.org")
           "* NEXT %?%:annotation\n  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)
          ("I" "immediate Panther NEXT" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%:annotation\n  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)
          ("n" "personal NEXT" entry
           (file "~/org/TODO.org")
           "* NEXT %?%a%^{Effort}p" :prepend t :jump-to-captured t)
          ("N" "Panther NEXT" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%:annotation" :prepend t :jump-to-captured t)
          ("m" "NEXT from personal mail" entry
           (file "~/org/TODO.org")
           "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
          ("M" "NEXT from Panther mail" entry
           (file "~/Panther/TODO.org")
           "* NEXT %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
          ("a" "personal diary entry" entry
           (file "~/org/diary.org")
           "* %^t %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
          ("L" "Panther learning material" entry
           (file+olp "~/Panther/TODO.org" "general learning")
           "* NEXT read%? %a" :prepend t :jump-to-captured t)
          ("d" "personal task DONE" entry
           (file "~/org/DONE.org")
           "* DONE %?\n  CLOSED: %U")
          ("D" "Panther task DONE" entry
           (file "~/Panther/DONE.org")
           "* DONE %?\n  CLOSED: %U")
          ("X" "nuisance phone call" entry
           (file "~/org/notes/NuisanceCalls.org")
           "* %T %?" :jump-to-captured t)
          ("p" "project" entry
           (file "~/org/TODO.org")
           "* PROJECT %^{project title}\n*** why\n    - %?\n    - \n    - \n    - \n    - \n*** outcome\n*** brainstorming\n***** Who?\n***** What?\n***** When?\n***** Where?\n***** Why?\n***** How?" :prepend t :jump-to-captured t)
          ("P" "Procrastination" entry
           (file "~/org/notes/ProcrastinationLog.org")
           "* %T %^{activity}\n  :PROPERTIES:\n  :thoughts/feelings: %^{thoughts/feelings}\n  :justification: %^{justification}\n  :attempted solution: %^{attempted solution}\n  :resultant thoughts/feelings: %^{resultant thoughts/feelings}\n  :END:" :jump-to-captured t)
          ("e" "emacs NEXT" entry
           (file+olp "" "computer and technology" "tool software development" "emacs")
           "* NEXT %?%a" :prepend t :jump-to-captured t))))

(provide 'as-org-capture)
