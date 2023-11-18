(with-packages org
  :config

  ;; Refresh agendas if any agenda file reverts due to the underlying
  ;; file on disk changing.
  ;;
  ;; Adapted from https://www.reddit.com/r/orgmode/comments/mu6n5b/comment/gv7yxul/
  (defadvice revert-buffer (after refresh-org-agenda-on-revert activate)
    (if (member (buffer-file-name (current-buffer)) org-agenda-files)
        (org-agenda-redo-all t)))

  :custom
  (org-agenda-custom-commands
   '(("b" "bandhand TODOs" alltodo ""
      ((org-agenda-files
        '("~/eventbook/design.org"))))
     ("d" "daily review"
      ((tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]" 'scheduled)))))
       (tags-todo "officehrs"
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#[AB]\\]" 'scheduled)))))
       (agenda ""
               ((org-agenda-ndays 3)))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if 'deadline 'scheduled))))))
     ("pd" "personal daily review"
      ((tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]" 'scheduled)))))
       (tags-todo "officehrs+CATEGORY=\"personal\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#[AB]\\]" 'scheduled)))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "personal"))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if 'deadline 'scheduled))))
       (org-agenda-prefix-format " %?-12t% s")))
     ("wd" "Panther daily review"
      ((tags-todo "+CATEGORY=\"Panther\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]" 'scheduled)))))
       (tags-todo "officehrs+CATEGORY=\"Panther\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#[AB]\\]" 'scheduled)))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "Panther"))))
       (tags-todo "+CATEGORY=\"Panther\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if 'deadline 'scheduled))))
       (org-agenda-prefix-format " %?-12t% s")))
     ("7" "weekly review"
      ((todo "CHASE"
             ((org-agenda-overriding-header "Items to CHASE")))
       (todo "WAITING"
             ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck "" nil)
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled)))))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t))
      nil)
     ("p7" "personal weekly review"
      ((tags-todo "+CATEGORY=\"personal\"/CHASE"
                  ((org-agenda-overriding-header "Items to CHASE")))
       (tags-todo "+CATEGORY=\"personal\"/WAITING"
                  ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck personal projects:")
               (org-stuck-projects
                '("+CATEGORY=\"personal\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil ""))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled)))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (as-org-agenda-skip-select-category-function "personal"))
       (org-agenda-prefix-format " %?-12t% s"))
      nil)
     ("w7" "Panther weekly review"
      ((tags-todo "+CATEGORY=\"Panther\"/CHASE"
                  ((org-agenda-overriding-header "Items to CHASE")))
       (tags-todo "+CATEGORY=\"Panther\"/WAITING"
                  ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck work projects")
               (org-stuck-projects
                '("+CATEGORY=\"Panther\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil ""))))
       (tags-todo "+CATEGORY=\"Panther\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]" 'scheduled)))))
       (tags-todo "+CATEGORY=\"Panther\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]" 'scheduled))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-prefix-format " %?-12t% s"))
      nil)
     ("w" . "work TODOs")
     ("p" . "personal TODOs")
     ("@" . "TODOs by context")
     ("t" . "TODOs by time constraint")
     ("s" . "TODOs by ETC")
     ("#" . "TODOs by priority")
     ("P" "stuck projects" stuck "" nil)
     ("# " "missing priorities" tags-todo "/-PROJECT-SOMEDAY-MAYBE"
      ((org-agenda-overriding-header "TODOs missing priorities")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'regexp "\\=.*\\[#[A-Z]\\]")))))
     ("s " "missing time estimates" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing time estimate")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'regexp ":sub")))))
     ("@ " "missing contexts" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing context")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'regexp ":@[a-zA-Z]")))))
     ("#a" "priority #A tasks" tags ""
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))))
     ("#A" "priority #A NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))))
     ("#b" "priority #B tasks" tags ""
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]")))))
     ("#B" "priority #B NEXT actions" tags "/PROJECT|NEXT"
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]")))))
     ("#c" "priority #C tasks" tags ""
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]")))))
     ("#C" "priority #C NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]")))))
     ("s1" "" tags "sub10" nil)
     ("s2" "" tags "sub120" nil)
     ("s3" "" tags "sub30" nil)
     ("s4" "" tags "sub4" nil)
     ("s6" "" tags "sub60" nil)
     ("sd" "" tags "subday" nil)
     ("tO" "within office hours" tags-todo "officehrs" nil)
     ("tS" "Saturday" tags-todo "Saturday" nil)
     ("@h" "at home" tags-todo "@home|@internet|@offline|@phone"
      ((org-agenda-overriding-header "at home")))
     ("@B" "in Bracknell office" tags-todo "@Bracknell" nil)
     ("@C" "in Canary Wharf" tags-todo "@CanaryWharf" nil)
     ("@L" "in London" tags-todo "@London" nil)
     ("@?" "elsewhere" tags-todo "-@Bracknell-@London-@CanaryWharf-@phone-@internet-@offline"
      ((org-agenda-overriding-header "elsewhere")))
     ("@i" "internet (online)" tags-todo "@internet" nil)
     ("@0" "offline (but at a computer)" tags-todo "@offline" nil)
     ("@p" "can make phone calls" tags-todo "@phone" nil)
     ("@." "current context"
      (lambda
        (a)
        (error "Not implemented yet"))
      "" nil)
     ("-" "easy" tags-todo "easy" nil)
     ("p-" "easy personal tasks" tags-todo "+easy+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("w-" "easy Panther tasks" tags-todo "+easy+CATEGORY=\"Panther\""
      ((org-agenda-prefix-format "")))
     ("pa" "personal assistant" tags-todo "assist|virtassist" nil)
     ("pA" "personal admin" tags-todo "+admin+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("po" "personal organisation" tags-todo "+admin+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pc" "personal computer" tags-todo "+computer+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pF" "personal F/OSS" tags-todo "+FOSS+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("p$" "personal finance" tags-todo "+finance+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pH" "personal homeimprovement" tags-todo "+homeimprovement+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pf" "personal fun" tags-todo "+fun+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pm" "personal music" tags-todo "+music+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pR" "personal OWRA" tags-todo "+OWRA"
      ((org-agenda-prefix-format "")))
     ("ps" "personal social" tags-todo "+social+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pt" "personal training" tags-todo "+training+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pw" "personal welfare" tags-todo "+welfare+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("p*" "personal community" tags-todo "+community+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("wa" "Panther admin" tags-todo "+admin+CATEGORY=\"Panther\""
      ((org-agenda-prefix-format "")))
     ("wo" "Panther org" tags-todo "+org+CATEGORY=\"Panther\""
      ((org-agenda-prefix-format "")))
     ("wc" "Panther computer" tags-todo "+computer+CATEGORY=\"Panther\""
      ((org-agenda-prefix-format "")))
     ("wL" "Panther learning" tags-todo "+learning+CATEGORY=\"Panther\""
      ((org-agenda-prefix-format "")))
     ("c" "CHASE" todo "CHASE" nil)
     ("W" "WAITING" todo "WAITING" nil)
     ("A" "admin" tags-todo "admin" nil)
     ("v" "virtual assistant" tags-todo "virtassist" nil)
     ("z" "personal agenda" agenda "CATEGORY=\"personal\"" nil)
     ("o" "org" tags-todo "org" nil)
     ("pl" "personal log" agenda "DONE"
      ((org-agenda-files
        '("~/org/TODO.org" "~/org/DONE.org" "~/eventbook/design.org"))
       (org-agenda-span 'week)
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        '(closed clock))
       (org-agenda-entry-types
        '(:timestamp :sexp))
       (org-agenda-overriding-header "")))
     ("wl" "work log" agenda "DONE"
      ((org-agenda-files
        '("~/Panther/TODO.org" "~/Panther/DONE.org"))
       (org-agenda-span 'week)
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        '(closed clock))
       (org-agenda-entry-types
        '(:timestamp :sexp))
       (org-agenda-overriding-header "")
       (org-agenda-prefix-format "  - "))))))

(use-package org-super-agenda)

(provide 'as-org-agenda)
