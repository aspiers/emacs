;; If you're using color-theme.el, this background should match the
;; background of your chosen theme
(setq as-default-background
;;    "seashell1"
;;    "wheat"
;;    "blanched almond"
      "antique white"
;;    "gray90"
      )

;; (setq frame-background-mode
;;       (cond ((and (= emacs-major-version 21) window-system) 'light)
;;             (t 'dark)))

;; (and (eq frame-background-mode 'light)
;;      (custom-set-variables
;;       `(vc-annotate-background ,as-default-background)))

;; (and (>= emacs-major-version 21)
;;      (progn
;;        (custom-set-faces
;;         `(default
;;            ((((class color) (background light))
;;              (:background ,as-default-background))))
;;         `(fringe
;;           ((((class color) (background light))
;;             (:background ,as-default-background))))
;;         `(mmm-default-submode-face
;;           ((((class color) (background light))
;;             (:background ,as-default-background)))))
;;        ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~/local/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(align-dq-string-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode python-mode)))
 '(align-exclude-rules-list (quote ((exc-dq-string (regexp . "\"\\([^\"
]+\\)\"") (repeat . t) (modes . align-dq-string-modes)) (exc-sq-string (regexp . "'\\([^'
]+\\)'") (repeat . t) (modes . align-sq-string-modes)) (exc-open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^ 	
\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (exc-c-comment (regexp . "/\\*\\(.+\\)\\*/") (repeat . t) (modes . align-c++-modes)) (exc-c-func-params (regexp . "(\\([^)
]+\\))") (repeat . t) (modes . align-c++-modes)) (exc-c-macro (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$") (group . 2) (modes . align-c++-modes)) (exc-perl-comment (regexp . "^\\s-*#.*$") (modes . align-perl-modes)))))
 '(align-sq-string-modes (quote (python-mode)))
 '(auto-save-interval 120)
 '(backup-directory-alist (quote (("." . ".emacs.backup"))))
 '(blink-cursor-delay 0.0)
 '(blink-cursor-interval 0.3)
 '(blinking-cursor-blink-frequency 4)
 '(blinking-cursor-colors (quote ("coral" "blue" "gold")))
 '(blinking-cursor-idle-states (quote (("coral" "box" 0.5) ("coral" -1 0.5))))
 '(blinking-cursor-non-idle-state (quote ("coral" "box")))
 '(blinking-cursor-states (quote (("coral" "box" 0.7) ("coral" 2 0.4))))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "url_handler.sh")
 '(bs-alternative-configuration "cvs")
 '(bs-attributes-list (quote (("" 1 1 left bs--get-marked-string) ("M" 1 1 left bs--get-modified-string) ("R" 2 2 left bs--get-readonly-string) ("Buffer" bs--get-name-length 10 left bs--get-name) ("" 1 1 left " ") ("Size" 5 8 right bs--get-size-string) ("" 1 1 left " ") ("Mode" 5 12 right bs--get-mode-name) ("" 2 2 left "  ") ("File" 12 12 left bs--get-file-name) ("" 2 2 left "  "))))
 '(bs-configurations (quote (("all" nil nil nil nil nil) ("files" nil nil ".~[0-9.]+~$" bs-visits-non-file bs-sort-buffer-interns-are-last) ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last) ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last) ("cvs" "\\*cvs\\*$" nil "" nil bs--sort-by-name))))
 '(bs-default-sort-name "by nothing")
 '(bs-max-window-height 24)
 '(bs-maximal-buffer-name-column 22)
 '(bs-minimal-buffer-name-column 5)
 '(case-fold-search t)
 '(color-theme-is-cumulative t)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-autoindent-on-semi t)
 '(cperl-close-paren-offset -2)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lineup-step 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-under-as-char nil)
 '(cvs-buffer-switch-alist (quote ("diff" "status" "log")))
 '(cvs-buffer-switch-list (quote ("diff" "status" "log")))
 '(cvs-find-file-and-jump t)
 '(cvs-invert-ignore-marks (quote ("diff")))
 '(cvs-parse-ignored-messages (quote ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" ".*New directory `.*' -- ignored.*$" ".*warning: directory CVS specified in argument.*$" ".*but CVS uses CVS for its own purposes; skipping CVS directory.*$")))
 '(cvs-reuse-cvs-buffer (quote subdir))
 '(cvs-use-fileinfo-caches t)
 '(delete-old-versions t)
 '(delete-selection-mode nil nil (delsel))
 '(diff-switches "-u")
 '(dired-kept-versions 0)
 '(dired-listing-switches "-l")
 '(dvc-tips-enabled nil)
 '(echo-keystrokes 0.01)
 '(ediff-custom-diff-options "-u")
 '(eldoc-minor-mode-string "")
 '(eldoc-mode t t)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(fast-lock-cache-directories (quote ("~/.emacs-flc")))
 '(fast-lock-minimum-size 4096)
 '(folding-mode-prefix-key "")
 '(global-font-lock-mode t nil (font-lock))
 '(global-msf-abbrev-mode t)
 '(gnus-asynchronous t)
 '(gnus-cache-active-file "~/.gnus/cache/active")
 '(gnus-cache-directory "~/.gnus/cache/")
 '(gnus-expert-user t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-group-line-format "%M%S%p%P%3y: %(%g%)%l
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-group-sort-function (quote gnus-group-sort-by-rank))
 '(gnus-novice-user nil)
 '(gnus-secondary-servers (quote ("news.linuxprinting.org")))
 '(gnus-select-method (quote (nntp "news.pipex.net")))
 '(gnus-startup-hook (quote ((lambda nil (if (<= (frame-width) 80) (gnus-add-configuration (quote (article (vertical 1.0 (horizontal 0.25 (summary 0.75 point) (tree 1.0)) (article 1.0))))))))))
 '(gnus-summary-exit-hook (quote (gnus-summary-bubble-group)))
 '(gnus-summary-line-format "%U%R%z%4L:%([%1{%-15,15n%}]%) %2t %3{%B%}%2{%s%}
")
 '(gnus-suppress-duplicates t)
 '(gnus-use-trees t)
 '(ido-case-fold nil)
 '(ido-max-prompt-path 0.8)
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(ispell-program-name "aspell")
 '(iswitchb-case nil)
 '(iswitchb-default-method (quote samewindow))
 '(jit-lock-stealth-nice 0.1)
 '(jit-lock-stealth-time 1)
 '(kept-old-versions 0)
 '(lazy-lock-defer-on-scrolling t)
 '(lazy-lock-defer-time 0.1)
 '(lazy-lock-minimum-size 4096)
 '(lazy-lock-stealth-time 15)
 '(lazy-lock-stealth-verbose t)
 '(make-backup-file-name-function (quote as-make-backup-file-name))
 '(message-default-news-headers "From: Adam Spiers <usenet@adamspiers.org>
Reply-To: Adam Spiers <usenet@adamspiers.org>
")
 '(mouse-wheel-follow-mouse t)
 '(mouse-yank-at-point t)
 '(msf-abbrev-indent-after-expansion t)
 '(muse-colors-autogen-headings (quote outline))
 '(muse-mode-auto-p nil)
 '(muse-project-alist (quote (("AS-notes" ("~/roaming/notes") nil) ("Novell-notes" ("~/ifolder/notes") nil) ("all-notes" ("~/roaming/notes" "~/ifolder/notes") nil))))
 '(muse-wiki-allow-nonexistent-wikiword t)
 '(muse-wiki-wikiword-regexp "\\<\\([A-Z]+[a-z]+[A-Z]+[a-zA-Z0-9]*+\\)")
 '(mwheel-follow-mouse t)
 '(org-agenda-custom-commands (quote (("w" . "work TODOs") ("p" . "personal TODOs") ("@" . "TODOs by context") ("t" . "TODOs by time constraint") ("s" . "TODOs by ETC") ("#" . "TODOs by priority") ("d" "daily review" ((tags-todo "/-PROJECT" ((org-agenda-overriding-header "Unscheduled #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]" (quote scheduled)))))) (agenda)) ((org-agenda-compact-blocks t))) ("P" "stuck projects" stuck "" nil) ("# " "missing priorities" tags-todo "/-PROJECT-SOMEDAY-MAYBE" ((org-agenda-overriding-header "TODOs missing priorities") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) "\\=.*\\[#[A-C]\\]"))))) ("s " "missing time estimates" tags-todo "/NEXT" ((org-agenda-overriding-header "TODOs missing time estimate") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) ":sub"))))) ("@ " "missing contexts" tags-todo "/NEXT" ((org-agenda-overriding-header "TODOs missing context") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) ":@[a-zA-Z]"))))) ("#A" "priority #A tasks" tags "" ((org-agenda-overriding-header "priority #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))))) ("#B" "priority #B tasks" tags "" ((org-agenda-overriding-header "priority #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]"))))) ("#C" "priority #C tasks" tags "" ((org-agenda-overriding-header "priority #C TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#C\\]"))))) ("T" "" tags "Ten" nil) ("s1" "" tags "sub10" nil) ("s2" "" tags "sub120" nil) ("s3" "" tags "sub30" nil) ("s4" "" tags "sub4" nil) ("s6" "" tags "sub60" nil) ("sd" "" tags "subday" nil) ("tO" "within office hours" tags-todo "officehrs" nil) ("tS" "Saturday" tags-todo "Saturday" nil) ("@h" "at home" tags-todo "@home|@internet|@offline|@phone" ((org-agenda-overriding-header "at home"))) ("@B" "in Bracknell office" tags-todo "@Bracknell" nil) ("@C" "in Canary Wharf" tags-todo "@CanaryWharf" nil) ("@L" "in London" tags-todo "@London" nil) ("@?" "elsewhere" tags-todo "-@Bracknell-@London-@CanaryWharf-@phone-@internet-@offline" ((org-agenda-overriding-header "elsewhere"))) ("@i" "internet (online)" tags-todo "@internet" nil) ("@0" "offline (but at a computer)" tags-todo "@offline" nil) ("@p" "can make phone calls" tags-todo "@phone" nil) ("@." "current context" (lambda (a) (error "Not implemented yet")) "" nil) ("-" "easy" tags-todo "easy" nil) ("p-" "easy personal tasks" tags-todo "+easy+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("w-" "easy Novell tasks" tags-todo "+easy+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("pa" "personal admin" tags-todo "+admin+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("po" "personal organisation" tags-todo "+admin+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pc" "personal computer" tags-todo "+computer+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pF" "personal F/OSS" tags-todo "+FOSS+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("p$" "personal finance" tags-todo "+finance+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pH" "personal homeimprovement" tags-todo "+homeimprovement+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pf" "personal fun" tags-todo "+fun+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pm" "personal music" tags-todo "+music+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("ps" "personal social" tags-todo "+social+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pt" "personal training" tags-todo "+training+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pw" "personal welfare" tags-todo "+welfare+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("p*" "personal community" tags-todo "+community+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("wa" "Novell admin" tags-todo "+admin+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wo" "Novell org" tags-todo "+org+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wc" "Novell computer" tags-todo "+computer+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wD" "Novell direct" tags-todo "+direct+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wP" "Novell partner" tags-todo "+partner+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wt" "Novell TAM" tags-todo "+tam+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wE" "Novell enablement" tags-todo "+enablement+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wT" "Novell TS community" tags-todo "+TS+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wl" "Novell learning" tags-todo "+learning+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wF" "Novell F/OSS" tags-todo "+FOSS+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wC" "Novell competitive" tags-todo "+competitive+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wm" "Novell marketing" tags-todo "+marketing+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("we" "Novell engineering/BU" tags-todo "+eng+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("c" "CHASE" tags-todo "/CHASE" nil) ("W" "WAITING" tags-todo "/WAITING" nil) ("A" "admin" tags-todo "admin" nil) ("o" "org" tags-todo "org" nil))))
 '(org-agenda-deadline-leaders (quote ("Deadline: " "In %3dd: ")))
 '(org-agenda-files (quote ("~/ifolder/TODO.org" "~/roaming/TODO.org" "~/ifolder/Novell-diary.org" "~/roaming/diary.org")))
 '(org-agenda-fontify-priorities (quote ((65 (:bold t :weight bold)))))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 31)
 '(org-agenda-prefix-format (quote ((agenda . "  %-9:c%?-12t% s") (timeline . "  % s") (todo . "  %-9:c") (tags . "  %-9:c"))))
 '(org-agenda-scheduled-leaders (quote ("Sched: " "Sched.%2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down category-keep) (todo priority-down category-keep) (tags priority-down category-keep))))
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.6)))
 '(org-combined-agenda-icalendar-file "~/ifolder/org.ics")
 '(org-default-notes-file "~/roaming/TODO.org")
 '(org-directory "~/roaming")
 '(org-disputed-keys (quote (([(control shift right)] . [(control shift n)]) ([(control shift left)] . [(control shift p)]) ([(control 44)] . [(control 39)]) ([(control tab)] . [(control meta tab)]))))
 '(org-email-link-description-format "mail %c: %.30s")
 '(org-export-with-sub-superscripts (quote {}))
 '(org-from-is-user-regexp "\\<\\(adam@spiers\\.net\\|Adam Spiers\\|@\\(adamspiers\\|tigerpig\\)\\.org\\|aspiers@novell\\.com\\)\\>")
 '(org-hide-leading-stars t)
 '(org-link-abbrev-alist (quote (("bug" . "https://bugzilla.novell.com/show_bug.cgi?id="))))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (gnus . gnus-other-frame) (file . find-file))))
 '(org-lowest-priority 69)
 '(org-mairix-augmented-links nil)
 '(org-mairix-display-hook (quote org-mairix-mutt-display-results))
 '(org-mairix-mutt-display-command "mairix-profile --view novell %search% &")
 '(org-mairix-open-command "mairix-profile novell %args% '%search%'")
 '(org-mairix-threaded-links nil)
 '(org-remember-templates (quote (("new personal NEXT action" 110 "* NEXT %?" "~/roaming/TODO.org" nil) ("new work NEXT action" 78 "* NEXT %?" "~/ifolder/TODO.org" nil) ("NEXT from personal mail" 109 "%!* NEXT [#B] %?%^{description of personal TODO}
  %[~/.org-mairix-link]" "~/roaming/TODO.org" nil) ("NEXT from work mail" 77 "%!* NEXT [#B] %?%^{description of work TODO}
  %[~/.org-mairix-link]" "~/ifolder/TODO.org" nil) ("work learning material" 76 "* SOMEDAY %?%[~/.org-mairix-link]	:learning:" "~/ifolder/TODO.org" nil) ("personal task DONE" 100 "%!* DONE %?%^{work task done}" "~/ifolder/TODO.org" nil) ("work task DONE" 68 "%!* DONE %?%^{work task done}" "~/ifolder/TODO.org" nil) ("nuisance phone call" 88 "%!* %T %?%^{description}" "~/roaming/notes/NuisanceCalls.org" nil))))
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-stuck-projects (quote ("/PROJECT" ("TODO" "NEXT" "NEXTACTION" "STARTED") nil "")))
 '(org-subheading-todo-alist (quote (("PROJECT" . "NEXT") ("NEXT" . "NEXT"))))
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels t)
 '(org-todo-interpretation (quote type))
 '(org-todo-keyword-faces (quote (("STARTED" :foreground "LimeGreen" :weight bold) ("CHASE" :foreground "DarkOrange" :weight bold) ("WAITING" :foreground "#ffe000" :weight bold) ("PROJECT" :foreground "purple1" :background "AntiqueWhite1" :weight bold) ("SOMEDAY" :foreground "gray60" :weight bold) ("MAYBE" :foreground "gray85" :weight bold) ("CANCELLED" :foreground "black" :strike-through t))))
 '(org-todo-keywords (quote ((sequence "NEXT(n)" "STARTED(>)" "|" "DONE(d)") (sequence "PROJECT(p)" "PROJDONE(P)") (sequence "WAITING(w@)" "CHASE(C@)" "|") (sequence "SOMEDAY(s)" "MAYBE(m)" "|" "CANCELLED(c@)"))))
 '(org-use-fast-todo-selection t)
 '(org-use-property-inheritance (quote ("CRYPTKEY" "CATEGORY")))
 '(outline-auto-activation t)
 '(planner-use-day-pages t)
 '(ps-lpr-command "kprinter")
 '(ps-paper-type (quote a4))
 '(ps-print-color-p (quote black-white))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(remember-mode-hook (quote (org-remember-apply-template)))
 '(require-final-newline nil)
 '(rst-toc-insert-number-separator ". ")
 '(safe-local-variable-values (quote ((org-drawers quote ("PROPERTIES" "HIDE")) (byte-compile-warnings redefine callargs free-vars unresolved obsolete noruntime) (auto-recompile))))
 '(save-abbrevs (quote silently))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 10)
 '(scroll-margin 20)
 '(scroll-preserve-screen-position t)
 '(search-upper-case t)
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-ring-bell-on-mismatch nil)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.emacs\\.backup\\)\\'")
 '(tidy-shell-command "htmltidy")
 '(tla-non-recursive-inventory nil)
 '(uniquify-after-kill-buffer-p nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background "nil")
 '(vc-follow-symlinks t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(custom-face-tag ((t (:weight bold :height 1.44 :family "helvetica"))))
 '(custom-group-tag ((t (:weight bold :height 1.6 :family "helvetica"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold :height 1.4 :family "helvetica"))))
 '(cvs-msg-face ((t (:slant italic))))
 '(org-done ((t (:background "ForestGreen" :foreground "snow1" :weight bold))))
 '(rpm-spec-dir-face ((((class color) (background light)) (:foreground "olive drab")))))
