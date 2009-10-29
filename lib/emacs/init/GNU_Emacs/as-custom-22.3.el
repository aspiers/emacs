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
 '(LilyPond-pdf-command "kpdf")
 '(adaptive-fill-regexp "[ 	]\\{,10\\}\\([-!|#%;>*·•‣⁃◦]+[ 	]*\\)*")
 '(align-dq-string-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode python-mode)))
 '(align-exclude-rules-list (quote ((exc-dq-string (regexp . "\"\\([^\"
]+\\)\"") (repeat . t) (modes . align-dq-string-modes)) (exc-sq-string (regexp . "'\\([^'
]+\\)'") (repeat . t) (modes . align-sq-string-modes)) (exc-open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^ 	
\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (exc-c-comment (regexp . "/\\*\\(.+\\)\\*/") (repeat . t) (modes . align-c++-modes)) (exc-c-func-params (regexp . "(\\([^)
]+\\))") (repeat . t) (modes . align-c++-modes)) (exc-c-macro (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$") (group . 2) (modes . align-c++-modes)) (exc-perl-comment (regexp . "^\\s-*#.*$") (modes . align-perl-modes)))))
 '(align-rules-list (quote ((org-in-buffer-settings (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+") (modes quote (org-mode))) (lisp-second-arg (regexp . "\\(^\\s-+[^( 	
]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)") (group . 3) (modes . align-lisp-modes) (run-if lambda nil current-prefix-arg)) (lisp-alist-dot (regexp . "\\(\\s-*\\)\\.\\(\\s-*\\)") (group 1 2) (modes . align-lisp-modes)) (open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^ 	
\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (c-macro-definition (regexp . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)") (modes . align-c++-modes)) (c-variable-declaration (regexp . "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|=[^=
].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?\\s-*[;,]\\|)\\s-*$\\)") (group . 1) (modes . align-c++-modes) (justify . t) (valid lambda nil (not (or (save-excursion (goto-char (match-beginning 1)) (backward-word 1) (looking-at "\\(goto\\|return\\|new\\|delete\\|throw\\)")) (if (and (boundp (quote font-lock-mode)) font-lock-mode) (eq (get-text-property (point) (quote face)) (quote font-lock-comment-face)) (eq (caar (c-guess-basic-syntax)) (quote c))))))) (c-assignment (regexp . "[^-=!^&*+<>/| 	
]\\(\\s-*[-=!^&*+<>/|]*\\)=\\(\\s-*\\)\\([^= 	
]\\|$\\)") (group 1 2) (modes . align-c++-modes) (justify . t) (tab-stop)) (perl-assignment (regexp . "[^=!^&*-+<>/| 	
]\\(\\s-*\\)=[~>]?\\(\\s-*\\)\\([^>= 	
]\\|$\\)") (group 1 2) (modes . align-perl-modes) (tab-stop)) (python-assignment (regexp . "[^=!<> 	
]\\(\\s-*\\)=\\(\\s-*\\)\\([^>= 	
]\\|$\\)") (group 1 2) (modes quote (python-mode)) (tab-stop)) (make-assignment (regexp . "^\\s-*\\w+\\(\\s-*\\):?=\\(\\s-*\\)\\([^	
 \\\\]\\|$\\)") (group 1 2) (modes quote (makefile-mode)) (tab-stop)) (c-comma-delimiter (regexp . ",\\(\\s-*\\)[^/ 	
]") (repeat . t) (modes . align-c++-modes) (run-if lambda nil current-prefix-arg)) (basic-comma-delimiter (regexp . ",\\(\\s-*\\)[^# 	
]") (repeat . t) (modes append align-perl-modes (quote (python-mode))) (run-if lambda nil current-prefix-arg)) (c++-comment (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$") (modes . align-c++-modes) (column . comment-column) (valid lambda nil (save-excursion (goto-char (match-beginning 1)) (not (bolp))))) (c-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-c++-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(/[*/]\\|$\\)")))) (perl-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-perl-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\)")))) (python-chain-logic (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)") (modes quote (python-mode)) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\|\\\\\\)")))) (c-macro-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes . align-c++-modes) (column . c-backslash-column)) (basic-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes quote (python-mode makefile-mode))) (tex-record-separator (regexp lambda (end reverse) (align-match-tex-pattern "&" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t)) (tex-tabbing-separator (regexp lambda (end reverse) (align-match-tex-pattern "\\\\[=>]" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t) (run-if lambda nil (eq major-mode (quote latex-mode)))) (tex-record-break (regexp . "\\(\\s-*\\)\\\\\\\\") (modes . align-tex-modes)) (text-column (regexp . "\\(^\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)") (group . 2) (modes . align-text-modes) (repeat . t) (run-if lambda nil (and current-prefix-arg (not (eq (quote -) current-prefix-arg))))) (text-dollar-figure (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.") (modes . align-text-modes) (justify . t) (run-if lambda nil (eq (quote -) current-prefix-arg))) (css-declaration (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;") (group 1) (modes quote (css-mode html-mode))))))
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
 '(erc-modules (quote (autojoin button completion fill irccontrols log match menu netsplit noncommands readonly ring stamp track)))
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
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-case-fold nil)
 '(ido-max-prompt-path 0.8)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
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
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-custom-commands (quote (("O" "PlateSpin Orchestrate TODOs" alltodo "" ((org-agenda-files (quote ("~/ifolder/notes/PSO.org" "~/ifolder/notes/PSOcodebase.org"))))) ("e" "eventbook TODOs" alltodo "" ((org-agenda-files (quote ("~/roaming/RotC/eventbook/design.org"))))) ("d" "daily review" ((tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]" (quote scheduled)))))) (tags-todo "officehrs" ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#[AB]\\]" (quote scheduled)))))) (agenda "" ((org-agenda-ndays 3))) (tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]" (quote scheduled))))))) ((org-agenda-compact-blocks t) (org-agenda-skip-function (lambda nil (and nil (org-agenda-skip-entry-if (quote deadline) (quote scheduled))))))) ("pd" "personal daily review" ((tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]" (quote scheduled)))))) (tags-todo "officehrs+CATEGORY=\"personal\"" ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#[AB]\\]" (quote scheduled)))))) (agenda "" ((org-agenda-ndays 3) (org-agenda-skip-function (as-org-agenda-skip-select-category-function "personal")))) (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]" (quote scheduled))))))) ((org-agenda-compact-blocks t) (org-agenda-skip-function (lambda nil (and nil (org-agenda-skip-entry-if (quote deadline) (quote scheduled))))) (org-agenda-prefix-format " "))) ("wd" "Novell daily review" ((tags-todo "+CATEGORY=\"Novell\"/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]" (quote scheduled)))))) (tags-todo "officehrs+CATEGORY=\"Novell\"" ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#[AB]\\]" (quote scheduled)))))) (agenda "" ((org-agenda-ndays 3) (org-agenda-skip-function (as-org-agenda-skip-select-category-function "Novell")))) (tags-todo "+CATEGORY=\"Novell\"/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]" (quote scheduled))))))) ((org-agenda-compact-blocks t) (org-agenda-skip-function (lambda nil (and nil (org-agenda-skip-entry-if (quote deadline) (quote scheduled))))) (org-agenda-prefix-format " "))) ("7" "weekly review" ((todo "CHASE" ((org-agenda-overriding-header "Items to CHASE"))) (todo "WAITING" ((org-agenda-overriding-header "Items still WAITING on somebody"))) (tags-todo "Ten" ((org-agenda-overriding-header "Ten UK"))) (stuck "" nil) (tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]" (quote scheduled)))))) (tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "Unscheduled #C TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#C\\]" (quote scheduled))))))) ((org-agenda-compact-blocks t)) nil) ("w" . "work TODOs") ("p" . "personal TODOs") ("@" . "TODOs by context") ("t" . "TODOs by time constraint") ("s" . "TODOs by ETC") ("#" . "TODOs by priority") ("P" "stuck projects" stuck "" nil) ("# " "missing priorities" tags-todo "/-PROJECT-SOMEDAY-MAYBE" ((org-agenda-overriding-header "TODOs missing priorities") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) "\\=.*\\[#[A-Z]\\]"))))) ("s " "missing time estimates" tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "TODOs missing time estimate") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) ":sub"))))) ("@ " "missing contexts" tags-todo "/NEXT|STARTED" ((org-agenda-overriding-header "TODOs missing context") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote regexp) ":@[a-zA-Z]"))))) ("#a" "priority #A tasks" tags "" ((org-agenda-overriding-header "priority #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))))) ("#A" "priority #A NEXT actions" tags "/PROJECT|NEXT|STARTED" ((org-agenda-overriding-header "priority #A TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))))) ("#b" "priority #B tasks" tags "" ((org-agenda-overriding-header "priority #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]"))))) ("#B" "priority #B NEXT actions" tags "/PROJECT|NEXT" ((org-agenda-overriding-header "priority #B TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#B\\]"))))) ("#c" "priority #C tasks" tags "" ((org-agenda-overriding-header "priority #C TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#C\\]"))))) ("#C" "priority #C NEXT actions" tags "/PROJECT|NEXT|STARTED" ((org-agenda-overriding-header "priority #C TODOs") (org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#C\\]"))))) ("T" "" tags "Ten" nil) ("s1" "" tags "sub10" nil) ("s2" "" tags "sub120" nil) ("s3" "" tags "sub30" nil) ("s4" "" tags "sub4" nil) ("s6" "" tags "sub60" nil) ("sd" "" tags "subday" nil) ("tO" "within office hours" tags-todo "officehrs" nil) ("tS" "Saturday" tags-todo "Saturday" nil) ("@h" "at home" tags-todo "@home|@internet|@offline|@phone" ((org-agenda-overriding-header "at home"))) ("@B" "in Bracknell office" tags-todo "@Bracknell" nil) ("@C" "in Canary Wharf" tags-todo "@CanaryWharf" nil) ("@L" "in London" tags-todo "@London" nil) ("@?" "elsewhere" tags-todo "-@Bracknell-@London-@CanaryWharf-@phone-@internet-@offline" ((org-agenda-overriding-header "elsewhere"))) ("@i" "internet (online)" tags-todo "@internet" nil) ("@0" "offline (but at a computer)" tags-todo "@offline" nil) ("@p" "can make phone calls" tags-todo "@phone" nil) ("@." "current context" (lambda (a) (error "Not implemented yet")) "" nil) ("-" "easy" tags-todo "easy" nil) ("p-" "easy personal tasks" tags-todo "+easy+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("w-" "easy Novell tasks" tags-todo "+easy+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("pa" "personal admin" tags-todo "+admin+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("po" "personal organisation" tags-todo "+admin+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pc" "personal computer" tags-todo "+computer+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pF" "personal F/OSS" tags-todo "+FOSS+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("p$" "personal finance" tags-todo "+finance+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pH" "personal homeimprovement" tags-todo "+homeimprovement+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pf" "personal fun" tags-todo "+fun+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pm" "personal music" tags-todo "+music+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pR" "personal OWRA" tags-todo "+OWRA" ((org-agenda-prefix-format ""))) ("ps" "personal social" tags-todo "+social+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pt" "personal training" tags-todo "+training+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("pw" "personal welfare" tags-todo "+welfare+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("p*" "personal community" tags-todo "+community+CATEGORY=\"personal\"" ((org-agenda-prefix-format ""))) ("wa" "Novell admin" tags-todo "+admin+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wo" "Novell org" tags-todo "+org+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wc" "Novell computer" tags-todo "+computer+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wD" "Novell direct" tags-todo "+direct+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wP" "Novell partner" tags-todo "+partner+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wt" "Novell TAM" tags-todo "+tam+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wE" "Novell enablement" tags-todo "+enablement+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wT" "Novell TS community" tags-todo "+TS+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wl" "Novell learning" tags-todo "+learning+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wF" "Novell F/OSS" tags-todo "+FOSS+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wC" "Novell competitive" tags-todo "+competitive+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("wm" "Novell marketing" tags-todo "+marketing+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("we" "Novell engineering/BU" tags-todo "+eng+CATEGORY=\"Novell\"" ((org-agenda-prefix-format ""))) ("c" "CHASE" tags-todo "/CHASE" nil) ("W" "WAITING" tags-todo "/WAITING" nil) ("A" "admin" tags-todo "admin" nil) ("z" "personal agenda" agenda "CATEGORY=\"personal\"" nil) ("o" "org" tags-todo "org" nil))))
 '(org-agenda-deadline-leaders (quote ("Deadline: " "In %3dd: ")))
 '(org-agenda-files (quote ("~/ifolder/TODO.org" "~/roaming/TODO.org" "~/roaming/diary.org")))
 '(org-agenda-fontify-priorities (quote ((65 (:bold t :weight bold)))))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 31)
 '(org-agenda-prefix-format (quote ((agenda . "  %-9:c%?-12t% s") (timeline . "  % s") (todo . "  %-9:c") (tags . "  %-9:c"))))
 '(org-agenda-scheduled-leaders (quote ("Sched: " "Sched.%2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down category-keep) (todo priority-down category-keep) (tags priority-down category-keep))))
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-time-grid (quote (nil "----------------" (800 1000 1200 1400 1600 1800 2000))))
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.6)))
 '(org-archive-save-context-info (quote (time file category todo priority itags olpath ltags)))
 '(org-clock-idle-time 5)
 '(org-clock-in-switch-to-state (quote as-org-clock-in-switch-to-state))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-persist-query-save t)
 '(org-clock-sound "/usr/share/sounds/KDE_Error_3.ogg")
 '(org-columns-default-format "%TODO %PRIORITY %40ITEM(Task) %Effort(ETC){:} %CLOCKSUM(Taken){:} %TAGS(Tags)")
 '(org-combined-agenda-icalendar-file "~/ifolder/org.ics")
 '(org-default-extensions nil)
 '(org-default-notes-file "~/roaming/TODO.org")
 '(org-directory "~/roaming")
 '(org-disputed-keys (quote (([(control shift right)] . [(control shift n)]) ([(control shift left)] . [(control shift p)]) ([(control 44)] . [(control 39)]) ([(control tab)] . [(control meta tab)]))))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "HIDE" "STATE")))
 '(org-email-link-description-format "mail %c: %.30s")
 '(org-emphasis-regexp-components (quote (" 	('\"" "- 	.,:?;'\")" " 	
,\"'" "." 5)))
 '(org-export-html-style "<style type=\"text/css\">
  html {
	font-family: Times, serif;
	font-size: 12pt;
  }
  .title { text-align: center; }
  .initialtext {
        text-align: center;
        font-size: 16pt;
  }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	<!--border: 1pt solid #ADB9CC;-->
  }
</style>")
 '(org-export-with-sub-superscripts (quote {}))
 '(org-from-is-user-regexp "\\<\\(adam@spiers\\.net\\|Adam Spiers\\|@\\(adamspiers\\|tigerpig\\)\\.org\\|aspiers@novell\\.com\\)\\>")
 '(org-global-properties (quote (("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00 16:00"))))
 '(org-goto-max-level 7)
 '(org-hide-leading-stars t)
 '(org-icalendar-store-UID t)
 '(org-icalendar-timezone "Europe/London")
 '(org-link-abbrev-alist (quote (("bug" . "https://bugzilla.novell.com/show_bug.cgi?id="))))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (gnus . gnus-other-frame) (file . find-file))))
 '(org-lowest-priority 69)
 '(org-mairix-augmented-links nil)
 '(org-mairix-display-hook (quote org-mairix-mutt-display-results))
 '(org-mairix-mutt-display-command "mairix-profile --view novell %search% &")
 '(org-mairix-open-command "mairix-profile novell %args% '%search%'")
 '(org-mairix-threaded-links nil)
 '(org-odd-levels-only t)
 '(org-refile-targets (quote ((nil :maxlevel . 3))))
 '(org-refile-use-outline-path t)
 '(org-remember-default-headline "bottom")
 '(org-remember-templates (quote (("new personal NEXT action" 110 "* NEXT %?%&" "~/roaming/TODO.org" top nil) ("new work NEXT action" 78 "* NEXT %?%&" "~/ifolder/TODO.org" top nil) ("NEXT from personal mail" 109 "* NEXT [#B] %?%&%[~/.org-mairix-link]" "~/roaming/TODO.org" top nil) ("NEXT from work mail" 77 "* NEXT [#B] %?%&%[~/.org-mairix-link]" "~/ifolder/TODO.org" top nil) ("new personal diary entry" 100 "* %^t %!%?%&%[~/.org-mairix-link]" "~/roaming/diary.org" top nil) ("work learning material" 76 "* SOMEDAY %?%&%[~/.org-mairix-link]	:learning:" "~/ifolder/TODO.org" top nil) ("personal task DONE" 100 "* DONE %?%&%!" "~/roaming/DONE.org" bottom nil) ("work task DONE" 68 "* DONE %?%!%&" "~/ifolder/DONE.org" bottom nil) ("nuisance phone call" 88 "* %T %?%!%&" "~/roaming/notes/NuisanceCalls.org" bottom nil) ("Wipfel learning" 119 "* SOMEDAY %[~/.org-mairix-link]%&%!" "~/ifolder/TODO.org" "PROJECT rwipfel" nil) ("PSO standup calls etc." 83 "*** %t
***** me
******* %?%&" "~/ifolder/notes/PSO.org" "stand-up calls and sprint planning" nil) ("project" 112 "* PROJECT %^{project title}
*** why
    - %?%&
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
***** How?" nil top nil))))
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-stuck-projects (quote ("/PROJECT" ("TODO" "NEXT" "NEXTACTION" "STARTED") nil "")))
 '(org-subheading-todo-alist (quote (("PROJECT" . "NEXT") ("NEXT" . "NEXT"))))
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels t)
 '(org-time-stamp-rounding-minutes (quote (15 5)))
 '(org-todo-interpretation (quote type))
 '(org-todo-keyword-faces (quote (("STARTED" :foreground "LimeGreen" :weight bold) ("CHASE" :foreground "DarkOrange" :weight bold) ("WAITING" :foreground "#ffe000" :weight bold) ("PROJECT" :foreground "purple1" :background "AntiqueWhite1" :weight bold) ("SOMEDAY" :foreground "gray60" :weight bold) ("MAYBE" :foreground "gray85" :weight bold) ("CANCELLED" :foreground "black" :strike-through t))))
 '(org-todo-keywords (quote ((sequence "NEXT(n)" "STARTED(>)" "|" "DONE(d)") (sequence "PROJECT(p)" "PROJDONE(P)") (sequence "ONGOING(o)" "WAITING(w@)" "CHASE(C@)" "|") (sequence "SOMEDAY(s)" "MAYBE(m)" "|" "CANCELLED(c)"))))
 '(org-use-extra-keys t)
 '(org-use-fast-todo-selection t)
 '(org-use-property-inheritance (quote ("CRYPTKEY" "CATEGORY")))
 '(org-yank-adjusted-subtrees t)
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
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren t)
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
 '(erc-current-nick-face ((t (:background "green yellow" :weight bold))))
 '(erc-input-face ((t (:foreground "DarkOrange1" :weight bold))))
 '(erc-my-nick-face ((t (:background "plum1" :foreground "black"))))
 '(org-done ((t (:background "ForestGreen" :foreground "snow1" :weight bold))))
 '(rpm-spec-dir-face ((((class color) (background light)) (:foreground "olive drab")))))
