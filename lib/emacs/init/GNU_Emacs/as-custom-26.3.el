(message "custom-set-variables starting")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("~/share/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(LilyPond-command-alist
   (quote
    (quote
     (("LilyPond" "lilypond %s" "%s" "%l" "View")
      ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
      ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
      ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
      ("View" "pdf %f")
      ("ViewPDF" "pdf %f")
      ("ViewPS" "gv --watch %p")
      ("Midi" "")
      ("MidiAll" "")))))
 '(LilyPond-pdf-command "okular")
 '(adaptive-fill-regexp "[ 	]\\{,10\\}\\([-!|#%;>*·•‣⁃◦]+[ 	]*\\)*")
 '(align-dq-string-modes
   (quote
    (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode python-mode)))
 '(align-exclude-rules-list
   (quote
    ((exc-dq-string
      (regexp . "\"\\([^\"
]+\\)\"")
      (repeat . t)
      (modes . align-dq-string-modes))
     (exc-sq-string
      (regexp . "'\\([^'
]+\\)'")
      (repeat . t)
      (modes . align-sq-string-modes))
     (exc-open-comment
      (regexp lambda
              (end reverse)
              (funcall
               (if reverse
                   (quote re-search-backward)
                 (quote re-search-forward))
               (concat "[^ 	
\\\\]"
                       (regexp-quote comment-start)
                       "\\(.+\\)$")
               end t))
      (modes . align-open-comment-modes))
     (exc-c-comment
      (regexp . "/\\*\\(.+\\)\\*/")
      (repeat . t)
      (modes . align-c++-modes))
     (exc-c-func-params
      (regexp . "(\\([^)
]+\\))")
      (repeat . t)
      (modes . align-c++-modes))
     (exc-c-macro
      (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$")
      (group . 2)
      (modes . align-c++-modes))
     (exc-perl-comment
      (regexp . "^\\s-*#.*$")
      (modes . align-perl-modes)))))
 '(align-rules-list
   (quote
    ((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes quote
             (org-mode)))
     (lisp-second-arg
      (regexp . "\\(^\\s-+[^( 	
]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)")
      (group . 3)
      (modes . align-lisp-modes)
      (run-if lambda nil current-prefix-arg))
     (lisp-alist-dot
      (regexp . "\\(\\s-*\\)\\.\\(\\s-*\\)")
      (group 1 2)
      (modes . align-lisp-modes))
     (open-comment
      (regexp lambda
              (end reverse)
              (funcall
               (if reverse
                   (quote re-search-backward)
                 (quote re-search-forward))
               (concat "[^ 	
\\\\]"
                       (regexp-quote comment-start)
                       "\\(.+\\)$")
               end t))
      (modes . align-open-comment-modes))
     (c-macro-definition
      (regexp . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)")
      (modes . align-c++-modes))
     (c-variable-declaration
      (regexp . "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|=[^=
].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?\\s-*[;,]\\|)\\s-*$\\)")
      (group . 1)
      (modes . align-c++-modes)
      (justify . t)
      (valid lambda nil
             (not
              (or
               (save-excursion
                 (goto-char
                  (match-beginning 1))
                 (backward-word 1)
                 (looking-at "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
               (if
                   (and
                    (boundp
                     (quote font-lock-mode))
                    font-lock-mode)
                   (eq
                    (get-text-property
                     (point)
                     (quote face))
                    (quote font-lock-comment-face))
                 (eq
                  (caar
                   (c-guess-basic-syntax))
                  (quote c)))))))
     (c-assignment
      (regexp . "[^-=!^&*+<>/| 	
]\\(\\s-*[-=!^&*+<>/|]*\\)=\\(\\s-*\\)\\([^= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-c++-modes)
      (justify . t)
      (tab-stop))
     (perl-assignment
      (regexp . "[^=!^&*-+<>/| 	
]\\(\\s-*\\)=[~>]?\\(\\s-*\\)\\([^>= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-perl-modes)
      (tab-stop))
     (python-assignment
      (regexp . "[^=!<> 	
]\\(\\s-*\\)=\\(\\s-*\\)\\([^>= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-python-modes)
      (tab-stop))
     (make-assignment
      (regexp . "^\\s-*\\w+\\(\\s-*\\):?=\\(\\s-*\\)\\([^	
 \\\\]\\|$\\)")
      (group 1 2)
      (modes quote
             (makefile-mode))
      (tab-stop))
     (c-comma-delimiter
      (regexp . ",\\(\\s-*\\)[^/ 	
]")
      (repeat . t)
      (modes . align-c++-modes)
      (run-if lambda nil current-prefix-arg))
     (basic-comma-delimiter
      (regexp . ",\\(\\s-*\\)[^# 	
]")
      (repeat . t)
      (modes append align-perl-modes
             (quote
              (python-mode)))
      (run-if lambda nil current-prefix-arg))
     (c++-comment
      (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$")
      (modes . align-c++-modes)
      (column . comment-column)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-beginning 1))
               (not
                (bolp)))))
     (c-chain-logic
      (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
      (modes . align-c++-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(/[*/]\\|$\\)"))))
     (perl-chain-logic
      (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
      (modes . align-perl-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(#\\|$\\)"))))
     (python-chain-logic
      (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)")
      (modes . align-python-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(#\\|$\\|\\\\\\)"))))
     (c-macro-line-continuation
      (regexp . "\\(\\s-*\\)\\\\$")
      (modes . align-c++-modes)
      (column . c-backslash-column))
     (basic-line-continuation
      (regexp . "\\(\\s-*\\)\\\\$")
      (modes append align-python-modes
             (quote
              (makefile-mode))))
     (tex-record-separator
      (regexp lambda
              (end reverse)
              (align-match-tex-pattern "&" end reverse))
      (group 1 2)
      (modes . align-tex-modes)
      (repeat . t))
     (tex-tabbing-separator
      (regexp lambda
              (end reverse)
              (align-match-tex-pattern "\\\\[=>]" end reverse))
      (group 1 2)
      (modes . align-tex-modes)
      (repeat . t)
      (run-if lambda nil
              (eq major-mode
                  (quote latex-mode))))
     (tex-record-break
      (regexp . "\\(\\s-*\\)\\\\\\\\")
      (modes . align-tex-modes))
     (text-column
      (regexp . "\\(^\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)")
      (group . 2)
      (modes . align-text-modes)
      (repeat . t)
      (run-if lambda nil
              (and current-prefix-arg
                   (not
                    (eq
                     (quote -)
                     current-prefix-arg)))))
     (text-dollar-figure
      (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.")
      (modes . align-text-modes)
      (justify . t)
      (run-if lambda nil
              (eq
               (quote -)
               current-prefix-arg)))
     (css-declaration
      (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;")
      (group 1)
      (modes quote
             (css-mode html-mode))))))
 '(align-sq-string-modes (quote (python-mode)))
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-revert-check-vc-info nil)
 '(auto-revert-interval 10)
 '(auto-save-interval 120)
 '(backup-directory-alist (quote (("." . ".emacs.backup"))))
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 0.0)
 '(blink-cursor-interval 0.3)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "url-handler")
 '(bs-alternative-configuration "cvs")
 '(bs-attributes-list
   (quote
    (("" 1 1 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Buffer" bs--get-name-length 10 left bs--get-name)
     ("" 1 1 left " ")
     ("Size" 5 8 right bs--get-size-string)
     ("" 1 1 left " ")
     ("Mode" 5 12 right bs--get-mode-name)
     ("" 2 2 left "  ")
     ("File" 12 12 left bs--get-file-name)
     ("" 2 2 left "  "))))
 '(bs-configurations
   (quote
    (("all" nil nil nil nil nil)
     ("files" nil nil ".~[0-9.]+~$" bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last)
     ("cvs" "\\*cvs\\*$" nil "" nil bs--sort-by-name))))
 '(bs-default-sort-name "by nothing")
 '(bs-max-window-height 24)
 '(bs-maximal-buffer-name-column 22)
 '(bs-minimal-buffer-name-column 5)
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(case-fold-search t)
 '(coffee-tab-width 4)
 '(color-theme-is-cumulative t)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(column-number-mode t)
 '(comment-empty-lines (quote (quote eol)))
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".hg/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-autoindent-on-semi t)
 '(cperl-close-paren-offset -2)
 '(cperl-continued-statement-offset 0)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lineup-step 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-under-as-char nil)
 '(custom-enabled-themes (quote (pastels-on-dark-aspiers)))
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes")
 '(cvs-buffer-switch-alist (quote ("diff" "status" "log")))
 '(cvs-buffer-switch-list (quote ("diff" "status" "log")))
 '(cvs-find-file-and-jump t)
 '(cvs-invert-ignore-marks (quote ("diff")))
 '(cvs-parse-ignored-messages
   (quote
    ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" ".*New directory `.*' -- ignored.*$" ".*warning: directory CVS specified in argument.*$" ".*but CVS uses CVS for its own purposes; skipping CVS directory.*$")))
 '(cvs-reuse-cvs-buffer (quote subdir))
 '(cvs-use-fileinfo-caches t)
 '(debugger-batch-max-lines 1000)
 '(delete-old-versions t)
 '(delete-selection-mode nil)
 '(diff-switches "-u")
 '(dired-guess-shell-alist-user
   (quote
    (("\\.pdf\\'" "evince")
     ("\\.\\(docx?\\|pptx?\\|xlsx?|od[stp]\\)\\'" "libreoffice")
     ("\\.jpg\\'" "pinta")
     ("\\.png\\'" "pinta")
     ("\\.java\\'" "idea"))))
 '(dired-kept-versions 0)
 '(dired-listing-switches "-l")
 '(display-time-mode nil)
 '(dvc-tips-enabled nil)
 '(echo-keystrokes 0.01)
 '(ediff-custom-diff-options "-u")
 '(edit-server-port 9292)
 '(el-get-sources
   (quote
    ((:name org2blog :pkgname "aspiers/org2blog")
     (:name smooth-scrolling :pkgname "DarwinAwardWinner/smooth-scrolling"))))
 '(eldoc-minor-mode-string "")
 '(eldoc-mode t t)
 '(emms-player-list (quote (\"mplayer\" \"vlc\")))
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(fast-lock-cache-directories (quote ("~/.emacs-flc")))
 '(fast-lock-minimum-size 4096)
 '(fci-rule-color "gray32")
 '(fci-rule-column 80)
 '(flx-ido-mode t)
 '(flx-ido-threshhold 60000)
 '(folding-mode-prefix-key "")
 '(gc-cons-threshold 200000)
 '(gdb-many-windows t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)
 '(git-commit-without-user-email nil)
 '(git-gutter:lighter "")
 '(git-rebase-auto-advance t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-msf-abbrev-mode t)
 '(global-whitespace-mode t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
    ("C-x 4" "C-x C-k" "C-x n" "C-x r" "C-x v" "C-x 8" "C-x p" "C-c" "C-c i" "C-c g" "C-c m" "C-c t" "C-c C-v" "C-c w" "C-c j" "C-h" "C-S-SPC" "M-o" "zp" "pz" "<key-chord>" "M-s"
     (dired-mode "%" "*" "C-t")
     (org-mode "C-c C-x"))))
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-auto-merge-delay-time 3)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-case-fold nil)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-hacks-mode t)
 '(ido-max-directory-size 100000)
 '(ido-max-prompt-path 0.8)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(ido-vertical-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(iswitchb-case nil)
 '(iswitchb-default-method (quote samewindow))
 '(jit-lock-stealth-nice 0.1)
 '(jit-lock-stealth-time 1)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(lazy-lock-defer-on-scrolling t)
 '(lazy-lock-defer-time 0.1)
 '(lazy-lock-minimum-size 4096)
 '(lazy-lock-stealth-time 15)
 '(lazy-lock-stealth-verbose t)
 '(lua-indent-level 2)
 '(magit-branch-prefer-remote-upstream (quote ("master" "stable/3.0")))
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-display-buffer-function (quote magit-display-buffer-fullscreen))
 '(magit-gitk-executable "/usr/bin/gitk")
 '(magit-log-auto-more t)
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-prefer-remote-upstream t)
 '(magit-push-always-verify t)
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs
   (quote
    ("/home/adam/.GIT" "/home/adam/SUSE/git" "/home/adam/SUSE/cloud/git" "/home/adam/SUSE/cloud/chef/git" "/home/adam/SUSE/cloud/OpenStack/git")))
 '(magit-rewrite-inclusive t)
 '(magit-section-initial-visibility-alist (quote ((stashes . show))))
 '(magit-stage-all-confirm nil)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(magit-status-headers-hook
   (quote
    (magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-remote-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header)))
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote)))
 '(magit-status-show-hashes-in-headers t)
 '(magit-unstage-all-confirm nil)
 '(mail-envelope-from (quote header))
 '(mail-self-blind t)
 '(make-backup-file-name-function (quote as-make-backup-file-name))
 '(mark-even-if-inactive t)
 '(message-default-news-headers
   "From: Adam Spiers <usenet@adamspiers.org>
Reply-To: Adam Spiers <usenet@adamspiers.org>
")
 '(message-log-max 1000)
 '(message-sendmail-f-is-evil t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-yank-at-point t)
 '(msf-abbrev-indent-after-expansion t)
 '(muse-colors-autogen-headings (quote outline))
 '(muse-mode-auto-p nil)
 '(muse-wiki-allow-nonexistent-wikiword t)
 '(muse-wiki-wikiword-regexp "\\<\\([A-Z]+[a-z]+[A-Z]+[a-zA-Z0-9]*+\\)")
 '(mwheel-follow-mouse t)
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-custom-commands
   (quote
    (("b" "bandhand TODOs" alltodo ""
      ((org-agenda-files
        (quote
         ("~/eventbook/design.org")))))
     ("d" "daily review"
      ((tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs"
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))))
     ("pd" "personal daily review"
      ((tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs+CATEGORY=\"personal\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "personal"))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))
       (org-agenda-prefix-format " %?-12t% s")))
     ("wd" "SUSE daily review"
      ((tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs+CATEGORY=\"SUSE\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "SUSE"))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))
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
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
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
                (quote
                 ("+CATEGORY=\"personal\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil "")))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (as-org-agenda-skip-select-category-function "personal"))
       (org-agenda-prefix-format " %?-12t% s"))
      nil)
     ("w7" "SUSE weekly review"
      ((tags-todo "+CATEGORY=\"SUSE\"/CHASE"
                  ((org-agenda-overriding-header "Items to CHASE")))
       (tags-todo "+CATEGORY=\"SUSE\"/WAITING"
                  ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck work projects")
               (org-stuck-projects
                (quote
                 ("+CATEGORY=\"SUSE\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil "")))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
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
          (org-agenda-skip-entry-if
           (quote regexp)
           "\\=.*\\[#[A-Z]\\]")))))
     ("s " "missing time estimates" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing time estimate")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote regexp)
           ":sub")))))
     ("@ " "missing contexts" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing context")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote regexp)
           ":@[a-zA-Z]")))))
     ("#a" "priority #A tasks" tags ""
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))))
     ("#A" "priority #A NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))))
     ("#b" "priority #B tasks" tags ""
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#B\\]")))))
     ("#B" "priority #B NEXT actions" tags "/PROJECT|NEXT"
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#B\\]")))))
     ("#c" "priority #C tasks" tags ""
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#C\\]")))))
     ("#C" "priority #C NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#C\\]")))))
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
     ("w-" "easy SUSE tasks" tags-todo "+easy+CATEGORY=\"SUSE\""
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
     ("wa" "SUSE admin" tags-todo "+admin+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wo" "SUSE org" tags-todo "+org+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wc" "SUSE computer" tags-todo "+computer+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wL" "SUSE learning" tags-todo "+learning+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("c" "CHASE" todo "CHASE" nil)
     ("W" "WAITING" todo "WAITING" nil)
     ("A" "admin" tags-todo "admin" nil)
     ("v" "virtual assistant" tags-todo "virtassist" nil)
     ("z" "personal agenda" agenda "CATEGORY=\"personal\"" nil)
     ("o" "org" tags-todo "org" nil)
     ("pl" "personal log" agenda "DONE"
      ((org-agenda-files
        (quote
         ("~/org/TODO.org" "~/org/DONE.org" "~/eventbook/design.org")))
       (org-agenda-span
        (quote week))
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        (quote
         (closed clock)))
       (org-agenda-entry-types
        (quote
         (:timestamp :sexp)))
       (org-agenda-overriding-header "")))
     ("wl" "work log" agenda "DONE"
      ((org-agenda-files
        (quote
         ("~/SUSE/TODO.org" "~/SUSE/DONE.org")))
       (org-agenda-span
        (quote week))
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        (quote
         (closed clock)))
       (org-agenda-entry-types
        (quote
         (:timestamp :sexp)))
       (org-agenda-overriding-header "")
       (org-agenda-prefix-format "  - "))))))
 '(org-agenda-deadline-leaders (quote ("Deadline: " "In %3dd: " "%2dd ago: ")))
 '(org-agenda-files
   (quote
    ("~/Blocko/TODO.org" "~/SUSE/TODO.org" "~/org/TODO.org")))
 '(org-agenda-fontify-priorities (quote ((65 (:bold t :weight bold)))))
 '(org-agenda-include-deadlines t)
 '(org-agenda-include-diary t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-9:c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-9:c")
     (tags . "  %-9:c"))))
 '(org-agenda-scheduled-leaders (quote ("Sched: " "Sched.%2dx: ")))
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown (quote not-today))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up priority-down category-keep effort-up)
     (todo priority-down category-keep effort-up)
     (tags priority-down category-keep effort-up)
     (search category-keep))))
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-sticky t)
 '(org-agenda-time-grid
   (quote
    (nil "----------------"
         (800 1000 1200 1400 1600 1800 2000))))
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.6)))
 '(org-agenda-window-setup (quote current-window))
 '(org-archive-mark-done t)
 '(org-archive-save-context-info
   (quote
    (time file category todo priority itags olpath ltags)))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (lilypond . t)
     (ruby . t)
     (python . t)
     (shell . t))))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-capture-templates
   (quote
    (("s" "supportconfig madness" entry
      (file+headline "~/SUSE/TODO.org" "individual instances")
      "***** %u %?%[~/.org-mairix-link]" :prepend t :jump-to-captured t)
     ("o" "org mailing list item" entry
      (file+headline "~/org/TODO.org" "to list")
      "* NEXT %?%:annotation" :prepend t)
     ("z" "property test" entry
      (file "~/org/TODO.org")
      "%^{Effort}p" :prepend t)
     ("i" "immediate personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?%:annotation
  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)
     ("I" "immediate work NEXT" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?%:annotation
  SCHEDULED: %T" :prepend t :immediate-finish t :jump-to-captured t :clock-in t)
     ("n" "personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?%:annotation" :prepend t)
     ("N" "work NEXT" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?%:annotation" :prepend t)
     ("m" "NEXT from personal mail" entry
      (file "~/org/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t)
     ("M" "NEXT from work mail" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t)
     ("a" "personal diary entry" entry
      (file "~/org/diary.org")
      "* %^t %?%[~/.org-mairix-link]" :prepend t)
     ("L" "work learning material" entry
      (file "~/SUSE/TODO.org")
      "* SOMEDAY %?%[~/.org-mairix-link]	:learning:" :prepend t)
     ("d" "personal task DONE" entry
      (file "~/org/DONE.org")
      "* DONE %?
  CLOSED: %U")
     ("D" "work task DONE" entry
      (file "~/SUSE/DONE.org")
      "* DONE %?
  CLOSED: %U")
     ("X" "nuisance phone call" entry
      (file "~/org/notes/NuisanceCalls.org")
      "* %T %?")
     ("w" "Wipfel learning" entry
      (file+headline "~/SUSE/TODO.org" "PROJECT rwipfel")
      "* SOMEDAY %[~/.org-mairix-link]" :prepend t)
     ("S" "PSO standup calls etc." entry
      (file+headline "~/SUSE/notes/PSO.org" "logs of stand-up calls and sprint planning")
      "*** %t
***** me
******* %?" :prepend t)
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
***** How?" :prepend t)
     ("P" "Procrastination" entry
      (file "~/org/notes/ProcrastinationLog.org")
      "* %T %^{activity}
  :PROPERTIES:
  :thoughts/feelings: %^{thoughts/feelings}
  :justification: %^{justification}
  :attempted solution: %^{attempted solution}
  :resultant thoughts/feelings: %^{resultant thoughts/feelings}
  :END:"))))
 '(org-catch-invisible-edits (quote smart))
 '(org-clock-idle-time 5)
 '(org-clock-in-switch-to-state (quote as-org-clock-in-switch-to-state))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-persist-query-save t)
 '(org-clock-sound "~/lib/emacs/utils/org-clock-sound-quiet.wav")
 '(org-clone-delete-id t)
 '(org-columns-default-format
   "%TODO %PRIORITY %40ITEM(Task) %Effort(ETC){:} %CLOCKSUM(Taken){:} %TAGS(Tags)")
 '(org-combined-agenda-icalendar-file "~/SUSE/org.ics")
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-deadline-warning-days 3)
 '(org-default-extensions nil t)
 '(org-default-notes-file "~/org/TODO.org")
 '(org-directory "~/org")
 '(org-disputed-keys
   (quote
    (([(control shift right)]
      .
      [(control shift n)])
     ([(control shift left)]
      .
      [(control shift p)])
     ([(control 44)]
      .
      [(control 39)])
     ([(control tab)]
      .
      [(control meta tab)]))))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "HIDE" "STATE")))
 '(org-email-link-description-format "mail %c: %.30s")
 '(org-emphasis-regexp-components (quote (" 	('\"" "- 	.,:?;'\")" " 	
,\"'" "." 5)) t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-dispatch-use-expert-ui nil)
 '(org-export-html-style
   "<style type=\"text/css\">
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
 '(org-extend-today-until 3)
 '(org-from-is-user-regexp
   "\\<\\(adam@spiers\\.net\\|Adam Spiers\\|@\\(adamspiers\\|tigerpig\\)\\.org\\|aspiers@\\(novell\\|suse\\)\\.com\\)\\>")
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00 16:00 0"))))
 '(org-goto-interface (quote outline))
 '(org-goto-max-level 7)
 '(org-hide-leading-stars t)
 '(org-html-allow-name-attribute-in-anchors t)
 '(org-icalendar-store-UID t)
 '(org-icalendar-timezone "Europe/London")
 '(org-link-abbrev-alist
   (quote
    (("bnc" . "https://bugzilla.novell.com/show_bug.cgi?id=")
     ("bug" . "https://bugzilla.novell.com/show_bug.cgi?id=")
     ("psorev" . "https://svn.innerweb.novell.com/viewsvn/pso-source?view=rev&revision="))))
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (gnus . gnus-other-frame)
     (file . find-file))))
 '(org-log-done (quote time))
 '(org-lowest-priority 69)
 '(org-mairix-augmented-links nil)
 '(org-mairix-display-hook (quote org-mairix-mutt-display-results))
 '(org-mairix-mutt-display-command "mairix-profile --view novell %search% &")
 '(org-mairix-open-command "mairix-profile novell %args% '%search%'")
 '(org-mairix-threaded-links nil)
 '(org-mobile-agendas (quote ("d" "pd" "wd" "7" "p7" "w7" "c" "W" "e")))
 '(org-mobile-directory "/scpx:adamspiers.org:org/")
 '(org-mobile-files (quote ("~/org/mobile")))
 '(org-mobile-inbox-for-pull "~/org/org-mobile-incoming.org")
 '(org-modules
   (quote
    (org-habit org-id org-info org-mouse org-protocol org-w3m org-man org-timer org-docview orgit)))
 '(org-odd-levels-only t)
 '(org-priority-faces (quote ((65 :weight bold))))
 '(org-publish-project-alist
   (quote
    (("OWRA" :components
      ("OWRA-2008" "OWRA-2009" "OWRA-2010" "OWRA-2011" "OWRA-2012" "OWRA-2013"))
     ("OWRA-2008" :base-directory "~/OWRA/meetings/2008" :publishing-directory "~/OWRA/meetings/2008" :publishing-function org-html-publish-to-html)
     ("OWRA-2009" :base-directory "~/OWRA/meetings/2009" :publishing-directory "~/OWRA/meetings/2009" :publishing-function org-html-publish-to-html)
     ("OWRA-2010" :base-directory "~/OWRA/meetings/2010" :publishing-directory "~/OWRA/meetings/2010" :publishing-function org-html-publish-to-html)
     ("OWRA-2011" :base-directory "~/OWRA/meetings/2011" :publishing-directory "~/OWRA/meetings/2011" :publishing-function org-html-publish-to-html)
     ("OWRA-2012" :base-directory "~/OWRA/meetings/2012" :publishing-directory "~/OWRA/meetings/2012" :publishing-function org-html-publish-to-html)
     ("OWRA-2013" :base-directory "~/OWRA/meetings/2013" :publishing-directory "~/OWRA/meetings/2013" :publishing-function org-html-publish-to-html)
     ("OWRA-2014" :base-directory "~/OWRA/meetings/2014" :publishing-directory "~/OWRA/meetings/2014" :publishing-function org-html-publish-to-html)
     ("OWRA-2015" :base-directory "~/OWRA/meetings/2015" :publishing-directory "~/OWRA/meetings/2015" :publishing-function org-html-publish-to-html))))
 '(org-refile-targets (quote ((nil :maxlevel . 3))))
 '(org-refile-use-outline-path t)
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-stuck-projects
   (quote
    ("/PROJECT"
     ("TODO" "NEXT" "NEXTACTION" "STARTED")
     nil "")))
 '(org-subheading-todo-alist (quote (("PROJECT" . "NEXT") ("NEXT" . "NEXT"))))
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels t)
 '(org-time-stamp-rounding-minutes (quote (15 5)))
 '(org-todo-interpretation (quote type))
 '(org-todo-keyword-faces
   (quote
    (("STARTED" :foreground "LimeGreen" :weight bold)
     ("ONGOING" :foreground "DarkOrange" :weight bold)
     ("CHASE" :background "orange red" :weight bold)
     ("WAITING" :foreground "#ffe000" :weight bold)
     ("PROJECT" :foreground "purple1" :weight bold)
     ("SOMEDAY" :foreground "gray70" :weight bold)
     ("MAYBE" :foreground "gray55" :weight bold)
     ("CANCELLED" :foreground "gray35" :strike-through t)
     ("BUG" :foreground "red" :weight bold)
     ("WORKAROUND" :foreground "dark magenta" :weight bold)
     ("USABILITY" :foreground "medium sea green" :weight bold)
     ("HOWTO" :foreground "slate blue" :weight bold)
     ("IGNORE" :foreground "slate grey" :strike-through t))))
 '(org-todo-keywords
   (quote
    ((sequence "NEXT(n)" "STARTED(>)" "|" "DONE(d)")
     (sequence "PROJECT(p)" "PROJDONE(P)")
     (sequence "ONGOING(o)" "WAITING(w@)" "CHASE(C@)" "|")
     (sequence "SOMEDAY(s)" "MAYBE(m)" "|" "CANCELLED(c@)"))))
 '(org-use-extra-keys t)
 '(org-use-fast-todo-selection t)
 '(org-use-property-inheritance (quote ("CRYPTKEY" "CATEGORY")))
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts (quote {}))
 '(org-yank-adjusted-subtrees t)
 '(org2blog/wp-blog-alist
   (quote
    (("blog.adamspiers.org" :url "https://blog.adamspiers.org/xmlrpc.php" :username "adam")
     ("suse.com" :url "https://www.suse.com/c/xmlrpc.php"))))
 '(org2blog/wp-image-thumbnails nil)
 '(org2blog/wp-sourcecode-default-params "")
 '(org2blog/wp-use-sourcecode-shortcode t)
 '(outline-auto-activation t)
 '(package-archives
   (quote
    (("Org" . "http://orgmode.org/elpa/")
     ("ELPA" . "http://tromey.com/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("MELPA" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (company-lua lua-mode forge org-bullets ocp-indent pastels-on-dark-theme sunlight-theme zenburn-theme color-theme-modern amx use-package orgit ghub quelpa-use-package quelpa flycheck-ocaml tuareg caml go-gopath go-mode flymake-solidity solidity-mode org-crypt use-package-ensure-system-package use-package-chords use-package-el-get messages-are-flowing yasnippet projectile tidy git-timemachine git-gutter+ git-gutter-fringe+ auto-package-update git-gutter-fringe projectile-codesearch projectile-variable ruby-mode gmpl-mode magit-gerrit sclang-extensions magit magit-annex org-plus-contrib yaml-mode web-mode vc-osc undo-tree switch-window smex smartrep smartparens smart-mode-line sass-mode rubocop rsense rpm-spec-mode req-package region-bindings-mode rainbow-delimiters python phi-search-mc paredit org2blog org-sync org-magit muttrc-mode mmm-mode mediawiki markdown-mode+ magit-topgit macrostep kmacro-decision keywiz key-chord iy-go-to-char idomenu ido-vertical-mode ido-ubiquitous hideshow-org guide-key goto-chg git-gutter gist gerrit-download folding flymake-shell flymake-sass flymake-ruby flymake-css flycheck-package flx-ido fill-column-indicator feature-mode expand-region emms edit-server-htmlize company coffee-mode bundler beeminder beacon autotest asciidoc apache-mode ace-jump-mode)))
 '(passive-voice nil)
 '(planner-use-day-pages t)
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-keymap-prefix "p")
 '(projectile-switch-project-action (quote projectile-vc))
 '(projectile-use-git-grep t)
 '(ps-lpr-command "kprinter")
 '(ps-paper-type (quote a4))
 '(ps-print-color-p (quote black-white))
 '(quelpa-update-melpa-p nil)
 '(quelpa-upgrade-p nil)
 '(region-bindings-mode-disable-predicates (quote ((lambda nil buffer-read-only))))
 '(region-bindings-mode-enabled-modes
   (quote
    (c-mode shell-script-mode emacs-lisp-mode ruby-mode python-mode org-mode)))
 '(remote-file-name-inhibit-cache 1800)
 '(req-package-log-level (quote debug))
 '(require-final-newline nil)
 '(rm-blacklist
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit")))
 '(rm-excluded-modes
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit")))
 '(rst-toc-insert-number-separator ". ")
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(safe-local-variable-values
   (quote
    ((org-drawers quote
                  ("PROPERTIES" "HIDE"))
     (byte-compile-warnings redefine callargs free-vars unresolved obsolete noruntime)
     (auto-recompile))))
 '(sass-indent-offset 4)
 '(save-abbrevs (quote silently))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 10)
 '(scroll-margin 20)
 '(scroll-preserve-screen-position t)
 '(search-upper-case t)
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "msmtp-personal")
 '(sentence-end-base "\\([.?!…‽][]\"'”’)}]*\\|[;:]-?[])[(|DoO/]\\)")
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch nil)
 '(show-trailing-whitespace nil)
 '(smartparens-global-mode nil)
 '(sml/active-background-color "grey39")
 '(sml/active-foreground-color "black")
 '(sml/hidden-modes
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit")))
 '(sml/inactive-background-color "gray20")
 '(sml/line-number-format "%4l")
 '(sml/mode-width (quote full))
 '(sml/name-width (quote (20 . 45)))
 '(sml/outside-modified-char "M")
 '(sml/read-only-char "%")
 '(sml/replacer-regexp-list
   (quote
    (("^~/\\.STOW/emacs/\\.emacs\\.d/" ":ED:")
     ("^~/\\.GIT/adamspiers\\.org/emacs/\\.emacs\\.d/" ":ED:")
     ("^~/\\.STOW/emacs/lib/emacs/" ":LE:")
     ("~/lib/emacs/" ":LE:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^:ED:init.d/" ":EID:")
     ("^/sudo:.*:" ":SU:")
     ("^~/.GIT/adamspiers.org/" ":GG:")
     ("^~/.GIT/3rd-party/" ":G3:")
     ("^~/SUSE/cloud/crowbar/git/2.0/" ":CR2:")
     ("^~/SUSE/cloud/crowbar/git/1.6/" ":CRP:")
     (":CR2:barclamps/" ":C2B:")
     (":CRP:barclamps/" ":CPB:")
     ("^~/SUSE/IBS/" ":IBS:")
     ("^~/SUSE/OBS/" ":OBS:")
     ("^:IBS:Devel/Cloud/" ":DC:")
     ("^:DC:2.0/" ":DC2:")
     ("^:DC:3/" ":DC3:")
     ("^:DC2:Staging/" ":DC2S:")
     ("^:DC3:Staging/" ":DC3S:")
     ("^\\(.+:\\)crowbar-barclamp-" "\\1bc-"))))
 '(sml/show-battery nil)
 '(sml/show-client t)
 '(sml/vc-mode-show-backend t)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.emacs\\.backup\\)\\'")
 '(speedbar-tag-split-minimum-length 30)
 '(tidy-shell-command "htmltidy")
 '(tla-non-recursive-inventory nil)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tooltip-mode t)
 '(tramp-completion-reread-directory-timeout 1800 nil (tramp))
 '(tramp-verbose 9 nil (tramp))
 '(transient-mark-mode t)
 '(undo-tree-mode-lighter "")
 '(uniquify-after-kill-buffer-p nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-package-always-ensure t)
 '(user-mail-address "adam@spiers.net")
 '(vc-annotate-background "nil")
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-empty-at-bob-regexp "^\\(\\(\\([ 	]*
\\)+\\)\\{2\\}\\)")
 '(whitespace-empty-at-eob-regexp "^\\(\\(\\([ 	]*
\\)+\\)\\{2\\}\\)")
 '(whitespace-line-column 80)
 '(whitespace-style
   (quote
    (face trailing space-before-tab newline empty tab-mark tabs)))
 '(whitespace-trailing-regexp "[^>]\\([	  ]+\\)$")
 '(yas-snippet-dirs
   (quote
    ("~/lib/emacs/minor-modes/yasnippet/snippets" "~/lib/emacs/minor-modes/yasnippets-rails/rails-snippets"))))

(message "custom-set-variables done")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
