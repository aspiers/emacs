;; If you're using color-theme.el, this background should match the
;; background of your chosen theme
(setq as-default-background
;;    "seashell1"
    "wheat"
;;    "gray90"
      )

(setq frame-background-mode
      (cond ((and (= emacs-major-version 21) window-system) 'light)
            (t 'dark)))

(and (eq frame-background-mode 'light)
     (custom-set-variables
      `(vc-annotate-background ,as-default-background)))

(and (>= emacs-major-version 21)
     (progn
       (custom-set-faces
        `(default
           ((((class color) (background light))
             (:background ,as-default-background))))
        '(fringe
          ((((class color) (background light))
            (:background "wheat1"))))
        `(mmm-default-submode-face
          ((((class color) (background light))
            (:background ,as-default-background)))))
       ))

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(Info-additional-directory-list (quote ("~/local/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(align-dq-string-modes (quote (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode python-mode)))
 '(align-exclude-rules-list (quote ((exc-dq-string (regexp . "\"\\([^\"
]+\\)\"") (repeat . t) (modes . align-dq-string-modes)) (exc-sq-string (regexp . "'\\([^'
]+\\)'") (repeat . t) (modes . align-sq-string-modes)) (exc-open-comment (regexp lambda (end reverse) (funcall (if reverse (quote re-search-backward) (quote re-search-forward)) (concat "[^ 	
\\\\]" (regexp-quote comment-start) "\\(.+\\)$") end t)) (modes . align-open-comment-modes)) (exc-c-comment (regexp . "/\\*\\(.+\\)\\*/") (repeat . t) (modes . align-c++-modes)) (exc-c-func-params (regexp . "(\\([^)
]+\\))") (repeat . t) (modes . align-c++-modes)) (exc-c-macro (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$") (group . 2) (modes . align-c++-modes)) (exc-perl-comment (regexp . "^\\s-*#.*$") (modes . align-perl-modes)))))
 '(align-rules-list (quote ((lisp-second-arg (regexp . "\\(^\\s-+[^( 	
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
]") (repeat . t) (modes append align-perl-modes (quote (python-mode))) (run-if lambda nil current-prefix-arg)) (c++-comment (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$") (modes . align-c++-modes) (column . comment-column) (valid lambda nil (save-excursion (goto-char (match-beginning 1)) (not (bolp))))) (c-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-c++-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(/[*/]\\|$\\)")))) (perl-chain-logic (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)") (modes . align-perl-modes) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\)")))) (perl-short-method-braces (regexp . "^\\s-*sub\\(\\s-+\\)[a-zA-Z0-9_]+\\(\\s-*\\){\\(\\s-*\\).*?\\(\\s-*\\)}\\s-*$") (modes . align-perl-modes) (group 1 2 3 4) (separate . "^dontmatchthis$")) (python-chain-logic (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)") (modes quote (python-mode)) (valid lambda nil (save-excursion (goto-char (match-end 2)) (looking-at "\\s-*\\(#\\|$\\|\\\\\\)")))) (c-macro-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes . align-c++-modes) (column . c-backslash-column)) (basic-line-continuation (regexp . "\\(\\s-*\\)\\\\$") (modes quote (python-mode makefile-mode sh-mode))) (tex-record-separator (regexp lambda (end reverse) (align-match-tex-pattern "&" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t)) (tex-tabbing-separator (regexp lambda (end reverse) (align-match-tex-pattern "\\\\[=>]" end reverse)) (group 1 2) (modes . align-tex-modes) (repeat . t) (run-if lambda nil (eq major-mode (quote latex-mode)))) (tex-record-break (regexp . "\\(\\s-*\\)\\\\\\\\") (modes . align-tex-modes)) (text-column (regexp . "\\(^\\|\\S-\\)\\(\\s-+\\)\\(\\S-\\|$\\)") (group . 2) (modes . align-text-modes) (repeat . t) (run-if lambda nil (and current-prefix-arg (not (eq (quote -) current-prefix-arg))))) (text-dollar-figure (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.") (modes . align-text-modes) (justify . t) (run-if lambda nil (eq (quote -) current-prefix-arg))))))
 '(align-sq-string-modes (quote (python-mode)))
 '(auto-save-interval 120)
 '(backup-directory-alist (quote (("/home/.*" . "/tmp/.emacs.d/backup") ("." . ".emacs.backup"))))
 '(blink-cursor-delay 0.0)
 '(blink-cursor-interval 0.3)
 '(blinking-cursor-blink-frequency 4)
 '(blinking-cursor-colors (quote ("coral" "blue" "gold")))
 '(blinking-cursor-idle-states (quote (("coral" "box" 0.5) ("coral" -1 0.5))))
 '(blinking-cursor-non-idle-state (quote ("coral" "box")))
 '(blinking-cursor-states (quote (("coral" "box" 0.7) ("coral" 2 0.4))))
 '(bs-alternative-configuration "cvs")
 '(bs-attributes-list (quote (("" 1 1 left bs--get-marked-string) ("M" 1 1 left bs--get-modified-string) ("R" 2 2 left bs--get-readonly-string) ("Buffer" bs--get-name-length 10 left bs--get-name) ("" 1 1 left " ") ("Size" 5 8 right bs--get-size-string) ("" 1 1 left " ") ("Mode" 5 12 right bs--get-mode-name) ("" 2 2 left "  ") ("File" 12 12 left bs--get-file-name) ("" 2 2 left "  "))))
 '(bs-configurations (quote (("all" nil nil nil nil nil) ("files" nil nil ".~[0-9.]+~$" bs-visits-non-file bs-sort-buffer-interns-are-last) ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last) ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last) ("cvs" "\\*cvs\\*$" nil "" nil bs--sort-by-name))))
 '(bs-default-sort-name "by nothing")
 '(bs-max-window-height 24)
 '(bs-maximal-buffer-name-column 22)
 '(bs-minimal-buffer-name-column 5)
 '(case-fold-search t)
 '(color-theme-is-cumulative nil)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-close-paren-offset -2)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lineup-step 1)
 '(cperl-merge-trailing-else nil)
 '(cperl-syntaxify-by-font-lock t)
 '(cperl-under-as-char nil)
 '(cvs-buffer-switch-alist (quote ("diff" "status" "log")))
 '(cvs-buffer-switch-list (quote ("diff" "status" "log")))
 '(cvs-find-file-and-jump t)
 '(cvs-invert-ignore-marks (quote ("diff")))
 '(cvs-parse-ignored-messages (quote ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" ".*New directory `.*' -- ignored.*$" ".*warning: directory CVS specified in argument.*$" ".*but CVS uses CVS for its own purposes; skipping CVS directory.*$")))
 '(cvs-reuse-cvs-buffer (quote subdir))
 '(cvs-use-fileinfo-caches t)
 '(delete-old-versions t)
 '(diff-switches "-u" t)
 '(dired-kept-versions 0)
 '(dired-listing-switches "-l" t)
 '(echo-keystrokes 0.01)
 '(eldoc-minor-mode-string "" t)
 '(eldoc-mode t t)
 '(fast-lock-cache-directories (quote ("~/.emacs-flc")))
 '(fast-lock-minimum-size 4096)
 '(folding-mode-key-prefix "")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-expert-user t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-group-line-format "%M%S%p%P%3y: %(%g%)%l
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-group-sort-function (quote gnus-group-sort-by-rank))
 '(gnus-novice-user nil)
 '(gnus-select-method (quote (nntp "news.guideguide.com")))
 '(gnus-startup-hook (quote ((lambda nil (if (<= (frame-width) 80) (gnus-add-configuration (quote (article (vertical 1.0 (horizontal 0.25 (summary 0.75 point) (tree 1.0)) (article 1.0))))))))))
 '(gnus-summary-exit-hook (quote (gnus-summary-bubble-group)))
 '(gnus-summary-line-format "%U%R%z%4L:%([%1{%-15,15n%}]%) %2t %3{%B%}%2{%s%}
")
 '(gnus-suppress-duplicates t)
 '(gnus-use-trees t)
 '(indent-tabs-mode nil)
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
 '(message-default-news-headers "From: Adam Spiers <usenet@adamspiers.org>
Reply-To: Adam Spiers <usenet@adamspiers.org>
")
 '(mouse-yank-at-point t)
 '(mwheel-follow-mouse t)
 '(mwheel-scroll-amount (quote (5 1 nil)))
 '(ps-paper-type (quote a4) t)
 '(ps-print-color-p (quote black-white))
 '(scroll-conservatively 10)
 '(scroll-margin 20)
 '(scroll-preserve-screen-position t)
 '(search-upper-case t)
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-ring-bell-on-mismatch nil)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.emacs\\.backup\\)\\'")
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background "nil")
 '(vc-follow-symlinks t))

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(custom-face-tag-face ((t (:weight bold :height 1.44 :family "helvetica"))))
 '(custom-group-tag-face ((t (:weight bold :height 1.6 :family "helvetica"))))
 '(custom-variable-tag-face ((t (:foreground "blue" :weight bold :height 1.4 :family "helvetica"))))
 '(cvs-msg-face ((t (:slant italic))))
 )
