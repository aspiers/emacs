;; If you're using color-theme.el, this background should match the
;; background of your chosen theme
(setq as-default-background
;;    "seashell1"
      "wheat"
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
       (custom-set-variables
        '(tool-bar-mode nil nil (tool-bar)))))

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(Info-additional-directory-list (quote ("~/local/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(auto-save-interval 120)
 '(backup-directory-alist (quote (("." . ".emacs.backup"))))
 '(blinking-cursor-blink-frequency 4)
 '(blinking-cursor-colors (quote ("coral" "blue" "gold")))
 '(blinking-cursor-idle-states (quote (("coral" "box" 0.5) ("coral" -1 0.5))))
 '(blinking-cursor-non-idle-state (quote ("coral" "box")))
 '(blinking-cursor-states (quote (("coral" "box" 0.7) ("coral" 2 0.4))))
 '(bs-attributes-list (quote (("" 1 1 left bs--get-marked-string) ("M" 1 1 left bs--get-modified-string) ("R" 2 2 left bs--get-readonly-string) ("Buffer" bs--get-name-length 10 left bs--get-name) ("" 1 1 left " ") ("Size" 5 8 right bs--get-size-string) ("" 1 1 left " ") ("Mode" 5 12 right bs--get-mode-name) ("" 2 2 left "  ") ("File" 12 12 left bs--get-file-name) ("" 2 2 left "  "))))
 '(bs-configurations (quote (("all" nil nil nil nil nil) ("files" nil nil ".~[0-9.]+~$" bs-visits-non-file bs-sort-buffer-interns-are-last) ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last) ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last))))
 '(bs-default-sort-name "by nothing")
 '(bs-max-window-height 24)
 '(bs-maximal-buffer-name-column 22)
 '(bs-minimal-buffer-name-column 5)
 '(case-fold-search t)
 '(color-theme-is-cumulative nil)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lineup-step 1)
 '(cperl-syntaxify-by-font-lock t)
 '(cperl-under-as-char nil)
 '(cvs-buffer-switch-alist (quote ("diff" "status" "log")))
 '(cvs-buffer-switch-list (quote ("diff" "status" "log")))
 '(cvs-find-file-and-jump t)
 '(cvs-invert-ignore-marks nil)
 '(cvs-parse-ignored-messages (quote ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" ".*New directory `.*' -- ignored.*$" ".*warning: directory CVS specified in argument.*$" ".*but CVS uses CVS for its own purposes; skipping CVS directory.*$")))
 '(diff-switches "-u" t)
 '(dired-listing-switches "-l")
 '(echo-keystrokes 0.01)
 '(eldoc-minor-mode-string "")
 '(eldoc-mode t)
 '(fast-lock-minimum-size 4096)
 '(folding-mode-key-prefix "")
 '(font-lock-support-mode (quote fast-lock-mode))
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-expert-user t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-group-sort-function (quote gnus-group-sort-by-rank))
 '(gnus-novice-user nil)
 '(gnus-select-method (quote (nntp "news.guideguide.com")))
 '(gnus-startup-hook (quote ((lambda nil (if (<= (frame-width) 80) (gnus-add-configuration (quote (article (vertical 1.0 (horizontal 0.25 (summary 0.75 point) (tree 1.0)) (article 1.0))))))))))
 '(gnus-summary-exit-hook (quote (gnus-summary-bubble-group)))
 '(gnus-group-line-format "%M%S%p%P%3y: %(%g%)%l
")
 '(gnus-summary-line-format "%U%R%z%4L:%([%1{%-15,15n%}]%) %2t %3{%B%}%2{%s%}
")
 '(gnus-suppress-duplicates t)
 '(gnus-use-trees t)
 '(indent-tabs-mode nil)
 '(iswitchb-case nil)
 '(jit-lock-stealth-nice 0.1)
 '(jit-lock-stealth-time 1)
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
 '(scroll-conservatively 20)
 '(scroll-margin -1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 2)
 '(search-upper-case t)
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-ring-bell-on-mismatch nil)
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
