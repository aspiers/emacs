(setq as-default-background "seashell1")
(setq frame-background-mode
      (cond ((and (= emacs-major-version 21) window-system) 'light)
            (t 'dark)))

(and (eq frame-background-mode 'light)
     (custom-set-variables
      '(vc-annotate-background "white")))

(and (>= emacs-major-version 21)
     (custom-set-faces
      `(default ((((class color) (background light)) (:background ,as-default-background))))
      `(mmm-default-submode-face ((((class color) (background light)) (:background ,as-default-background))))))

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(Info-additional-directory-list (quote ("~/local/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(auto-save-interval 120)
 '(blinking-cursor-blink-frequency 4)
 '(blinking-cursor-colors (quote ("coral" "blue" "gold")))
 '(blinking-cursor-idle-states (quote (("coral" "box" 0.5) ("coral" -1 0.5))))
 '(blinking-cursor-non-idle-state (quote ("coral" "box")))
 '(blinking-cursor-states (quote (("coral" "box" 0.7) ("coral" 2 0.4))))
 '(case-fold-search t)
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-electric-keywords t)
 '(cperl-electric-linefeed t)
 '(cperl-electric-parens t)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lazy-help-time 2)
 '(cperl-lineup-step 1)
 '(cperl-syntaxify-by-font-lock t)
 '(cperl-under-as-char nil)
 '(diff-switches "-u" t)
 '(fast-lock-minimum-size 4096)
 '(folding-mode-key-prefix "")
 '(font-lock-support-mode (quote lazy-lock-mode))
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(iswitchb-case nil)
 '(lazy-lock-defer-on-scrolling t)
 '(lazy-lock-defer-time 0.1)
 '(lazy-lock-minimum-size 4096)
 '(lazy-lock-stealth-time 15)
 '(lazy-lock-stealth-verbose t)
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
 '(vc-annotate-background "white"))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(cperl-array-face ((((class color) (background dark)) (:bold t :foreground "yellow"))))
 '(cperl-hash-face ((t (:bold t :italic t :foreground "red"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "aquamarine"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "Firebrick"))))
 '(highlight ((t (:foreground "white" :background "orange"))))
 '(show-paren-match-face ((((class color)) (:background "MediumBlue"))))
 '(underline ((((class color) (background light)) (:underline t :foreground "Yellow Green")))))
