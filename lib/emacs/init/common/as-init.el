;; emacs startup file
;; Adam Spiers

;;{{{ To do list

;;
;;  - Start using same-window-regexps?
;;

;;}}}

;;{{{ Compiler declarations

(defun as-quick-startup nil
  "Non-nil if the current emacs was required to start up quickly."
  (getenv "QUICK_EMACS"))

;;}}}

;;{{{ Functions

(load-library "as-loaddefs")
(require 'as-progress)
(require 'as-require)

(as-progress "functions...")

;;{{{ Appearance

;;{{{ as-font-lock-mode-if-window-system

(defun as-font-lock-mode-if-window-system
  ()
  "Turns on font-lock mode if X windows is active."
  (interactive)
  (if window-system (font-lock-mode)))

;;}}}

;;}}}
;;{{{ elisp helper functions

(autoload 'function-arg-types "as-elisp")
(autoload 'function-arity "as-elisp")

;;}}}
;;{{{ ll => load-library

(defun ll (library)
  "Shortcut to `load-library'."
  (interactive "sLoad library: ")
  (load-library library))

;;}}}
;;{{{ Enable disabled functions

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left     'disabled nil)

;;}}}

(as-progress "functions...done")

;;}}}
;;{{{ Key bindings

(require 'as-bindings)

;;}}}
;;{{{ Point movement

;; Scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;; Show position in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Default right margin
(setq fill-column 70)

;; Stop down cursor adding newlines to end of buffer.
(setq next-line-add-newlines nil)

;; IntelliMouse
(cond ((and (< emacs-major-version 22) window-system)
       (load "mwheel" t)
       (mwheel-install)))

;;}}}
;;{{{ Little odds and ends

;;{{{ find-function-source-path

;; Don't need this, as the Makefile now copys all .el files into the
;; installdir alongside the .elc files, and `find-library' looks in
;; `load-path'.

;; (defvar as-emacs-dir)

;; (custom-set-variables
;;  '(find-function-source-path
;;    (append load-path
;;       (mapcar (lambda (p) (concat as-emacs-dir "/" p))
;;               (list
;;                "fun"
;; [snipped]
;;                "utils"
;;                )))))

;;}}}
;;{{{ e-mail address

(setq user-mail-address "adam@spiers.net")

;;}}}
;;{{{ kill-line kill whole line if at beginning of line

(setq kill-whole-line t)

;;}}}
;;{{{ Visible bell

(setq-default visible-bell t)

;;}}}
;;{{{ Saving sessions

(autoload 'desktop-save "desktop" "Saves desktop session state." t)

;;}}}
;;{{{ midnight-mode - automatically kill unused buffers

(unless (as-quick-startup) (as-soft-require 'midnight))

;;}}}
;;{{{ client/server mode

(autoload 'server-start "server" "server-start" t)
(defun ss ()
  "Abbreviation for `server-start'."
  (interactive)
  (server-start))

(autoload 'edit-server-start "edit-server" "edit-server" t)
(autoload 'edit-server-stop  "edit-server" "edit-server" t)
(unless (as-quick-startup)
  (server-start)
  (condition-case err
      (edit-server-start)
    (file-error (message "%s" (error-message-string err))))
  (defun edit-server-restart (interactive)
    "Equivalent to `edit-server-stop' followed by `edit-server-start'."
    (edit-server-stop)
    (edit-server-start)))

(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;;}}}
;;{{{ Diary, appointments

;; suspect I don't need this any more
(autoload 'appt-make-list "appt")
(add-hook 'diary-hook 'appt-make-list)

;;}}}
;;{{{ UTF-16 support

(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

;;}}}
;;{{{ Color themes

(require 'color-theme-autoloads nil 'noerror)
(autoload 'color-theme-pastels-on-dark "color-theme-pastels-on-dark" "pastels-on-dark-theme" t)
(if (as-check-feature-loaded 'color-theme-autoloads)
    (progn
      (require 'pastels-on-dark-theme nil 'noerror)
      (if (as-check-feature-loaded 'pastels-on-dark-theme)
          (color-theme-pastels-on-dark)
        (message "Couldn't load pastels-on-dark-theme")))
  (message "Couldn't load color-theme"))

;;}}}

;;}}}
;;{{{ Mode-related settings

(as-progress "mode settings...")

;;{{{ Startup mode selection

(as-progress "Startup mode selection...")

(setq major-mode 'indented-text-mode)

;; Iterate over auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".
(mapc (function
       (lambda (x)
         (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
      auto-mode-alist)

;; Each mode has its own block of elisp which will usually modify
;; auto-mode-alist, but here we add in some misc modes.
(dolist (elt '(("\\.po[tx]?\\'\\|\\.po\\."              . po-mode)
               ("\\.htaccess$"                          . apache-mode)
               ("\\(httpd\\|srm\\|access\\)\\.conf$"    . apache-mode)
               ("\\.dump$"                              . tar-mode)
               ("/\\.zsh\\(env\\|rc\\)"                 . sh-mode)
               ("/\\.zsh/functions/"                    . sh-mode)
               ("\\.stp$"                               . sh-mode)
               ("\\.make$"                              . makefile-mode)

               ;; TWiki
               ("\\.tmpl$"                              . html-helper-mode)
               ("TWiki\\.cfg$"                          . cperl-mode)
               ))
  (add-to-list 'auto-mode-alist elt))

;;}}}
;;{{{ Major modes

(as-progress "Major modes...")

;;{{{ Programming languages

;;{{{ C

(add-hook 'c-mode-hook 'as-font-lock-mode-if-window-system)

;;}}}
;;{{{ CPerl/Perl

;; one of these two will work
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)

(add-hook 'perl-mode-hook 'as-font-lock-mode-if-window-system)

(autoload 'as-cperl-setup "as-cperl" "as-cperl-setup")
(add-hook 'cperl-mode-hook 'as-cperl-setup)
(add-hook 'cperl-mode-hook 'turn-on-auto-fill)
(defun cp () "Abbreviation for `cperl-mode'." (interactive) (cperl-mode))
(add-hook 'cperl-mode-hook (lambda () (setq comment-start "#")))

(add-to-list 'auto-mode-alist '("\\.\\(pod\\|t\\)\\'" . cperl-mode))

;;}}}
;;{{{ python-mode

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(cond ((>= emacs-major-version 22)
       (autoload 'python-mode "python" "python-mode" t))
      (t
       (autoload 'python-mode "python-mode" "python-mode" t)))

;; Allow modes such as jdl-mode to be given Python alignment rules
(defcustom align-python-modes '(python-mode)
  "A list of modes where Python syntax is to be seen."
  :type '(repeat symbol)
  :group 'align)

(defvar align-rules-list)
(defun as-tweak-align-rules ()
  "Replace occurrences of `python-mode' in `align-rules-list'
with references to `align-python-modes'.

FIXME: needs to tweak align-*-modes too."
  (dolist (rule align-rules-list)
    (let* ((title (car rule))
           (pairs (cdr rule))
           (modespair (assq 'modes pairs))
           (modes (cdr modespair))
           (search 'python-mode)
           (replacement 'align-python-modes)
           (result nil
                   ;(format "no change for %s" title)
                   ))
      ;(if (eq title 'basic-comma-delimiter) (edebug))
      (if modes
          (cond
           ;; modes could have form: align-foo-modes
           ((equal modes `(quote (,search)))
            (setcdr modespair replacement)
            (setq result (format "single change for %s" title)))

           ;; FIXME: handle modes like
           ;;   (append align-perl-modes '(python-mode))
           ;; as seen in basic-comma-delimiter

           ;; handle modes with form: '(mode1 mode2 ...)
           ;; i.e. (quote (mode1 mode2 ...))
           ((and (listp modes)
                 (eq 'quote (car modes))
                 (listp (cadr modes)))
            (let ((to-replace (memq search (cadr modes))))
              (when to-replace
                ;; remove this element from the list
                (setcar to-replace (cadr to-replace))
                (setcdr to-replace (cddr to-replace))
                (setcdr modespair `(append ,replacement ,modes))
                (setq result (format "list change for %s" title)))))))
       ;(message result)
       )))

(eval-after-load "align"
  '(progn
     (as-tweak-align-rules)
     (add-to-list 'align-rules-list
                  '(python-dictionary-braces
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\S-")
                    (group    . (2))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))
     (add-to-list 'align-rules-list
                  '(python-dictionary-pairs
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\([^:]+\\)\\(\\s-*\\):\\(\\s-*\\)")
                    (group    . (4 5))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))))

;;}}}
;;{{{ Ruby

(add-to-list 'auto-mode-alist '("\\(\\.rb\\|\\.erb\\|\\.rjs\\|\\.rake\\|Rakefile\\|Guardfile\\)\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "ruby-mode" t)
(add-to-list 'auto-mode-alist '("\\.gem\\'" . tar-mode))

;;{{{ rcov.el

;; corrections for rcov.el, although it kinda looks lame - no red/green overlaying

(autoload 'rcov "rcov" "rcov" t)

(defvar rcov-command-line "rake test:units:rcov RCOVOPTS='--gcc --no-html'"
  "Rcov command line to find uncovered code.
It is good to use rcov with Rake because it `cd's appropriate directory.
`--gcc' option is strongly recommended because `rcov' uses compilation-mode.")
(defvar rcovsave-command-line "rake rcov RCOVOPTS='--gcc --no-html --save=coverage.info'"
  "Rcov command line to save coverage status. See also `rcov-command-line'.")
(defvar rcovdiff-command-line "rake rcov RCOVOPTS='-D --gcc --no-html'"
  "Rcov command line to find new uncovered code. See also `rcov-command-line'.")

;;}}}

(autoload 'rcov-buffer "rcov-overlay" "rcov-overlay" t)
;; (global-set-key (kbd "C-c C-r")   'rcov-buffer)
(autoload 'autotest "autotest" "autotest" t)

;;{{{ rsense

(defvar rsense-home (concat (getenv "HOME") "/.STOW/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
;;(require 'rsense nil t)

;;}}}

;;}}}
;;{{{ Shell-script

;; Autoload sh-script on invocation
(autoload 'shell-script-mode "sh-script"
  "Major mode for editing shell scripts" t)

(autoload 'sh-ins-template "as-sh-mode-exts"
  "Template for new shell script texts" t)
(autoload 'make-buffer-file-executable-if-script-p "as-sh-mode-exts"
  "Make shell scripts executable" t)
(add-hook 'sh-mode-hook 'sh-ins-template)

;; This can accidentally change permissions in git repos, for instance.
;;(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(global-set-key "\C-cmx" 'make-buffer-file-executable)

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'shell-script-mode-hook (lambda () (setq comment-start "#")))

(defun sm () "Abbreviation for `sm-mode'." (interactive) (sh-mode))

;;}}}
;;{{{ Lisp

(add-hook 'lisp-mode-hook 'as-font-lock-mode-if-window-system)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; ")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; ")))

;;{{{ local-unset-key M-tab for hippie-expand

(if (eq (boundp 'running-xemacs) t)
    (add-hook 'emacs-lisp-mode-hook
              (lambda () (local-unset-key [(meta tab)]))))

;;}}}

;;}}}
;;{{{ ecmascript-mode

;; (add-to-list 'auto-mode-alist '("\\.js$" . ecmascript-mode))
;; (autoload 'ecmascript-mode "ecmascript-mode" "ecmascript-mode" t)

;; Steve Yegge to the rescue
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(\.erb\\)?$" . js2-mode))

;;}}}

;;}}}
;;{{{ Other technical languages

;;{{{ DTD

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks 'turn-on-font-lock)

;; auto-mode-alist:
;;       ("\\.dtd$"                               . dtd-mode)
;;       ("\\.dcl$"                               . dtd-mode)
;;       ("\\.dec$"                               . dtd-mode)
;;       ("\\.ele$"                               . dtd-mode)
;;       ("\\.ent$"                               . dtd-mode)
;;       ("\\.mod$"                               . dtd-mode)

;;}}}
;;{{{ XML / SGML

;;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(dolist (elt '(("\\.sgml$" . sgml-mode)
               ("\\.dtd$"  . sgml-mode)
               ("\\.xsd$"  . xml-mode)))
  (add-to-list 'auto-mode-alist elt))

(setq-default sgml-indent-data t)

;; fontify straight away
(setq-default sgml-auto-activate-dtd t)

(defvar sgml-warn-about-undefined-elements nil)
(defvar sgml-warn-about-undefined-entities nil)

;; Some convenient key definitions:
;; (define-key sgml-mode-map "\C-cm\C-e" 'sgml-describe-element-type)
;; (define-key sgml-mode-map "\C-cm\C-i" 'sgml-general-dtd-info)
;; (define-key sgml-mode-map "\C-cm\C-t" 'sgml-describe-entity)

;;{{{ faces

(setq-default sgml-set-face t)

(make-face 'sgml-comment-face)
(make-face 'sgml-doctype-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-ignored-face)
(make-face 'sgml-ms-end-face)
(make-face 'sgml-ms-start-face)
(make-face 'sgml-pi-face)
(make-face 'sgml-sgml-face)
(make-face 'sgml-short-ref-face)
(make-face 'sgml-start-tag-face)

(add-hook 'sgml-mode-hook
          (lambda ()
            (set-face-foreground 'sgml-comment-face   "dark green")
            (set-face-foreground 'sgml-doctype-face   "dark red")
            (set-face-foreground 'sgml-start-tag-face "dark cyan")
            (set-face-foreground 'sgml-end-tag-face   "dark cyan")
            (set-face-foreground 'sgml-entity-face    "magenta")
            (set-face-foreground 'sgml-ignored-face   "gray40")
            (set-face-foreground 'sgml-ms-end-face    "green")
            (set-face-foreground 'sgml-ms-start-face  "yellow")
            (set-face-foreground 'sgml-pi-face        "dark blue")
            (set-face-foreground 'sgml-sgml-face      "brown")
            (set-face-foreground 'sgml-short-ref-face "deep sky blue")))

(setq-default sgml-markup-faces
              '((CDATA      . font-lock-doc-face)
                (comment    . sgml-comment-face)
                (doctype    . sgml-doctype-face)
                (end-tag    . sgml-end-tag-face)
                (entity     . sgml-entity-face)
                (ignored    . sgml-ignored-face)
                (ms-end     . sgml-ms-end-face)
                (ms-start   . sgml-ms-start-face)
                (pi         . sgml-pi-face)
                (sgml       . sgml-sgml-face)
                (short-ref  . sgml-short-ref-face)
                (start-tag  . sgml-start-tag-face)))

;; (setq sgml-markup-faces '((start-tag . font-lock-function-name-face)
;;                           (end-tag . font-lock-function-name-face)
;;                           (comment . font-lock-comment-face)
;;                           (pi . bold)
;;                           (sgml . bold)
;;                           (doctype . bold)
;;                           (entity . font-lock-type-face)
;;                           (shortref . font-lock-function-name-face)))

;;}}}
;;{{{ sgml-specials

;; See the documentation for this.  The default is (34 45) which
;; includes - as a special character for comments.  The problem
;; is that emacs doesn't allow predicates to context-sensitively
;; confirm the syntax of characters, so the fontifying of comments
;; often screws up.

(eval-when-compile
  (defvar sgml-specials))
(setq sgml-specials '(34))

;;}}}

;;}}}
;;{{{ nxml

;; FIXME: this fucks up load-path somewhat - superfluous trailing slashes
;;(when (string-match "/\"" (prin1-to-string load-path)) (error 'fucked))

(defun nx () "Loads nxml-mode." (interactive) (nxml-mode))
(autoload 'mhj-format-xml "mhj-xml" "Mark's nxml hacks." t)
(add-hook 'nxml-mode-hook
          (lambda () (local-set-key [(control meta q)] 'mhj-format-xml)))
;;(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xhtml\\)$" . nxml-mode))

;;}}}
;;{{{ HTML

(defalias 'html-mode 'html-helper-mode)
(autoload 'html-helper-mode "html-helper-mode" t)
(add-hook 'html-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            ;; (setq truncate-lines t)
            ))
(defvar tempo-interactive t)

(dolist (re '("\\.prehtml\\'"
              "\\.php3\\'"
              "\\.\\(mason\\|m[cd]\\)\\'"
              "\\(auto\\|d\\)handler\\'"
              ))
  (add-to-list 'auto-mode-alist (cons re 'html-mode)))

;; htmltidy support
(autoload 'tidy-buffer "htmltidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "htmltidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "htmltidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "htmltidy" "Install an options menu for HTML Tidy." t)

;; (eval-when-compile
;;   (defvar html-helper-mode-map)
;;   (defvar sgml-validate-command))

;; This broke with:
;;   File mode specification error: (void-variable html-helper-mode-map)
;;
;; (defun as-html-mode-tidy-hook () "Add htmltidy support to an HTML mode."
;;   (tidy-build-menu html-helper-mode-map)
;;   (local-set-key [(control c) (control c)] 'tidy-buffer)
;;   (setq sgml-validate-command "htmltidy"))

;;(add-hook 'html-mode-hook 'as-html-mode-tidy-hook)

(defun hhm () "Loads `html-helper-mode'." (interactive) (html-helper-mode))

;;}}}

;;{{{ feature-mode for Cucumber's feature DSL ("Gherkin")

;;(add-to-list 'load-path "~/lib/emacs/major-modes/cucumber")
;(setq feature-default-language "en")
;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
(autoload 'feature-mode "feature-mode" nil t)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;}}}


;;}}}
;;{{{ Configuration languages

;;{{{ rpm-spec-mode

(add-to-list 'auto-mode-alist '("\\.spec$" . rpm-spec-mode))
(autoload 'rpm-spec-mode "rpm-spec-mode" "RPM spec mode." t)

;;}}}
;;{{{ Apache

(autoload 'apache-mode "apache-mode" "mode for editing Apache config files")

;;}}}
;;{{{ xrdb

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

(dolist (re '("\\.Xdefaults$"
              "\\.Xenvironment$"
              "\\.Xresources$"
              ".*\\.ad$"
              ".*\\.x?rdb$"))
  (add-to-list 'auto-mode-alist (cons re 'xrdb-mode)))

(defvar as-find-file-matching-regexp-alist '())
(add-to-list 'as-find-file-matching-regexp-alist
             '("\*\.rdb$" . (lambda () (setq comment-start "! "))))

;;}}}

;;}}}
;;{{{ Semi-natural languages / documentation

;;{{{ Text

(autoload 'server-edit "server")
(defun as-save-server-buffer ()
  "Save server buffer and quit editing."
  (interactive)
  (save-buffer)
  (server-edit))

(defun as-set-local-server-edit-keys ()
  "Set key bindings in local mode for editing via firefox It's
All Text add-on."
  (interactive)
  (local-set-key [(control c) (control c)] 'as-save-server-buffer))

;; (defun as-set-local-edit-server-keys ()
;;   "Set key bindings in local mode for editing via
;; `edit-server-text-mode', which is used by the Chrome 'Edit with
;; Emacs' add-on."
;;   (interactive)
;;   ;; C-c C-c already set up by edit-server
;;   (local-set-key [foo bar] 'as-.....))

(defun as-setup-text-mode ()
  "Set up `text-mode' how Adam likes it."
  ;; Turn on auto-fill if composing e-mail or news.
  (let ((bn (or (buffer-file-name)
                ;; For some reason I've seen mutt-invoked emacs
                ;; instances yield nil for (buffer-file-name)
                (buffer-name))))
    (if (string-match "\\.article\\|\\.letter\\|itsalltext" bn)
        (turn-on-auto-fill))

    (if (string-match "itsalltext\\|/git-rebase\\|COMMIT_EDITMSG" bn)
        (as-set-local-server-edit-keys)))

  ;; Expand all newly inserted tabs to spaces
  (setq indent-tabs-mode nil)

  (as-setup-mode-for-discussion))  

(add-hook 'text-mode-hook 'as-setup-text-mode)

(defun as-setup-mode-for-discussion ()
  "Sets up a text mode in the way Adam likes for discussion with
other people."

   ;; Nicer version of fill-paragraph
   (local-set-key [(control meta q)] 'fill-common-prefix-region)

   ;; Treat single quoted ("> > ") lines the same as multiple
   ;; w.r.t. filling.
   (setq adaptive-fill-first-line-regexp adaptive-fill-regexp)

   (setq comment-start "> "))

(defun itm () "Shortcut to indented-text-mode."
  (interactive)
  (indented-text-mode))

;;}}}
;;{{{ ReStructuredText mode

;; (autoload 'rst-mode "rst")
;; (add-to-list 'auto-mode-alist '("\\.re?st$" . rst-mode))
;; (autoload 'rst-text-mode-bindings "rst")

;; This stomps over everything :-(
;;(add-hook 'text-mode-hook 'rst-text-mode-bindings)

(eval-when-compile (load-library "rst"))
(defun as-rst-bindings ()
  "Adam's key bindings for `rst-mode'."
  (interactive)

  (local-set-key [(control ?=)] 'rst-adjust)

  (local-set-key [(control up  )] 'rst-backward-section)
  (local-set-key [(control down)] 'rst-forward-section)
  (local-set-key [(control c) (control r)] 'rst-shift-region-right)
  (local-set-key [(control c) (control l)] 'rst-shift-region-left)
;;   (define-key mode-specific-map [(control p)] 'rst-backward-section)
;;   (define-key mode-specific-map [(control n)] 'rst-forward-section)
;;   (define-key mode-specific-map [(control r)] 'rst-shift-region-right)
;;   (define-key mode-specific-map [(control l)] 'rst-shift-region-left)

  ;; Bind the rst commands on the C-c p prefix.
;; FIXME: I can't find rst-prefix-map in rst.el any more, was it removed?
;;  (local-set-key [(control c) (control p)] rst-prefix-map)
  ;;(define-key mode-specific-map [(p)] 'rst-prefix-map)
  )

;; FIXME: do we always want these in text mode?
;;(add-hook 'text-mode-hook 'as-rst-bindings)
(add-hook 'rst-mode-hook 'as-rst-bindings)

;; Update the TOC automatically everytime you adjust a section title::
(add-hook 'rst-adjust-hook 'rst-toc-insert-update)

;;}}}
;;{{{ Info

(eval-after-load "info"
  '(define-key Info-mode-map [(shift tab)] 'Info-prev-reference))

;;}}}

;;{{{ TeX

;;{{{ Set up tex-dvi-view (C-c C-v)

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;;}}}

;; Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'as-font-lock-mode-if-window-system)

;;}}}
;;{{{ man

(eval-when-compile
  (defvar Man-notify-method))
(setq Man-notify-method 'pushy)

;;}}}
;;{{{ lilypond

;; included in site-start.d now
;;(add-to-list 'auto-mode-alist '(".ly\\'" . lilypond-mode))
;;(autoload 'lilypond-mode "lilypond" "Mode for editing lilypond files" t)

;;}}}
;;{{{ erin (TWiki editing mode)

(autoload 'erin-mode "erin" nil t)

;;}}}

;; muse mode is under productivity section

;;}}}
;;{{{ Organisation / productivity

;;{{{ org-mode

(as-progress "loading org-install ...")
(require 'org-install nil 'noerror)
(when (as-check-feature-loaded 'org-install)
  (as-progress "org-install loaded")
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(defvar org-mode-map)
(add-hook
 'org-mode-hook
 (lambda ()
   (as-soft-require 'as-gtd)
   (imenu-add-to-menubar "Imenu")
   (setq comment-start nil)))

(declare-function org-crypt-use-before-save-magic "org-crypt")
(add-hook
 'org-mode-hook
 (lambda ()
   (and (as-soft-require 'org-crypt)
        (org-crypt-use-before-save-magic))))


;;{{{ org keyword switching

(defun org-todo-previous-keyword ()
  "Move to previous TODO keyword in all sets."
  (interactive)
  (org-todo 'left))

(defun org-todo-next-keyword ()
  "Move to next TODO keyword in all sets."
  (interactive)
  (org-todo 'right))

(defun org-todo-previous-set ()
  "Move to previous TODO keyword set."
  (interactive)
  (org-todo 'previousset))

(defun org-todo-next-set ()
  "Move to next TODO keyword set."
  (interactive)
  (org-todo 'nextset))

(add-hook
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map [(control shift f)] 'org-todo-next-keyword)
   (define-key org-mode-map [(control shift b)] 'org-todo-previous-keyword)
   (define-key org-mode-map [(control shift p)] 'org-todo-previous-set)
   (define-key org-mode-map [(control shift n)] 'org-todo-next-set)
   ))

;;}}}
;;{{{ org-new-subheading*

(defun org-new-subheading ()
  "Add a new heading, demoted from the current heading level."
  (interactive)
  (org-insert-heading)
  (org-do-demote))

(defcustom org-subheading-todo-alist nil
  "An associative map to help define which TODO keyword should be
used for new subheadings, depending on the current heading's TODO
keyword.  See the documentation for `org-new-subheading-todo' for
an example."
  :group 'org-todo
  :type '(alist :key-type   (string :tag "Current heading keyword")
                :value-type (string :tag "New sub-heading keyword")))

(defun org-new-subheading-todo (&optional arg)
  "Add a new TODO item, demoted from the current heading level.

The TODO keyword for the new item can be specified by a numeric
prefix argument, as with `org-todo'.

Otherwise, if `org-subheading-todo-alist' is non-nil, it is used
to map the new keyword from the current one, and if it is nil,
the next TODO keyword in the sequence is used, or the first one
if the current heading does not have one.

This allows a TODO keyword hierarchy to be imposed, e.g.
if org-subheading-todo-alist is

  '((\"MASTERPLAN\" . \"PROJECT\")
    (\"PROJECT\"    . \"NEXTACTION\")
    (\"NEXTACTION\" . \"NEXTACTION\"))

then invoking this function four times would yield:

* MASTERPLAN
** PROJECT
*** NEXTACTION
**** NEXTACTION"
  (interactive "P")
  (save-excursion
    (org-back-to-heading)
    (looking-at org-todo-line-regexp))
  (let* ((current-keyword (match-string 2))
         (new-keyword
          (if arg
              (nth (1- (prefix-numeric-value arg))
                   org-todo-keywords-1)
            (or
             (and current-keyword
                  (or (cdr (assoc current-keyword org-subheading-todo-alist))
                      (cadr (member current-keyword org-todo-keywords-1))))
             (car org-todo-keywords-1)))))
    (org-new-subheading)
    (insert new-keyword " ")))

(add-hook
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map [(meta j)]       'org-new-subheading)
   (define-key org-mode-map [(shift meta j)] 'org-new-subheading-todo)
   ))

;;}}}

(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

;;{{{ pomodoro

;; http://orgmode.org/worg/org-gtd-etc.php

(add-to-list 'org-modules 'org-timer)
;; FIXME: something changed here, but I use Pomodroido now anyway.
;;(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :
(add-hook 'org-clock-in-hook '(lambda () 
      (if (not org-timer-current-timer) 
      (org-timer-set-timer '(16)))))

;;}}}

;;}}}

;;}}}
;;{{{ Interaction with other people

;;{{{ mutt

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")
(add-to-list 'auto-mode-alist '("/mutt-\\|itsalltext.*mail\\.google" . mail-mode))
(eval-when-compile (require 'sendmail))
(add-hook
 'mail-mode-hook
 (lambda ()
   (turn-on-auto-fill)
   (as-setup-mode-for-discussion)
   (as-set-local-server-edit-keys)))

;;}}}
;;{{{ crm114-mode

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

;;}}}
;;{{{ w3m-mode

;; Pull in autoloads
(require 'w3m-load "w3m-load" t)
(autoload 'w3m-find-file "w3m" nil t)

(eval-when-compile (require 'dired))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "\C-xm" 'dired-w3m-find-file)))

(defun dired-w3m-find-file ()
  (interactive)
  (require 'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))

(defun mhj-w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-" nil) ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

;;}}}
;;{{{ wordpress

(add-to-list 'auto-mode-alist
             '("blog\\.adamspiers\\.org\\..*\\.txt\\'" . html-helper-mode))

;;}}}

;;}}}
;;{{{ Version control

;;{{{ cvs helper modes

;; diff mode
(add-hook 'diff-mode-hook 'mhj-set-q-to-close)

;; cvs-status mode
(add-hook 'cvs-status-mode-hook 'mhj-set-q-to-close)

;; log-view mode
(add-hook 'log-view-mode-hook 'mhj-set-q-to-close)

;;}}}
;;{{{ psvn

(autoload 'svn-status "psvn" "svn-status" t)
(global-set-key "\C-css" 'svn-status)
(global-set-key "\C-csu" 'svn-status-update-cmd)
(fset 'as-next-svn-buffer "\C-xb*svn-status*")
(global-set-key "\C-csb"   'as-next-svn-buffer)
;; (require 'psvn)

;;}}}

;;}}}
;;{{{ Miscellaneous

;;{{{ MMM mode

(defvar mmm-mode-ext-classes-alist)
(defvar mmm-global-mode)
(setq mmm-global-mode 'maybe)

(defvar mmm-mode-ext-classes-alist)
(setq mmm-mode-ext-classes-alist
      '((nil "\\.\\(mason\\|m[dc]\\)\\'" mason)))

(defun mmm-add-mode-ext-class (a b c))
(mmm-add-mode-ext-class 'html-mode "\\(auto\\|d\\)handler\\'" 'mason)
(autoload 'mmm-mode "mmm-auto" "mmm mode" t)

;;}}}
;;{{{ gtypist-mode

(add-to-list 'auto-mode-alist '("\\.typ\\'" . gtypist-mode))
(autoload 'gtypist-mode "gtypist-mode" "gtypist-mode" t)

;;}}}

;;}}}

;;}}}
;;{{{ Minor modes

(as-progress "Minor modes...")

;;{{{ vc

(require 'vc)

(require 'vc-osc nil t)
(if (featurep 'vc-osc)
    (setq vc-handled-backends (append vc-handled-backends '(osc))))

;;}}}
;;{{{ iswitchb - better buffer switching

;; (eval-when-compile (require 'iswitchb))
;; (iswitchb-default-keybindings)
;; (add-hook 'iswitchb-define-mode-map-hook 'as-iswitchb-keys)

;; (defun iswitchb-bury-buffer ()
;;   "Bury the buffer at the head of `iswitchb-matches'."
;;   (interactive)
;;   (let ((enable-recursive-minibuffers t) buf)
;;     (setq buf (car iswitchb-matches))
;;     (if buf
;; 	(progn
;; 	  (bury-buffer buf)
;;           (iswitchb-next-match)
;;           (setq iswitchb-rescan t)))))

;; (defun as-iswitchb-keys ()
;;  "Adam's keybindings for iswitchb."
;;  (define-key iswitchb-mode-map "\C-z" 'iswitchb-bury-buffer)
;;  )

;;}}}
;;{{{ ido - superior replacement for iswitchb

(require 'ido)
(ido-mode t)

;;}}}
(as-progress "Minor modes... 10%")
;;{{{ Folding mode

;;{{{ Set marks for individual modes

(autoload 'folding-add-to-marks-list "folding" "folding mode")

(eval-after-load "folding"
  '(progn
    (folding-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
    (folding-add-to-marks-list 'Fundamental-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'shellscript-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'shell-script-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'Shellscript-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'Shell-script-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'makefile-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'makefile-gmake-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'sh-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'tex-mode "% {{{ " "% }}}")
    (folding-add-to-marks-list 'ml-mode "\(* {{{ " "\(* }}} ")
    (folding-add-to-marks-list 'sawfish-mode ";; {{{ " ";; }}}")
    (folding-add-to-marks-list 'lilypond-mode "% {{{ " "% }}}")
    (folding-add-to-marks-list 'LilyPond-mode "% {{{ " "% }}}")
    ))

;;}}}
;;{{{ Autoload mode via local variables

(autoload 'folding-mode "folding" "folding mode")
(autoload 'folding-mode-find-file "folding" "folding mode")
(autoload 'folding-mode-add-find-file-hook "folding" "folding mode")
(autoload 'folding-set-marks "folding" "folding mode")

(defun fm () "Loads folding-mode." (interactive) (folding-mode))
(defun as-folding-init ()
  "Sets up folding-mode for use."
  (require 'folding)
  (folding-mode-add-find-file-hook))

;; FIXME - preactivation?
(eval-after-load "find-func" '(as-folding-init))
;; (defadvice find-function-search-for-symbol (before as-folding act)
;;     "blah"
;;     (require 'folding))
;; (defadvice find-function (before as-folding act)
;;     "blah"
;;     (require 'folding))

(or (as-quick-startup) (as-folding-init))

;;}}}
;;{{{ Key bindings

(defvar folding-mode-map) ;; avoid compile warnings
(autoload 'fold-show "folding")

(add-hook 'folding-mode-hook
          (lambda ()
            ;; Quick navigation
            (local-set-key [(control meta <)] 'folding-shift-out)
            (local-set-key [(control meta >)] 'folding-shift-in)
            (local-set-key [(shift left)] 'folding-shift-out)
            (local-set-key [(shift right)] 'folding-shift-in)
            ))

(eval-when-compile
  (defvar folding-default-keys-function))
(setq folding-default-keys-function 'folding-bind-backward-compatible-keys)

;;}}}

;;}}}
(as-progress "Minor modes... 25%")
;;{{{ Transient Mark mode

(eval-when-compile (defun transient-mark-mode (arg1) nil))
(if (not (boundp 'running-xemacs)) (transient-mark-mode 1))

;;}}}
;;{{{ Font-Lock mode

;;(global-font-lock-mode t)

;; Do this via customisation since it's different for xemacs
;;(and window-system (not (boundp 'running-xemacs)) (global-font-lock-mode t))

;;}}}
;;{{{ Load paren library

(require 'paren)

;;}}}
;;{{{ Time-stamp mode

(autoload 'time-stamp "time-stamp")
;;(time-stamp)
;;(setq time-stamp-format "------ %02d %03b %4y %2H%2M %2H%2M  : %u")

;;}}}
(as-progress "Minor modes... 50%")
;;{{{ Time

;;(display-time)

;;}}}
;;{{{ Auto-compression mode

(cond ((as-quick-startup)
       (defun lac () "Load auto-compression-mode."
         (interactive)
         (auto-compression-mode 1)))
      (t
       (auto-compression-mode 1)))

;;}}}
;;{{{ blinking-cursor

(eval-when-compile (defun blinking-cursor-mode (&optional arg)))
(and window-system
     (not (boundp 'running-xemacs))
     (= emacs-major-version 20)
     (load "blinking-cursor" t)
     (blinking-cursor-mode 1))

;;}}}
;;{{{ recentf

(and window-system
     (load "recentf" 'noerror)
     (recentf-mode t))

;;}}}
;;{{{ no toolbar

;; This is best done with X resources, otherwise you get funny
;; frame-resizing problems.  See ~/.Xresources/emacs.rdb.

;; (and window-system
;;      (not (boundp 'running-xemacs))
;;      (>= emacs-major-version 21)
;;      (tool-bar-mode -1))

;; (setq default-frame-alist
;;       '((tool-bar-lines . 0)))

;;}}}
(as-progress "Minor modes... 75%")
;;{{{ Visible whitespace mode

(autoload 'visible-whitespace-mode "visws" "Visible whitespace mode" t)

;;}}}
;;{{{ tempo

(defvar tempo-initial-pos nil
  "Initial position in template after expansion")

;; FIXME: this should be done using tempo-user-elements, not defadvice.
(defadvice tempo-insert (around tempo-insert-pos act)
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it))

(defadvice tempo-insert-template (around tempo-insert-template-pos act)
  "Set initial position when defined.  ChristophConrad"
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
  "Skip self-insert if template function is called by an abbrev."
  (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

;;}}}
;;{{{ allout

;; (defun as-allout-init ()
;;   "Initialize allout-mode the way Adam likes it."
;;   (interactive)
;;   (when (not (featurep 'allout))
;;     (load "allout.el") ;; Loading .elc causes problems?
;;     (if (boundp 'outline-init)
;;         ;; Old versions init in a different way
;;         (outline-init t)
;;       (allout-init t))
;;     (substitute-key-definition 'beginning-of-line 'move-beginning-of-line global-map)
;;     (substitute-key-definition 'end-of-line 'move-end-of-line global-map)))

;; (eval-after-load "outline" '(as-allout-init))
;; (eval-after-load "muse" '(as-allout-init))

;; (defun as-allout-maybe-init ()
;;   "Hook for use within `find-file-hooks' to check whether a file needs
;; allout mode initialized."
;;   (interactive)
;;   (when (boundp 'allout-layout)
;;     (as-allout-init)))

;; (add-hook 'find-file-hooks 'as-allout-maybe-init)

(defvar allout-mode-leaders '((emacs-lisp-mode . ";;;_")
                              (muse-mode       . "*")))

;;}}}
;;{{{ msf-abbrev

(autoload 'msf-abbrev-mode "msf-abbrev" nil t)

(eval-after-load "msf-abbrev"
  '(setq msf-abbrev-root
         (concat as-emacs-dir "/minor-modes/msf-abbrev/mode-abbrevs")))

(global-set-key [(control <)] 'msf-cmd-previous-real)
(global-set-key [(control >)] 'msf-cmd-next-real)

;; Has indent-region changed arity?  Do we need a compatability wrapper?
;; (eval-after-load "msf-abbrev"
;;   (let ((arity (function-arity 'indent-region)))
;;     (if (eq (car arity) 2)
;;         (defun indent-region ...

;; This goes into an infinite loop :-(
;; (defadvice abbrev-mode (before abbrev-mode-msf-advice act)
;;   "Always use `msf-abbrev-mode' when `abbrev-mode' is enabled."
;;   (msf-abbrev-mode 1))

;; custom-set-variables takes care of this:
;;(eval-after-load "abbrev" '(require 'msf-abbrev))

(defun msf (&optional prefix)
  "Alias for msf-abbrev-mode."
  (interactive "p")
  (msf-abbrev-mode (or prefix 1)))

;;}}}
;;{{{ yasnippet

(autoload 'yas/minor-mode "yasnippet" nil nil)

;; Customize yas/snippet-dirs variables for snippet search path
;; (eval-after-load "yasnippet" '(yas/load-directory ...

(global-set-key [(control <)] 'yasnippet-cmd-previous-real)
(global-set-key [(control >)] 'yasnippet-cmd-next-real)

(defun yasm (&optional prefix)
  "Alias for yasnippet-mode."
  (interactive "p")
  (yas/minor-mode (or prefix 1)))

;;}}}
;;{{{ auto-complete-mode

(require 'auto-complete-config nil t)
(defun ac-config-default)
(when (as-check-feature-loaded 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "/home/adam/.emacs.d/ac-dict")
  (ac-config-default))

;;}}}
;;{{{ git-gutter

(require 'git-gutter)
(global-git-gutter-mode t)

;;}}}

;;}}}
;;{{{ Dual major/minor modes

(as-progress "Dual major/minor modes...")

;;{{{ outline-mode

(mapc (lambda (x)
        (add-hook x 'turn-on-auto-fill))
      '(outline-mode-hook outline-minor-mode-hook))

;;}}}

;;}}}

(as-progress "mode settings...done")

;;}}}

;;{{{ local variables

;;; Local Variables:
;;; auto-recompile: nil
;;; End:

;;}}}
