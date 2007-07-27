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

(require 'as-progress)
(require 'as-bufs-files-auto)
(require 'as-editing-auto)

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
(setq default-fill-column 70)

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
;; installdir alongside the .elc files.

;; (defvar as-emacs-dir)

;; (custom-set-variables
;;  '(find-function-source-path
;;    (append load-path
;;       (mapcar (lambda (p) (concat as-emacs-dir "/" p))
;;               (list
;;                "fun"
;;                "major-modes/monkey-2"
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
;;{{{ client/server mode

(defun ss ()
  "Abbreviation for `server-start'."
  (interactive)
  (server-start))

;;}}}
;;{{{ Diary, appointments

(display-time) ;; this is required
(add-hook 'diary-hook 'appt-make-list)

;;}}}

;;}}}
;;{{{ Mode-related settings

(as-progress "mode settings...")

;;{{{ Startup mode selection

(setq default-major-mode 'indented-text-mode)

;; Iterate over a copy of auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".
(mapcar (function
         (lambda (x)
           (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
        auto-mode-alist)

;; Each mode has its own block of elisp which will usually modify
;; auto-mode-alist, but here we add in some misc modes.
(setq auto-mode-alist
      (append '(
                ("\\.po[tx]?\\'\\|\\.po\\."              . po-mode)
                ("\\.htaccess$"                          . apache-mode)
                ("\\(httpd\\|srm\\|access\\)\\.conf$"    . apache-mode)
                ("\\.dump$"                              . tar-mode)
                ("/.zsh\\(env\\|rc\\)"                   . sh-mode)
                ("/.zsh/functions/"                      . sh-mode)
                ("\\.make$"                              . makefile-mode)

                ;; TWiki
                ("\\.tmpl$"                              . html-helper-mode)
                ("TWiki\\.cfg$"                          . cperl-mode)
                )
              auto-mode-alist))

;;}}}
;;{{{ Major modes

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

(add-to-list 'auto-mode-alist '("\\.\\(pod\\|t\\)\\'" . cperl-mode))

;;}}}
;;{{{ python-mode

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(autoload 'python-mode "python-mode" "python-mode" t)

;;}}}
;;{{{ Ruby

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "ruby-mode" t)

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
(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'as-font-lock-mode-if-window-system)

(defun sm () "Abbreviation for `sm-mode'." (interactive) (sh-mode))

;;}}}
;;{{{ Lisp

(add-hook 'lisp-mode-hook 'as-font-lock-mode-if-window-system)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;{{{ local-unset-key M-tab for hippie-expand

(if (eq (boundp 'running-xemacs) t)
    (add-hook 'emacs-lisp-mode-hook
              (lambda () (local-unset-key [(meta tab)]))))

;;}}}

;;}}}
;;{{{ ecmascript-mode

(add-to-list 'auto-mode-alist '("\\.js$" . ecmascript-mode))
(autoload 'ecmascript-mode "ecmascript-mode" "ecmascript-mode" t)

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
;;{{{ psgml (SGML and XML)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(setq auto-mode-alist
      (append '(
                ("\\.sgml$"                              . sgml-mode)
                ("\\.dtd$"                               . sgml-mode)
                )
              auto-mode-alist))

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

;;}}}
;;{{{ SGML

;;{{{ Set sgml-specials

;; See the documentation for this.  The default is (34 45) which
;; includes - as a special character for comments.  The problem
;; is that emacs doesn't allow predicates to context sensitively
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

(autoload 'nxml-mode "rng-auto" "Major mode to edit XML files." t)
(defun nx () "Loads nxml-mode." (interactive) (nxml-mode))
(autoload 'mhj-format-xml "mhj-xml" "Mark's nxml hacks." t)
(add-hook 'nxml-mode-hook
          (lambda () (local-set-key [(control meta q)] 'mhj-format-xml)))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xhtml\\)$" . nxml-mode))

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

(setq auto-mode-alist
      (append '(
                ("\\.prehtml\\'"                         . html-mode)
                ("\\.php3\\'"                            . html-mode)
                ("\\.\\(mason\\|m[cd]\\)\\'"             . html-mode)
                ("\\(auto\\|d\\)handler\\'"              . html-mode)
                )
              auto-mode-alist))

;; htmltidy support
(autoload 'tidy-buffer "htmltidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "htmltidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "htmltidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "htmltidy" "Install an options menu for HTML Tidy." t)

(eval-when-compile
  (defvar html-helper-mode-map)
  (defvar sgml-validate-command))

(defun as-html-mode-tidy-hook () "Add htmltidy support to an HTML mode."
  (tidy-build-menu html-helper-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "htmltidy"))

(add-hook 'html-mode-hook 'as-html-mode-tidy-hook)

(defun hhm () "Loads `html-helper-mode'." (interactive) (html-helper-mode))

;;}}}
;;{{{ CSS

(autoload 'css-mode "css-mode" "mode for editing CSS files" t)
(defvar cssm-indent-function 'cssm-c-style-indenter
  "Which function to use when deciding which column to indent to. To get
C-style indentation, use cssm-c-style-indenter.")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;}}}

;;}}}
;;{{{ Configuration languages

;;{{{ rpm-spec-mode

(add-to-list 'auto-mode-alist '("\\.spec$" . rpm-spec-mode))
(autoload 'rpm-spec-mode "rpm-spec-mode" "RPM spec mode." t)

;;}}}
;;{{{ sawfish

(autoload 'sawfish-mode "sawfish" "Mode for editing sawfish rep (lisp) files" t)
;;(add-hook 'sawfish-mode-hook
;;          (lambda () (turn-on-font-lock)))
;;(add-hook 'sawfish-mode-hook 'as-font-lock-mode-if-window-system)
(add-to-list 'auto-mode-alist
             '(".saw\\(mill\\|fish\\)rc\\'\\|\\.jl\\'" . sawfish-mode))

;;}}}
;;{{{ Apache

(autoload 'apache-mode "apache-mode" "mode for editing Apache config files")

;;}}}
;;{{{ xrdb

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
                ("\\.Xenvironment$" . xrdb-mode)
                ("\\.Xresources$"   . xrdb-mode)
                (".*\\.ad$"         . xrdb-mode)
                (".*\\.x?rdb$"      . xrdb-mode)
                )
              auto-mode-alist))

;;}}}

;;}}}
;;{{{ Semi-natural languages / documentation

;;{{{ Text

;; Turn on auto-fill if composing e-mail or news.
;;
;; For some reason the local buffer-file-name isn't set at the
;; stage when text-mode-hook gets run (possibly because it isn't
;; the current buffer at that stage?), but fortunately the
;; symbol filename is set to the loading file so we can use that
;; instead.

;; Silence compile errors (I know what I'm doing, honest)
(or (boundp 'filename) (defvar filename "" "sod knows"))

(add-hook 'text-mode-hook
          (lambda ()
            (local-unset-key "\e\t")
            (and
             (boundp 'filename)
             (not (eq filename t))
             (string-match
              "mutt-\\|\\.article\\|\\.letter"
              filename)
             (turn-on-auto-fill))))

;; Expand all tabs to spaces
(add-hook 'text-mode-hook (lambda () (setq indent-tabs-mode nil)))
(defun itm () "Shortcut to indented-text-mode."
  (interactive)
  (indented-text-mode))

;;}}}
;;{{{ ReStructuredText mode

(autoload 'rst-mode "rst")
(add-to-list 'auto-mode-alist '("\\.re?st$" . rst-mode))
(autoload 'rst-text-mode-bindings "rst")

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
  (local-set-key [(control c) (control p)] rst-prefix-map)
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

;;{{{ gnus

(defvar gnus-face-1 'gnus-cite-face-1)
(defvar gnus-face-2 'italic)
(defvar gnus-face-3 'gnus-cite-face-4)
(defvar gnus-sum-thread-tree-root "")

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
;;{{{ SDF

(add-to-list 'auto-mode-alist '("\\.sdf\\'" . sdf-mode))
(autoload 'sdf-mode "sdf-mode" "Mode for editing SDF files" t)

;;}}}
;;{{{ lilypond

(add-to-list 'auto-mode-alist '(".ly\\'" . lilypond-mode))
(autoload 'lilypond-mode "lilypond" "Mode for editing lilypond files" t)

;;}}}

;; muse mode is under productivity section

;;}}}
;;{{{ Organisation / productivity

;;{{{ muse-mode

(autoload 'muse-mode "muse" "muse-mode autoload" t)
(eval-after-load "muse"
  '(progn
     (require 'muse-mode)
     (require 'muse-html)
     (require 'muse-docbook)
     (require 'muse-latex)
     (require 'muse-texinfo)
     (require 'muse-wiki)
     (require 'muse-project)
;;      (defun muse-wiki-backlink ()
;;        "Grep for the current pagename in all the Wiki directories."
;;        (interactive)
;;        (emacs-wiki-grep (muse-page-name)))
;;      (define-key muse-mode-map "\C-c\C-b" 'muse-wiki-backlink)
     ))
(eval-when-compile (require 'muse-mode))

(autoload 'orgstruct-mode "org" t)
(defvar org-mode-map)
(add-hook 'muse-mode-hook
          (lambda ()
            (orgstruct-mode)
            (define-key muse-mode-map "\C-c."                 'org-time-stamp)
            (define-key muse-mode-map [(shift up)]            'org-timestamp-up)
            (define-key muse-mode-map [(shift down)]          'org-timestamp-down)
            (define-key org-mode-map  [(control left )]       'hide-subtree)
            (define-key org-mode-map  [(control right)]       'as-show-current)
            (define-key org-mode-map  [(control shift left)]  'hide-subtree)
            (define-key org-mode-map  [(control shift right)] 'show-subtree)
            ))

(defun mm () "Abbreviation for `muse-mode'." (interactive) (muse-mode))
(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))

;;}}}
;;{{{ remember-mode

(autoload 'remember "remember" nil t)
;; (autoload 'remember-handler-functions "remember" nil t)
;; (autoload 'remember-annotation-functions "remember" nil t)

;; (add-hook 'remember-mode-hook
;;           (lambda ()
;;             (require 'remember-planner)
;;             (setq remember-handler-functions '(remember-planner-append))
;;             (setq remember-annotation-functions planner-annotation-functions)))

;;}}}
;;{{{ planner-mode

;; (autoload 'planner-mode "planner.el" nil t)
;; (autoload 'plan "planner.el" nil t)

;; (eval-when-compile (require 'planner)
;;                    (require 'planner-accomplishments))

;; (add-hook 'planner-mode-hook
;;           (lambda ()
;;             (define-key planner-mode-map "\C-c\C-k" 'planner-delete-task)
;;             (planner-install-extra-task-keybindings)
;;             (planner-install-extra-note-keybindings)
;;             (planner-install-extra-context-keybindings)
;;             (require 'remember-planner)
;;             (require 'planner-id)
;;             (require 'planner-ical)
;;             (require 'planner-multi)
;;             (require 'planner-accomplishments)
;;             (require 'planner-trunk)
;;             (require 'planner-rank)
;;             (planner-accomplishments-insinuate)
;;             (require 'planner-deadline)
;;             (require 'planner-tasks-overview)))

;;}}}
;;{{{ org-mode

;; org-mode
(autoload 'org-mode "org" "Org mode" t)
(autoload 'org-time-stamp "org" "org-time-stamp" t)
(autoload 'org-timestamp-up "org" "org-timestamp-up" t)
(autoload 'org-timestamp-down "org" "org-timestamp-down" t)

(defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(autoload 'org-diary "org" "Diary entries from Org mode")
(autoload 'org-agenda "org" "Multi-file agenda from Org mode" t)
(autoload 'org-store-link "org" "Store a link to the current location" t)
(autoload 'orgtbl-mode "org" "Org tables as a minor mode" t)
(autoload 'turn-on-orgtbl "org" "Org tables as a minor mode")
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key [(control c)(control a)] 'org-agenda)
            ;; (local-set-key [(control c)(control l)] 'org-store-link)
            ))

;;}}}
;;{{{ etask-mode

(autoload 'etask "etask" "etask project management mode" t)

;;}}}

;;}}}
;;{{{ Interaction with other people

;;{{{ mutt

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")

;;}}}
;;{{{ crm114-mode

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

;;}}}
;;{{{ w3m-mode

;; Pull in autoloads
(require 'w3m-load "w3m-load" t)

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
;; (require 'psvn)

;;}}}
;;{{{ xtla

;; Used to build xtla with:
;;
;; ./configure --with-lispdir=~/lib/emacs/major-modes/xtla --infodir=~/local/info --with-other-dirs=~/lib/emacs/utils/tree-widget-2.0
;; make
;; make install
;; emk

;; now integrated into lib/emacs/Makefile

;;(load "xtla-load" 'noerror)

;;}}}
;;{{{ dvc

(require 'dvc-autoloads)
;;(load "dvc-load" 'noerror)
(global-set-key "\C-xTA"    'dvc-browse)
(global-set-key "\C-xT\C-m" 'dvc)

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

;;{{{ vc

(require 'vc)

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
    (folding-add-to-marks-list 'sh-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'tex-mode "% {{{ " "% }}}")
    (folding-add-to-marks-list 'ml-mode "\(* {{{ " "\(* }}} ")
    (folding-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
    (folding-add-to-marks-list 'sawfish-mode ";; {{{ " ";; }}}")
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
;;{{{ Time

;;(display-time)

;;}}}
;;{{{ Auto-compression mode

(cond ((as-quick-startup)
       (defun lac () "Load auto-compression-mode."
         (interactive)
         (auto-compression-mode)))
      (t
       (auto-compression-mode)))

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

;;}}}
;;{{{ Dual major/minor modes

;;{{{ outline-mode

(mapc (lambda (x)
        (add-hook x 'turn-on-auto-fill))
      '(outline-mode-hook outline-minor-mode-hook))

;;}}}

;;}}}

;;{{{ comment-start

(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; ")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; ")))
(add-hook 'text-mode-hook (lambda () (setq comment-start "> ")))
(add-hook 'cperl-mode-hook (lambda () (setq comment-start "#")))
(add-hook 'shell-script-mode-hook (lambda () (setq comment-start "#")))

(setq as-find-file-matching-regexp-alist
      (append '(("\*\.rdb$" . (lambda () (setq comment-start "! "))))
              as-find-file-matching-regexp-alist))

;;}}}

(as-progress "mode settings...done")

;;}}}

;;{{{ local variables

;;; Local Variables:
;;; auto-recompile: nil
;;; End:

;;}}}
