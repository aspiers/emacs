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

(add-to-list 'auto-mode-alist
             '("/\\.zsh\\(env\\|rc\\|/functions/\\)\\|\\.stp$" . sh-mode))

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

(use-package apache-mode
  :mode ("\\.htaccess$\\|\\(httpd\\|srm\\|access\\)\\.conf$" . apache-mode))

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

;;(setq major-mode 'indented-text-mode)

;; Iterate over auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".
(mapc (function
       (lambda (x)
         (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
      auto-mode-alist)

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

    (if (string-match "itsalltext" bn)
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
;;{{{ TWiki

(autoload 'erin-mode "erin" nil t)

(add-to-list 'auto-mode-alist '("\\.tmpl$" . html-helper-mode))

;;}}}

;; muse mode is under productivity section

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

(autoload 'w3m-find-file "w3m")
(autoload 'dired-get-filename "dired")
(defun dired-w3m-find-file ()
  (interactive)
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
;;{{{ File handling

(add-to-list 'auto-mode-alist '("\\.dump$" . tar-mode))

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

(provide 'as-major-modes)
