;; emacs startup file
;; Adam Spiers

;;{{{ To do list

;;
;;  - Start using same-window-regexps?
;;

;;}}}

;;{{{ Compiler declarations

(defvar running-xemacs nil "non-nil if the current emacs is an XEmacs")
(defun as-quick-startup nil
  "Non-nil if the current emacs was required to start up quickly."
  (getenv "QUICK_EMACS"))

;;}}}

;;{{{ Functions

;;{{{ Buffers/files

;;{{{ as-find-file-matching-regexp-hook

(defvar as-find-file-matching-regexp-alist '()
  "alist mapping filename regexps to functions which will be evaluated when 
filenames matching the regexps are visited.

This allows you to set local variables specific to sets of files, e.g.

(setq as-find-file-matching-regexp-alist
      '((\"/foo/bar/.*\.pm\" . (lambda () (setq cperl-indent-level 2)))))")

(defun as-find-file-matching-regexp-hook ()
  "Hook to run arbitrary functions on newly visited files.

Controlled by `as-find-file-matching-regexp-alist'."
  (mapcar
   (lambda (x)
     (cond
      ((let ((case-fold-search nil))
         (string-match (concat ".*" (car x)) (buffer-file-name)))
       ;; (message (format "%s matched %s" (buffer-file-name) (car x)))
       (funcall (cdr x)))
      (t
       ;; (message (format "%s didn't match %s" (buffer-file-name) (car x)))
       )))
   as-find-file-matching-regexp-alist))

(add-hook 'find-file-hooks 'as-find-file-matching-regexp-hook)

;;}}}
;;{{{ as-bounce-buffer

(defvar as-bounce-buffer-regexp-alist '()
  "Controls the behaviour of `as-bounce-buffer'.")

(defun as-bounce-buffer ()
  "For each element of `as-bounce-buffer-regexp-alist', attempts a search
and replace on the current buffer's filename.  (The CARs are the
search regexps, and the CDRs the corresponding strings to replace the
matches with).  As soon a search is successful, the filename resulting
from the replace is visited via `find-file'."
  (interactive)
  (catch 'gotcha
    (mapcar
     (lambda (x)
       (let ((case-fold-search nil) 
             (match (car x))
             (replace (cdr x)))
         (cond
          ((string-match match (buffer-file-name))
           (let ((bounce-to (replace-match replace t t (buffer-file-name) nil)))
             (message (format "Bounced to %s" bounce-to))
             (find-file bounce-to))
           (throw 'gotcha nil)))))
     as-bounce-buffer-regexp-alist)))

;;}}}
;;{{{ as-buffer-rename-via-alist-hook

(defvar as-buffer-renamings-alist '() 
  "Each element in this alist is a buffer renaming directive of the form

  (REGEXP . REPLACE)

When a find-file is performed, this hook matches the filename against
each REGEXP, and for the first one that matches, the matching part of
the buffer name is replaced with REPLACE.

The buffer is then renamed to the result.")

(defun as-buffer-rename-via-alist-hook ()
  "Hook to rename a buffer by matching it against regexps in
`as-buffer-renamings-alist', for which see the documentation."
  (catch 'endloop
    (mapcar
     (lambda (x)
       (cond ((let ((case-fold-search nil))
                (string-match (concat ".*" (car x)) (buffer-file-name)))
              (let ((rename-to
                     (replace-match (cdr x) t nil (buffer-file-name) nil)))
                (rename-buffer rename-to t)
;;              (message (format "renamed to %s" rename-to))
                (throw 'endloop t)))))
     as-buffer-renamings-alist)))

(add-hook 'find-file-hooks 'as-buffer-rename-via-alist-hook)

;; These are now obselete:
;;
;; (defun as-containing-dir (filename)
;;   "Return the containing directory of a filename when given the full path."
;;   (string-match "\\([^/]+\\)/[^/]+$" filename)
;;   (match-string 1 filename))
;;
;; (defun as-last-dir-and-filename (filename)
;;   "Strip a full path of all of its directory components but the last."
;;   (string-match "\\(.*/\\).+/.+$" (buffer-file-name))
;;   (replace-match "" t t (buffer-file-name) 1))
;;
;; (defun as-buffer-rename-add-one-dir ()
;;   "Add the name of the containing directory of the buffer's file
;; to the beginning of the buffer name."
;;   (interactive)
;;   (rename-buffer (as-last-dir-and-filename (buffer-name))) t)
;;
;;(add-hook 'find-file-hooks 'as-buffer-rename-add-one-dir)

(defun as-buffer-rename-remove-unique-id ()
  "Attempt to remove the unique suffix (e.g. \"<1>\") from the current
buffer's name.  It will fail if a buffer already exists with that name."
  (interactive)
  (and
   (string-match "<[0-9]+>$" (buffer-name))
   (rename-buffer (replace-match "" t t (buffer-name) nil))))

;;}}}
;;{{{ as-display-buffer-filename

(defun as-display-buffer-filename
  ()
  "Displays the current buffer's filename in the minibuffer."
  (interactive)
  (message buffer-file-name))

;;}}}

;;}}}
;;{{{ Editing

;;{{{ as-duplicate-line

(defun as-duplicate-line
  () 
  "Duplicates the current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line)
  (kill-new (buffer-substring (mark) (point)))
  (insert "\n")
  (yank))

;;}}}
;;{{{ as-join-line-with-next

(defun join-line-with-next ()
  "Joins the current line with the next.  This just calls join-line with
a prefix argument."
  (interactive)
  (join-line 1))

;;}}}

;;}}}
;;{{{ Appearance

;;{{{ font-lock-mode-if-window-system

(defun font-lock-mode-if-window-system
  ()
  "Turns on font-lock mode if X windows is active."
  (interactive)
  (if window-system (font-lock-mode)))

;;}}}

;;}}}

;;{{{ Enable disabled functions

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;}}}

;;}}}
;;{{{ Global key bindings

;;{{{ Keypad maps

(global-set-key [(kp-4)]        'backward-char)
(global-set-key [(kp-6)]        'forward-char)
(global-set-key [(kp-2)]        'next-line)
(global-set-key [(kp-8)]        'previous-line)
(global-set-key [(kp-7)]        'beginning-of-buffer)
(global-set-key [(kp-1)]        'end-of-buffer)
(global-set-key [(kp-9)]        'scroll-down)
(global-set-key [(kp-3)]        'scroll-up)
(global-set-key [(meta kp-4)]   'backward-word)
(global-set-key [(meta kp-6)]   'forward-word)

;;}}}
;;{{{ Delete key

(global-set-key [(delete)] 'delete-char)

;;}}}
;;{{{ Define fast scroll keys

(defun timsup () "Move up two lines." (interactive) (forward-line -2))
(defun timsdown () "Move down two lines." (interactive) (forward-line 2))

;;; Can only use M-down and M-up in X

(if window-system
    (progn
      (global-set-key [(meta down)] 'timsdown)
      (global-set-key [(meta up)]   'timsup)
      (global-set-key [(meta kp-2)] 'timsdown)
      (global-set-key [(meta kp-8)] 'timsup)))

;;}}}
;;{{{ hippie-expand ROCKS!

(global-set-key [(meta tab)] 'hippie-expand)
(global-set-key "\e\C-i"     'hippie-expand)

(setq hippie-expand-try-functions-list 
      '(
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers 
        try-expand-line
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially 
        try-complete-file-name
        try-expand-list
        try-complete-lisp-symbol-partially 
        try-complete-lisp-symbol
        ))

;;}}}
;;{{{ C-x 9

(global-set-key "\C-x9 "   'set-mark-command)
(global-set-key "\C-x9a"   'save-buffer)

;;{{{ toggle-indent-tabs-mode

(defun toggle-indent-tabs-mode
  () 
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode)
)

;;}}}
(global-set-key "\C-x9b"   'toggle-indent-tabs-mode)

(global-set-key "\C-x9c"   'comment-region)

;;{{{ insert-date-and-time

(defun insert-date-and-time
  ()
  "Inserts the current date and time into the current buffer at the point."
  (interactive)
  (insert
   (shell-command-to-string "date")
   )
  (backward-delete-char 1)
;;(insert (current-time-string))
)

;;}}}
(global-set-key "\C-x9d"   'insert-date-and-time)

(global-set-key "\C-x9g"   'goto-line)
(global-set-key "\C-x9i"   'auto-fill-mode)

;;{{{ insert-log-timestamp

(defun insert-log-timestamp
  ()
  "Inserts the current date, time and username into the current buffer at the point."
  (interactive)
  (insert
   (shell-command-to-string "date")
   )
  (backward-delete-char 1)
  (insert
;;   (current-time-string)
   " "
   (user-login-name))
)

;;}}}
(global-set-key "\C-x9l"   'insert-log-timestamp)

(global-set-key "\C-x9r"   'toggle-read-only)

;;{{{ toggle-truncate-lines

(defun toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "truncate-lines set to %s" truncate-lines)
)

;;}}}
(global-set-key "\C-x9t"   'toggle-truncate-lines)

(global-set-key "\C-x9v"   'set-variable)

;;{{{ set-tab-width

(defun set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "Nnew hard tab width: ")
  (setq tab-width width))

;;}}}
(global-set-key "\C-x9w"   'set-tab-width)

(global-set-key "\C-x9z"   'suspend-emacs)

;;}}}
;;{{{ Auto-text

(global-unset-key "\C-x\C-z")

;;{{{ Signatures

(global-set-key "\C-x\C-zj"     
                (function (lambda ()
                            "Inserts Adam's cool JAPH .sig"
                            (interactive)
                            (insert-file "~/.sig/perl/japh_indirect"))))

;;}}}
;;{{{ Scissors

(global-set-key "\C-x\C-zs"
                (function (lambda ()
                            "Inserts a cut-here-with-scissors"
                            (interactive)
                            (open-line 1)
                            (insert "--------- 8< --------- 8< --------- 8< --------- 8< --------- 8< ---------")
                            (beginning-of-line)
                            (next-line 1)
                            )))

;;}}}
;;{{{ Home pages

(global-set-key "\C-x\C-zh"
                (function (lambda ()
                            "Inserts Adam's homepage URL"
                            (interactive)
                            (insert "http://adamspiers.org/"))))

(global-set-key "\C-x\C-zo"
                (function (lambda ()
                            "Inserts Adam's old homepage URL"
                            (interactive)
                            (insert "http://www.new.ox.ac.uk/~adam/"))))

(global-set-key "\C-x\C-zt"
                (function (lambda ()
                            "Inserts the tigerpig.org URL"
                            (interactive)
                            (insert "http://tigerpig.org/"))))

;;}}}
;;{{{ e-mail address

(global-set-key "\C-x\C-ze"
                (function (lambda ()
                            "Inserts Adam's e-mail address"
                            (interactive)
                            (insert "adam@spiers.net"))))

;;}}}
;;{{{ Name (how lazy am I?)

(global-set-key "\C-x\C-zm"
                (function (lambda ()
                            "Inserts Adam's name"
                            (interactive)
                            (insert "Adam Spiers"))))

;;}}}
;;{{{ Me (name & e-mail)

(global-set-key "\C-x\C-zn"
                (function (lambda ()
                            "Inserts Adam's name and e-mail address"
                            (interactive)
                            (insert "Adam Spiers <adam@spiers.net>"))))

;;}}}

;;}}}
;;{{{ Miscellaneous

(global-set-key "\C-x\C-a"      'bury-buffer)
(global-set-key "\M-\\"         'fixup-whitespace)
(global-set-key "\C-xt"         'revert-buffer)
(global-set-key "\C-ha"         'apropos)
(global-set-key "\M-g"          'goto-line)
(global-set-key "\C-ha"         'apropos)

(global-set-key [(control tab)]         'other-window)
(global-set-key [(control meta $)]      'ispell-buffer)
(global-set-key [(control meta tab)]    'ispell-complete-word)
(global-set-key [(meta i)]              'indent-relative)

(global-set-key [(f1)] 'ispell-word)
(global-set-key [(f2)] 'as-bounce-buffer)
(global-set-key [(f3)] 'as-display-buffer-filename)
(global-set-key [(f4)] 'as-duplicate-line)

(global-set-key [(insert)] 'overwrite-mode)
(global-set-key [(meta o)] 'overwrite-mode)

(global-set-key [(control meta y)] 'join-line-with-next)

;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.

(global-set-key "\C-x\C-b"      'buffer-menu)

;;}}}

;;}}}
;;{{{ Point movement

;;{{{ Track end of line

;; This is turning out to be annoying
;;(setq track-eol t)

;;}}}
;;{{{ Scrolling

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

;;}}}
;;{{{ Show position in modeline

(line-number-mode 1)
(column-number-mode 1)

;;}}}
;;{{{ Default right margin

(setq default-fill-column 70)

;;}}}
;;{{{ Stop down cursor adding newlines to end of buffer.

(setq next-line-add-newlines nil)

;;}}}
;;{{{ IntelliMouse

(cond (window-system (load "mwheel" t)))

;;}}}

;;}}}
;;{{{ Little odds and ends

;;{{{ Minibuffer

;; GNU emacs 21 obseletes resize-minibuffer-mode
(cond ((or running-xemacs (<= emacs-major-version 20))
       (resize-minibuffer-mode)
       (setq resize-minibuffer-window-max-height 5 
             resize-minibuffer-frame-max-height 5)))

;;}}}
;;{{{ Apropos extension

(defvar apropos-do-all)
(setq apropos-do-all t)

;;}}}
;;{{{ Make kill-line kill whole line if at beginning of line

(setq kill-whole-line t)

;;}}}
;;{{{ Visible bell

(setq-default visible-bell t)

;;}}}
;;{{{ Saving sessions

(autoload 'desktop-save "desktop" "Saves desktop session state." t)

;;}}}

;;}}}
;;{{{ Mode-related settings

;;{{{ Startup mode selection

;;{{{ Mode selection based on filename

;; Iterate over a copy of auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".

(mapcar (function
         (lambda (x) 
           (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
        auto-mode-alist)


;; Add in other modes

(setq auto-mode-alist
      (append '(
                ("\\.\\(pod\\|t\\)\\'"                   . cperl-mode)
                ("\\.prehtml\\'"                         . html-mode)
                ("\\.php3\\'"                            . html-mode)
                ("\\.sdf\\'"                             . sdf-mode)
                ("\\.po[tx]?\\'\\|\\.po\\."              . po-mode)
                (".saw\\(mill\\|fish\\)rc\\'\\|\\.jl\\'" . sawfish-mode)
                (".ly\\'"                                . lilypond-mode)
                ("\\.\\(mason\\|m[cd]\\)\\'"             . html-mode)
                ("\\(auto\\|d\\)handler\\'"              . html-mode)
                ("\\.css\\'"                             . css-mode)
                ("\\.htaccess$"                          . apache-mode)
                ("\\(httpd\\|srm\\|access\\)\\.conf$"    . apache-mode)
                )
              auto-mode-alist))

;;}}}
;;{{{ Default major mode

(setq default-major-mode 'indented-text-mode)

;;}}}

;;}}}
;;{{{ Major modes

;;{{{ Text

;;{{{ Auto-fill

;; Turn on auto-fill if composing e-mail or news.
;;
;; For some reason the local buffer-file-name isn't set at the
;; stage when text-mode-hook gets run (possibly because it isn't
;; the current buffer at that stage?), but fortunately the
;; symbol filename is set to the loading file so we can use that
;; instead.

;; Try to silence compile errors (I know what I'm doing, honest)
(or (boundp 'filename) (defvar filename "" "sod knows"))

(add-hook 'text-mode-hook
          (lambda ()
            (if (and (boundp 'filename)
                     (not (eq filename t))
                     (string-match 
                      "mutt-thelonious\\|\\.article\\|\\.letter" filename))
                (turn-on-auto-fill))))

;;}}}
;;{{{ Expand all tabs to spaces

(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;;}}}

;;}}}
;;{{{ Perl

;;{{{ Tab widths

(defvar perl-indent-level 2 "*")
(defvar perl-continued-statement-offset 2 "*")
(defvar perl-continued-brace-offset -2 "*")
(defvar perl-label-offset -1 "*")

(add-hook 
 'perl-mode-hook
 (function (lambda () 
             (setq 
              perl-indent-level 2
              perl-continued-statement-offset 2
              perl-continued-brace-offset -2
              perl-label-offset -1)
;;           (set-face-background 'font-lock-emphasized-face "black")
;;           (set-face-background 'font-lock-other-emphasized-face "black")
             )))

;;}}}
;;{{{ Turn on tab/commenting

(eval-when-compile
  (defvar perl-tab-to-comment))
(setq perl-tab-to-comment t)

;;}}}
;;{{{ Turn on font-lock-mode on entry

(add-hook 'perl-mode-hook 'font-lock-mode-if-window-system)

;;}}}

;;}}}
;;{{{ CPerl

;;{{{ Autoload

;; one of these two will work
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)

;;}}}
;;{{{ Indentation

(make-variable-buffer-local 'cperl-indent-level)
(set-default 'cperl-indent-level 2)

;;}}}
;;{{{ Hairy options

(eval-when-compile
  (defvar cperl-font-lock)
  (defvar cperl-electric-lbrace-space)
  (defvar cperl-electric-parens)
  (defvar cperl-electric-linefeed)
  (defvar cperl-electric-keywords)
  (defvar cperl-info-on-command-no-prompt)
  (defvar cperl-clobber-lisp-bindings)
  (defvar cperl-lazy-help-time))

(setq cperl-font-lock t
      cperl-electric-lbrace-space nil
      cperl-electric-parens nil
      cperl-electric-linefeed nil
      cperl-electric-keywords nil
      cperl-info-on-command-no-prompt t
      cperl-clobber-lisp-bindings nil
      cperl-lazy-help-time 5)

;;}}}
;;{{{ Font-lock on

(setq cperl-font-lock t)

;;}}}
;;{{{ Local key bindings

;;{{{ as-cperl-set-indent-level

(defvar cperl-indent-level)
(defun as-cperl-set-indent-level (width)
  "Sets the cperl-indent-level variable to the given argument."
  (interactive "NNew cperl-indent-level: ")
  (setq cperl-indent-level width))

;;}}}
;;{{{ as-cperl-insert-self-line

(defun cperl-indent-command (&optional whole-exp))
(defun as-cperl-insert-self-line ()
  "Inserts a

  my $self = shift;

line before the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (cperl-indent-command)
    (insert "my $self = shift;")))

;;}}}
;;{{{ as-cperl-insert-args-line

(defun as-cperl-insert-args-line ()
  "Inserts a

  my () = @_;

line before the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (cperl-indent-command)
  (insert "my () = @_;")
  (backward-char 7))

;;}}}
;;{{{ as-cperl-make-method

(fset 'as-cperl-make-method
      "\C-asub \C-e {\C-m\C-imy $self = shift;\C-m}\C-o\C-a\C-o\C-i")

;;}}}

(add-hook 'cperl-mode-hook 
          (function
           (lambda ()
             (local-set-key "\C-ca" 'as-cperl-insert-args-line)
             (local-set-key "\C-cm" 'as-cperl-make-method)
             (local-set-key "\C-cp" 'cperl-find-pods-heres)
             (local-set-key "\C-ci" 'as-cperl-set-indent-level)
             (local-set-key "\C-cs" 'as-cperl-insert-self-line)
             (local-set-key [(backspace)] 'cperl-electric-backspace)
             (local-set-key [(control c) (control h) (control j)] 'imenu)
             (setq indent-tabs-mode nil)
             )))

;;}}}
;;{{{ Set faces

;; (add-hook 
;;  'cperl-mode-hook
;;  (function
;;   (lambda ()
;;     (if window-system
;;      (set-face-background 'font-lock-emphasized-face "black")
;;       (set-face-background 'font-lock-other-emphasized-face "black")
;;       (set-face-font
;;        'font-lock-function-name-face
;;        "-*-courier-bold-r-*-*-*-220-*-*-*-*-*-*")
;;       (set-face-foreground
;;        'font-lock-function-name-face
;;        "steelblue"))
;;     )))

;;}}}

;;}}}
;;{{{ Shell-script

;;{{{ Autoload sh-script on invocation

(autoload 'shell-script-mode "sh-script"
  "Major mode for editing shell scripts" t)

;;}}}
;;{{{ Turn on font-lock mode on entry

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'font-lock-mode-if-window-system)

;;}}}

;;}}}
;;{{{ Lisp

;;{{{ Turn on font-lock mode on entry

(add-hook 'lisp-mode-hook 'font-lock-mode-if-window-system)

;;}}}
;;{{{ local-unset-key M-tab for hippie-expand

(if (eq running-xemacs t)
    (add-hook 'emacs-lisp-mode-hook
              (function (lambda () (local-unset-key [(meta tab)])))))

;;}}}

;;}}}
;;{{{ sawfish

(autoload 'sawfish-mode "sawfish" "Mode for editing sawfish rep (lisp) files" t)
;;(add-hook 'sawfish-mode-hook
;;          (function (lambda () (turn-on-font-lock))))
;;(add-hook 'sawfish-mode-hook 'font-lock-mode-if-window-system)

;;}}}
;;{{{ HTML

(add-hook 'html-mode-hook 
          (function (lambda ()
                      (auto-fill-mode -1)
;;                    (setq truncate-lines t)
                      )))

;;}}}
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
;;{{{ CSS

(autoload 'css-mode "css-mode" "mode for editing CSS files")

;;}}}
;;{{{ Apache

(autoload 'apache-mode "apache-mode" "mode for editing Apache config files")

;;}}}
;;{{{ mutt

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")

;;}}}
;;{{{ TeX

;;{{{ Set up tex-dvi-view (C-c C-v)

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;;}}}
;;{{{ Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'font-lock-mode-if-window-system)

;;}}}

;;}}}
;;{{{ C

;;{{{ C indentation setup

;; for compiling
(eval-when-compile
  (defvar c-tab-always-indent)
  (defvar c-indent-level)
  (defvar c-continued-statement-offset)
  (defvar c-brace-offset)
  (defvar c-brace-imaginary-offset)
  (defvar c-argdecl-indent)
  (defvar c-label-offset)
  (defvar )
  (defvar )
  (defvar )
  )

(setq c-tab-always-indent t
      ;; setq c-auto-newline t
      c-indent-level 4
      c-continued-statement-offset 4
      c-brace-offset -4
      c-brace-imaginary-offset 0
      c-argdecl-indent 0
      c-label-offset -4)

;; Whatever this is supposed to do, it buggers up XEmacs' idea of
;; version compatability with byte-compiled code!
;;(setq default-case-fold-search nil)

;;}}}
;;{{{ Turn on font-lock mode on entry

(add-hook 'c-mode-hook 'font-lock-mode-if-window-system)

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
;;{{{ man

(eval-when-compile
  (defvar Man-notify-method))
(setq Man-notify-method 'pushy)

;;}}}
;;{{{ SDF

(autoload 'sdf-mode "sdf-mode" "Mode for editing SDF files" t)

;;}}}
;;{{{ lilypond

(autoload 'lilypond-mode "lilypond" "Mode for editing lilypond files" t)

;;}}}

;;}}}
;;{{{ Minor modes

;;{{{ vc

(require 'vc)

;;}}}
;;{{{ iswitchb - better buffer switching

(iswitchb-default-keybindings)

;;}}}
;;{{{ Folding mode

;;{{{ Fix compiler warnings

(eval-and-compile
  (defun fold-point-folded-p (arg))
  (defun fold-top-level ())
  (defun fold-use-overlays-p ())
  (defun fold-show (&optional arg1 &optional arg2 &optional arg3))
  (defun fold-enter (&optional arg))
  (defun fold-narrow-to-region (&optional arg1 &optional arg2 &optional arg3))
  (defun fold-open-buffer ())
  )

;;}}}

;;{{{ Set marks for individual modes

(autoload 'fold-add-to-marks-list "folding" "folding mode")

(eval-after-load "folding"
  '(progn
    (fold-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
    (fold-add-to-marks-list 'Fundamental-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'shellscript-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'shell-script-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'Shellscript-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'Shell-script-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'sh-mode "\# {{{ " "\# }}}")
    (fold-add-to-marks-list 'tex-mode "% {{{ " "% }}}")
    (fold-add-to-marks-list 'ml-mode "\(* {{{ " "\(* }}} ")
    (fold-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
    (fold-add-to-marks-list 'sawfish-mode ";; {{{ " ";; }}}")
    ))

(setq as-find-file-matching-regexp-alist
      (append '(("\*\.rdb$" . (lambda () (fold-set-marks "! {{{ " "! }}} "))))
              as-find-file-matching-regexp-alist))

;;}}}
;;{{{ Autoload mode via local variables

(autoload 'folding-mode "folding" "folding mode")
(autoload 'folding-mode-find-file "folding" "folding mode")
(autoload 'folding-mode-add-find-file-hook "folding" "folding mode")
(autoload 'fold-set-marks "folding" "folding mode")
(cond ((as-quick-startup)
       (defun lf () "Loads folding-mode." (interactive) (folding-mode)))
      (t
       (folding-mode-add-find-file-hook)))

;;}}}
;;{{{ Key bindings

(defvar folding-mode-map) ;; avoid compile warnings
(autoload 'fold-show "folding")

(add-hook 'folding-mode-hook
          (lambda ()
            ;; keypad
            (define-key folding-mode-map [kp-7] 'fold-enter)
            (define-key folding-mode-map [kp-9] 'fold-exit)
            (define-key folding-mode-map [kp-1] 'fold-show)
            (define-key folding-mode-map [kp-3] 'fold-hide)
            (define-key folding-mode-map [kp-4] 'fold-backward-char)
            (define-key folding-mode-map [kp-6] 'fold-forward-char)

            ;; These ones for VT100s (I think)
            (define-key folding-mode-map "\M-Ow" 'fold-enter)
            (define-key folding-mode-map "\M-Oy" 'fold-exit)
            (define-key folding-mode-map "\M-Oq" 'fold-show)
            (define-key folding-mode-map "\M-Os" 'fold-hide)
            (define-key folding-mode-map "\M-OP" 'fold-enter)
            (define-key folding-mode-map "\M-OQ" 'fold-exit)

            ;; Quick navigation
            (local-set-key [(control meta <)] 'fold-exit)
            (local-set-key [(control meta >)] 'fold-enter)
            ))

(eval-when-compile
  (defvar fold-default-keys-function))
(setq fold-default-keys-function 'fold-bind-backward-compatible-keys)

;;}}}

;;}}}
;;{{{ Transient Mark mode

(if (not (boundp 'transient-mark-mode))
    (defun transient-mark-mode (arg1) nil))
(if (not running-xemacs) (transient-mark-mode 1))

;;}}}
;;{{{ Font-Lock mode

(eval-when-compile
  (defvar font-lock-support-mode))
(setq font-lock-support-mode 'lazy-lock-mode)

;; Turn it on
;;(global-font-lock-mode 1)

;; Do this via customisation since it's different for xemacs
;;(if (and window-system (not running-xemacs)) (global-font-lock-mode t))

;; This one probably a waste of time
(font-lock-mode-if-window-system)

;; Set some colours

(if window-system
    (progn
;;      (set-face-foreground 'font-lock-comment-face "PaleGreen")
      (set-face-underline-p 'underline nil)))

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

(display-time)

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

(defun blinking-cursor-mode (&optional arg))
(and window-system
     (not running-xemacs)
     (= emacs-major-version 20)
     (load "blinking-cursor" t)
     (blinking-cursor-mode 1))

;;}}}
;;{{{ recentf

(and window-system (load "recentf" 'noerror))

;;}}}

;;}}}
;;{{{ Hooks

;;{{{ comment-start

(add-hook 'lisp-mode-hook (function (lambda () (setq comment-start ";; "))))
(add-hook 'emacs-lisp-mode-hook (function (lambda () (setq comment-start ";; "))))
(add-hook 'text-mode-hook (function (lambda () (setq comment-start "> "))))
(add-hook 'cperl-mode-hook (function (lambda () (setq comment-start "#"))))
(add-hook 'shell-script-mode-hook (function (lambda () (setq comment-start "#"))))

(setq as-find-file-matching-regexp-alist
      (append '(("\*\.rdb$" . (lambda () (setq comment-start "! "))))
              as-find-file-matching-regexp-alist))

;;}}}
;;{{{ hippie-expand

(add-hook 'text-mode-hook (function (lambda () (local-unset-key "\e\t"))))

;;}}}

;;}}}

;;}}}

;;{{{ local variables

;;; Local Variables:
;;; auto-recompile: nil
;;; End:

;;}}}
