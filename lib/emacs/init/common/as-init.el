;; emacs startup file
;; Adam Spiers

;;{{{ To do list

;;
;;  - fix compile warnings:
;;
;;  you must predefine autoloads for used functions so that compiler knows
;;  about them.
;;  (eval-and-compile
;;     (autoload 'fold-set-marks "folding)
;;     etc...
;;     )
;;  for variables, use `defvar' trick in hooks, where you are sure that
;;  module has already defined the variable. `defvar' is no-op if variable
;;  has already been defined, but it makes compiler happy; thinking that
;;  you DID define variable before using it.
;;      ...in hook
;;          (defvar folding-mode-map)
;;          (define-key folding-mode-map [kp-7] 'fold-enter)  
;;
;;  - Start using same-window-regexps?
;;

;;}}}

;;{{{ Function definitions

;;{{{ set-fill-column-to-normal

(defun set-fill-column-to-normal
  () 
  "Sets the fill column to 75."
  (interactive)
  (setq fill-column 75))

;;}}}
;;{{{ toggle-truncate-lines

(defun toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "truncate-lines set to %s" truncate-lines)
)

;;}}}
;;{{{ toggle-indent-tabs-mode

(defun toggle-indent-tabs-mode
  () 
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode)
)

;;}}}
;;{{{ duplicate-line

(defun duplicate-line
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
;;{{{ emacs-Info

(defun emacs-Info (&optional file)
  "Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir' 
in all the directories in that path."
  (interactive (if current-prefix-arg
                   (list (read-file-name "Info file name: " nil nil t))))
  (if file
      (info file)
    (if (get-buffer "*info*")
        (pop-to-buffer "*info*")
      (info "/usr/info/emacs.gz"))))

;;}}}
;;{{{ kill-whole-line

;; (defun kill-whole-line
;;   ()
;;   "Kills the current line."
;;   (interactive) 
;;   (beginning-of-line)
;;   (kill-line 1))

;;}}}
;;{{{ font-lock-mode-if-window-system

(defun font-lock-mode-if-window-system
  ()
  "Turns on font-lock mode if X windows is active."
  (interactive)
  (if window-system (font-lock-mode)))

;;}}}

;;}}}
;;{{{ Key bindings

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
;;{{{ Function keys

;;(define-key function-key-map (as-key-sequence [delete] [(delete)]) "\C-d")
(global-set-key [(delete)] 'delete-char)

;;}}}
;;{{{ define fast scroll keys

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
;;{{{ Personal (including c-x 9) extensions

(global-set-key "\C-x\C-a"      'bury-buffer)

(global-set-key "\C-x9 "        'set-mark-command)
(global-set-key "\C-x9a"        'save-buffer)
(global-set-key "\C-x9b"        'toggle-indent-tabs-mode)
(global-set-key "\C-x9c"        'comment-region)
(global-set-key "\C-x9d"        'insert-date-and-time)
(global-set-key "\C-x9f"        'set-fill-column-to-normal)
(global-set-key "\C-x9g"        'goto-line)
(global-set-key "\C-x9i"        'auto-fill-mode)
(global-set-key "\C-x9l"        'insert-log-timestamp)
(global-set-key "\C-x9r"        'toggle-read-only)
(global-set-key "\C-x9t"        'toggle-truncate-lines)
(global-set-key "\C-x9v"        'set-variable)
(global-set-key "\C-x9z"        'suspend-emacs)

;;}}}
;;{{{ Hippie expand ROCKS!

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
;;{{{ Miscellaneous

(global-set-key "\M-\\"         'fixup-whitespace)
(global-set-key "\C-xt"         'revert-buffer)
(global-set-key "\C-ha"         'apropos)
(global-set-key "\M-g"          'goto-line)
(global-set-key "\C-ha"         'apropos)

(global-set-key [(control tab)]         'other-window)
(global-set-key [(control meta $)]      'ispell-buffer)
(global-set-key [(control meta tab)]    'ispell-complete-word)
(global-set-key [(meta i)]              'indent-relative)

(global-set-key [(f1)]          'ispell-word)
(global-set-key [(f2)]          'emacs-Info)
(global-set-key [(f4)]          'duplicate-line)
(global-set-key [(insert)]      'overwrite-mode)
(global-set-key [(meta o)]      'overwrite-mode)

;; Set C-x C-b to buffer-menu rather than list-buffers
;; so that the point automatically gets put in the
;; buffer menu.

(global-set-key "\C-x\C-b"      'buffer-menu)

;;}}}
;;{{{ Autotext

(global-unset-key "\C-x\C-z")

;; Signatures

(global-set-key "\C-x\C-zc"     
                (function (lambda () (interactive)
                            (insert-file "~/.sig/cool"))))
(global-set-key "\C-x\C-zj"     
                (function (lambda () (interactive)
                            (insert-file "~/.sig/japh_indirect"))))
(global-set-key "\C-x\C-zp"     
                (function (lambda () (interactive)
                            (insert-file "~/.sig/personal"))))
(global-set-key "\C-x\C-zr"     
                (function (lambda () (interactive)
                            (insert-file "~/.sig/RTFM"))))

;; Scissors

(global-set-key "\C-x\C-zs"
                (function (lambda () (interactive)
                            (open-line 1)
                            (insert "--------- 8< --------- 8< --------- 8< --------- 8< --------- 8< ---------")
                            (beginning-of-line)
                            (next-line 1)
                            )))

;; Home-page

(global-set-key "\C-x\C-zh"
                (function (lambda () (interactive)
                            (insert "http://www.new.ox.ac.uk/~adam/"))))
(global-set-key "\C-x\C-zu"
                (function (lambda () (interactive)
                            (insert "http://www.new.ox.ac.uk/~adam/"))))

;; e-mail address

(global-set-key "\C-x\C-ze"
                (function (lambda () (interactive)
                            (insert "adam@spiers.net"))))

;; name (how lazy am I?)

(global-set-key "\C-x\C-zn"
                (function (lambda () (interactive)
                            (insert "Adam Spiers"))))

;; me (name & e-mail)

(global-set-key "\C-x\C-zn"
                (function (lambda () (interactive)
                            (insert "Adam Spiers <adam@spiers.net>"))))

;;}}}

;;}}}
;;{{{ Little odds and ends

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
;;{{{ Minibuffer

(resize-minibuffer-mode)
(setq resize-minibuffer-window-max-height 5 
      resize-minibuffer-frame-max-height 5)

;;}}}
;;{{{ Enable eval-expression

(put 'eval-expression 'disabled nil)

;;}}}
;;{{{ Enable set-goal-column

(put 'set-goal-column 'disabled nil)

;;}}}
;;{{{ Stop down cursor adding newlines to end of buffer.

(setq next-line-add-newlines nil)

;;}}}
;;{{{ Apropos extension

(setq apropos-do-all t)

;;}}}
;;{{{ Make kill-line kill whole line if at beginning of line

(setq kill-whole-line t)

;;}}}
;;{{{ Visible bell

(setq-default visible-bell t)

;;}}}
;;{{{ IntelliMouse

(load "mwheel" t)

;;}}}
;;{{{ Start server

;; Set Emacs as server for all other emacsclients.
;; This needs EDITOR enviroment variable (and VISUAL etc.) to be
;; set to 'emacsclient'.
;;
;; Screen is aliased to ~/lib/screen which on entry aliases emacs to
;; emacsclient, and on exit unaliases it.

;;(load-if-exists "server-x")

;;(server-start)

;;}}}
;;{{{ Buffer renaming based on filename

(defvar as-buffer-renamings-alist '() 
  "Maps regexps matching file names to new buffer names.

If a find-file is performed on a filename which matches one of these
regexps, the buffer name is renamed to the corresponding entry in this
alist.")

(add-hook 'find-file-hooks 
          (function (lambda ()
                      (catch 'endloop
                        (mapcar
                         (lambda (x)
                           (if (string-match (car x) (buffer-file-name))
                               (progn
                                 (rename-buffer (cdr x) t)
                                 (throw 'endloop t))))
                         as-buffer-renamings-alist)))))

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
      (append (list
               '("\\.prehtml\\'" . html-mode)
               '("\\.php3\\'" . html-mode)
               '("\\.sdf\\'" . sdf-mode)
;;             '("\\.info\\(\\.gz\\)?\\'" . info)
               '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
               '(".saw\\(mill\\|fish\\)rc\\'\\|\\.jl\\'"
                 . sawfish-mode)
               '(".ly\\'" . lilypond-mode)
               )
              auto-mode-alist))

;;}}}
;;{{{ Default major mode

(setq default-major-mode 'indented-text-mode)

;;}}}

;;}}}

;; Major modes

;;{{{ Fundamental

;;{{{ Tab widths

(add-hook 'fundamental-mode-hook
          (function (lambda () (setq tab-width 4))))

;;}}}

;;}}}
;;{{{ C

;;{{{ C indentation setup

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
;;{{{ TeX

;;{{{ Set up tex-dvi-view (C-c C-v)

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;;}}}
;;{{{ Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'font-lock-mode-if-window-system)

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

(setq perl-tab-to-comment t)

;;}}}
;;{{{ Turn on font-lock-mode on entry

(add-hook 'perl-mode-hook 'font-lock-mode-if-window-system)

;;}}}

;;}}}
;;{{{ CPerl

;;{{{ Autoload

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)

;;}}}
;;{{{ Hairy options

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
;;{{{ Change some local key mappings

(add-hook 'cperl-mode-hook 
          (function 
           (lambda ()
             (local-set-key "\C-xpr" 'cperl-find-pods-heres)
             (local-set-key [(backspace)] 'cperl-electric-backspace)

;;           What the hell is this one??
;;           (local-set-key [(delete)] 'backward-or-forward-delete-char)

             (local-set-key "(" 'self-insert-command)
             (local-set-key "<" 'self-insert-command)
             (local-set-key "[" 'self-insert-command)
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

;;{{{ Tab widths

(add-hook 'shell-script-mode-hook
          (lambda () (setq tab-width 4)))

;;}}}
;;{{{ Autoload sh-script on invocation

(autoload 'shell-script-mode "sh-script"
  "Major mode for editing shell scripts" t)

;;}}}
;;{{{ Turn on font-lock mode on entry

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'font-lock-mode-if-window-system)

;;}}}

;;}}}
;;{{{ Info

;; (global-set-key [\s-TAB]     'Info-prev-reference)
;; (global-set-key "\C-hi"              'emacs-Info)

;;}}}
;;{{{ Text

;;{{{ Tab widths

(add-hook 'text-mode-hook
          (lambda () (setq tab-width 4)))

;;}}}
;;{{{ Auto-fill

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Turn on auto-fill if composing e-mail or news.
;;
;; For some reason the local buffer-file-name isn't set at the
;; stage when text-mode-hook gets run (possibly because it isn't
;; the current buffer at that stage?), but fortunately the
;; symbol filename is set to the loading file so we can use that
;; instead.

;; This screws things up badly! :
;; (defvar filename nil "sod knows")

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
;;{{{ HTML

(add-hook 'html-mode-hook 
          (function (lambda ()
                      (auto-fill-mode -1)
;;                    (setq truncate-lines t)
                      )))

;;}}}
;;{{{ SGML

;;{{{ Set sgml-specials

;; See the documentation for this.  The default is (34 45) which
;; includes - as a special character for comments.  The problem
;; is that emacs doesn't allow predicates to context sensitively
;; confirm the syntax of characters, so the fontifying of comments
;; often screws up.

(setq sgml-specials '(34))

;;}}}

;;}}}
;;{{{ man

(setq Man-notify-method 'pushy)

;;}}}
;;{{{ sawmill

(autoload 'sawfish-mode "sawfish" "mode for editing sawfish rep (lisp) files" t)

;;}}}
;;{{{ lilypond

(autoload 'lilypond-mode "lilypond" "mode for editing lilypond files" t)

;;}}}
;;{{{ SDF

(autoload 'sdf-mode "sdf-mode" "mode for editing SDF files" t)

;;}}}
;;{{{ po

;;{{{ Autoload

(autoload 'po-mode "po-mode")

;;}}}
;;{{{ Autoload the international fonts where necessary

;; ... I think.  RTF info page, dude.

(autoload 'po-find-file-coding-system "po-mode")
(modify-coding-system-alist 'file "\\.po[tx]?\\'\\|\\.po\\."
                            'po-find-file-coding-system)

;;}}}

;;}}}
;;{{{ mutt

(autoload 'mutt-mode "mutt" "mode for editing mutt files")

;;}}}

;; Minor modes

;;{{{ iswitchb - better buffer switching

(iswitchb-default-keybindings)

;;}}}
;;{{{ Folding mode

;;{{{ Key bindings

(add-hook 'folding-mode-hook
          (lambda ()
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

            ;; Override C-b binding
            ;; (define-key folding-mode-map "\C-b"  'kill-whole-line)
            ))

(setq fold-default-keys-function 'fold-bind-backward-compatible-keys)

;;}}}
;;{{{ Load folding mode

(load "folding" t)

;;}}}
;;{{{ Set default marks

(fold-set-marks "{{{" "}}}")

;;}}}
;;{{{ Enable auto-loading of mode via local variables

(folding-mode-add-find-file-hook)

;;}}}
;;{{{ Set marks for individual modes

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

;;}}}

;;}}}
;;{{{ Transient Mark mode

(if (eq running-xemacs t) t (transient-mark-mode 1))

;;}}}
;;{{{ Font-Lock mode

(setq font-lock-support-mode 'lazy-lock-mode)

;; Turn it on

(if (and window-system (not running-xemacs)) (global-font-lock-mode t))

;; This one probably a waste of time
(font-lock-mode-if-window-system)

;; Set some colours

(if window-system
    (progn
;;      (set-face-foreground 'font-lock-comment-face "PaleGreen")
      (set-face-underline-p 'underline nil)))

;;}}}
;;{{{ Load paren library

(if window-system (load-library "paren"))

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

(auto-compression-mode)

;;}}}

;;{{{ comment-start

(add-hook 'lisp-mode-hook (function (lambda () (setq comment-start ";; "))))
(add-hook 'emacs-lisp-mode-hook (function (lambda () (setq comment-start ";; "))))
(add-hook 'text-mode-hook (function (lambda () (setq comment-start "> "))))
(add-hook 'cperl-mode-hook (function (lambda () (setq comment-start "#"))))
(add-hook 'shell-script-mode-hook (function (lambda () (setq comment-start "#"))))

;;}}}

;;}}}