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

;;{{{ Obsolete functions, may come in handy another time

;; (defun as-containing-dir (filename)
;;   "Return the containing directory of a filename when given the full path."
;;   (string-match "\\([^/]+\\)/[^/]+$" filename)
;;   (match-string 1 filename))

;; (defun as-last-dir-and-filename (filename)
;;   "Strip a full path of all of its directory components but the last."
;;   (string-match "\\(.*/\\).+/.+$" (buffer-file-name))
;;   (replace-match "" t t (buffer-file-name) 1))

;; (defun as-buffer-rename-add-one-dir ()
;;   "Add the name of the containing directory of the buffer's file
;; to the beginning of the buffer name."
;;   (interactive)
;;   (rename-buffer (as-last-dir-and-filename (buffer-name))) t)

;;}}}

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

(defun as-buffer-rename-remove-unique-id ()
  "Attempt to remove the unique suffix (e.g. \"<1>\") from the current
buffer's name.  It will fail silently if a buffer already exists with
that name."
  (interactive)
  (and
   (string-match "<[0-9]+>$" (buffer-name))
   (condition-case nil
       (rename-buffer (replace-match "" t t (buffer-name) nil))
     (error nil))))

;; Always try to do this; occasionally things screw up and leave
;; you with a foo<2> buffer when there's no need.
(add-hook 'find-file-hooks 'as-buffer-rename-remove-unique-id)

;;}}}

(autoload 'as-display-buffer-filename
                                 "as-bufs-files" "Display buffer filename"  t)
(autoload 'as-bounce-buffer      "as-bufs-files" "Bounce buffers"           t)
(autoload 'as-destroy-buffer-delete-file 
                                 "as-bufs-files" "Destroy buffer & file"    t)
(autoload 'as-destroy-buffer     "as-bufs-files" "Destroy buffer"           t)
(autoload 'bury-and-close-buffer "as-bufs-files" "Bury and close buffers"   t)
(autoload 'mhj-set-q-to-close    "as-bufs-files" "Bind q to bury and close" t)

;;}}}
;;{{{ Editing

(autoload 'as-duplicate-line      "as-editing" "Duplicate the current line" t)
(autoload 'as-join-line-with-next "as-editing" "Join line with next"        t)
(autoload 'as-copy-previous-line-suffix
                                  "as-editing" "Copy previous line suffix"  t)
(autoload 'as-align-to-previous-line
                                  "as-editing" "Align to previous line"     t)
;; as-transpose-lines autoloaded elsewhere
;; mark-list not bound
(autoload 'vim-yy                 "as-editing" "Simulate vim's yy command"  t)
;; bn-end-of-line-but-one autoloaded elsewhere

;;}}}
;;{{{ Appearance

;;{{{ as-font-lock-mode-if-window-system

(defun as-font-lock-mode-if-window-system
  ()
  "Turns on font-lock mode if X windows is active."
  (interactive)
  (if window-system (font-lock-mode)))

;;}}}

;;}}}

;;{{{ Enable disabled functions

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'set-goal-column 'disabled nil)

;;}}}

;;}}}
;;{{{ Key bindings

;; Ben uses (define-key global-map ...)
;; instead of (global-set-key ...)
;; Think maybe the latter is better?

;;{{{ Global keymap (possibly naughty)

;;{{{ Rebinding for improvement - naughty but nice

(global-set-key "\M-\\"        'fixup-whitespace)   ;; was delete-horizontal-space
(global-set-key "\M-g"         'goto-line)          ;; was set-face
(global-set-key "\C-ha"        'apropos)            ;; was apropos-command
(autoload 'as-transpose-lines "as-editing" "as-transpose-lines" t)
(global-set-key "\C-x\C-t"     'as-transpose-lines) ;; was transpose-lines

(autoload 'bn-kill-region-or-backword-word "as-editing" "bn-kill-region-or-backword-word" t)
(global-set-key [(control w)]  'bn-kill-region-or-backword-word) ;; was kill-region
(autoload 'bn-kill-line-or-region-save "as-editing" "bn-kill-line-or-region-save" t)
(global-set-key [(meta w)]     'bn-kill-line-or-region-save)     ;; kill-ring-save
(autoload 'bn-zap-nearly-to-char "as-editing" "bn-zap-nearly-to-char" t)
(global-set-key [(meta z)]     'bn-zap-nearly-to-char)           ;; was zap-to-char

(global-set-key [(delete)]     'delete-char)        ;; to make sure
(global-set-key [(insert)]     'overwrite-mode)     ;; to make sure

;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.
(global-set-key "\C-x\C-b"     'buffer-menu)

;; But if bs-show is available, choose that cos it's much nicer.
(and (functionp 'bs-show)
     (global-set-key "\C-x\C-b" (function (lambda (arg)
                                            (interactive "P")
                                            (bs-show arg)
                                            (let ((inhibit-read-only t))
                                              (save-excursion
                                                (end-of-buffer)
                                                (insert "\n")))))))

;;}}}
;;{{{ Additions (hope for no conflicts)

(global-set-key "\C-xK" 'as-destroy-buffer) ;; more powerful than C-x k
(autoload 'bn-end-of-line-but-one "as-editing" "bn-end-of-line-but-one" t)
(global-set-key [(control E)] 'bn-end-of-line-but-one)
(global-set-key [(control ?')] 'speedbar-get-focus)
(global-set-key [(control ,)] 'delete-other-windows)
(global-set-key [(control .)] 'delete-window)
(global-set-key [(control \;)] 'bury-buffer)
(global-set-key [(control meta y)] 'as-join-line-with-next)
(global-set-key [(control x) (control y)] 'vim-yy)
(global-set-key [(control x) (I)] 'insert-buffer)
(global-set-key [(meta o)] 'overwrite-mode)
(global-set-key [(control meta return)] 'repeat-complex-command)
(autoload 'bn-strip-parentheses "as-editing" "bn-strip-parentheses" t)
(global-set-key [(control meta \()] 'bn-strip-parentheses)

;;}}}
;;{{{ TAB and family

(global-set-key "\C-c\C-i"      'indent-region)
(global-set-key [(control tab)]         'other-window)
(global-set-key [(control meta $)]      'ispell-buffer)
(global-set-key [(control meta tab)]    'ispell-complete-word)
(global-set-key [(meta i)]              'indent-relative)

;;{{{ hippie-expand

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

;;}}}

;;}}}
;;{{{ Keypad

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
;;{{{ Define fast scroll keys

(defun as-fast-up () "Move up two lines." (interactive) (forward-line -2))
(defun as-fast-down () "Move down two lines." (interactive) (forward-line 2))

;;; Can only use M-down and M-up in X

(if window-system
    (progn
      (global-set-key [(meta down)] 'as-fast-down)
      (global-set-key [(meta up)]   'as-fast-up)
      (global-set-key [(meta kp-2)] 'as-fast-down)
      (global-set-key [(meta kp-8)] 'as-fast-up)))

;;}}}
;;{{{ Mouse

(and window-system (not running-xemacs)
     (global-set-key [(M-mouse-4)] 'raise-frame)
     (global-set-key [(M-mouse-5)] 'lower-frame))

;;}}}
;;{{{ FSF-compliant user bindings

;;{{{ C-c - 

(global-set-key "\C-ca"   'bury-buffer)
(global-set-key "\C-cA"   'as-align-to-previous-line)
(fset 'as-next-cvs-buffer "\C-xb*cvs*") ;; must be something nicer than this
(global-set-key "\C-cb"   'as-next-cvs-buffer)
(global-set-key "\C-cc"   'comment-region)
(global-set-key "\C-cd"   'as-duplicate-line)
(global-set-key "\C-ce"   'cvs-examine)
(global-set-key "\C-cf"   'auto-fill-mode)
(global-set-key "\C-cF"   'font-lock-fontify-buffer)

;;{{{ Auto-text (C-c i)

(autoload 'as-insert-snail-mail "as-autotext" "Insert snail mail" t)
(global-set-key "\C-cia" 'as-insert-snail-mail)

(autoload 'as-insert-date-and-time "as-autotext" "Insert date and time" t)
(global-set-key "\C-cid" 'as-insert-date-and-time)

(autoload 'as-insert-date "as-autotext" "Insert date" t)
(global-set-key "\C-ciD" 'as-insert-date)

(autoload 'as-insert-email-address "as-autotext" "Insert email address" t)
(global-set-key "\C-cie" 'as-insert-email-address)

(autoload 'as-insert-local-mode "as-autotext" "Insert emacs local mode setting" t)
(global-set-key "\C-cim" 'as-insert-local-mode)

(autoload 'as-insert-email-address "as-autotext" "Insert work email address" t)
(global-set-key "\C-ciw" 'as-insert-work-email-address)

(autoload 'as-insert-name-and-work-email "as-autotext" "Insert name and work email address" t)
(global-set-key "\C-ciW" 'as-insert-name-and-work-email)

(autoload 'as-insert-homepage-url "as-autotext" "Insert homepage url" t)
(global-set-key "\C-cih" 'as-insert-homepage-url)

(autoload 'as-insert-japh-method-chain-sig "as-autotext" "Insert JAPH method chain sig" t)
(global-set-key "\C-cij" 'as-insert-japh-method-chain-sig)

(autoload 'as-insert-japh-indirect-sig "as-autotext" "Insert JAPH indirect method sig" t)
(global-set-key "\C-ciJ" 'as-insert-japh-indirect-sig)

(autoload 'as-insert-log-timestamp "as-autotext" "Insert log timestamp" t)
(global-set-key "\C-cil" 'as-insert-log-timestamp)

(autoload 'as-insert-log-datestamp "as-autotext" "Insert log datestamp" t)
(global-set-key "\C-ciL" 'as-insert-log-datestamp)

(autoload 'as-insert-name-and-email "as-autotext" "Insert name and email" t)
(global-set-key "\C-ciN" 'as-insert-name-and-email)

(autoload 'as-insert-name "as-autotext" "Insert name" t)
(global-set-key "\C-cin" 'as-insert-name)

(autoload 'as-insert-old-homepage-url "as-autotext" "Insert old homepage url" t)
(global-set-key "\C-cio" 'as-insert-old-homepage-url)

(autoload 'as-insert-scissors "as-autotext" "Insert scissors" t)
(global-set-key "\C-cis" 'as-insert-scissors)

(autoload 'as-snip-region "as-autotext" "Snip region" t)
(global-set-key "\C-ciS" 'as-snip-region)

(autoload 'as-insert-time "as-autotext" "Insert time" t)
(global-set-key "\C-cit" 'as-insert-time)

(autoload 'as-insert-tigerpig-url "as-autotext" "Insert tigerpig url" t)
(global-set-key "\C-ciT" 'as-insert-tigerpig-url)

;;}}}

(global-set-key "\C-ck"   'delete-file)
(global-set-key "\C-cK"   'as-destroy-buffer-delete-file)

(global-set-key "\C-cl"   'align)                      ;; new in emacs 21
;; I reserve C-c m for mode-specific user bindings
(global-set-key "\C-cn"   'as-display-buffer-filename)
(global-set-key "\C-cp"   'as-copy-previous-line-suffix)
(global-set-key "\C-cP"   'as-align-to-previous-line)
(global-set-key "\C-cq"   'cvs-quickdir)
(global-set-key "\C-cr"   'revert-buffer)
(global-set-key "\C-cR"   'rename-file)
(global-set-key "\C-cs"   'cvs-status)

;;{{{ Toggles and settings (C-c t)

;;{{{ as-toggle-indent-tabs-mode

(defun as-toggle-indent-tabs-mode
  () 
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

;;}}}
(global-set-key "\C-ctb"   'as-toggle-indent-tabs-mode)

(global-set-key "\C-ctf"   'auto-fill-mode)

;;{{{ as-toggle-truncate-lines

(defun as-toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display)
  (message "truncate-lines set to %s" truncate-lines))

;;}}}
(global-set-key "\C-cts"   'as-toggle-truncate-lines) ;; mnemonic: less -S

;;{{{ as-set-tab-width

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

;;}}}
(global-set-key "\C-ctw"   'as-set-tab-width)

;;}}}

(global-set-key "\C-cu"   'cvs-update)

(autoload 'set-any-variable "set-any-var" "set-any-variable" t)
(global-set-key "\C-cv"   'set-any-variable)
(global-set-key "\C-c+"   'make-directory)

;;}}}
;;{{{ Function keys f5--f9 (no modifiers)

(global-set-key [(f5)] 'as-duplicate-line)
(global-set-key [(f6)] 'as-bounce-buffer)
(global-set-key [(f7)] 'as-align-to-previous-line)
(global-set-key [(f8)] 'as-copy-previous-line-suffix)

;;}}}

;;}}}

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
(cond (window-system (load "mwheel" t)))

;;}}}
;;{{{ Little odds and ends

;;{{{ find-function-source-path

(defvar as-emacs-dir)

(defvar nxml-sub-dir "major-modes/nxml-mode-20041004"
  "Path of nxml-mode subdirectory relative to Adam's emacs library root")

(defvar xtla-sub-dir "major-modes/xtla"
  "Path of xtla mode subdirectory relative to Adam's emacs library root")

(custom-set-variables
 '(find-function-source-path
   (append load-path
      (mapcar (lambda (p) (concat as-emacs-dir "/" p))
              (list
               "fun" 
               "major-modes/monkey-2"
               "major-modes/mmm" 
               "major-modes/tdtd" 
               "major-modes/pcl-cvs" 
               nxml-sub-dir
               xtla-sub-dir
               "major-modes" 
               "minor-modes" 
               "utils" 
               )))))

;;}}}
;;{{{ e-mail address

(setq user-mail-address "adam@spiers.net")

;;}}}
;;{{{ Minibuffer (GNU Emacs 21 obsoletes resize-minibuffer-mode)

(cond ((or running-xemacs (<= emacs-major-version 20))
       (resize-minibuffer-mode)
       (setq resize-minibuffer-window-max-height 5 
             resize-minibuffer-frame-max-height 5)))

;;}}}
;;{{{ Apropos extension

(defvar apropos-do-all)
(setq apropos-do-all t)

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

;;}}}
;;{{{ Mode-related settings

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
          (function (lambda ()
                      (local-unset-key "\e\t")
                      (and
		       (boundp 'filename)
		       (not (eq filename t))
                       (string-match
			"mutt-\\|\\.article\\|\\.letter"
			filename)
                       (turn-on-auto-fill)))))

;; Expand all tabs to spaces
(add-hook 'text-mode-hook (function (lambda () (setq indent-tabs-mode nil))))
(defun itm () "Shortcut to indented-text-mode."
  (interactive)
  (indented-text-mode))

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
;;(add-hook 'sawfish-mode-hook 'as-font-lock-mode-if-window-system)
(add-to-list 'auto-mode-alist
             '(".saw\\(mill\\|fish\\)rc\\'\\|\\.jl\\'" . sawfish-mode))

;;}}}
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
;;{{{ nxml

(add-to-list 'load-path (concat as-emacs-dir "/" nxml-sub-dir))
(load "rng-auto")
(autoload 'nxml-mode "rng-auto" "Major mode to edit XML files." t)
(defun nx () "Loads nxml-mode." (interactive) (nxml-mode))
(autoload 'mhj-format-xml "mhj-xml" "Mark's nxml hacks." t)
(add-hook 'nxml-mode-hook
          (lambda () (local-set-key [(control meta q)] 'mhj-format-xml)))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xhtml\\)$" . nxml-mode))

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
;;{{{ CSS

(autoload 'css-mode "css-mode" "mode for editing CSS files" t)
(defvar cssm-indent-function 'cssm-c-style-indenter
  "Which function to use when deciding which column to indent to. To get
C-style indentation, use cssm-c-style-indenter.")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

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
;;{{{ HTML

(defalias 'html-mode 'html-helper-mode)
(autoload 'html-helper-mode "html-helper-mode" t)
(add-hook 'html-mode-hook 
          (function (lambda ()
                      (auto-fill-mode -1)
;;                    (setq truncate-lines t)
                      )))
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

;; Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'as-font-lock-mode-if-window-system)

;;}}}
;;{{{ C

(add-hook 'c-mode-hook 'as-font-lock-mode-if-window-system)

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
;;{{{ pjb-manager

(autoload 'pjb-manager "pjb-manager" "PJB manager mode" t)

;;}}}
;;{{{ cvs helper modes

;; diff mode
(add-hook 'diff-mode-hook 'mhj-set-q-to-close)

;; cvs-status mode
(add-hook 'cvs-status-mode-hook 'mhj-set-q-to-close)

;; log-view mode
(add-hook 'log-view-mode-hook 'mhj-set-q-to-close)

;;}}}
;;{{{ gnus

(defvar gnus-face-1 'gnus-cite-face-1)
(defvar gnus-face-2 'italic)
(defvar gnus-face-3 'gnus-cite-face-4)
(defvar gnus-sum-thread-tree-root "")

;;}}}
;;{{{ rpm-spec-mode

(add-to-list 'auto-mode-alist '("\\.spec$" . rpm-spec-mode))
(autoload 'rpm-spec-mode "rpm-spec-mode" "RPM spec mode." t)

;;}}}
;;{{{ gtypist-mode

(add-to-list 'auto-mode-alist '("\\.typ\\'" . gtypist-mode))
(autoload 'gtypist-mode "gtypist-mode" "gtypist-mode" t)

;;}}}
;;{{{ python-mode

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(autoload 'python-mode "python-mode" "python-mode" t)

;;}}}
;;{{{ ecmascript-mode

(add-to-list 'auto-mode-alist '("\\.js$" . ecmascript-mode))
(autoload 'ecmascript-mode "ecmascript-mode" "ecmascript-mode" t)

;;}}}
;;{{{ xtla

;; Build xtla with:
;;
;; ./configure --with-lispdir=~/lib/emacs/major-modes/xtla --infodir=~/local/info --with-other-dirs=~/lib/emacs/utils/tree-widget-2.0
;; make
;; make install
;; emk

;; not necessary
;;(add-to-list 'load-path (concat as-emacs-dir "/" xtla-sub-dir))

(require 'xtla-autoloads)

;;}}}
;;{{{ etask-mode

(autoload 'etask "etask" "etask project management mode" t)

;;}}}
;;{{{ w3m-mode

(require 'w3m-load)

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
;;{{{ outline and org-mode

(eval-after-load "outline" '(require 'foldout))
(mapc (function
       (lambda (x)
         (add-hook x
                   (lambda ()
                     ;; Quick navigation
                     (auto-fill-mode 1)
                     (local-set-key [(shift left)] 'foldout-exit-fold)
                     (local-set-key [(shift right)] 'foldout-zoom-subtree)
                     (local-set-key [(control shift left)] 'foldout-exit-fold)
                     (local-set-key [(control shift right)] 'foldout-zoom-subtree)
                     ))))
      '(outline-mode-hook outline-minor-mode-hook org-mode-hook))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)

(autoload 'org-mode "org" "Org mode" t)
(autoload 'org-diary "org" "Diary entries from Org mode")
(autoload 'org-agenda "org" "Multi-file agenda from Org mode" t)
(autoload 'org-store-link "org" "Store a link to the current location" t)
(autoload 'orgtbl-mode "org" "Org tables as a minor mode" t)
(autoload 'turn-on-orgtbl "org" "Org tables as a minor mode")

;;}}}

;;}}}
;;{{{ Minor modes

;;{{{ vc

(require 'vc)

;;}}}
;;{{{ iswitchb - better buffer switching

(eval-when-compile (require 'iswitchb))
(iswitchb-default-keybindings)
(add-hook 'iswitchb-define-mode-map-hook 'as-iswitchb-keys)

(defun iswitchb-bury-buffer ()
  "Bury the buffer at the head of `iswitchb-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t) buf)
    (setq buf (car iswitchb-matches))
    (if buf
	(progn
	  (bury-buffer buf)
          (iswitchb-next-match)
          (setq iswitchb-rescan t)))))

(defun as-iswitchb-keys ()
 "Adam's keybindings for iswitchb."
 (define-key iswitchb-mode-map "\C-z" 'iswitchb-bury-buffer)
 )

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
       (defun fm () "Loads folding-mode." (interactive) (folding-mode)))
      (t
       (folding-mode-add-find-file-hook)))

;;}}}
;;{{{ Key bindings

(defvar folding-mode-map) ;; avoid compile warnings
(autoload 'fold-show "folding")

(add-hook 'folding-mode-hook
          (lambda ()
            ;; keypad
            (define-key folding-mode-map [kp-7] 'folding-shift-in)
            (define-key folding-mode-map [kp-9] 'folding-shift-out)
            (define-key folding-mode-map [kp-1] 'fold-show)
            (define-key folding-mode-map [kp-3] 'fold-hide)
            (define-key folding-mode-map [kp-4] 'fold-backward-char)
            (define-key folding-mode-map [kp-6] 'fold-forward-char)

            ;; These ones for VT100s (I think)
            (define-key folding-mode-map "\M-Ow" 'folding-shift-in)
            (define-key folding-mode-map "\M-Oy" 'folding-shift-out)
            (define-key folding-mode-map "\M-Oq" 'fold-show)
            (define-key folding-mode-map "\M-Os" 'fold-hide)
            (define-key folding-mode-map "\M-OP" 'folding-shift-in)
            (define-key folding-mode-map "\M-OQ" 'folding-shift-out)

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
(if (not running-xemacs) (transient-mark-mode 1))

;;}}}
;;{{{ Font-Lock mode

;;(global-font-lock-mode t)

;; Do this via customisation since it's different for xemacs
;;(and window-system (not running-xemacs) (global-font-lock-mode t))

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
     (not running-xemacs)
     (= emacs-major-version 20)
     (load "blinking-cursor" t)
     (blinking-cursor-mode 1))

;;}}}
;;{{{ recentf

(and window-system (load "recentf" 'noerror))

;;}}}
;;{{{ no toolbar

;; This is best done with X resources, otherwise you get funny
;; frame-resizing problems.  See ~/.Xresources/emacs.rdb.

;; (and window-system
;;      (not running-xemacs)
;;      (>= emacs-major-version 21)
;;      (tool-bar-mode -1))

;; (setq default-frame-alist
;;       '((tool-bar-lines . 0)))

;;}}}
;;{{{ Visible whitespace mode

(autoload 'visible-whitespace-mode "visws" "Visible whitespace mode" t)

;;}}}

;;}}}

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

;;}}}

;;{{{ local variables

;;; Local Variables:
;;; auto-recompile: nil
;;; End:

;;}}}
