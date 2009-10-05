;; mgp-mode-cd.el - An Emacs major-mode for editing MagicPoint files.

;; Filename: mgp-mode-cd.el
;; Author:   Christoph Dalitz
;; Version:  1.8
;; License:  GNU General Public License

;; Abstract:
;;
;;  This mode is called "mgp-mode-cd" in order to distinguish it from the 
;;  official "mgp-mode" that is shipped with MagicPoint. In contrast to the
;;  official "mgp-mode", which I find too complicated for the average user,
;;  this mode focuses on ease of use: all actions are accessible from a
;;  self-explanatory main menu entry. Moreover this mode supports the
;;  option to preview only the current page.

;; Installation:
;;
;;  1) Optionally, byte-compile this file
;;  2) Put this file in a directory Emacs can find
;;  3) Tell Emacs when to load this file
;;  4) Customize some variables for your system
;;
;; ad 1)
;;  Byte-compilation speeds up the load time for this mode
;;  and is therefore recommended. Just load this file into
;;  Emacs and select "Byte-compile This File" from the
;;  "Emacs-Lisp" main menu. This will create the compiled
;;  file with the extension "elc".
;;
;; ad 2)
;;  The directories that Emacs searches are given by the 
;;  load-path variable, which you can query within Emacs with
;;     ESC-x describe-variable RET load-path Ret
;;  To add a directory (eg. ~/.emacs) to load-path, add 
;;  the following code to your $HOME/.emacs file:
;;     (add-to-list 'load-path "~/elisp")
;;
;; ad 3)
;;  Add the following lines to your $HOME/.emacs file:
;;     (autoload 'mgp-mode "mgp-mode-cd" "MGP mode." t)
;;     (add-to-list 'auto-mode-alist '("\\.mgp$" . mgp-mode))
;;  The first line tells Emacs to load mgp-mode-cd.elc or
;;  mgp-mode-cd.el when the command 'mgp-mode' is called.
;;  The second line tells emacs to invoke the command 'mgp-mode'
;;  when a file with a name ending on ".mgp" is opened.
;;
;; ad 4)
;;  Some variables might need adjustment to your local system
;;  environment. You can do it in your $HOME/.emacs file with 
;;  commands like
;;     (setq mgp-command     "mgp -x vflib")
;;     (setq mgp-tmpfile     "/tmp/preview.mgp")
;;     (setq mgp-helpcommand "zcat /usr/share/doc/mgp/SYNTAX.gz | xless")
;;  You can also set these variables interactively from the
;;  entry "Options" in the "MGP" main menu that is created
;;  when mgp-mode is entered.

;; History
;;
;;   01.05.2001  first creation for GNU Emacs
;;   27.02.2002  added installation instructions
;;               added options menu for setting of mgp command line
;;               bugfix preview-file when cursor in header
;;   01.03.2002  bugfix in mark-page for last page
;;   10.05.2002  new option "Syntax Help" in MGP main menu
;;   30.05.2002  "Syntax Help" now displayed asynchronously
;;               ported to Xemacs
;;   12.06.2002  workaround for undefined builtin-face in Xemacs
;;               preview-page now also works when cursor in preamble
;;   12.12.2002  added support for ispell
;;   20.03.2008  ported to GNU Emacs 22:
;;                - set paragraph-separate
;;                - how-many behaves incompatible between Emacs 21 and 22
;;                  => explicitly copied the code to this mode
;;               TAB now always inserts TAB

;; List of functions called when mgp-mode is entered
(defcustom mgp-mode-hook '()
  "*Hook for customising `mgp-mode'."
  :type 'hook
  :group 'Mgp)

;; custom variables
(defcustom mgp-command "mgp -x vflib"
  "mgp command line.
Must be adjusted according to the compilation options,
eg. 'mgp -x vflib' when mgp is compiled with vflib, but vflib 
is not installed."
  :group 'Mgp)
(defcustom mgp-tmpfile "/tmp/page.mgp"
  "Temporary file generated when only parts are previewed."
  :group 'Mgp)
(defcustom mgp-helpcommand "zcat /usr/share/doc/mgp/SYNTAX.gz | xless"
  "Command for display of MGP syntax documentation."
  :group 'Mgp)

;; shortcut key bindings
(defvar mgp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'mgp-preview-file)
    (define-key map (kbd "C-c C-p") 'mgp-preview-page)
    (define-key map (kbd "C-c C-c") 'mgp-comment-region)
    (define-key map (kbd "C-c C-u") 'mgp-uncomment-region)
    (define-key map (kbd "C-c C-m") 'mgp-mark-page)
    map)
  "Mode map used for `mgp-mode'.")

;; main menu entry
(easy-menu-define
 mgp-mode-menu mgp-mode-map
 "Menu for `mgp-mode'."
 '("MGP"
   ["Preview Buffer" mgp-preview-file ]
   ["Preview Page" mgp-preview-page ]
   ;; automatic menu deactivation when no region marked
   ;; only works with GNU Emacs and crashes (!) Xemacs
   ;; ["Comment Region" mgp-comment-region (region-beginning) ]
   ;; ["Uncomment Region" mgp-uncomment-region (region-beginning) ]
   ["Comment Region" mgp-comment-region ]
   ["Uncomment Region" mgp-uncomment-region ]
   ["Mark Page" mgp-mark-page ]
   ["Syntax Help" (start-process-shell-command "bla" nil mgp-helpcommand) ]
   ["-" nil ]
   ["Options" (customize-group "Mgp") ]
))

;; syntax table 
;; currently not used; see comment below font-lock-defaults
(defvar mgp-mode-syntax-table 
  (let ((table (make-syntax-table)))
    ;; comments start with # and end with newline
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry 10  ">" table)
    (modify-syntax-entry 12  ">" table)
    table)
  "Syntax table used in 'mgp-mode'.")

(defvar mgp-mode-font-lock-keywords nil
  "Mgp keywords used by font-lock.")
(if mgp-mode-font-lock-keywords ()
  (let ()
    (setq mgp-mode-font-lock-keywords
	  (list 
       ;; comments
	   (cons "^#.*" 'font-lock-comment-face)
	   (cons "^%%.*" 'font-lock-comment-face)
       ;; new page
	   (cons "^%page" 'font-lock-string-face)
       ;; filters
	   (cons "^%filter.*" 'font-lock-builtin-face)
	   (cons "^%endfilter.*" 'font-lock-builtin-face)
       ;; other format parameters
	   (cons "^%.*" 'font-lock-function-name-face)
	  ))
))

;; function definitions
(defun private-how-many (regexp &optional rstart rend interactive)
  "The signature of how-many has changed before Emacs 22.
Hence we must redefine this function here so that it can be used across
different emacs versions

Return number of matches for REGEXP following point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."
  (interactive
   (keep-lines-read-args "How many matches for (regexp): "))
  (save-excursion
    (if rstart
	(progn
	  (goto-char (min rstart rend))
	  (setq rend (max rstart rend)))
      (if (and interactive transient-mark-mode mark-active)
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
	  opoint
	  (case-fold-search (and case-fold-search
				 (isearch-no-upper-case-p regexp t))))
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward regexp rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (when interactive (message "%d occurrence%s"
				 count
				 (if (= count 1) "" "s")))
      count))
)
(defun mgp-comment-region (start end)
  "Comments out the current region with '# '."
  (interactive "r")
  (goto-char end) (beginning-of-line) (setq end (point))
  (goto-char start) (beginning-of-line) (setq start (point))
  (let ()
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (while (not (eobp))
		(insert "# ")
		(forward-line 1)))))
)
(defun mgp-uncomment-region (start end)
  "Remove '# ' comments from current region."
  (interactive "r")
  (goto-char end) (beginning-of-line) (setq end (point))
  (goto-char start) (beginning-of-line) (setq start (point))
  (let ((prefix-len '2))
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (while (not (eobp))
		(if (string= "# "
					 (buffer-substring
					  (point) (+ (point) prefix-len)))
			(delete-char prefix-len))
		(forward-line 1)))))
)
(defun mgp-preview-file ()
  "Previews current file with mgp.
Starts at the page where the cursor currently is."
  (interactive)
  (save-buffer)
  (save-excursion
    ;;(let ((pnum (string-to-number (how-many "^%page"))))
	(let ((pnum (private-how-many "^%page")))
      ;; calculate current page number
      (goto-char (point-min))
      ;;(setq pnum (- (string-to-number (how-many "^%page")) pnum))
	  (setq pnum (- (private-how-many "^%page") pnum))
	  (when (< pnum 1) (setq pnum 1))
      ;;(y-or-n-p (format "Pages %d" pnum))
      (shell-command
       (format "%s -p %d %s" mgp-command pnum
               (shell-quote-argument buffer-file-name)))
      ))
)
(defun mgp-mark-page ()
  "Marks the current page.
In mgp-mode, one page is one paragraph. Thus you can mark
a page with `mark-paragraph' as well."
  (interactive)
  (mark-paragraph)
)
(defun mgp-preview-page ()
  "Previews current page with mgp."
  (interactive)
  (save-buffer)
  ;; write preamble...
  (save-excursion
	(goto-char (point-min))
	(mgp-mark-page)
	(write-region (region-beginning) (region-end) mgp-tmpfile)
	)
  ;; ...and current page
  (save-excursion
	;; move to first page when cursor before first page
    ;;(let ((pnum (string-to-number (how-many "^%page"))))
	(let ((pnum (private-how-many "^%page")))
	  (save-excursion
		(goto-char (point-min))
		;;(setq pnum (- (string-to-number (how-many "^%page")) pnum)))
		(setq pnum (- (private-how-many "^%page") pnum)))
	  (when (< pnum 1) (re-search-forward "^%page" nil t)))
	(mgp-mark-page)
	(append-to-file (region-beginning) (region-end) mgp-tmpfile)
	(shell-command (format "%s %s" mgp-command mgp-tmpfile))
	)
)

;;;###autoload
(defun mgp-mode ()
  "Major mode for editing mgp2 source.
Comments etc. are highlighted with font-lock. There are also a 
number of commands that make editing and working with MGP files 
more convenient. These commands are available from the main menu 
`MGP' or via the following shortcuts:

\\[mgp-preview-file]	- Run mgp on the current file.
\\[mgp-comment-region]	- Comments out the current region.
\\[mgp-uncomment-region]	- Uncomments the current region.
\\[mgp-mark-page]	- Marks the current page (== paragraph).
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mgp-mode)
  (setq mode-name "mgp")

  (use-local-map mgp-mode-map)

  ;; workarounds for xemacs:
  ;; - menu must be explicitly added
  ;; - xemacs uses preprocessor-face instead of builtin-face
  (easy-menu-add mgp-mode-menu mgp-mode-map)
  (if (string-match "XEmacs\\|Lucid" emacs-version)
	  (progn (make-face 'font-lock-builtin-face)
			 (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face)))

  ;; syntax table is not used (see below)
  (set-syntax-table mgp-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mgp-mode-font-lock-keywords
				 t t ((?_ . "w") (?. . "w"))))
  ;;            ^^^
  ;; (otherwise syntax table wins and keywords are ignored!)

  ;; paragraph == page
  ;; we set paragraph-separate to "^^^L" in the hope that
  ;; this never occurs in an mgp file...
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "[%#]?.*%page") 
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^^^L") 

  ;; additional options

  ;;(make-local-variable 'require-final-newline)
  ;;(setq require-final-newline t)

  ;;(setq blink-matching-paren t)

  ;; TAB must insert TAB rather than Spaces
  (setq indent-tabs-mode t)
  (setq indent-line-function 'insert-tab)
  ;; let ispell skip %-directives
  (make-local-variable 'ispell-skip-region-alists)
  (add-to-list 'ispell-skip-region-alist (list "^%.*$"))

  ;; case insensitive pattern matching
  (setq font-lock-keywords-case-fold-search t)
  (put 'mgp-mode 'font-lock-keywords-case-fold-search t)

  (run-hooks 'mgp-mode-hook)
)
