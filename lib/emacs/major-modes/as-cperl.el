;; Adam's cperl setup

(eval-when-compile (require 'cperl-mode))

;;{{{ as-cperl-set-indent-level

(defvar cperl-indent-level)
(defvar cperl-close-paren-offset)
(defun as-cperl-set-indent-level (width)
  "Sets the `cperl-indent-level' variable to the given argument."
  (interactive "NNew cperl-indent-level: ")
  (setq cperl-indent-level width)
  (setq cperl-close-paren-offset (- width)))

;;}}}
;;{{{ as-cperl-insert-unique-warning

(defvar as-cperl-unique-warning-counter 0
  "Counter for `as-cperl-insert-unique-warning'.")

(defun as-cperl-insert-unique-warning (&optional start)
  "Inserts a

  warn <n>;

line, where <n> is incremented each invocation.

Can be optionally given a numeric prefix which 
"
  (interactive "p")
  (message (format "%s" start))
  (and current-prefix-arg (setq as-cperl-unique-warning-counter start))
  (beginning-of-line)
  (open-line 1)
  ;; Don't append \n to warn string so that we can clearly see
  ;; which file the warning's coming from.
  (insert (format "warn \"%2d\";" as-cperl-unique-warning-counter))
  (forward-line 2)
  (setq as-cperl-unique-warning-counter (+ as-cperl-unique-warning-counter 1)))

;;}}}
;;{{{ as-cperl-insert-check-syscall

(defun as-cperl-insert-check-syscall ()
  "Inserts a template for checking system calls."
  (interactive)
  (insert " or die \"")
  (save-excursion (insert "() failed: $!\\n\";")))

;; Better way of doing it, using tempo and abbrevs:

(require 'tempo)

(defvar tempo-template-perl-function-call
  '((p "Function call: " fn) "(" (p "Arguments: " args) ")" n>
                         "or die \"" (s fn) "(" (s args) ") failed: $!\\n\";")
  "Template for function calls which need the return value checked.")

(defun tempo-template-perl-file-function-call (function)
  "Returns a tempo template for file-oriented syscalls which need the return value checked.
Used internally for mass generation of tempo templates."
  `(,function "("
    (p "Handle: " handle) ", "
    (p "Arguments: " args) ")" n>
    "or die qq{" ,function "(" (s handle) ", " (s args) ") failed: $!\\n};"))

;; (tempo-define-template "perl-open-call" tempo-template-perl-function-call "open")
(tempo-define-template "perl-open-call" (tempo-template-perl-file-function-call "open") "op2en")
                       
(define-abbrev cperl-mode-abbrev-table "open" "" 'tempo-template-perl-open-call)

;;}}}
;;{{{ as-cperl-reinvert-if-unless

(defun as-cperl-reinvert-if-unless ()
  "Performs the rough opposite of `cperl-invert-if-unless'.  Position your
point somewhere *before* the if/unless/while/until/for/foreach modifier."
  (interactive)
  (let ((modifier-start-re
         "[\t\n ]*\\(\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>\\)[\t\n ]*")
        (modifier-end-re ";[\t\n ]*")
        expr-search-bound ws-start
        modifier-start modifier-end
        expr-start expr-end expr-end-ws
        modifier expr)
    (save-excursion
      (re-search-forward ";")
      (or (cperl-after-expr-p)
          (error "Couldn't find `;' terminating expr"))
      (setq expr-search-bound (match-beginning 0)))
    (save-excursion
      (or (looking-at modifier-start-re)
          (re-search-forward modifier-start-re
                             expr-search-bound
                             'noerror)
          (error "Not in statement with an `if', `unless', `while', `until', `for', or `foreach'")))
    (setq ws-start (match-beginning 0))
    (setq modifier-start (match-beginning 1))
    (setq modifier-end (match-end 1))
    (setq modifier (buffer-substring modifier-start modifier-end))
    (setq expr-start (match-end 0))

    (re-search-forward modifier-end-re)
    (setq expr-end (match-beginning 0))
    (setq expr-end-ws (match-end 0))
    (setq expr (buffer-substring expr-start expr-end))
    ;; Insert closing brace first.
    (newline-and-indent)
    (insert "}")
    (delete-region ws-start expr-end-ws)
    (goto-char ws-start)
    (insert ";")
    (back-to-indentation)
    (insert modifier " (" expr ") {")
    (newline-and-indent)
    (backward-up-list)
    (cperl-indent-exp)
    ))

;;}}}

(defun as-cperl-setup ()
  "Set up cperl-mode the way Adam likes it."
  (local-set-key "\C-cmj"      'imenu)
  (local-set-key "\C-cmo"      'as-cperl-insert-check-syscall)
  (local-set-key "\C-cmp"      'cperl-find-pods-heres)
  (local-set-key "\C-cmi"      'as-cperl-set-indent-level)
  (local-set-key "\C-cmt"      'as-cperl-reinvert-if-unless)
  (local-set-key [(f10)]       'as-cperl-insert-unique-warning)
  (local-set-key [(backspace)] 'cperl-electric-backspace)
  (setq indent-tabs-mode nil)
  (make-variable-buffer-local 'cperl-indent-level)
  (set-default 'cperl-indent-level 2))
