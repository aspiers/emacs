;; Adam's cperl setup

;;{{{ as-cperl-set-indent-level

(defvar cperl-indent-level)
(defvar cperl-close-paren-offset)
(defun as-cperl-set-indent-level (width)
  "Sets the cperl-indent-level variable to the given argument."
  (interactive "NNew cperl-indent-level: ")
  (setq cperl-indent-level width)
  (setq cperl-close-paren-offset (- width)))

;;}}}
;;{{{ as-cperl-insert-self-and-args-line

(eval-when-compile (defun cperl-indent-command (&optional whole-exp)))
(defun as-cperl-insert-self-and-args-line ()
  "Inserts

  my $self = shift;
  my () = @_;

line before the current line, and leaves the point poised for adding
subroutine arguments."
  (interactive)
;;   (beginning-of-line)
;;   (open-line 1)
;;   (cperl-indent-command)
;;   (insert "my ($self) = @_;")
;;   (backward-char 7)
  (as-cperl-insert-self-line)
  (as-cperl-insert-args-line)
  )

;;}}}
;;{{{ as-cperl-insert-self-line

(defun as-cperl-insert-self-line ()
  "Inserts a

  my $self = shift;

line before the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (cperl-indent-command)
  (insert "my $self = shift;")
  (beginning-of-line)
  (forward-line 1)
  )

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
;;{{{ as-cperl-insert-data-dumper-line

(defun as-cperl-insert-data-dumper-line ()
  "Inserts a

  use Data::Dumper;
  warn Dumper();

line before the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (cperl-indent-command)
  (insert "use Data::Dumper;")
  (newline-and-indent)
  (insert "warn Dumper();")
  (backward-char 2))

;;}}}
;;{{{ as-cperl-insert-carp-line

(defun as-cperl-insert-carp-line ()
  "Inserts a

  use Carp qw(carp cluck croak confess);

line before the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (cperl-indent-command)
  (insert "use Carp qw(carp cluck croak confess);")
  (newline-and-indent))

;;}}}
;;{{{ as-cperl-make-method

(defun as-cperl-make-method (method)
  "Makes a new Perl method."
  (interactive "sMethod name: ")
  (beginning-of-line)
  (open-line 1)
  (cperl-indent-command)
  (insert "sub " method " {")
  (newline-and-indent)
  (insert "}\n")
  (forward-line -1)
  (cperl-indent-command)
  (beginning-of-line)
  (as-cperl-insert-self-and-args-line)
)

;;}}}
;;{{{ as-cperl-make-method-and-pod

(defun as-cperl-make-method-and-pod (method)
  "Makes a new Perl method with an accompanying pod stub."
  (interactive "sMethod name: ")
  (beginning-of-line)
  (insert "=head2 " method "()")
  (newline 2)
  (insert "=cut")
  (newline 2)
  (cperl-find-pods-heres)
  (as-cperl-make-method method)
)

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
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (insert "warn ")
    (insert (format "%d" as-cperl-unique-warning-counter))
    (insert ";"))
  (setq as-cperl-unique-warning-counter (+ as-cperl-unique-warning-counter 1)))

;;}}}
;;{{{ as-cperl-insert-self-method-call

(fset 'as-cperl-insert-self-method-call "$self->")

;;}}}

(defun as-cperl-setup ()
  "Set up cperl-mode the way Adam likes it."
  (local-set-key "\C-cma"      'as-cperl-insert-args-line)
  (local-set-key "\C-cmc"      'as-cperl-insert-self-method-call)
  (local-set-key "\C-cmC"      'as-cperl-insert-carp-line)
  (local-set-key "\C-cmD"      'as-cperl-insert-data-dumper-line)
  (local-set-key "\C-cmj"      'imenu)
  (local-set-key "\C-cmm"      'as-cperl-make-method)
  (local-set-key "\C-cmM"      'as-cperl-make-method-and-pod)
  (local-set-key "\C-cmp"      'cperl-find-pods-heres)
  (local-set-key "\C-cmi"      'as-cperl-set-indent-level)
  (local-set-key "\C-cms"      'as-cperl-insert-self-and-args-line)
  (local-set-key "\C-cmS"      'as-cperl-insert-self-line)
  (local-set-key [(f10)]        'as-cperl-insert-unique-warning)
  (local-set-key [(backspace)] 'cperl-electric-backspace)
  (setq indent-tabs-mode nil)
  (make-variable-buffer-local 'cperl-indent-level)
  (set-default 'cperl-indent-level 2))
