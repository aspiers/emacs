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
;;{{{ as-cperl-insert-self-and-args-line

;;(eval-when-compile (defun cperl-indent-command (&optional whole-exp)))
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
;;{{{ as-cperl-insert-head

(defun as-cperl-insert-head ()
  "Inserts an empty pod =head directive."
  (interactive)
  (beginning-of-line)
  (insert "=head2 

=cut

")
  (forward-line -4)
  (end-of-line)
  (cperl-find-pods-heres)
)

;;}}}
;;{{{ as-cperl-insert-pod-list

(defun as-cperl-insert-pod-list ()
  "Inserts Makes a new Perl method with an accompanying pod stub."
  (interactive)
  (beginning-of-line)
  (insert "=over 4

=item * 

=back

")
  (forward-line -4)
  (end-of-line)
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
;;{{{ as-cperl-insert-pkg-template

(defun as-cperl-insert-pkg-template (pkg)
  "Inserts a template for a complete Perl package."
  (interactive "sPackage name: ")
  (beginning-of-buffer)
  (insert "package " pkg ";

=head1 NAME

" pkg " -

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

use strict;
use warnings;

=head1 CONSTRUCTORS

=cut

=head1 METHODS

=cut

=head1 BUGS

=head1 SEE ALSO

=cut

1;
")
  (forward-line -3))

;;}}}
;;{{{ as-cperl-reinvert-if-unless

(defun as-cperl-reinvert-if-unless ()
  "Performs the opposite of `cperl-invert-if-unless'.  Position your
point somewhere *before* the if/unless/while/until modifier."
  (interactive)
  (let ((modifier-start-re
         "[\t\n ]*\\(\\<\\(if\\|unless\\|while\\|until\\)\\>\\)[\t\n ]*")
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
          (error "Not in statement with an `if', `unless', `while', or `until'")))
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
  (local-set-key "\C-cma"      'as-cperl-insert-args-line)
  (local-set-key "\C-cmc"      'as-cperl-insert-self-method-call)
  (local-set-key "\C-cmC"      'as-cperl-insert-carp-line)
  (local-set-key "\C-cmD"      'as-cperl-insert-data-dumper-line)
  (local-set-key "\C-cmh"      'as-cperl-insert-head)
  (local-set-key "\C-cmj"      'imenu)
  (local-set-key "\C-cml"      'as-cperl-insert-pod-list)
  (local-set-key "\C-cmm"      'as-cperl-make-method)
  (local-set-key "\C-cmM"      'as-cperl-make-method-and-pod)
  (local-set-key "\C-cmp"      'cperl-find-pods-heres)
  (local-set-key "\C-cmP"      'as-cperl-insert-pkg-template)
  (local-set-key "\C-cmi"      'as-cperl-set-indent-level)
  (local-set-key "\C-cms"      'as-cperl-insert-self-and-args-line)
  (local-set-key "\C-cmS"      'as-cperl-insert-self-line)
  (local-set-key "\C-cmt"      'as-cperl-reinvert-if-unless)
  (local-set-key [(f10)]        'as-cperl-insert-unique-warning)
  (local-set-key [(backspace)] 'cperl-electric-backspace)
  (setq indent-tabs-mode nil)
  (make-variable-buffer-local 'cperl-indent-level)
  (set-default 'cperl-indent-level 2))
