(require 'as-org-mode)

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

(provide 'as-org-new-subheading)
