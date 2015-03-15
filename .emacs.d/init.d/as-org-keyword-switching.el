(require 'as-org-mode)

(defun org-todo-previous-keyword ()
  "Move to previous TODO keyword in all sets."
  (interactive)
  (org-todo 'left))

(defun org-todo-next-keyword ()
  "Move to next TODO keyword in all sets."
  (interactive)
  (org-todo 'right))

(defun org-todo-previous-set ()
  "Move to previous TODO keyword set."
  (interactive)
  (org-todo 'previousset))

(defun org-todo-next-set ()
  "Move to next TODO keyword set."
  (interactive)
  (org-todo 'nextset))

(add-hook
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map [(control shift f)] 'org-todo-next-keyword)
   (define-key org-mode-map [(control shift b)] 'org-todo-previous-keyword)
   (define-key org-mode-map [(control shift p)] 'org-todo-previous-set)
   (define-key org-mode-map [(control shift n)] 'org-todo-next-set)
   ))

(provide 'as-org-keyword-switching)
