;; org-mode needs to be loaded before el-get packages which depend on it,
;; otherwise we get 

(as-progress "loading org-install ...")
(require 'org-install nil 'noerror)

(require 'as-require)
(when (as-check-feature-loaded 'org-install)
  (as-progress "org-install loaded")
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|o2b\\)$" . org-mode)))

(defvar org-mode-map)
(add-hook
 'org-mode-hook
 (lambda ()
   (as-soft-require 'as-gtd)
   (imenu-add-to-menubar "Imenu")
   (setq comment-start nil)))

(declare-function org-crypt-use-before-save-magic "org-crypt")
(add-hook 'org-mode-hook
          (lambda ()
            (and (as-soft-require 'org-crypt)
                 (org-crypt-use-before-save-magic))
            (add-to-list 'org-modules 'org-timer)))

;;{{{ org keyword switching

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

;;}}}
;;{{{ org-new-subheading*

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

;;}}}

(autoload 'bzg/org-annotation-helper "org-annotation-helper" nil t)

;;{{{ pomodoro

;; http://orgmode.org/worg/org-gtd-etc.php

;; FIXME: something changed here, but I use Pomodroido now anyway.
;;(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :
(add-hook 'org-clock-in-hook '(lambda () 
      (if (not org-timer-current-timer) 
      (org-timer-set-timer '(16)))))

;;}}}
