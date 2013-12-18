(eval-when-compile
  ;; org-default-extensions defaults to (org-irc) which causes a
  ;; compile to require erc.el which is not in emacs 21.
  (if (or (not (boundp 'org-default-extensions))
          (memq 'org-irc org-default-extensions))
      (defvar org-default-extensions '(org-mouse))))

(as-progress "loading org-install ...")
(require 'org-install nil 'noerror)

(require 'as-require)
(when (as-check-feature-loaded 'org-install)
  (as-progress "org-install loaded")
  (defun om () "Abbreviation for `org-mode'." (interactive) (org-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|o2b\\)$" . org-mode)))

(defvar org-mode-map)
(add-hook 'org-mode-hook
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

;;{{{ guide-key integration

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

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

;;{{{ stuff from as-bindings (FIXME - rearrange)

(global-set-key "\C-co" 'overwrite-mode)
(global-unset-key [(meta o)])
;(global-unset-key "\eo")
(global-set-key "\eoa" 'org-agenda)
(global-set-key "\eA"  'as-org-switch-to-agenda-buffer) ;; X11 only
(global-set-key "\eob" 'as-org-switch-to-agenda-buffer)
(global-set-key "\eoq" 'org-remember)
(global-set-key "\eo\eo" 'as-org-jump-clock-or-agenda)

(defun org-show-effort ()
  "Shows the effort of the entry at the current point."
  (interactive)
  (let ((effort (org-entry-get (point) org-effort-property)))
    (message (if effort (format "Effort is %s" effort)
               "No effort defined"))))

(add-hook
 'org-mode-hook
 (lambda ()
   ;; Zero effort is last (10th) element of global Effort_ALL property
   ;; so that we get zero effort when pressing '0' in the Effort column
   ;; in Column view, since this invokes `org-set-effort' with arg 0,
   ;; which stands for the 10th allowed value.
   (let ((effort-values
          (car
           (read-from-string
            (concat "("
                    (cdr (assoc "Effort_ALL" org-global-properties))
                    ")")))))
     (dotimes (effort-index 10)
       (let* ((effort (nth effort-index effort-values))
              (key-suffix (number-to-string
                           (if (= effort-index 9) 0 (1+ effort-index))))
              (fn-name (concat "org-set-effort-"
                               (number-to-string effort-index)))
              (fn (intern fn-name)))
         ;; (message "Binding M-o %s to %s which sets effort to %s"
         ;;          key-suffix fn-name effort)
         (fset fn `(lambda ()
                     ,(format "Sets effort to %s." effort)
                     (interactive)
                     (org-set-effort ,(1+ effort-index))))
         (local-set-key (concat "\eo" key-suffix) fn)
         (local-set-key "\eo\eo" 'org-show-effort))))))

(defun org-unset-effort ()
  "Unsets the Effort property for the current headline."
  (interactive)
  (org-delete-property org-effort-property))
(global-set-key "\eo " 'org-unset-effort)

(global-set-key [(control c)(control x)(control j)] 'org-clock-goto)
(global-set-key "\C-cc"   'org-capture)

(require 'ido)

(fset 'as-find-personal-todo "\C-x\C-f~/org/TODO.org")
(global-set-key "\C-cjt" 'as-find-personal-todo)
(global-set-key [(control \")] 'as-find-personal-todo)
(fset 'as-find-personal-diary "\C-x\C-f~/org/diary.org")
(global-set-key "\C-cjd" 'as-find-personal-diary)
;;(fset 'as-find-personal-note "\C-x\C-f~/org/notes/")

(autoload 'org-store-link "org" "org-store-link" t)
(global-set-key "\C-cL"   'org-store-link)
;; I reserve C-c m for mode-specific user bindings
;;{{{ C-c M for mairix

(autoload 'as-mairix-yank-links "as-gtd" "as-mairix-yank-links" t)
(autoload 'as-mairix-view-link-at-point "as-gtd" "as-mairix-view-link-at-point" t)
(global-set-key [(control c) (M) (y)]         'as-mairix-yank-links)
(global-set-key [(control c) (M) (control y)] 'as-mairix-yank-links)
(global-set-key [(control c) (M) (return)]    'as-mairix-view-link-at-point)

;;}}}
;;{{{ org-capture (C-c q for _q_uick)

;; Try to use C-c c but keeping this for backwards compatability with
;; my brain.
(global-set-key "\C-cq" 'org-capture)

;;}}}

(autoload 'org-occur-in-agenda-files "org" nil t)
(global-set-key [(control c) (control \?)] 'org-occur-in-agenda-files)

;;{{{ C-c j for quick jumping

(defun as-find-personal-note ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/org/notes/" "Find note: "))
(global-set-key "\C-cjn"  'as-find-personal-note)

(fset 'as-find-work-todo "\C-x\C-f~/SUSE/TODO.org")
(global-set-key "\C-cjT"  'as-find-work-todo)
(global-set-key [(control \%)] 'as-find-work-todo)


;;}}}

;;}}}

(provide 'as-org-mode)
