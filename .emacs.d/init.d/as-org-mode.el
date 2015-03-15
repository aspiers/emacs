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
  ;; Let's see if this is already taken care of
  ;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  )

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

(bind-key "C-c o" 'overwrite-mode)
(global-unset-key "\M-o")
(bind-key "M-o a" 'org-agenda)
(bind-key "M-S-a"  'as-org-switch-to-agenda-buffer) ;; X11 only
(bind-key "M-o b" 'as-org-switch-to-agenda-buffer)
(bind-key "M-o q" 'org-remember)
(bind-key "M-o M-o" 'as-org-jump-clock-or-agenda)

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
(bind-key "M-o SPC" 'org-unset-effort)

(bind-key "C-c C-x C-j" 'org-clock-goto)
(bind-key "C-c c"   'org-capture)

(require 'ido)

(fset 'as-find-personal-todo "\C-x\C-f~/org/TODO.org")
(bind-key "C-c j t" 'as-find-personal-todo)
(bind-key "C-\""    'as-find-personal-todo)
(fset 'as-find-personal-diary "\C-x\C-f~/org/diary.org")
(bind-key "C-c j d" 'as-find-personal-diary)
;;(fset 'as-find-personal-note "\C-x\C-f~/org/notes/")


;;}}}
;;{{{ org-capture (C-c q for _q_uick)

;; Try to use C-c c but keeping this for backwards compatability with
;; my brain.
(bind-key "C-c q" 'org-capture)

;;}}}

(autoload 'org-occur-in-agenda-files "org" nil t)
(bind-key "C-c C-?" 'org-occur-in-agenda-files)

;;{{{ C-c j for quick jumping

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-personal-note
  "~/org/notes" "Find note: ")
(bind-key "C-c j n"  'as-find-personal-note)

;;}}}

;;}}}

(provide 'as-org-mode)
