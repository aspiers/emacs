;; Programming languages

;;{{{ C

(add-hook 'c-mode-hook 'as-font-lock-mode-if-window-system)

;;}}}
;;{{{ CPerl/Perl

;; one of these two will work
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)

(add-hook 'perl-mode-hook 'as-font-lock-mode-if-window-system)

(autoload 'as-cperl-setup "as-cperl" "as-cperl-setup")
(add-hook 'cperl-mode-hook 'as-cperl-setup)
(add-hook 'cperl-mode-hook 'turn-on-auto-fill)
(defun cp () "Abbreviation for `cperl-mode'." (interactive) (cperl-mode))
(add-hook 'cperl-mode-hook (lambda () (setq comment-start "#")))

(add-to-list 'auto-mode-alist '("\\.\\(pod\\|t\\)\\'" . cperl-mode))

;;}}}
;;{{{ python-mode

(use-package python-mode
  :mode ("\\.py\\'" . python-mode))

;; Allow modes such as jdl-mode to be given Python alignment rules
(defcustom align-python-modes '(python-mode)
  "A list of modes where Python syntax is to be seen."
  :type '(repeat symbol)
  :group 'align)

(defvar align-rules-list)
(defun as-tweak-align-rules ()
  "Replace occurrences of `python-mode' in `align-rules-list'
with references to `align-python-modes'.

FIXME: needs to tweak align-*-modes too."
  (dolist (rule align-rules-list)
    (let* ((title (car rule))
           (pairs (cdr rule))
           (modespair (assq 'modes pairs))
           (modes (cdr modespair))
           (search 'python-mode)
           (replacement 'align-python-modes)
           (result nil
                   ;(format "no change for %s" title)
                   ))
      ;(if (eq title 'basic-comma-delimiter) (edebug))
      (if modes
          (cond
           ;; modes could have form: align-foo-modes
           ((equal modes `(quote (,search)))
            (setcdr modespair replacement)
            (setq result (format "single change for %s" title)))

           ;; FIXME: handle modes like
           ;;   (append align-perl-modes '(python-mode))
           ;; as seen in basic-comma-delimiter

           ;; handle modes with form: '(mode1 mode2 ...)
           ;; i.e. (quote (mode1 mode2 ...))
           ((and (listp modes)
                 (eq 'quote (car modes))
                 (listp (cadr modes)))
            (let ((to-replace (memq search (cadr modes))))
              (when to-replace
                ;; remove this element from the list
                (setcar to-replace (cadr to-replace))
                (setcdr to-replace (cddr to-replace))
                (setcdr modespair `(append ,replacement ,modes))
                (setq result (format "list change for %s" title)))))))
       ;(message result)
       )))

(eval-after-load "align"
  '(progn
     (as-tweak-align-rules)
     (add-to-list 'align-rules-list
                  '(python-dictionary-braces
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\S-")
                    (group    . (2))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))
     (add-to-list 'align-rules-list
                  '(python-dictionary-pairs
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\([^:]+\\)\\(\\s-*\\):\\(\\s-*\\)")
                    (group    . (4 5))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))))

;;}}}
;;{{{ Ruby

(add-to-list 'auto-mode-alist '("\\(\\.rb\\|\\.erb\\|\\.rjs\\|\\.rake\\|Rakefile\\|Guardfile\\)\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "ruby-mode" t)
(add-to-list 'auto-mode-alist '("\\.gem\\'" . tar-mode))

;;{{{ rcov.el

;; corrections for rcov.el, although it kinda looks lame - no red/green overlaying

(autoload 'rcov "rcov" "rcov" t)

(defvar rcov-command-line "rake test:units:rcov RCOVOPTS='--gcc --no-html'"
  "Rcov command line to find uncovered code.
It is good to use rcov with Rake because it `cd's appropriate directory.
`--gcc' option is strongly recommended because `rcov' uses compilation-mode.")
(defvar rcovsave-command-line "rake rcov RCOVOPTS='--gcc --no-html --save=coverage.info'"
  "Rcov command line to save coverage status. See also `rcov-command-line'.")
(defvar rcovdiff-command-line "rake rcov RCOVOPTS='-D --gcc --no-html'"
  "Rcov command line to find new uncovered code. See also `rcov-command-line'.")

;;}}}

(autoload 'rcov-buffer "rcov-overlay" "rcov-overlay" t)
;; (global-set-key (kbd "C-c C-r")   'rcov-buffer)
(autoload 'autotest "autotest" "autotest" t)

;;{{{ rsense

(defvar rsense-home (concat (getenv "HOME") "/.STOW/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
;;(require 'rsense nil t)

;;}}}

(autoload 'bundle-open "bundler" nil t)
(global-set-key "\C-cjb" 'bundle-open)

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

;; This can accidentally change permissions in git repos, for instance.
;;(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(global-set-key "\C-cmx" 'make-buffer-file-executable)

(add-to-list 'auto-mode-alist
             '("/\\.zsh\\(env\\|rc\\|/functions/\\)\\|\\.stp$" . sh-mode))

;; This doesn't work for some strange reason.
(add-hook 'shell-script-mode-hook 'as-font-lock-mode-if-window-system)

(add-hook 'shell-script-mode-hook (lambda () (setq comment-start "#")))

(defun sm () "Abbreviation for `sm-mode'." (interactive) (sh-mode))

;;}}}
;;{{{ Lisp

(add-hook 'lisp-mode-hook 'as-font-lock-mode-if-window-system)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; ")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; ")))
(use-package paredit
  :commands enable-paredit-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook       'enable-paredit-mode)))

;;{{{ macrostep

(use-package macrostep
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (define-key emacs-lisp-mode-map
                (kbd "C-c e") 'macrostep-expand))))

;;}}}

;; I don't visit the FAQ very often; find-function way more useful.
(global-set-key [(control h) (control f)] 'find-function)
(global-set-key [(control h) (control F)] 'view-emacs-FAQ)
(global-set-key [(control h) (control k)] 'find-function-on-key)

;;}}}
;;{{{ Javascript

;; Steve Yegge to the rescue
(use-package js2-mode
  :commands nil
  :mode ("\\.js\\(\.erb\\)?$" . js2-mode))

;;}}}

