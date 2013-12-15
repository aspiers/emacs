(require 'as-vars)

;;{{{ vc

(use-package vc-osc
  :config
  (add-to-list 'vc-handled-backends 'osc 'append))

;;}}}
;;{{{ ido - superior replacement for iswitchb

(require 'ido)
(ido-mode t)

;;}}}
;;{{{ Folding mode

;;{{{ Set marks for individual modes

(autoload 'folding-add-to-marks-list "folding" "folding mode")

(eval-after-load "folding"
  '(progn
    (folding-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
    (folding-add-to-marks-list 'Fundamental-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'shellscript-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'shell-script-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'Shellscript-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'Shell-script-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'makefile-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'makefile-gmake-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'sh-mode "\# {{{ " "\# }}}")
    (folding-add-to-marks-list 'tex-mode "% {{{ " "% }}}")
    (folding-add-to-marks-list 'ml-mode "\(* {{{ " "\(* }}} ")
    (folding-add-to-marks-list 'sawfish-mode ";; {{{ " ";; }}}")
    (folding-add-to-marks-list 'lilypond-mode "% {{{ " "% }}}")
    (folding-add-to-marks-list 'LilyPond-mode "% {{{ " "% }}}")
    ))

;;}}}
;;{{{ Autoload mode via local variables

(autoload 'folding-mode "folding" "folding mode")
(autoload 'folding-mode-find-file "folding" "folding mode")
(autoload 'folding-mode-add-find-file-hook "folding" "folding mode")
(autoload 'folding-set-marks "folding" "folding mode")

(defun fm () "Loads folding-mode." (interactive) (folding-mode))
(defun as-folding-init ()
  "Sets up folding-mode for use."
  (require 'folding)
  (folding-mode-add-find-file-hook))

;; FIXME - preactivation?
(eval-after-load "find-func" '(as-folding-init))
;; (defadvice find-function-search-for-symbol (before as-folding act)
;;     "blah"
;;     (require 'folding))
;; (defadvice find-function (before as-folding act)
;;     "blah"
;;     (require 'folding))

(or (as-quick-startup) (as-folding-init))

;;}}}
;;{{{ Key bindings

(defvar folding-mode-map) ;; avoid compile warnings
(autoload 'fold-show "folding")

(add-hook 'folding-mode-hook
          (lambda ()
            ;; Quick navigation
            (local-set-key [(control meta <)] 'folding-shift-out)
            (local-set-key [(control meta >)] 'folding-shift-in)
            (local-set-key [(shift left)] 'folding-shift-out)
            (local-set-key [(shift right)] 'folding-shift-in)
            ))

(eval-when-compile
  (defvar folding-default-keys-function))
(setq folding-default-keys-function 'folding-bind-backward-compatible-keys)

;;}}}

;;}}}
;;{{{ Load paren library

(require 'paren)

;;}}}
;;{{{ Time-stamp mode

(autoload 'time-stamp "time-stamp")
;;(time-stamp)
;;(setq time-stamp-format "------ %02d %03b %4y %2H%2M %2H%2M  : %u")

;;}}}
;;{{{ Time

;;(display-time)

;;}}}
;;{{{ Auto-compression mode

(cond ((as-quick-startup)
       (defun lac () "Load auto-compression-mode."
         (interactive)
         (auto-compression-mode 1)))
      (t
       (auto-compression-mode 1)))

;;}}}
;;{{{ recentf

(and window-system
     (load "recentf" 'noerror)
     (recentf-mode t))

;;}}}
;;{{{ no toolbar

;; This is best done with X resources, otherwise you get funny
;; frame-resizing problems.  See ~/.Xresources/emacs.rdb.

;; (and window-system
;;      (>= emacs-major-version 21)
;;      (tool-bar-mode -1))

;; (setq default-frame-alist
;;       '((tool-bar-lines . 0)))

;;}}}
;;{{{ Visible whitespace mode

(autoload 'visible-whitespace-mode "visws" "Visible whitespace mode" t)

;;}}}
;;{{{ tempo

(defvar tempo-initial-pos nil
  "Initial position in template after expansion")

;; FIXME: this should be done using tempo-user-elements, not defadvice.
(defadvice tempo-insert (around tempo-insert-pos act)
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it))

(defadvice tempo-insert-template (around tempo-insert-template-pos act)
  "Set initial position when defined.  ChristophConrad"
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
  "Skip self-insert if template function is called by an abbrev."
  (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))

;;}}}
;;{{{ allout

;; (defun as-allout-init ()
;;   "Initialize allout-mode the way Adam likes it."
;;   (interactive)
;;   (when (not (featurep 'allout))
;;     (load "allout.el") ;; Loading .elc causes problems?
;;     (if (boundp 'outline-init)
;;         ;; Old versions init in a different way
;;         (outline-init t)
;;       (allout-init t))
;;     (substitute-key-definition 'beginning-of-line 'move-beginning-of-line global-map)
;;     (substitute-key-definition 'end-of-line 'move-end-of-line global-map)))

;; (eval-after-load "outline" '(as-allout-init))
;; (eval-after-load "muse" '(as-allout-init))

;; (defun as-allout-maybe-init ()
;;   "Hook for use within `find-file-hooks' to check whether a file needs
;; allout mode initialized."
;;   (interactive)
;;   (when (boundp 'allout-layout)
;;     (as-allout-init)))

;; (add-hook 'find-file-hooks 'as-allout-maybe-init)

(defvar allout-mode-leaders '((emacs-lisp-mode . ";;;_")
                              (muse-mode       . "*")))

;;}}}
;;{{{ outline-minor-mode

(add-hook 'outline-minor-mode-hook 'turn-on-auto-fill)

;;}}}
;;{{{ msf-abbrev

(autoload 'msf-abbrev-mode "msf-abbrev" nil t)

(eval-after-load "msf-abbrev"
  '(setq msf-abbrev-root
         (concat as-emacs-dir "/minor-modes/msf-abbrev/mode-abbrevs")))

(global-set-key [(control <)] 'msf-cmd-previous-real)
(global-set-key [(control >)] 'msf-cmd-next-real)

;; Has indent-region changed arity?  Do we need a compatability wrapper?
;; (eval-after-load "msf-abbrev"
;;   (let ((arity (function-arity 'indent-region)))
;;     (if (eq (car arity) 2)
;;         (defun indent-region ...

;; This goes into an infinite loop :-(
;; (defadvice abbrev-mode (before abbrev-mode-msf-advice act)
;;   "Always use `msf-abbrev-mode' when `abbrev-mode' is enabled."
;;   (msf-abbrev-mode 1))

;; custom-set-variables takes care of this:
;;(eval-after-load "abbrev" '(require 'msf-abbrev))

(defun msf (&optional prefix)
  "Alias for msf-abbrev-mode."
  (interactive "p")
  (msf-abbrev-mode (or prefix 1)))

;;}}}
;;{{{ auto-complete-mode

(require 'auto-complete-config nil t)
(defun ac-config-default ())
(defvar ac-dictionary-directories)
(when (as-check-feature-loaded 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "/home/adam/.emacs.d/ac-dict")
  (ac-config-default))

;;}}}
;;{{{ git-gutter

(require 'git-gutter)
(global-git-gutter-mode t)

;;}}}
;;{{{ fill-column-indicator

(if (featurep 'fill-column-indicator)
    (dolist (hook '(c-mode-hook ruby-mode-hook shell-script-mode-hook
                    emacs-lisp-mode-hook python-mode-hook))
      (add-hook hook 'fci-mode)))

;;}}}
;;{{{ org2blog

(add-hook 'org-mode-hook
          (lambda ()
            (and (buffer-file-name)
                 (string-match "\\.o2b$" (buffer-file-name))
                 (org2blog/wp-mode))))

;;}}}
;;{{{ macrostep

(use-package macrostep
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (define-key emacs-lisp-mode-map
                (kbd "C-c e") 'macrostep-expand))))

;;}}}

(provide 'as-minor-modes)
