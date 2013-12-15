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
