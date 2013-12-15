(use-package yasnippet
  :commands yas-minor-mode
  :init
  (defalias 'yasm 'yas-minor-mode)
  :config
  (progn
    (global-set-key [(control <)] 'yasnippet-cmd-previous-real)
    (global-set-key [(control >)] 'yasnippet-cmd-next-real)))

(provide 'as-yasnippet)

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

(provide 'as-templating)
