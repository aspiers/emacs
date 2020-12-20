(use-package yasnippet
  :commands yas-minor-mode
  :config
  (defalias 'yamm 'yas-minor-mode)
  (defalias 'yagm 'yas-global-mode)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

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
;;{{{ _I_nsert auto-text (C-c i)

(bind-key "C-c i d" 'as-insert-date-and-time)
(bind-key "C-c i D" 'as-insert-date-interactive)
(bind-key "C-c i e" 'as-insert-email-address)
(bind-key "C-c i m" 'as-insert-local-mode)
(bind-key "C-c i w" 'as-insert-work-email-address)
(bind-key "C-c i W" 'as-insert-name-and-work-email)
(bind-key "C-c i j" 'as-insert-japh-method-chain-sig)
(bind-key "C-c i J" 'as-insert-japh-indirect-sig)
(bind-key "C-c i l" 'as-insert-log-timestamp)
(bind-key "C-c i L" 'as-insert-log-datestamp)
(bind-key "C-c i N" 'as-insert-name-and-email)
(bind-key "C-c i n" 'as-insert-name)
(bind-key "C-c i r" 'as-insert-rpm-changelog-datestamp)
(bind-key "C-c i s" 'as-insert-scissors)
(bind-key "C-c i S" 'as-snip-region)
(bind-key "C-c i t" 'as-insert-time)

;;}}}

(provide 'as-templating)
