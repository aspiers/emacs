(defvar as-which-key-no-delay-prefixes ()
  "List of regexps matching keymap prefix bindings for which
`which-key` should not delay showing the keymap's bindings.")

(use-package which-key
  :defer 0.1
  :diminish WK  ;; can't get this working
  :config

  ;; These have to go before the mode is activated
  (setq which-key-idle-delay 0.1)
  (setq which-key-idle-secondary-delay 0.1)
  ;; FIXME: This one doesn't work >-(
  (setq which-key-enable-extended-define-key t)
  (setq which-key-lighter "")

  (which-key-mode)

  (require 's)
  (defun as-which-key-delay-function (prefix length)
    (cond ((and as-which-key-no-delay-prefixes
                (string-match-p
                 (s-join "\\|" as-which-key-no-delay-prefixes)
                 prefix))
           0)
          (t 1.0)))
  (add-to-list 'which-key-delay-functions 'as-which-key-delay-function))

(provide 'as-which-key)
