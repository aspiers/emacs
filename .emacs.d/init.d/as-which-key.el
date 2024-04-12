(defvar as-which-key-no-delay-prefixes ()
  "List of regexps matching keymap prefix bindings for which
`which-key` should not delay showing the keymap's bindings.")

(require 'as-mode-lighters)

(use-package which-key
  :defer 0.1
  :diminish

  :init
  ;; FIXME: This one has to go before the mode is *loaded*!
  (setq which-key-enable-extended-define-key t)

  :custom

  ;; These have to go before the mode is activated
  (which-key-idle-delay 0.1)
  (which-key-idle-secondary-delay 0.1)
  (which-key-lighter "")
  (which-key-max-description-length nil)

  :config
  (which-key-mode)

  (require 's)
  (defun as-which-key-delay-function (prefix length)
    (cond ((and as-which-key-no-delay-prefixes
                (string-match-p
                 (s-join "\\|" as-which-key-no-delay-prefixes)
                 prefix))
           0)
          (t 1.0)))
  (add-to-list 'which-key-delay-functions
               'as-which-key-delay-function)

  ;; Can't name prefix keymap - reported in:
  ;;
  ;;   https://github.com/justbur/emacs-which-key/issues/253
  ;;
  ;; in which this workaround was suggested but doesn't work:
  ;;
  ;;   (define-prefix-command 'as-jump-ruby)
  ;;   (define-key as-jump-map "r" '("Ruby" . as-jump-ruby))

  ;; Neither does this:
  ;;
  ;;   (bind-keys :map as-jump-map ("r" "Ruby" . as-jump-ruby))

  ;; So write a helper function to fix it, which effectively does
  ;; something like this:
  ;;
  ;;   (push '((nil . "as-jump-ruby-map") . (nil . "Ruby")) which-key-replacement-alist)
  (defun as-which-key-add-map-title (map text)
    "Assign a which-key title of TEXT to the prefix map MAP, if it's
not already done."
    (let* ((map-name (symbol-name map))
           (key (cons nil map-name))
           (existing (cdr (assoc key which-key-replacement-alist)))
           (replacement (cons nil text)))
      (if existing
          (if (not (equal existing replacement))
              (warn "Tried to set %s to %s but was already %s"
                    map-name text (cdr existing)))
        (push (cons key replacement) which-key-replacement-alist)))))

(provide 'as-which-key)
