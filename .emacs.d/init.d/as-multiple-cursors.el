;; multiple-cursors has rectangular-region-mode.el which does
;; (defadvice er/expand-region ...).  This is a miserable fail
;; since er/expand-region is an autoload
(require 'as-point-motion)

(use-package multiple-cursors
  :config

  ;; FIXME: One-shot keys
  (bind-key "C-S-SPC A"         'mc/mark-all-like-this)
  (bind-key "C-S-SPC R"         'mc/mark-all-in-region)
  (bind-key "C-S-SPC W"         'mc/mark-all-words-like-this)
  (bind-key "C-S-SPC S"         'mc/mark-all-symbols-like-this)
  (bind-key "C-S-SPC D"         'mc/mark-all-dwim)
  (bind-key "C-S-SPC m"         'mc/mark-more-like-this-extended)

  (bind-key "C-S-SPC <mouse-1>" 'mc/add-cursor-on-click)
  (bind-key "C-S-SPC C-S-SPC"   'set-rectangular-region-anchor)

  (bind-key "C-S-SPC e"         'mc/edit-lines)
  (bind-key "C-S-SPC C-a"       'mc/edit-beginnings-of-lines)
  (bind-key "C-S-SPC C-e"       'mc/edit-ends-of-lines)

  (bind-key "C-S-SPC 1"         'mc/insert-numbers)
  (bind-key "C-S-SPC +"         'mc/sort-regions)
  (bind-key "C-S-SPC -"         'mc/reverse-regions)
  (bind-key "C-S-SPC <"         'mc/mark-sgml-tag-pair))

;; found smartrep here: http://stackoverflow.com/a/17209600/179332
(use-package smartrep
  :after multiple-cursors

  :config
  ;; Keys we want to be able to hit repeatedly
  (smartrep-define-key global-map "C-S-SPC"
    '(("n"   . mc/mark-next-like-this)
      ("p"   . mc/mark-previous-like-this)

      ("f"   . mc/mark-next-word-like-this)
      ("b"   . mc/mark-previous-word-like-this)

      ("s"   . mc/mark-next-symbol-like-this)
      ("t"   . mc/mark-previous-symbol-like-this)

      ("DEL" . mc/unmark-next-like-this)
      ("C-d" . mc/unmark-previous-like-this)

      ("C-n" . mc/skip-to-next-like-this)
      ("C-p" . mc/skip-to-previous-like-this)

      ("p"   . mc/pop-mark)

      ("q"   . smartrep-quit)
      ("C-g" . smartrep-quit))))

(use-package region-bindings-mode
  ;; FIXME: https://github.com/jwiegley/use-package/issues/71
  ;;
  ;; I added region-bindings-mode-enabled-modes to obsolete this:
  ;;
  ;; (dolist (hook '(c-mode-hook shell-script-mode-hook
  ;;                 emacs-lisp-mode-hook
  ;;                 ruby-mode-hook python-mode-hook))
  ;;   (add-hook hook 'region-bindings-mode-enable))
  :after multiple-cursors

  :config
  (define-key region-bindings-mode-map "q"   'region-bindings-mode-off)

  (define-key region-bindings-mode-map "m"   'mc/mark-more-like-this-extended)

  (define-key region-bindings-mode-map "n"   'mc/mark-next-like-this)
  (define-key region-bindings-mode-map "p"   'mc/mark-previous-like-this)

  (define-key region-bindings-mode-map "f"   'mc/mark-next-word-like-this)
  (define-key region-bindings-mode-map "b"   'mc/mark-previous-word-like-this)

  (define-key region-bindings-mode-map "s"   'mc/mark-next-symbol-like-this)
  (define-key region-bindings-mode-map "t"   'mc/mark-previous-symbol-like-this)

  (define-key region-bindings-mode-map "<delete>" 'mc/unmark-next-like-this)
  (define-key region-bindings-mode-map "C-d" 'mc/unmark-previous-like-this)

  (define-key region-bindings-mode-map "C-n" 'mc/skip-to-next-like-this)
  (define-key region-bindings-mode-map "C-p" 'mc/skip-to-previous-like-this)

  (define-key region-bindings-mode-map "A"   'mc/mark-all-like-this)
  (define-key region-bindings-mode-map "R"   'mc/mark-all-in-region)
  (define-key region-bindings-mode-map "W"   'mc/mark-all-words-like-this)
  (define-key region-bindings-mode-map "S"   'mc/mark-all-symbols-like-this)
  (define-key region-bindings-mode-map "D"   'mc/mark-all-dwim))

(use-package phi-search
  :after multiple-cursors)

(use-package phi-search-mc
  :after multiple-cursors)

(provide 'as-multiple-cursors)
