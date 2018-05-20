;; Semi-natural languages / documentation
;;
;; See also as-config-langs.el and as-tech-langs.el.

;;{{{ Text

;;(setq major-mode 'indented-text-mode)

;; Iterate over auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".
(mapc (function
       (lambda (x)
         (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
      auto-mode-alist)

(autoload 'server-edit "server")
(defun as-save-server-buffer ()
  "Save server buffer and quit editing."
  (interactive)
  (save-buffer)
  (server-edit))

(defun as-set-local-server-edit-keys ()
  "Set key bindings in local mode for editing via firefox It's
All Text add-on."
  (interactive)
  (local-set-key [(control c) (control c)] 'as-save-server-buffer))

;; (defun as-set-local-edit-server-keys ()
;;   "Set key bindings in local mode for editing via
;; `edit-server-text-mode', which is used by the Chrome 'Edit with
;; Emacs' add-on."
;;   (interactive)
;;   ;; C-c C-c already set up by edit-server
;;   (local-set-key [foo bar] 'as-.....))

(defun as-setup-text-mode ()
  "Set up `text-mode' how Adam likes it."
  ;; Turn on auto-fill if composing e-mail or news.
  (let ((bn (or (buffer-file-name)
                ;; For some reason I've seen mutt-invoked emacs
                ;; instances yield nil for (buffer-file-name)
                (buffer-name))))
    (if (string-match "\\.article\\|\\.letter\\|itsalltext" bn)
        (turn-on-auto-fill))

    (if (string-match "itsalltext" bn)
        (as-set-local-server-edit-keys)))

  ;; Expand all newly inserted tabs to spaces
  (setq indent-tabs-mode nil)

  (as-setup-mode-for-discussion))  

(add-hook 'text-mode-hook 'as-setup-text-mode)

(defun itm () "Shortcut to indented-text-mode."
  (interactive)
  (indented-text-mode))

;;}}}
;;{{{ Markdown mode

(req-package markdown-mode+)

;;}}}
;;{{{ asciidoc mode

(req-package asciidoc)

;;}}}
;;{{{ ReStructuredText mode

;; (autoload 'rst-mode "rst")
;; (add-to-list 'auto-mode-alist '("\\.re?st$" . rst-mode))
;; (autoload 'rst-text-mode-bindings "rst")

;; This stomps over everything :-(
;;(add-hook 'text-mode-hook 'rst-text-mode-bindings)

(eval-when-compile (load-library "rst"))
(defun as-rst-bindings ()
  "Adam's key bindings for `rst-mode'."
  (interactive)

  (local-set-key [(control ?=)] 'rst-adjust)

  (local-set-key [(control up  )] 'rst-backward-section)
  (local-set-key [(control down)] 'rst-forward-section)
  (local-set-key [(control c) (control r)] 'rst-shift-region-right)
  (local-set-key [(control c) (control l)] 'rst-shift-region-left)
;;   (define-key mode-specific-map [(control p)] 'rst-backward-section)
;;   (define-key mode-specific-map [(control n)] 'rst-forward-section)
;;   (define-key mode-specific-map [(control r)] 'rst-shift-region-right)
;;   (define-key mode-specific-map [(control l)] 'rst-shift-region-left)

  ;; Bind the rst commands on the C-c p prefix.
;; FIXME: I can't find rst-prefix-map in rst.el any more, was it removed?
;;  (local-set-key [(control c) (control p)] rst-prefix-map)
  ;;(define-key mode-specific-map [(p)] 'rst-prefix-map)
  )

;; FIXME: do we always want these in text mode?
;;(add-hook 'text-mode-hook 'as-rst-bindings)
(add-hook 'rst-mode-hook 'as-rst-bindings)

;; Update the TOC automatically everytime you adjust a section title::
(add-hook 'rst-adjust-hook 'rst-toc-insert-update)

;;}}}
;;{{{ Info

(eval-after-load "info"
  '(define-key Info-mode-map [(shift tab)] 'Info-prev-reference))

;;}}}

;;{{{ TeX

;;{{{ Set up tex-dvi-view (C-c C-v)

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;;}}}

;; Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'as-font-lock-mode-if-window-system)

;;}}}
;;{{{ man

(eval-when-compile
  (defvar Man-notify-method))
(setq Man-notify-method 'pushy)

;;}}}
;;{{{ TWiki

;; Needs to be loaded via the el-get recipe, since it's not in MELPA:
;;
;;   - http://www.neilvandyke.org/erin-twiki-emacs/
;;   - extension at https://github.com/fitzsim/erin
;;
(req-package erin
  :ensure nil
  :el-get t
  :mode ("\\.twiki\\'" . erin-mode))
;;
;; See also https://github.com/christopherjwhite/emacs-twiki-mode/issues/5

;;}}}

(req-package mediawiki)

(provide 'as-doc-langs)
