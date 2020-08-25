;; Semi-natural languages / documentation
;;
;; See also as-config-langs.el and as-tech-langs.el.

;;; Text

;;(setq major-mode 'indented-text-mode)

;; Iterate over auto-mode-alist, replacing "text-mode"
;; with "indented-text-mode".
(mapc (function
       (lambda (x)
         (if (eq (cdr x) 'text-mode) (setcdr x 'indented-text-mode))))
      auto-mode-alist)

(use-package server
  :commands server-edit

  :config
  (defun as-save-server-buffer ()
    "Save server buffer and quit editing."
    (interactive)
    (save-buffer)
    (server-edit))

  (defun as-set-local-server-edit-keys ()
    "Set key bindings in local mode for editing via browser extensions."
    (interactive)
    (local-set-key "C-c C-c" 'as-save-server-buffer)))

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

;;; Markdown mode

(req-package markdown-mode+)

;;; asciidoc mode

(req-package asciidoc)

;;; ReStructuredText mode

;; (add-to-list 'auto-mode-alist '("\\.re?st$" . rst-mode))

(use-package rst
  :commands rst-mode

  :bind
  (:map rst-mode-map
        ("C-="      . rst-adjust)
        ("C-<up>"   . rst-backward-section)
        ("C-<down>" . rst-forward-section)
        ("C-c C-r"  . rst-shift-region-right)
        ("C-c C-l"  . rst-shift-region-left)
        ("C-p"      . rst-backward-section)
        ("C-n"      . rst-forward-section)
        ("C-r"      . rst-shift-region-right)
        ("C-l"      . rst-shift-region-left))

  :config
  ;; Update the TOC automatically everytime you adjust a section title::
  (add-hook 'rst-adjust-hook 'rst-toc-insert-update))

;;; Info

(eval-after-load "info"
  '(define-key Info-mode-map [(shift tab)] 'Info-prev-reference))

;;; TeX

;;;; Set up tex-dvi-view (C-c C-v)

(setq tex-dvi-view-command
      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))

;;;; Turn on font-lock mode on entry

(add-hook 'tex-mode-hook 'as-font-lock-mode-if-window-system)

;;; man

(eval-when-compile
  (defvar Man-notify-method))
(setq Man-notify-method 'pushy)

;;; mediawiki

;; Don't use until https://github.com/hexmode/mediawiki-el/issues/36
;; is fixed:
;; (use-package mediawiki)

;;; provide

(provide 'as-doc-langs)
