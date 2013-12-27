;; Technical languages (excluding programming languages)

;;{{{ DTD

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks 'turn-on-font-lock)

;; auto-mode-alist:
;;       ("\\.dtd$"                               . dtd-mode)
;;       ("\\.dcl$"                               . dtd-mode)
;;       ("\\.dec$"                               . dtd-mode)
;;       ("\\.ele$"                               . dtd-mode)
;;       ("\\.ent$"                               . dtd-mode)
;;       ("\\.mod$"                               . dtd-mode)

;;}}}
;;{{{ XML / SGML

(dolist (elt '(("\\.sgml$" . sgml-mode)
               ("\\.dtd$"  . sgml-mode)
               ("\\.xsd$"  . xml-mode)))
  (add-to-list 'auto-mode-alist elt))

(setq-default sgml-indent-data t)

;; fontify straight away
(setq-default sgml-auto-activate-dtd t)

(defvar sgml-warn-about-undefined-elements nil)
(defvar sgml-warn-about-undefined-entities nil)

;; Some convenient key definitions:
;; (define-key sgml-mode-map "\C-cm\C-e" 'sgml-describe-element-type)
;; (define-key sgml-mode-map "\C-cm\C-i" 'sgml-general-dtd-info)
;; (define-key sgml-mode-map "\C-cm\C-t" 'sgml-describe-entity)

;;{{{ faces

(setq-default sgml-set-face t)

(make-face 'sgml-comment-face)
(make-face 'sgml-doctype-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-ignored-face)
(make-face 'sgml-ms-end-face)
(make-face 'sgml-ms-start-face)
(make-face 'sgml-pi-face)
(make-face 'sgml-sgml-face)
(make-face 'sgml-short-ref-face)
(make-face 'sgml-start-tag-face)

(add-hook 'sgml-mode-hook
          (lambda ()
            (set-face-foreground 'sgml-comment-face   "dark green")
            (set-face-foreground 'sgml-doctype-face   "dark red")
            (set-face-foreground 'sgml-start-tag-face "dark cyan")
            (set-face-foreground 'sgml-end-tag-face   "dark cyan")
            (set-face-foreground 'sgml-entity-face    "magenta")
            (set-face-foreground 'sgml-ignored-face   "gray40")
            (set-face-foreground 'sgml-ms-end-face    "green")
            (set-face-foreground 'sgml-ms-start-face  "yellow")
            (set-face-foreground 'sgml-pi-face        "dark blue")
            (set-face-foreground 'sgml-sgml-face      "brown")
            (set-face-foreground 'sgml-short-ref-face "deep sky blue")))

(setq-default sgml-markup-faces
              '((CDATA      . font-lock-doc-face)
                (comment    . sgml-comment-face)
                (doctype    . sgml-doctype-face)
                (end-tag    . sgml-end-tag-face)
                (entity     . sgml-entity-face)
                (ignored    . sgml-ignored-face)
                (ms-end     . sgml-ms-end-face)
                (ms-start   . sgml-ms-start-face)
                (pi         . sgml-pi-face)
                (sgml       . sgml-sgml-face)
                (short-ref  . sgml-short-ref-face)
                (start-tag  . sgml-start-tag-face)))

;; (setq sgml-markup-faces '((start-tag . font-lock-function-name-face)
;;                           (end-tag . font-lock-function-name-face)
;;                           (comment . font-lock-comment-face)
;;                           (pi . bold)
;;                           (sgml . bold)
;;                           (doctype . bold)
;;                           (entity . font-lock-type-face)
;;                           (shortref . font-lock-function-name-face)))

;;}}}
;;{{{ sgml-specials

;; See the documentation for this.  The default is (34 45) which
;; includes - as a special character for comments.  The problem
;; is that emacs doesn't allow predicates to context-sensitively
;; confirm the syntax of characters, so the fontifying of comments
;; often screws up.

(eval-when-compile
  (defvar sgml-specials))
(setq sgml-specials '(34))

;;}}}

;;}}}
;;{{{ nxml

;; FIXME: this fucks up load-path somewhat - superfluous trailing slashes
;;(when (string-match "/\"" (prin1-to-string load-path)) (error 'fucked))

(defun nx () "Loads nxml-mode." (interactive) (nxml-mode))
(autoload 'mhj-format-xml "mhj-xml" "Mark's nxml hacks." t)
(add-hook 'nxml-mode-hook
          (lambda () (local-set-key [(control meta q)] 'mhj-format-xml)))
;;(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xhtml\\)$" . nxml-mode))

;;}}}
;;{{{ HTML

;; htmltidy support
(autoload 'tidy-buffer "htmltidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "htmltidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "htmltidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "htmltidy" "Install an options menu for HTML Tidy." t)

;; This broke with:
;;   File mode specification error: (void-variable html-helper-mode-map)
;;
;; (defun as-html-mode-tidy-hook () "Add htmltidy support to an HTML mode."
;;   (tidy-build-menu html-helper-mode-map)
;;   (local-set-key [(control c) (control c)] 'tidy-buffer)
;;   (setq sgml-validate-command "htmltidy"))

;;(add-hook 'html-mode-hook 'as-html-mode-tidy-hook)


;;}}}
;;{{{ feature-mode for Cucumber's feature DSL ("Gherkin")

(use-package feature-mode
  :mode ("\.feature$" . feature-mode))

;;}}}

