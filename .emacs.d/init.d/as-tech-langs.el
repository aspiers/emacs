;; Technical languages, excluding programming languages.
;;
;; See also as-doc-langs.el and as-config-langs.el,
;; and as-{ruby,javascript}.el etc.

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

(req-package feature-mode
  :mode ("\.feature$" . feature-mode))

;;}}}

(req-package yaml-mode)

(provide 'as-tech-langs)
