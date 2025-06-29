;; Technical languages, excluding programming languages.
;;
;; See also as-doc-langs.el and as-config-langs.el,
;; and as-{ruby,javascript}.el etc.

;; feature-mode for Cucumber's feature DSL ("Gherkin")
(use-package feature-mode
  :mode ("\\.feature\\'" . feature-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(provide 'as-tech-langs)
