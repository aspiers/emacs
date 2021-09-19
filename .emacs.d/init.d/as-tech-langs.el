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
  ;; https://github.com/kiennq/json-mode is currently ahead of
  ;; the upstream https://github.com/joshwnj/json-mode, and it
  ;; seems the upstream may be dead:
  ;; https://github.com/joshwnj/json-mode/issues/64
  :straight (:host github :repo "kiennq/json-mode")
  :mode ("\\.json\\'" . json-mode))

(provide 'as-tech-langs)
