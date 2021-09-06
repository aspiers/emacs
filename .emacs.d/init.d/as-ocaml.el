(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))

;; If this causes issues with merlin-company, make sure merlin is up
;; to date.
(use-package flycheck-ocaml)

(use-package ocp-indent)

(provide 'as-ocaml)
