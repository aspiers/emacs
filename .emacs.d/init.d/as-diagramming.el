(use-package plantuml-mode
  :mode ("\\.puml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/java/plantuml.jar")
  (plantuml-indent-level 4))

(use-package flycheck-plantuml)

(with-packages (org plantuml-mode)
  :custom
  (org-plantuml-jar-path plantuml-jar-path)

  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(provide 'as-diagramming)
