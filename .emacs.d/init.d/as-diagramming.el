(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path "/usr/share/java/plantuml.jar"))

(use-package flycheck-plantuml)

(with-packages (org plantuml-mode)
  :config
  (setq org-plantuml-jar-path plantuml-jar-path)

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(provide 'as-diagramming)
