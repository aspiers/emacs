(use-package prettier
  :diminish " pr"
  :straight (prettier :type git :flavor melpa
                      :files (:defaults "dist/*")
                      :branch "master" :host github
                      :repo "jscheid/prettier.el")
  :config
  (dir-locals-set-class-variables 'prettier
                                  '((nil . ((eval . (prettier-mode t)))))))

(provide 'as-linting)
