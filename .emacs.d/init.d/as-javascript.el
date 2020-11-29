;; Steve Yegge to the rescue
(use-package js2-mode
  :mode ("\\.jsx?\\(\.erb\\)?$")
  :config

  ;; Allow easy configuring of 3rd party repos for hard tab indents via:
  ;;    (dir-locals-set-directory-class "/path/to/repo/" 'js2-tab-8-indent)
  (dir-locals-set-class-variables
   'js2-tab-8-indent
   '((js2-mode . ((indent-tabs-mode . t)
                  (js2-basic-offset . 8))))))

(use-package coffee-mode)
(use-package flymake-eslint)
(use-package flymake-jshint)
(use-package flymake-jslint)
(use-package typescript-mode)
(use-package tss)

(defvar as-prettier-js-dir-locals-variables
  '((js-mode . ((eval . (prettier-mode t)))))
  "Variables for use with `dir-locals-set-class-variables' to
enable prettier.el for Javascript files.")

(dir-locals-set-class-variables 'prettier-js
                                as-prettier-js-dir-locals-variables)

(provide 'as-javascript)
