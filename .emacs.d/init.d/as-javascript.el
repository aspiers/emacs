;; Steve Yegge to the rescue
(use-package js2-mode
  :mode ("\\.js\\(\.erb\\)?$"))

;; Allow easy configuring of 3rd party repos for various indentation
;; strategies, via statements like:
;;
;;    (dir-locals-set-directory-class "/path/to/repo/" 'js-tab-8-indent)
(dir-locals-set-class-variables
 'js-indent-8-tabs
 '((nil . ((indent-tabs-mode . t)
           (js-indent-level . 8)
           (js2-basic-offset . 8)))))
(dir-locals-set-class-variables
 'js-indent-2-no-tabs
 '((nil . ((indent-tabs-mode . nil)
           (js-indent-level . 2)
           (js2-basic-offset . 2)))))

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
