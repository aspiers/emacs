(defvar prettier-el-home "~/.GIT/3rd-party/prettier.el")

(if (file-directory-p prettier-el-home)
    (use-package prettier
      :load-path prettier-el-home)
  (warn "%s did not exist; won't install prettier.el"))

;; Was experimenting with this quelpa/MELPA recipe, but first see
;; https://github.com/jscheid/prettier.el/issues/11
;;
;; (prettier :repo "jscheid/prettier.el"
;;           :fetcher github
;;           :branch "melpa"
;;           :files ("*.el" "*.js" "*.base64" "dir" "*.info" "COPYING*"
;;                   (:exclude "*-tests.el" "*-pkg.el")))

(provide 'as-linting)