;; FIXME: this breaks forward-word?!  FFS.
;; https://gist.github.com/aspiers/775ce717bd06d43d7adb

;; Steve Yegge to the rescue
;; (req-package js2-mode
;;   :commands nil
;;   :mode ("\\.js\\(\.erb\\)?$" . js2-mode))

(req-package coffee-mode)

(defvar as-prettier-js-dir-locals-variables
  '((js-mode . ((eval . (prettier-mode t)))))
  "Variables for use with `dir-locals-set-class-variables' to
enable prettier.el for Javascript files.")

(dir-locals-set-class-variables 'prettier-js
                                as-prettier-js-dir-locals-variables)

(provide 'as-javascript)
