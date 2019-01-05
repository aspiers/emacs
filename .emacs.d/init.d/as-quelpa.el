(if (require 'quelpa nil t)
    ;; Skipping buggy quelpa-self-upgrade
    ;;(quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; install use-package and the quelpa handler
(quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))

(require 'quelpa-use-package)

(provide 'as-quelpa)
