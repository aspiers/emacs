(package-initialize)

(if (require 'quelpa nil t)
    ;; Skipping buggy quelpa-self-upgrade
    ;;(quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(provide 'as-quelpa)
