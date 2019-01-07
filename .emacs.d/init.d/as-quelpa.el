(if (require 'quelpa nil t)
    (as-progress "Skipping buggy quelpa-self-upgrade")
    ;;(quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(as-progress "Setting up quelpa-use-package")

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-quelpa-package
  "~/.emacs.d/quelpa/build" "Find quelpa package: ")
(bind-key "C-c j q" 'as-find-quelpa-package)

(provide 'as-quelpa)
