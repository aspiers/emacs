(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

(as-progress "Setting up quelpa-use-package")
(message "%s" load-path)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; Make sure it works with use-package-always-ensure set to t (which
;; is how I generally like it).  This is needed for installing stuff
;; via quelpa, otherwise it will default to searching ELPA archives.
(quelpa-use-package-activate-advice)

(require 'find-file-in-dir)
(define-find-file-in-dir-function as-find-quelpa-package
  "~/.emacs.d/quelpa/build" "Find quelpa package: ")

;; See as-jump.el / as-package-loading.el for explanation of usage
(use-package as-jump
  :ensure nil
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("q" "quelpa package" . as-find-quelpa-package)))

(provide 'as-quelpa)
