(if (require 'quelpa nil t)
    (as-progress "Skipping buggy quelpa-self-upgrade")
    ;;(quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (as-progress "Downloaded quelpa.el (%d bytes)" (buffer-size))
    (eval-buffer)))

(as-progress "Setting up quelpa-use-package")

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

(use-package as-jump
  :ensure nil
  :config
  ;; Need to load which-key to ensure define-key is advised
  (require 'which-key)
  (bind-keys :map as-jump-map
             ("q" "quelpa package" . as-find-quelpa-package)))

(provide 'as-quelpa)
