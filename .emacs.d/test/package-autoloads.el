;; This minimal test case shows how (package-initialize) sets up
;; autoloads for all installed packages without further effort.
;;
;; Usage:
;;   emacs -Q --debug-init -l ~/.emacs.d/test/package-autoloads.el

(package-initialize)

;; (require 'use-package)
;; (setq use-package-verbose 'debug)

(add-to-list 'load-path "/home/adam/.emacs.d/lib")
(add-to-list 'load-path "/home/adam/.emacs.d/init.d")

;; This is only needed to ensure it's installed:
;;
;; (use-package helm-org
;;   :defer t :ensure t)

(message "helm-org loaded: %s" (featurep 'helm-org))
(message "helm-org-parent-headings autoloaded: %s"
         (autoloadp (symbol-function 'helm-org-parent-headings)))
(describe-function 'helm-org-parent-headings)
(switch-to-buffer "*Messages*")
