;; Ensure that use-package, req-package, and any dependencies (such as
;; use-package extensions) are all available and ready to use.
;;
;; Since req-package and its dependencies are used so commonly, we
;; load this file up-front via as-pre-init-d, and then all the other
;; files don't need to explicitly require it.

(eval-and-compile (as-loading-started))

(require 'package)

;; Make sure we have our customized value of package-archives
;;
;; Also req-package-log-level only takes effect if defined before
;; req-package is loaded.
;; https://github.com/edvorg/req-package/issues/33#issuecomment-211359690
(require 'as-load-custom)

(as-progress "package-initialize...")
(package-initialize)
(as-progress "package-initialize... done")

;; Copied from https://github.com/edvorg/emacs-configs/blob/master/init-real.el
(defun require-elpa-package (package)
  "Require a package from ELPA archives.  If it's not installed,
first refresh local package archive metadata, and then install if
it's available."
  (if (null (require package nil 'noerror))
      ;; This was copied from req-package's req-package-bootstrap
      ;; which it invokes to bootstrap its dependencies (including
      ;; el-get).
      (progn
        (let* ((ARCHIVES (or package-archive-contents
                             (progn (package-refresh-contents)
                                    package-archive-contents)))
               (AVAIL (assoc package ARCHIVES)))
          (if AVAIL
              (progn
                (as-progress "installing ELPA package %s..." package)
                (package-install package)
                (as-progress "installing ELPA package %s... done" package))
            (as-progress "ELPA package %s not available in package-archive-contents"
                         package)))
        (require package))))

(defun require-elpa-packages (&rest packages)
  (dolist (package packages)
    (require-elpa-package package)))

;; This has to be loaded before req-package, otherwise req-package
;; will install el-get itself from MELPA.
(require 'as-el-get)

(as-progress "bootstrapping use-package and req-package...")
(require-elpa-packages
 'use-package
 'req-package
 'use-package-ensure-system-package
 'use-package-chords)

(require 'with-packages)

(as-progress "bootstrapping use-package and req-package... done")

(setq use-package-verbose 'debug)

(require 'find-file-in-dir)
(define-find-file-in-dir-function as-find-elpa-package
  "~/.emacs.d/elpa" "Find ELPA package: ")
(define-find-file-in-dir-function as-find-el-get-package
  "~/.el-get" "Find el-get package: ")

;; Need as-jump to ensure that as-jump-map is defined.
(use-package as-jump
  :ensure nil
  ;; Need to load which-key to ensure define-key is advised before it
  ;; is used via bind-keys.
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("l" "ELPA package" . as-find-elpa-package)
             ("L" "el-get package" . as-find-el-get-package)))

(as-progress "bootstrapping quelpa...")
(require 'as-quelpa)
(as-progress "bootstrapping quelpa... done")

;; This is nice for trying out packages
(use-package try
  :commands try)

(provide 'as-package-loading)
(eval-and-compile (as-loading-done))
