;; Ensure that use-package, req-package, and any dependencies (such as
;; use-package extensions) are all available and ready to use.
;;
;; Since req-package and its dependencies are used so commonly, we
;; load this file up-front via as-pre-init-d, and then all the other
;; files don't need to explicitly require it.

(eval-and-compile (as-loading-started))

(require 'package)

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-elpa-package
  "~/.emacs.d/elpa" "Find ELPA package: ")
(bind-key "C-c j l" 'as-find-elpa-package)

(define-find-file-in-dir-function as-find-el-get-package
  "~/.el-get" "Find el-get package: ")
(bind-key "C-c j L" 'as-find-el-get-package)

;; Make sure we have our customized value of package-archives
;;
;; Also req-package-log-level only takes effect if defined before
;; req-package is loaded.
;; https://github.com/edvorg/req-package/issues/33#issuecomment-211359690
(require 'as-custom)

(package-initialize)

;; Copied from https://github.com/edvorg/emacs-configs/blob/master/init-real.el
(defun require-elpa-package (package)
  "Require a package from ELPA archives.  If it's not installed,
refresh local package archive metadata and install if it's not
installed."
  (if (null (require package nil 'noerror))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

(defun require-elpa-packages (&rest packages)
  (dolist (package packages)
    (require-elpa-package package)))

(require-elpa-packages
 'use-package
 'req-package
 'use-package-ensure-system-package
 'use-package-chords
 )

(require 'as-el-get)
(require 'as-quelpa)

(provide 'as-package-loading)
(eval-and-compile (as-loading-done))
