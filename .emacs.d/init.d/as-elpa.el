;;; This provides support for the package system and interfacing with
;;; ELPA package archives.

(require 'package)

;; Make sure we have our customized value of package-archives
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

(require-elpa-packages 'use-package-el-get)
(use-package-el-get-setup)

(provide 'as-elpa)
