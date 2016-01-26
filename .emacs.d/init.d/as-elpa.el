;;; This provides support for the package system and interfacing with
;;; ELPA package archives.

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

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

(provide 'as-elpa)
