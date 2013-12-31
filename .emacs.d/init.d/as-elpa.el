;;; This was originally installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.

(defvar as-elpa-packages
  '(
    ;; MELPA
    edit-server-htmlize
    flx-ido       ;; flx is flex with better ordering
    guide-key
    iy-go-to-char ;; http://emacsrocks.com/e04.html
    markdown-mode+
    )
  "Adam's list of packages to install from ELPA-like archives,
either with el-get or `package-install'.")

(defun as-setup-elpa ()
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  ;; (require 'cl)
  ;; (when (not (every (lambda (pkg) (car (assq pkg package-archive-contents)))
  ;;                   as-elpa-packages))
  ;;     ...)
  (let ((missing-pkgs
         (remove-if (lambda (pkg)
                      (assq pkg package-archive-contents))
                    as-elpa-packages)))
    (when missing-pkgs
      (message "Packages missing from `package-archive-contents': %s" missing-pkgs)
      (package-refresh-contents))))

(when
    (or (load "package")
        (load (expand-file-name (concat edotdir "/.emacs.d/elpa/package.el"))))
  (as-setup-elpa))

(provide 'as-elpa)
