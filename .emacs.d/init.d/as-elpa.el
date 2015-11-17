;;; This was originally installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.

(defvar as-elpa-packages
  '(
    ;; MELPA
    beeminder
    edit-server-htmlize
    flx-ido       ;; flx is flex with better ordering
    guide-key
    kmacro-decision
    iy-go-to-char ;; like 'f' in vim - http://emacsrocks.com/e04.html
    markdown-mode+
    muttrc-mode

    ;; marmalade
    gerrit-download
    )
  "Adam's list of packages to install from ELPA-like archives,
either with el-get or `package-install'.")

(defun as-setup-elpa ()
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))

;; el-get doesn't let elpa download dependencies for package.el packages
;; since <https://github.com/dimitri/el-get/pull/1798>.
;; However it now causes warnings like:
;;
;;   Warning (emacs): Unable to activate package `flycheck-package'.
;;   Required package `flycheck-0.22' is unavailable
;;
;; https://github.com/dimitri/el-get/issues/1790#issuecomment-46508273
;;
;; and also:
;;
;;   Warning (el-get): The package `cl-lib' has already been loaded by
;;   package.el, attempting to load el-get version instead. To avoid
;;   this warning either uninstall one of the el-get or package.el
;;   version of cl-lib, or call `el-get' before `package-initialize' to
;;   prevent package.el from loading it.
;;
;; This seems to be solved by skipping the call to `package-initialize'.
;; Presumably el-get takes care of that later, when needed.

;;  (package-initialize)

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
  (with-demoted-errors
    (as-setup-elpa)))

(provide 'as-elpa)
