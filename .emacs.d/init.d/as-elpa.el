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
;; as per https://github.com/dimitri/el-get/issues/1790#issuecomment-46508273
;;
;; This is because both flycheck and flycheck-package get installed
;; via el-get; however flycheck comes from a native el-get
;; non-versioned recipe, whereas flycheck-package comes from MELPA and
;; has a dependency on flycheck-0.22 which is not satisfied by el-get.
;; In this case a solution might be to prefer the versioned flycheck
;; from MELPA.
;;
;; Another problem is that we don't always want to prefer the version from
;; *ELPA, since that would result in:
;;
;;   Warning (el-get): The package `cl-lib' has already been loaded by
;;   package.el, attempting to load el-get version instead. To avoid
;;   this warning either uninstall one of the el-get or package.el
;;   version of cl-lib, or call `el-get' before `package-initialize' to
;;   prevent package.el from loading it.
;;
;; However we need to initialize package-archive-contents so that we
;; can check for missing packages below.  Luckily we can do that
;; without activating any *ELPA packages by specifying an argument to
;; `package-initialize':

  (package-initialize 'no-activate)

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

;; We don't have use-package yet (because we might have to install it
;; via el-get and/or elpa)
(when (require 'package)
  (with-demoted-errors
      (as-setup-elpa)))

(provide 'as-elpa)
