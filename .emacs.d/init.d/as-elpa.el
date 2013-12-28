;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(when
    (or (load "package")
        (load (expand-file-name (concat edotdir "/.emacs.d/elpa/package.el"))))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  (or (assq 'edit-server-htmlize package-archive-contents)
      (package-refresh-contents)))

(provide 'as-elpa)
