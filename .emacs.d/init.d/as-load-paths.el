;; At this point emacs doesn't know where to find as-progress.
(eval-and-compile (message "> Loading as-load-paths ..."))

;; ~/.emacs.d/init.d was already added to load-path by as-pre-init-d.el
(require 'as-vars)

;; save original load-path - e.g. useful for finding site-lisp directory
(setq orig-load-path load-path)

(add-to-list 'load-path as-init-d)
(add-to-list 'load-path as-lib-dir)

(defvar find-function-source-path load-path)

;; (require 'cl)
;; (setq org-source-paths
;;       (remove-if-not
;;        (lambda (dir) (file-directory-p dir))
;;        (directory-files (concat as-emacs-dir "/major-modes") 'full-paths "org[-.]")))
;; (mapc (lambda (dir) (add-to-list 'find-function-source-path dir))
;;       org-source-paths)

(defvar as-source-paths
  '(
    "major-modes"
    "major-modes/org-mode.git/lisp"
    "major-modes/org-mode.git/contrib/lisp"
    "minor-modes"
    "utils"
    "fun"))

(defun as-add-to-find-function-source-path (paths)
  "Adds paths to `find-function-source-path'."
  (dolist (x as-source-paths)
    (let ((path (concat as-emacs-dir "/" x)))
      (and (file-directory-p path)
           (add-to-list 'find-function-source-path path)))))

(as-add-to-find-function-source-path as-source-paths)

(add-to-list 'load-path as-version-pre-lib-dir)
(add-to-list 'load-path (concat as-version-post-lib-dir "/loaddefs") 'append-at-end)
(add-to-list 'load-path as-version-post-lib-dir 'append-at-end)

;; Add $ZDOTDIR/local/share/emacs/site-lisp and subdirs to load-path
(let ((orig-dir default-directory))
  (dolist (template '("%s/share/emacs/site-lisp"
                    "%s/local/share/emacs/site-lisp"))
    (let ((dir (format template edotdir)))
      (when (file-accessible-directory-p dir)
        (add-to-list 'load-path                 dir 'append-at-end)
        (add-to-list 'find-function-source-path dir 'append-at-end)
        (cd dir)
        (normal-top-level-add-subdirs-to-load-path))))
  (cd orig-dir))

(require 'as-progress)

(provide 'as-load-paths)

;;(eval-and-compile (as-loading-done))
(eval-and-compile (message "> Loading as-load-paths ... done"))
