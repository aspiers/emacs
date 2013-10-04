(require 'as-progress)

;; save original load-path - e.g. useful for finding site-lisp directory
(setq orig-load-path load-path)

(defvar find-function-source-path load-path)

(defun as-add-to-find-function-source-path (paths)
  "Adds paths to `find-function-source-path'."
  (as-progress "Adding to find-function-source-path:")
  (dolist (x as-source-paths)
    (let ((path (concat as-emacs-dir "/" x)))
      (as-progress "  %s" path)
      (and (file-directory-p path)
           (add-to-list 'find-function-source-path path)))))

(as-add-to-find-function-source-path as-source-paths)

(add-to-list 'load-path (concat edotdir "/.emacs.d"))
(let ((home (getenv "HOME")))
  (or (equal edotdir home)
      (add-to-list 'load-path (concat home "~/.emacs.d"))))
(add-to-list 'load-path as-version-pre-lib-dir)
(add-to-list 'load-path (concat as-version-post-lib-dir "/loaddefs") 'append-at-end)
(add-to-list 'load-path as-version-post-lib-dir 'append-at-end)

;; Add $ZDOTDIR/local/share/emacs/site-lisp and subdirs to load-path
(let ((dir (format "%s/local/share/emacs/site-lisp" edotdir))
      (orig-dir default-directory))
  (when (file-accessible-directory-p dir)
      (add-to-list 'load-path                 dir 'append-at-end)
      (add-to-list 'find-function-source-path dir 'append-at-end)
      (cd dir)
      (normal-top-level-add-subdirs-to-load-path)
      (cd orig-dir)))

(require 'as-progress)

(as-progress "load-path set up")
