;; -*- Mode: Emacs-Lisp -*-
;;
;; emacs and Xemacs startup file
;; Adam Spiers
;;

(defvar edotdir
  (or (getenv "ZDOTDIR") "~")
  "Home directory to be used to retrieve emacs init files.")

;; save original load-path - e.g. useful for finding site-lisp directory
(setq orig-load-path load-path)

(defvar find-function-source-path load-path)

(defun as-add-to-find-function-source-path (paths)
  "Adds paths to `find-function-source-path'."
  (message "Adding to find-function-source-path:")
  (dolist (x as-source-paths)
    (let ((path (concat as-emacs-dir "/" x)))
      (message "  %s" path)
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

(cond 
 ((boundp 'running-xemacs)
  ;; XEmacs automatically saved settings go here:
  (setq save-options-init-file (concat as-init-dir "/as-options-init.el"))
  (setq save-options-file (concat as-init-dir "/as-options.el"))
  (load (concat as-init-dir "/as-options") 'noerror)))

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

;; (when (getenv "EMACS_PROFILE_INIT")
;;   (load "elp")
;;   (elp-instrument-package "blah")

(as-progress "loading as-init ...")

(load (concat as-version-post-lib-dir "/as-init"))

(as-progress "loaded as-init")

(defun as-find-hooks (hook-name)
  "Uses $ZDOT_FIND_HOOKS to return a list of hooks for `hook-name'."
  (let ((lines (split-string
                (shell-command-to-string (concat ". $ZDOT_FIND_HOOKS " hook-name))
                "\n"
                'omit-nulls)))
    (mapcar
     ;; trim .el from end to allow `load' to use byte-compiled form
     (lambda (file)
       (if (string-match "\\.el\\'" file)
           (replace-match "" nil t file)
         file))
     lines)))

(as-progress "running .emacs-hooks.d ...")

(mapcar (lambda (hook) (if (> (length hook) 0) (load hook)))
        ;; .emacs.d already taken
        (as-find-hooks ".emacs-hooks.d"))

(as-progress "ran .emacs-hooks.d")

;; Stop Red Hat trampling over my nice config :-(
(setq inhibit-default-init t)

(as-progress (format "loading %s ..." custom-file))
;; This load is required, according to the info pages:
(load custom-file)
(as-progress (format "loaded %s" custom-file))

(defvar el-get-dir)
(setq el-get-dir (concat edotdir "/.emacs.d/el-get/"))
(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch) ;; ditch this `let' to get stable branch
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

(defvar as-el-get-packages
  '(
    el-get

    ace-jump-mode ;; http://emacsrocks.com/e10.html

    ;; artbollocks-mode ;; way too aggressive
    asciidoc
    auto-complete-css
    auto-complete-emacs-lisp
    auto-complete-ruby
    auto-complete-yasnippet
    bundler
    coffee-mode
    expand-region
    flymake-css
    flymake-ruby
    flymake-sass
    flymake-shell
    folding
    gist
    git-gutter
    ;;git-gutter-fringe
    git-modes
    goto-chg
    haml-mode
    idomenu ;; http://emacsrocks.com/e10.html
    ido-ubiquitous
    inf-ruby
    iy-go-to-char ;; http://emacsrocks.com/e04.html
    key-chord ;; http://emacsrocks.com/e07.html
    keywiz
    magit
    ;; https://github.com/nex3/magithub/pull/11
    ;; https://github.com/nex3/magithub/pull/12
    ;;magithub
    multiple-cursors
    ;org2blog
    org-magit ;; support for magit: links in org buffers
    org-sync
    rinari
    ruby-mode
    smex
    smooth-scrolling
    switch-window
    yaml-mode
    yasnippet
    )
  "Adam's list of packages to install with el-get.")

;; Remove packages not in the above list.  If I absent-mindedly
;; install something via `el-get-list-packages', this reduces the
;; chance I'll get used to it being there on one machine and then get
;; surprised when it isn't on another machine I switch to.
(el-get-cleanup as-el-get-packages)

(defvar el-get-install-sync nil
  "Non-nil means install packages synchronously")

(el-get (if el-get-install-sync 'sync) as-el-get-packages)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(when
    (or (load "package")
        (load (expand-file-name (concat edotdir "/.emacs.d/elpa/package.el"))))
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")))

(as-progress "end of ~/.emacs")
