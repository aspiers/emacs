(eval-and-compile (as-loading-started))

;; org-mode needs to be loaded before el-get packages which depend on
;; it, otherwise the version of org-mode distributed with emacs will
;; automatically get pulled in when el-get packages which depend on
;; org-mode are initialized, and that would result in the default
;; values for `org-disputed-keys' taking effect and sticking as long
;; as emacs stays running.
(require 'as-org-mode)

(require 'as-vars)
(defvar el-get-dir (concat edotdir "/.emacs.d/el-get/"))
(add-to-list 'load-path (concat el-get-dir "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch) ;; ditch this `let' to get stable branch
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defvar as-el-get-packages
  '(
    el-get

    ace-jump-mode ;; http://emacsrocks.com/e10.html

    ;; artbollocks-mode ;; way too aggressive
    apache-mode
    asciidoc
    auto-complete-css
    auto-complete-emacs-lisp
    auto-complete-ruby
    auto-complete-yasnippet
    auto-recomp
    bundler
    coffee-mode
    color-theme
    darcsum
    ediff-trees
    edit-server
    edit-server-htmlize
    erin
    expand-region
    faith
    folding
    flymake-css
    flymake-ruby
    flymake-sass
    flymake-shell
    flx-ido ;; flx is flex with better ordering
    folding
    gist
    git-gutter
    ;;git-gutter-fringe
    git-modes
    goto-chg
    guide-key ;; comes from MELPA
    haml-mode
    hideshow-org
    idomenu ;; http://emacsrocks.com/e10.html
    ;; ido-hacks ;; ido-everywhere
    ido-ubiquitous
    ido-vertical-mode
    inf-ruby
    iy-go-to-char ;; http://emacsrocks.com/e04.html
    js2-mode
    key-chord ;; http://emacsrocks.com/e07.html
    keywiz
    macrostep
    magit

    ;; https://github.com/nex3/magithub/pull/11
    ;; https://github.com/nex3/magithub/pull/12
    ;;magithub

    markdown-mode
    markdown-mode+
    mediawiki
    multiple-cursors
    org2blog
    org-magit ;; support for magit: links in org buffers
    org-sync

    ;; https://github.com/dimitri/el-get/issues/1471
    ;; https://github.com/dimitri/el-get/issues/1472
    ;; https://github.com/dimitri/el-get/issues/1473
    ;; Finally managed to install by fixing dependencies manually
    ;; (#1471), #1472, #1473
    projectile

    rinari
    ruby-mode
    smart-mode-line
    smex
    ;;smooth-scrolling

    ;; useless until https://github.com/Fuco1/smartparens/issues/259 fixed :-(
    smartparens

    switch-window
    versions
    undo-tree
    use-package
    vc-osc
    xrdb-mode
    yaml-mode
    yasnippet
    )
  "Adam's list of packages to install with el-get.")

;; Remove packages not in the above list.  If I absent-mindedly
;; install something via `el-get-list-packages', this reduces the
;; chance I'll get used to it being there on one machine and then get
;; surprised when it isn't on another machine I switch to.
(as-progress (format "cleaning up unwanted el-get packages ..."))
(el-get-cleanup as-el-get-packages)
(as-progress (format "cleaning up unwanted el-get packages ... done"))

;; Reasons pro installing synchronously:
;;
;;   * code loaded after this file which depends on packages loaded
;;     by this file are guaranteed to work
;;   * stricter, more deterministic approach makes debugging easier
;;
;; Reasons against:
;;
;;   * have to wait longer during bootstrap
(defvar el-get-install-sync t
  "Non-nil means install packages synchronously")

;; We want to encourage use of use-package when consuming el-get
;; packages, so automatically provide it in order to save files which
;; depend on both el-get and use-package from having to require both
;; separately.
(el-get 'sync 'use-package)
(as-progress (format "loading use-package ... done"))

(require 'use-package)

(as-progress (format "loading packages via `el-get' ..."))
(el-get (if el-get-install-sync 'sync) as-el-get-packages)
(as-progress (format "loading packages via `el-get' ...  done"))

(provide 'as-el-get)
(eval-and-compile (as-loading-done))
