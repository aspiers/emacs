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
    flx-ido ;; flx is flex with better ordering
    folding
    gist
    git-gutter
    ;;git-gutter-fringe
    git-modes
    goto-chg
    guide-key ;; comes from MELPA
    haml-mode
    idomenu ;; http://emacsrocks.com/e10.html
    ;; ido-hacks ;; ido-everywhere
    ido-ubiquitous
    ido-vertical-mode
    inf-ruby
    iy-go-to-char ;; http://emacsrocks.com/e04.html
    key-chord ;; http://emacsrocks.com/e07.html
    keywiz
    magit
    ;; https://github.com/nex3/magithub/pull/11
    ;; https://github.com/nex3/magithub/pull/12
    ;;magithub
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
    smex
    ;;smooth-scrolling
    switch-window
    versions
    yaml-mode
    yasnippet
    )
  "Adam's list of packages to install with el-get.")

;; Remove packages not in the above list.  If I absent-mindedly
;; install something via `el-get-list-packages', this reduces the
;; chance I'll get used to it being there on one machine and then get
;; surprised when it isn't on another machine I switch to.
(el-get-cleanup as-el-get-packages)

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

(el-get (if el-get-install-sync 'sync) as-el-get-packages)
