(eval-and-compile (as-loading-started))

(require 'as-org-mode-early)

(require 'as-vars)

(defvar el-get-dir (concat edotdir "/.emacs.d/el-get/"))
(defvar el-get-el-get-dir (concat el-get-dir "el-get"))
(add-to-list 'load-path el-get-el-get-dir)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch) ;; ditch this `let' to get stable branch
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun as-el-get-owner-p ()
  "Returns `t' if the current effective uid matches the owner of
the directory pointed to by `el-get-el-get-dir'."
  (eq (nth 2 (file-attributes el-get-el-get-dir 'integer))
      (user-real-uid)))

(unless (as-el-get-owner-p)
  (message "Don't own %s; will skip various installation steps."
           el-get-el-get-dir))

;; Ensure we have ELPA recipes available
(require 'as-elpa)
(if (and (as-el-get-owner-p)
         (not (file-exists-p (concat el-get-el-get-dir "/recipes/elpa"))))
    (el-get-elpa-build-local-recipes))

(defvar as-el-get-emacswiki-packages
  '(
    auto-recomp
    buff-menu+
    ediff-trees
    faith
    rcov-overlay
    versions
    ))

(defun as-el-get-missing-emacswiki-recipes ()
  "Return a list of emacswiki recipes which are required but
  missing from el-get"
  (let ((missing-recipes
         (remove-if (lambda (pkg)
                      (el-get-recipe-filename pkg))
                    as-el-get-emacswiki-packages)))
    (when missing-recipes
      (message "emacswiki recipes missing: %s" missing-recipes))
    missing-recipes))

(if (and (as-el-get-owner-p)
         (or ((not (file-exists-p
                    (concat el-get-el-get-dir "/recipes/emacswiki")))
              (as-el-get-missing-emacswiki-recipes))))
    (with-demoted-errors
      (el-get-emacswiki-build-local-recipes)))

(defvar as-el-get-builtin-packages
  '(
    el-get

    ace-jump-mode ;; http://emacsrocks.com/e10.html

    ;; artbollocks-mode ;; way too aggressive
    apache-mode
    asciidoc
    ;; auto-complete-css
    ;; auto-complete-emacs-lisp
    ;; auto-complete-ruby
    ;; auto-complete-yasnippet
    bundler
    coffee-mode
    color-theme
    company-mode
    darcsum
    edit-server
    erin
    expand-region
    faith
    feature-mode
    fill-column-indicator
    folding
    flycheck
    flycheck-package
    flymake-css
    flymake-ruby
    flymake-sass
    flymake-shell
    gist
    git-gutter
    ;;git-gutter-fringe
    git-modes
    ;;golden-ratio  ;; not sure I like this after all
    goto-chg

    haml-mode
    hideshow-org
    idomenu ;; http://emacsrocks.com/e10.html
    ;; ido-hacks ;; ido-everywhere
    ido-ubiquitous
    ido-vertical-mode

    ;; I don't use this and it seems to have bugs with autoloading
    ;; and maybe also leaving processes running within emacs.
    ;; inf-ruby

    js2-mode
    key-chord ;; http://emacsrocks.com/e07.html
    keywiz
    macrostep
    magit

    ;; https://github.com/nex3/magithub/pull/11
    ;; https://github.com/nex3/magithub/pull/12
    ;;magithub

    markdown-mode
    mediawiki
    multiple-cursors
    org2blog
    org-magit ;; support for magit: links in org buffers
    org-sync
    paredit
    phi-search
    phi-search-mc

    ;; https://github.com/dimitri/el-get/issues/1471
    ;; https://github.com/dimitri/el-get/issues/1472
    ;; https://github.com/dimitri/el-get/issues/1473
    ;; Finally managed to install by fixing dependencies manually
    ;; (#1471), #1472, #1473
    projectile

    python

    rainbow-delimiters
    region-bindings-mode
    rinari
    rpm-spec-mode
    ruby-mode
    rudel
    smart-mode-line
    smex
    smooth-scrolling

    ;; useless until https://github.com/Fuco1/smartparens/issues/259
    ;; fixed or fully upgraded to emacs 24.3 :-/
    smartparens

    smartrep
    switch-window
    undo-tree
    use-package
    vc-osc
    web-mode
    xrdb-mode
    yaml-mode
    yasnippet
    )
  "Adam's list of packages to install via el-get's built-in
recipes, i.e. those not installed from automatically generated
ELPA recipes.")

(defun as-el-get-packages ()
  (append as-el-get-builtin-packages as-elpa-packages as-el-get-emacswiki-packages))

;; Remove packages not in the above list.  If I absent-mindedly
;; install something via `el-get-list-packages', this reduces the
;; chance I'll get used to it being there on one machine and then get
;; surprised when it isn't on another machine I switch to.
(when (as-el-get-owner-p)
  (as-progress (format "cleaning up unwanted el-get packages ..."))
  (el-get-cleanup (as-el-get-packages))
  (as-progress (format "cleaning up unwanted el-get packages ... done")))

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
(if (as-el-get-owner-p)
    (el-get 'sync 'use-package)
  (el-get-init 'use-package))

(as-progress (format "loading use-package ... done"))
(require 'use-package)

(defvar as-el-get-missing-packages ()
  "List of packages which `el-get' failed to load.")

(cond
 ((as-el-get-owner-p)
  (as-progress (format "loading packages via `el-get' ..."))
  (dolist (pkg (as-el-get-packages))
    (with-demoted-errors
      (el-get (if el-get-install-sync 'sync) pkg))
    (unless (string-equal (el-get-read-package-status pkg) "installed")
      (add-to-list as-el-get-missing-packages pkg t)))
  (as-progress (format "loading packages via `el-get' ...  done"))
  (if as-el-get-missing-packages
      (as-progress (format "failed to load packages: %s" as-el-get-missing-packages))))
 (t
  (as-progress (format "initialising packages via `el-get-init' ..."))
  (dolist (pkg (as-el-get-packages))
    (el-get-init pkg))
  (as-progress (format "initialising packages via `el-get-init' ... done"))))

(provide 'as-el-get)
(eval-and-compile (as-loading-done))
