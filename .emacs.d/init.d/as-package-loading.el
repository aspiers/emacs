;; Ensure that use-package, req-package, and any dependencies (such as
;; use-package extensions) are all available and ready to use.
;;
;; Since req-package and its dependencies are used so commonly, we
;; load this file up-front via as-pre-init-d, and then all the other
;; files don't need to explicitly require it.

(eval-and-compile (as-loading-started))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 10)  ;; or 'full
;;(setq straight-vc-git-auto-fast-forward t)

;; Make sure we have our customized value of package-archives
;;
;; Also req-package-log-level only takes effect if defined before
;; req-package is loaded.
;; https://github.com/edvorg/req-package/issues/33#issuecomment-211359690
(require 'as-load-custom)

(as-progress "bootstrapping use-package and req-package...")

(straight-use-package 'use-package)

;; Stolen from https://github.com/raxod502/radian/blob/develop/emacs/radian.el
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))
;; Looks like the above (declare ...) makes this unnecessary:
;;(put 'use-feature 'lisp-indent-function 'defun)

;; This has to be loaded before req-package, otherwise req-package
;; will install el-get itself from MELPA.
(require 'as-el-get)

(straight-use-package 'req-package)

;; Workaround missing ;;;###autoload cookie for req-package
(autoload 'req-package "req-package" "\
Add package PKG with ARGS to target list.

\(fn PKG &rest ARGS)" nil t)

;; Allow stuff like :ensure-system-package npm in
;; use-package declarations:
(straight-use-package 'use-package-ensure-system-package)

;; Allow stuff like :chords (("ZB" . bookmark-jump)) in
;; use-package declarations:
(straight-use-package 'use-package-chords)

;; Define a (with-packages (A B ...) BODY ...) macro for setting up
;; config which takes effect when the given packages are all loaded.
(require 'with-packages)

(as-progress "bootstrapping use-package and req-package... done")

(setq use-package-verbose 'debug)

(require 'find-file-in-dir)
(define-find-file-in-dir-function as-find-elpa-package
  "~/.emacs.d/elpa" "Find ELPA package: ")
(define-find-file-in-dir-function as-find-el-get-package
  "~/.el-get" "Find el-get package: ")
(define-find-file-in-dir-function as-find-straight-package
  (straight--repos-dir) "Find straight.el package repository: ")

(use-package hydra
  :config

  (defhydra hydra-straight-helper (:hint nil)
    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil))

  :bind ("C-c S" . hydra-straight-helper/body))

;; Need as-jump to ensure that as-jump-map is defined.
(use-feature as-jump
  ;; Need to load which-key to ensure define-key is advised before it
  ;; is used via bind-keys.
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("l" "ELPA package" . as-find-elpa-package)
             ("L" "el-get package" . as-find-el-get-package)
             ("s" "straight.el package" . as-find-straight-package)))

;; This is nice for trying out packages
(use-package try :commands try)

(defun as-straight-setup-git-config (repo-dir remote url branch
                                              depth commit)
  (let* ((default-directory repo-dir)
         (repo-name (file-name-nondirectory (directory-file-name repo-dir)))
         (identifier (format "as-straight-setup-%s" repo-name))
         (buffer-name (format " *%s*" identifier))
         (org-repo-p (equal repo-name "org"))
         (email (concat (cond (org-repo-p "orgmode")
                              (t "emacs"))
                        "@adamspiers.org"))
         (command (concat "git config --local user.email " email)))
    (start-process-shell-command
     identifier buffer-name
     (if org-repo-p
         (concat command "; "
                 "git config --local "
                 "sendemail.to emacs-orgmode@gnu.org")
       command))))

(add-hook 'straight-vc-git-post-clone-hook #'as-straight-setup-git-config)

(provide 'as-package-loading)
(eval-and-compile (as-loading-done))
