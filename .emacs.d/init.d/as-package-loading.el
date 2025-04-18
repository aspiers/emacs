;; Ensure that use-package, and any dependencies (such as use-package
;; extensions) are all available and ready to use.

(eval-and-compile (as-loading-started))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;; Putting the integer in a list avoids --single-branch
(setq straight-vc-git-default-clone-depth '(10))  ;; or 'full

;;(setq straight-vc-git-auto-fast-forward t)

;; Make sure we have our customized value of package-archives
(require 'as-load-custom)

(as-progress "bootstrapping use-package...")

(straight-use-package 'use-package)

;; Stolen from https://github.com/raxod502/radian/blob/develop/emacs/radian.el
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil :ensure nil
     ,@args))
;; Looks like the above (declare ...) makes this unnecessary:
;;(put 'use-feature 'lisp-indent-function 'defun)

;; Allow stuff like :chords (("ZB" . bookmark-jump)) in
;; use-package declarations:
(straight-use-package 'use-package-chords)

;; Define a (with-packages (A B ...) BODY ...) macro for setting up
;; config which takes effect when the given packages are all loaded.
(require 'with-packages)

(as-progress "bootstrapping use-package... done")

(setq use-package-verbose 'debug)

(require 'find-file-in-dir)
(define-find-file-in-dir-function as-find-elpa-package
  "~/.emacs.d/elpa" "Find ELPA package: ")
(define-find-file-in-dir-function as-find-straight-package
  (straight--repos-dir) "Find straight.el package repository: ")
(define-find-file-in-dir-function as-find-straight-package-build
  (straight--build-dir) "Find straight.el package build: ")

(use-package hydra
  :config

  (defhydra hydra-straight-helper (:hint nil)
    "
_c_heck package  |_f_etch package|_m_erge package  |_n_ormalize package|p_u_sh package
_C_heck all      |_F_etch all    |_M_erge all      |_N_ormalize all    |
^-^--------------+^-^------------+^-^--------------+^-^----------------+-^-^------------
_r_ebuild package|_p_ull package |_v_ersions freeze|_w_atcher start    |_g_et recipe
_R_ebuild all    |_P_ull all     |_V_ersions thaw  |_W_atcher quit     |_U_pdate recipes
prun_e_ build               || _q_uit ||"
    ("c" straight-check-package)
    ("C" straight-check-all)
    ("r" straight-rebuild-package)
    ("R" straight-rebuild-all)
    ("f" straight-fetch-package)
    ("F" straight-fetch-all)
    ("p" straight-pull-package)
    ("P" straight-pull-all)
    ("m" straight-merge-package)
    ("M" straight-merge-all)
    ("n" straight-normalize-package)
    ("N" straight-normalize-all)
    ("u" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("U" straight-pull-recipe-repositories)
    ("e" straight-prune-build)
    ("q" nil))

  :bind ("C-c S" . hydra-straight-helper/body))

;; Need as-jump to ensure that as-jump-map is defined.
(use-feature as-jump
  ;; Need to load which-key to ensure define-key is advised before it
  ;; is used via bind-keys.
  :after which-key
  :config
  (defun switch-to-use-package-buffer ()
    "Switches to the *use-package* buffer"
    (interactive)
    (switch-to-buffer-if-exists "*use-package*"))

  (bind-keys :map as-jump-map
             ("l" "ELPA package" . as-find-elpa-package)
             ("s" "straight.el package" . as-find-straight-package)
             ("b" "straight.el package" . as-find-straight-package-build)
             ("U" "*use-package*" . switch-to-use-package-buffer)))

;; This is nice for trying out packages
(use-package try :commands try)

(cl-defun as-straight-setup-git-config (&key repo-dir remote url branch
                                             depth commit &allow-other-keys)
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
