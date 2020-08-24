(require 'as-progress)

(eval-and-compile (as-loading-started))

(require 'as-org-mode-early)

(require 'as-vars) ;; we probably already have this via as-load-paths

;; https://github.com/dimitri/el-get/issues/2620
;; (require-elpa-packages 'el-get)

(defvar el-get-dir (concat edotdir "/.emacs.d/el-get/"))
(defvar el-get-el-get-dir (concat el-get-dir "/el-get"))
(add-to-list 'load-path el-get-el-get-dir)

(unless (and (file-exists-p el-get-el-get-dir)
             (require 'el-get nil 'noerror))
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; This would install the list of already installed packages,
;; presumably from /home/adam/.emacs.d/el-get/.status.el - however we
;; now rely on req-package instead.
;;
;; (el-get 'sync)

(use-package use-package-el-get)

(use-package-el-get-setup)

(defun as-el-get-owner-p ()
  "Returns `t' if the current effective uid matches the owner of
the directory pointed to by `el-get-el-get-dir'."
  (eq (nth 2 (file-attributes el-get-el-get-dir 'integer))
      (user-real-uid)))

(unless (as-el-get-owner-p)
  (message "Don't own %s; will skip various installation steps."
           el-get-el-get-dir))

(provide 'as-el-get)
(eval-and-compile (as-loading-done))
