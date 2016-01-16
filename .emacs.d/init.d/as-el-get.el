(eval-and-compile (as-loading-started))

(require 'as-org-mode-early)

(require 'as-vars) ;; we probably already have this via as-load-paths

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

(require 'as-el-get-emacswiki)

(provide 'as-el-get)
(eval-and-compile (as-loading-done))
