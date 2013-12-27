(defvar as-find-file-matching-regexp-alist '()
  "alist mapping filename regexps to functions which will be evaluated when
filenames matching the regexps are visited.

This allows you to set local variables specific to sets of files, e.g.

(setq as-find-file-matching-regexp-alist
      '((\"/foo/bar/.*\.pm\" . (lambda () (setq cperl-indent-level 2)))))")

;;;###autoload
(defun as-find-file-matching-regexp-hook ()
  "Hook to run arbitrary functions on newly visited files.

Controlled by `as-find-file-matching-regexp-alist'."
  (mapcar
   (lambda (x)
     (cond
      ((let ((case-fold-search nil))
         (string-match (concat ".*" (car x)) (buffer-file-name)))
       ;; (message (format "%s matched %s" (buffer-file-name) (car x)))
       (funcall (cdr x)))
      (t
       ;; (message (format "%s didn't match %s" (buffer-file-name) (car x)))
       )))
   as-find-file-matching-regexp-alist))

(add-hook 'find-file-hooks 'as-find-file-matching-regexp-hook)

(provide 'as-find-file-matching-regexp-hook)
