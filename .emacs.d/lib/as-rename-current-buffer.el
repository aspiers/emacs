;;{{{ as-rename-current-buffer-file

;;;###autoload
(defun as-rename-current-buffer-file (new-file-name)
  "Renames the file in the current buffer, and renames the buffer accordingly.

Wraps around `rename-file'."
  (interactive
   (let ((cur-buf-fn
          (or (buffer-file-name)
              (error "Current buffer does not have a filename"))))
     (list ;; interactive expects a list matching the defun arglist
      (expand-file-name
       (let ((read-file-name-function nil))
        (read-file-name
         (format "Rename %s to: " cur-buf-fn) ;; prompt

         ;; Directory to complete in.  We use a nasty trick here and
         ;; include the whole original filename - seems not to matter
         ;; that it's not a directory.  This means the point is
         ;; initially placed at the end of the initial string.
         cur-buf-fn
         ;;(file-name-directory cur-buf-fn)

         ;; Default filename
         cur-buf-fn

         ;; Do not require input to be an existing file
         nil

         ;; Initial input
         ;; (file-name-nondirectory cur-buf-fn)
         nil))))))

  ;; dired-rename-file will rename both the file and the buffer,
  ;; handling buffer name uniquification for us.
;;   (rename-file buffer-file-name new-file-name)
;;   (rename-buffer new-file-name t)

  (if (file-directory-p new-file-name)
      (setq new-file-name
            (concat new-file-name "/" (file-name-nondirectory (buffer-file-name)))))

  ;; The nil below means "not ok if already exists" and gets passed to
  ;; rename-file.
  (require 'dired) ;; does this avoid 'dired-fun-in-all-buffers: Symbol's function definition is void: dired-buffers-for-dir' error?
  (dired-rename-file (buffer-file-name) new-file-name nil)
  (message (format "Renamed to %s" new-file-name)))

;;}}}
;;{{{ as-rename-current-buffer

(defvar as-buffer-name-history nil
  "List containing history of values used by `as-rename-current-buffer'.")

;;;###autoload
(defun as-rename-current-buffer (new-name)
  "Renames the current buffer.  Wraps around `rename-buffer'."
  (interactive
   (let ((current-name (buffer-name)))
       (list ;; interactive expects a list matching the defun arglist
        (read-string
         (format "Rename %s to: " current-name) ;; prompt
         current-name  ;; initial value
         'as-buffer-name-history ;; no history
         nil           ;; no default
         nil           ;; don't inherit input method from previous buffer
         ))))
  (rename-buffer new-name t))

;;}}}
;;{{{ as-buffer-rename-via-alist-hook

(defvar as-buffer-renamings-alist '()
  "Each element in this alist is a buffer renaming directive of the form

  (REGEXP . REPLACE)

When a find-file is performed, this hook matches the filename against
each REGEXP, and for the first one that matches, the matching part of
the buffer name is replaced with REPLACE.

The buffer is then renamed to the result.")

(defun as-buffer-rename-via-alist-hook ()
  "Hook to rename a buffer by matching it against regexps in
`as-buffer-renamings-alist', for which see the documentation."
  (catch 'endloop
    (mapcar
     (lambda (x)
       (cond ((let ((case-fold-search nil))
                (string-match (concat ".*" (car x)) (buffer-file-name)))
              (let ((rename-to
                     (replace-match (cdr x) t nil (buffer-file-name) nil)))
                (rename-buffer rename-to t)
;;              (message (format "renamed to %s" rename-to))
                (throw 'endloop t)))))
     as-buffer-renamings-alist)))

(add-hook 'find-file-hooks 'as-buffer-rename-via-alist-hook)

(defun as-buffer-rename-remove-unique-id ()
  "Attempt to remove the unique suffix (e.g. \"<1>\") from the current
buffer's name.  It will fail silently if a buffer already exists with
that name."
  (interactive)
  (and
   (string-match "<[0-9]+>$" (buffer-name))
   (condition-case nil
       (rename-buffer (replace-match "" t t (buffer-name) nil))
     (error nil))))

;; Always try to do this; occasionally things screw up and leave
;; you with a foo<2> buffer when there's no need.
(add-hook 'find-file-hooks 'as-buffer-rename-remove-unique-id)

;;}}}

(provide 'as-rename-current-buffer)
