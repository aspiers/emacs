(autoload 'as-make-backup-file-name
                                 "as-bufs-files" "Make backup filenames"    t)
;; Need this in as-custom:
;; (custom-set-variables
;;  '(make-backup-file-name-function (quote as-make-backup-file-name)))


(autoload 'as-display-buffer-filename
                                 "as-bufs-files" "Display buffer filename"  t)
(autoload 'as-bounce-buffer      "as-bufs-files" "Bounce buffers"           t)
(autoload 'as-destroy-buffer-delete-file
                                 "as-bufs-files" "Destroy buffer & file"    t)
(autoload 'as-destroy-buffer     "as-bufs-files" "Destroy buffer"           t)
(autoload 'as-rename-current-buffer-file
                                 "as-bufs-files"
                                 "Renames the file in the current buffer"   t)
(autoload 'bury-and-close-buffer "as-bufs-files" "Bury and close buffers"   t)
(autoload 'mhj-set-q-to-close    "as-bufs-files" "Bind q to bury and close" t)

(defvar as-find-file-matching-regexp-alist '()
  "alist mapping filename regexps to functions which will be evaluated when
filenames matching the regexps are visited.

This allows you to set local variables specific to sets of files, e.g.

(setq as-find-file-matching-regexp-alist
      '((\"/foo/bar/.*\.pm\" . (lambda () (setq cperl-indent-level 2)))))")

(provide 'as-bufs-files-auto)
