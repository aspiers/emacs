;; Need this in as-custom:
;; (custom-set-variables
;;  '(make-backup-file-name-function (quote as-make-backup-file-name)))

(defvar as-make-backup-file-name-hooks '()
  "Hooks for adding more customisability when making backup filenames.
Each hook will get called in turn with the filename of the buffer to be
backed up.  The hook should return the backup filename if it wants to
suggest one, otherwise nil.")

;;;###autoload
(defun as-make-backup-file-name (file)
  "Adam's replacement for `make-backup-file-name', since
`backup-directory-alist' isn't flexible enough.  Set
`make-backup-file-name-function' to this function to use it.

Runs hooks in `as-make-backup-file-name-hooks' with the filename as
the only argument, until one returns a backup file-name.  If there is
no hook which does this, anything within the user's home directory
gets backed up under ~/.emacs.backup, and everything else to
/tmp/.emacs.backup-$USER.

Also ensures that the directory part of the proposed backup file-name
exists, so that hooks don't have to create directories."
  (let* ((home (getenv "HOME"))
         (backup-file
          (or
           (run-hook-with-args-until-success 'as-make-backup-file-name-hooks file)
           (cond
            ;; If the file's in the current user's home directory, keep the
            ;; backup at the top-level of the home directory.
            ((string-match (format "^%s/\\(.+\\)" home) file)
             (format "%s/.emacs.backup/%s" home (match-string 1 file)))
            ;; Otherwise stick it in /tmp.
            (t
             (format "/tmp/.emacs.backup-%s/%s"
                     user-login-name
                     ;; trim leading /
                     (substring file 1))))))
         (sub-dir (file-name-directory backup-file)))
    (make-directory sub-dir t)
    backup-file))
  
(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
  (and (or (string-match "~\\'" file)
           (string-match "\\.emacs\\.backup\\(-[a-zA-Z]+\\)?/" file))
       t))

;; Don't actually use this at the moment - create dir tree instead.
(defun as-absolute-path-to-file-name (file)
  "Converts a full path name to a file name by substituting path
separators for '!'.

Shamelessly ripped out of `make-backup-file-name-1' in `files.el'."
  (let ((backup-directory "/tmp"))
    (progn
      (when (memq system-type '(windows-nt ms-dos cygwin))
        ;; Normalize DOSish file names: downcase the drive
        ;; letter, if any, and replace the leading "x:" with
        ;; "/drive_x".
        (or (file-name-absolute-p file)
            (setq file (expand-file-name file))) ; make defaults explicit
        ;; Replace any invalid file-name characters (for the
        ;; case of backing up remote files).
        (setq file (expand-file-name (convert-standard-filename file)))
        (if (eq (aref file 1) ?:)
            (setq file (concat "/"
                               "drive_"
                               (char-to-string (downcase (aref file 0)))
                               (if (eq (aref file 2) ?/)
                                   ""
                                 "/")
                               (substring file 2)))))
      ;; Make the name unique by substituting directory
      ;; separators.  It may not really be worth bothering about
      ;; doubling `!'s in the original name...
      (expand-file-name
       (subst-char-in-string
        ?/ ?!
        (replace-regexp-in-string "!" "!!" file))
       backup-directory))))

(provide 'as-make-backup-file-name)
