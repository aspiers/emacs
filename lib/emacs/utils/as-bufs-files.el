;; Adam's buffer/file utilities

;;{{{ as-bounce-buffer

(defvar as-bounce-buffer-regexp-alist '()
  "Controls the behaviour of `as-bounce-buffer'.")

;;;###autoload
(defun as-bounce-buffer ()
  "For each element of `as-bounce-buffer-regexp-alist', attempts a search
and replace on the current buffer's filename.  (The CARs are the
search regexps, and the CDRs the corresponding strings to replace the
matches with).  As soon a search is successful, the filename resulting
from the replace is visited via `find-file'."
  (interactive)
  (catch 'gotcha
    (mapcar
     (lambda (x)
       (let ((case-fold-search nil) 
             (match (car x))
             (replace (cdr x)))
         (cond
          ((string-match match (buffer-file-name))
           (let ((bounce-to (replace-match replace t t (buffer-file-name) nil)))
             (message (format "Bounced to %s" bounce-to))
             (find-file bounce-to))
           (throw 'gotcha nil)))))
     as-bounce-buffer-regexp-alist)))

;;}}}
;;{{{ as-display-buffer-filename

(eval-when-compile
  (autoload 'x-select-text "term/x-win" nil t))

;;;###autoload
(defun as-display-buffer-filename (&optional save-to-clipboard)
  "Displays the current buffer's filename in the minibuffer.

If a prefix argument is given, stores the result in the kill ring
and in the X selection for other programs.  If the prefix argument
is 2, forks the external program abs (must be in $PATH) to convert
the filename to an absolute one with all symlinks resolved."
  (interactive "P")
  (or buffer-file-name (error "No file associated with this buffer"))
  (let ((fn (cond ((and save-to-clipboard (eq save-to-clipboard 2))
                   (substring
                    (shell-command-to-string (concat "abs " buffer-file-name))
                    0 -1))
                  (t buffer-file-name))))
    (cond (save-to-clipboard
           (kill-new fn)
;; No need for x-select-text since kill-new calls function set
;; in interprogram-cut-function, which is x-select-text anyway.
;;         (x-select-text buffer-file-name)
           (setq fn (concat fn " (stored in kill ring and X selection)"))))
    (message fn)))

;;}}}
;;{{{ as-destroy-buffer

;;;###autoload
(defun as-destroy-buffer ()
  "Kill the current buffer without leaving crappy auto-save files around."
  (interactive)
  (let ((tmpfile (format "/tmp/.emacs.as-destroy-buffer.%d" (emacs-pid)))
        (buf (buffer-name)))
    (write-file tmpfile)
    (kill-buffer nil)
    (delete-file tmpfile)
    (message (concat "Destroyed buffer " buf))))

;;}}}
;;{{{ as-destroy-buffer-delete-file

;;;###autoload
(defun as-destroy-buffer-delete-file ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (save-buffer)
  (let ((fn (buffer-file-name)))
    (delete-file fn)
    (kill-buffer nil)
    (message (format "Deleted %s" fn))))

;;}}}
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
       (read-file-name
        (format "Rename %s to: " cur-buf-fn) ;; prompt

        ;; Directory to complete in.  We use a nasty trick here and
        ;; include the whole original filename - seems not to matter
        ;; that it's not a directory.
        cur-buf-fn

        ;; Default filename
        cur-buf-fn
        
        ;; Do not require input to be an existing file
        nil
        
        ;; No initial input needed because of above trick
        nil)))))

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
;;{{{ as-make-backup-file-name

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

;;}}}

;;{{{ bury-and-close-buffer

;;;###autoload
(defun bury-and-close-buffer ()
  (interactive)
  (bury-buffer) 
  (when (not (one-window-p))
    (delete-window)))

;;}}}
;;{{{ mhj-set-q-to-close

;;;###autoload
(defun mhj-set-q-to-close ()
  (local-set-key "q" 'bury-and-close-buffer))

;;}}}

;;{{{ as-find-file-matching-regexp-hook

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

;;{{{ Obsolete functions, may come in handy another time

;; (defun as-containing-dir (filename)
;;   "Return the containing directory of a filename when given the full path."
;;   (string-match "\\([^/]+\\)/[^/]+$" filename)
;;   (match-string 1 filename))

;; (defun as-last-dir-and-filename (filename)
;;   "Strip a full path of all of its directory components but the last."
;;   (string-match "\\(.*/\\).+/.+$" (buffer-file-name))
;;   (replace-match "" t t (buffer-file-name) 1))

;; (defun as-buffer-rename-add-one-dir ()
;;   "Add the name of the containing directory of the buffer's file
;; to the beginning of the buffer name."
;;   (interactive)
;;   (rename-buffer (as-last-dir-and-filename (buffer-name))) t)

;;}}}

