;; Adam's buffer/file utilities

;; Should be autoloaded by as-init.el

;;{{{ as-bounce-buffer

(defvar as-bounce-buffer-regexp-alist '()
  "Controls the behaviour of `as-bounce-buffer'.")

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
        (format "Rename %s to: " cur-buf-fn)  ;; prompt

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
  (dired-rename-file (buffer-file-name) new-file-name nil)
  (message (format "Renamed to %s" new-file-name)))

;;}}}
;;{{{ as-make-backup-file-name

(defun as-make-backup-file-name (file)
  "Adam's replacement for `make-backup-file-name', since
`backup-directory-alist' isn't flexible enough."
  (let ((home (getenv "HOME"))
        backup-root
        sub-path)
    (cond
     ;; If the file's in the current user's home directory, keep the
     ;; backup at the top-level of the home directory.
     ((string-match (format "^\\(%s\\)/\\(.+\\)" home) file)
;;     ((string-match "^\\(/home/[^/]+\\)/\\(.+\\)" file)
      (setq backup-root (format "%s/.emacs.backup" (match-string 1 file)))
      (setq sub-path (match-string 2 file)))
     ;; Otherwise stick it in /tmp.
     (t
      (setq backup-root (format "/tmp/.emacs.backup-%s" user-login-name))
      (setq sub-path (substring file 1)))) ;; trim leading /
    (let* ((sub-dir (format "%s/%s" backup-root (file-name-directory sub-path))))
      (make-directory sub-dir t)
      (format "%s/%s" backup-root sub-path))))
  
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
      (when (memq system-type '(windows-nt ms-dos))
        ;; Normalize DOSish file names: convert all slashes to
        ;; directory-sep-char, downcase the drive letter, if any,
        ;; and replace the leading "x:" with "/drive_x".
        (or (file-name-absolute-p file)
            (setq file (expand-file-name file))) ; make defaults explicit
        ;; Replace any invalid file-name characters (for the
        ;; case of backing up remote files).
        (setq file (expand-file-name (convert-standard-filename file)))
        (setq dir-sep-string (char-to-string directory-sep-char))
        (if (eq (aref file 1) ?:)
            (setq file (concat dir-sep-string
                               "drive_"
                               (char-to-string (downcase (aref file 0)))
                               (if (eq (aref file 2) directory-sep-char)
                                   ""
                                 dir-sep-string)
                               (substring file 2)))))
      ;; Make the name unique by substituting directory
      ;; separators.  It may not really be worth bothering about
      ;; doubling `!'s in the original name...
      (expand-file-name
       (subst-char-in-string
        directory-sep-char ?!
        (replace-regexp-in-string "!" "!!" file))
       backup-directory))))

;;}}}

;;{{{ bury-and-close-buffer

(defun bury-and-close-buffer ()
  (interactive)
  (bury-buffer) 
  (when (not (one-window-p))
    (delete-window)))

;;}}}
;;{{{ mhj-set-q-to-close

(defun mhj-set-q-to-close ()
  (local-set-key "q" 'bury-and-close-buffer))

;;}}}

