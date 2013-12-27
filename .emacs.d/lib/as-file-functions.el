;; Obsolete functions, may come in handy another time

(defun as-containing-dir (filename)
  "Return the containing directory of a filename when given the full path."
  (string-match "\\([^/]+\\)/[^/]+$" filename)
  (match-string 1 filename))

(defun as-last-dir-and-filename (filename)
  "Strip a full path of all of its directory components but the last."
  (string-match "\\(.*/\\).+/.+$" (buffer-file-name))
  (replace-match "" t t (buffer-file-name) 1))

(defun as-buffer-rename-add-one-dir ()
  "Add the name of the containing directory of the buffer's file
to the beginning of the buffer name."
  (interactive)
  (rename-buffer (as-last-dir-and-filename (buffer-name))) t)

