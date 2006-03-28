;; Things to help me with Getting Things Done.

(defvar as-mairix-links-clipboard (concat (getenv "HOME") "/.clip-mairix")
  "Pseudo-clipboard file where mairix URLs get copied to.")

(defvar as-mairix-results-folder (concat (getenv "HOME") "/mail/novell/mairix")
  "Folder where mairix writes results.")

(defvar as-mairix-link-viewer-command "mairix %search% && xterm -title 'mairix search: %search%' -e 'unset COLUMNS; mutt -f %folder% -e \"push <display-message>\"' &"
  "Command to view messages linked to by 'mairix://...' links.")

(defun as-mairix-yank-links ()
  "Yank from file defined by `as-mairix-links-clipboard'."
  (interactive)
  (let ((bytes (cadr (insert-file-contents as-mairix-links-clipboard))))
    (forward-char bytes)
    (save-excursion
      (forward-char -1)
      (if (looking-at "\n")
          (delete-char 1)))))

(defun as-mairix-search-at-point ()
  "Return the start and end points of a mairix link at the current
point.  The result is a paired list of character positions for a
mairix link located at the current point in the current buffer."
  (save-excursion
    (if (looking-at "<mairix://")
        (forward-char 1)
      (skip-chars-backward "^<"))
    (if (looking-at "mairix://\\([^>]+?\\)>")
        (match-string 1)
      nil)))

(defun as-mairix-view-link-at-point ()
  "View the 'mairix://...' link at the point using the shell command
defined by `as-mairix-link-viewer-command'."
  (interactive)
  (let ((message-id (as-mairix-search-at-point)))
    (or message-id (error "No mairix URL found at point"))
    (let ((cmd as-mairix-link-viewer-command))
      (while (string-match "%search%" cmd)
        (setq cmd (replace-match message-id 'fixedcase 'literal cmd)))
      (while (string-match "%folder%" cmd)
        (setq cmd (replace-match as-mairix-results-folder 'fixedcase 'literal cmd)))
      ;; By default, async shell-command invocations display the temp
      ;; buffer, which is annoying here.  We choose a deterministic
      ;; buffer name so we can hide it again immediately.,
      (let ((tmpbufname (generate-new-buffer-name " *mairix-view*")))
        (shell-command cmd tmpbufname)
        (delete-windows-on (get-buffer tmpbufname))))))
