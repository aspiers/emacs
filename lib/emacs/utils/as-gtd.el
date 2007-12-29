;; Things to help me with Getting Things Done.

(require 'org-mairix)

(defvar as-mairix-links-clipboard "~/.clip-mairix"
  "[Deprecated] Pseudo-clipboard file where mairix URLs get copied to.")

(defun as-mairix-yank-links ()
  "[Deprecated] Yank from file defined by `as-mairix-links-clipboard'."
  (interactive)
  (let ((bytes (cadr (insert-file-contents
                      (expand-file-name as-mairix-links-clipboard)))))
    (forward-char bytes)
    (save-excursion
      (forward-char -1)
      (if (looking-at "\n")
          (delete-char 1)))))

(defun as-mairix-search-at-point ()
  "[Deprecated] Return the start and end points of a mairix link at
the current point.  The result is a paired list of character positions
for a mairix link located at the current point in the current buffer."
  (save-excursion
    (if (looking-at "<mairix://")
        (forward-char 1)
      (skip-chars-backward "^<"))
    (if (looking-at "mairix://\\([^>]+?\\)>")
        (match-string 1)
      nil)))

(defun as-mairix-view-link-at-point (show-async-buf)
  "[Deprecated] View the 'mairix://...' link at the point using the
shell command defined by `org-mairix-mutt-display-command'."
  (interactive "P")
  (let ((message-id (as-mairix-search-at-point)))
    (or message-id (error "No mairix URL found at point"))
    (let ((cmd org-mairix-mutt-display-command))
      (while (string-match "%search%" cmd)
        (setq cmd (replace-match message-id 'fixedcase 'literal cmd)))
      ;; By default, async `shell-command' invocations display the temp
      ;; buffer, which is annoying here.  We choose a deterministic
      ;; buffer name so we can hide it again immediately.
      ;; FIXME: use `call-process' instead.
      (let ((tmpbufname (generate-new-buffer-name " *mairix-view*")))
        (shell-command cmd tmpbufname)
        (or show-async-buf
            (delete-windows-on (get-buffer tmpbufname)))))))
