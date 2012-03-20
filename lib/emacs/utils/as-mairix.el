(require 'org-mairix nil t)

(defvar as-mairix-links-clipboard "~/.org-mairix-link"
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

(defcustom as-org-mairix-open-personal-command
  "mairix-profile personal %args% '%search%'"
  "The mairix command-line to use. If your paths are set up
correctly, you should not need to change this.

'%search%' will get substituted with the search expression, and
'%args%' with any additional arguments."
  :group 'org-mairix
  :type 'string)

(defcustom as-org-mairix-mutt-display-personal-command
  "mairix-profile --view personal %search% &"
  "Command to execute to display mairix search results for Adam's
personal mail via mutt within an xterm.

'%search%' will get substituted with the search expression, and
'%args%' with any additional arguments used in the search."
  :group 'org-mairix-mutt
  :type 'string)

(defun as-org-mairix-personal-open (search)
  "Function to open mairix link to personal mail.  Uses
`org-mairix-open' with a localised value of
`org-mairix-open-command'."
  (let ((org-mairix-open-command
         as-org-mairix-open-personal-command)
        (org-mairix-mutt-display-command
         as-org-mairix-mutt-display-personal-command))
    (org-mairix-open search)))

(org-add-link-type "mairixp" 'as-org-mairix-personal-open)

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
    ;; Remove text properties to make for cleaner debugging
    (set-text-properties 0 (length message-id) nil message-id)
    ;; By default, async `shell-command' invocations display the temp
    ;; buffer, which is annoying here.  We choose a deterministic
    ;; buffer name so we can hide it again immediately.
    ;; Note: `call-process' is synchronous so not useful here.
    (let ((cmd (org-mairix-command-substitution
                org-mairix-mutt-display-command message-id ""))
          (tmpbuf
           (generate-new-buffer " *as-mairix-view-link-at-point*")))
      (message "Executing '%s'" cmd)
      ;; shell-command seems to blow away any previous contents of
      ;; tmpbuf so we can't insert useful extra debug in it :-(
      (shell-command cmd tmpbuf tmpbuf)
      (or show-async-buf
          (delete-windows-on (get-buffer tmpbuf))))))

(provide 'as-mairix)
