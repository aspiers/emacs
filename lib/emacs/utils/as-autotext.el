;; Adam's auto-text utilities

;;{{{ Information about me

;;;###autoload
(defun as-insert-email-address (&optional prefix)
  "Inserts Adam's e-mail address."
  (interactive)
  (let ((p (point)))
    (if prefix (insert prefix))
    (insert "@adamspiers.org")
    ;; If no prefix provided, leave point where we can type it
    ;; straight away.
    (or prefix (goto-char p))))

(defvar as-work-email-address "aspiers@suse.com"
  "Adam's work email address.")

;;;###autoload
(defun as-insert-work-email-address ()
  "Inserts Adam's e-mail address"
  (interactive)
  (insert as-work-email-address))

;;;###autoload
(defun as-insert-name ()
  "Inserts Adam's name"
  (interactive)
  (insert "Adam Spiers"))

;;;###autoload
(defun as-insert-name-and-email (&optional email)
  "Inserts Adam's name and e-mail address"
  (interactive "MPrefix for '@adamspiers.org' ? ")
  (as-insert-name)
  (insert " <")
  (cond (email (insert email "@adamspiers.org"))
        (t (as-insert-email-address)))
  (insert ">"))

;;;###autoload
(defun as-insert-name-and-work-email ()
  "Inserts Adam's name and work e-mail address"
  (interactive)
  (as-insert-name)
  (insert " <" as-work-email-address ">"))

;;}}}
;;{{{ Date/time stamps

;; Functions I used to use for this:
;;
;; (current-time-string)
;; (shell-command-to-string "date")

(defun as-date-to-epoch (date-time-string)
  "Uses date(1) to convert an arbitrary date string to UNIX epoch
time.  Note that the epoch time is a number of seconds too big to be
an elisp integer, so it has to be a float."
  (let ((epoch
         (shell-command-to-string (format "date -d '%s' +%%s" date-time-string))))
    ;; trim trailing newline
    (if (string-match "\\(.+?\\)\n+" epoch)
        (setq epoch (match-string 1 epoch)))
    (string-to-number epoch)))

(defun as-epoch-to-high-low (float)
  "Converts a UNIX epoch time (as a float) to a list suitable for use
with `format-time-string' and similar functions."
  (let ((divisor (lsh 1 16)))
    (cons (round (/   float divisor))
          (round (mod float divisor)))))

;;;###autoload
(defun as-insert-date (format &optional date)
  "Inserts the current date into the current buffer at the point using
the given format.

If the optional argument is supplied, parses that as a string for an
alternative date to use."
  (insert
   (format-time-string format
                       (if date (as-epoch-to-high-low (as-date-to-epoch date))
                         nil))))

(defvar as-iso8601-date-format "%F %a"
  "ISO8601 date format suitable for passing to `format-time-string'.")
(defvar as-human-date-format "%a %b %e %Y"
  "Human-readable date format suitable for passing to `format-time-string'.")
(defvar as-human-time-format "%H:%M:%S %Z"
  "Human-readable time format suitable for passing to `format-time-string'.")
(defvar as-filename-date-format "%Y-%m-%d"
  "Filename-friendly date format suitable for passing to `format-time-string'.")
(defvar as-filename-time-format "%H%Mh%Ss"
  "Filename-friendly time format suitable for passing to `format-time-string'.")
(defvar as-rpm-changelog-format "%a %b %e %H:%M:%S %Z %Y"
  "rpm changelog date format suitable for passing to `format-time-string'.")

;;;###autoload
(defun as-insert-date-interactive (&optional prefix)
  "Inserts a date into the current buffer.

With no prefix argument, inserts the current date in ISO8601 format.

With any prefix argument other than 1 or 2, inserts the current date in
human-readable format.

With a prefix argument of 1, prompts for a date string to be parsed by
the date(1) command, and inserts that date in ISO8601 format.

With a prefix argument of 2, prompts for a date string to be parsed by
the date(1) command, and inserts that date in human-readable format."
  (interactive "*P")
  (let ((date (if (or (eq prefix 1) (eq prefix 2))
                  (read-string "Date: ")
                nil))
        (format (if (or (not prefix) (eq prefix 1))
                    as-iso8601-date-format
                  as-human-date-format)))
    (as-insert-date format date)))

;;;###autoload
(defun as-insert-time (&optional prefix)
  "Inserts the current time into the current buffer at the point.  If
a prefix argument, "
  (interactive "*P")
  (insert (format-time-string
           (if prefix as-filename-time-format as-human-time-format))))

;;;###autoload
(defun as-insert-date-and-time (&optional prefix)
  "Inserts the current date and time into the current buffer at the
point.  "
  (interactive "*P")
  (as-insert-date-interactive prefix)
  (insert " ")
  (as-insert-time))

;;;###autoload
(defun as-insert-file-timestamp (&optional prefix)
  "Inserts the current date and, if prefix given, time also, into the
current buffer at the point, in a format suitable for use within a
filename."
  (interactive "*P")
  (insert (format-time-string as-filename-date-format))
  (if prefix (insert (format-time-string (concat "-" as-filename-time-format)))))

;;;###autoload
(defun as-insert-log-timestamp (&optional prefix)
  "Inserts the current date, time and username into the current buffer
at the point.  Defaults to ISO 8601 date format suitable for
chronological/lexical sorting, but reverts to more human-readable
version if a prefix argument is given."
  (interactive "*P")
  (as-insert-date-and-time prefix)
  (insert "  Adam Spiers <>")
  (backward-char 1))

;;;###autoload
(defun as-insert-log-datestamp (&optional prefix)
  "Inserts the current date and username into the current buffer at
the point.  Defaults to ISO 8601 date format suitable for
chronological/lexical sorting, but reverts to more human-readable
version if a prefix argument is given."
  (interactive "*P")
  (as-insert-date-interactive prefix)
  (insert "  Adam Spiers <>")
  (backward-char 1))

;;;###autoload
(defun as-insert-rpm-changelog-datestamp (&optional prefix)
  "Inserts an rpm changelog header containing the current date,
time, and work email, into the current buffer at the point.
Designed to be run from the top of a .changes file."
  (interactive)
  (insert "-------------------------------------------------------------------\n")
  (as-insert-date as-rpm-changelog-format)
  (insert " - " as-work-email-address "\n\n- \n\n")
  (backward-char 2))

;;}}}
;;{{{ as-insert-local-mode

;;;###autoload
(defun as-insert-local-mode
  ()
  "Inserts an emacs local variables line which will set the local major mode."
  (interactive)
  (goto-char (point-min))
  (insert "# -*- mode:  -*-\n\n")
  (backward-char 6))

;;}}}
;;{{{ Signatures

;;;###autoload
(defun as-insert-japh-indirect-sig ()
  "Inserts Adam's cool japh_indirect .sig"
  (interactive)
  (insert-file-contents "~/.sig/perl/japh_indirect"))

;;;###autoload
(defun as-insert-japh-method-chain-sig ()
  "Inserts Adam's cool japh_method_chain .sig"
  (interactive)
  (insert-file-contents "~/.sig/perl/japh_method_chain"))

;;}}}
;;{{{ Scissors

;;;###autoload
(defun as-insert-scissors ()
  "Inserts a cut-here-with-scissors"
  (interactive)
  (open-line 1)
  (insert "--------- 8< --------- 8< --------- 8< --------- 8< --------- 8< ---------")
  (forward-line 1))

;;}}}
;;{{{ Snipping text

;;;###autoload
(defun as-snip-region ()
  "Replaces a region with `[snipped]' text, ensuring one blank
line before and after the text."
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert "\n[snipped]\n")
  (if (re-search-forward "\n*" nil t)
    (replace-match "\n" nil nil))
  (re-search-backward "\\[snipped\\]" nil t)
  (re-search-backward "[^\n]" nil t)
  (forward-char 1)
  (if (re-search-forward "\n*" nil t)
    (replace-match "\n\n" nil nil))
  (re-search-forward "\\[snipped\\]\n\n" nil t))

;;}}}
