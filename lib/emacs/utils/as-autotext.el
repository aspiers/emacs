;; Adam's auto-text utilities

;; Should be autoloaded by as-init.el

;;{{{ Date/time stamps

;; Functions I used to use for this:
;;
;; (current-time-string)
;; (shell-command-to-string "date")

(defun as-insert-date (&optional prefix)
  "Inserts the current date into the current buffer at the point.
Defaults to ISO 8601 format suitable for chronological/lexical
sorting, but reverts to more human-readable version if a prefix
argument is given."
  (interactive "*P")
  (insert (format-time-string (if prefix "%a %b %e %Y" "%F %a"))))

(defun as-insert-time ()
  "Inserts the current time into the current buffer at the point."
  (interactive)
  (insert (format-time-string "%H:%M:%S %Z")))

(defun as-insert-date-and-time (&optional prefix)
  "Inserts the current date and time into the current buffer at the
point.  Defaults to ISO 8601 date format suitable for
chronological/lexical sorting, but reverts to more human-readable
version if a prefix argument is given."
  (interactive "*P")
  (as-insert-date prefix)
  (insert " ")
  (as-insert-time))

(defun as-insert-log-timestamp (&optional prefix)
  "Inserts the current date, time and username into the current buffer
at the point.  Defaults to ISO 8601 date format suitable for
chronological/lexical sorting, but reverts to more human-readable
version if a prefix argument is given."
  (interactive "*P")
  (as-insert-date-and-time prefix)
  (insert "  Adam Spiers <>")
  (backward-char 1))

(defun as-insert-log-datestamp (&optional prefix)
  "Inserts the current date and username into the current buffer at
the point.  Defaults to ISO 8601 date format suitable for
chronological/lexical sorting, but reverts to more human-readable
version if a prefix argument is given."
  (interactive "*P")
  (as-insert-date prefix)
  (insert "  Adam Spiers <>")
  (backward-char 1))

;;}}}
;;{{{ as-insert-local-mode

(defun as-insert-local-mode
  ()
  "Inserts an emacs local variables line which will set the local major mode."
  (interactive)
  (beginning-of-buffer)
  (insert "# -*- mode:  -*-\n\n")
  (backward-char 6))

;;}}}
;;{{{ Signatures

(defun as-insert-japh-indirect-sig ()
  "Inserts Adam's cool japh_indirect .sig"
  (interactive)
  (insert-file "~/.sig/perl/japh_indirect"))

(defun as-insert-japh-method-chain-sig ()
  "Inserts Adam's cool japh_method_chain .sig"
  (interactive)
  (insert-file "~/.sig/perl/japh_method_chain"))

;;}}}
;;{{{ Scissors

(defun as-insert-scissors ()
  "Inserts a cut-here-with-scissors"
  (interactive)
  (open-line 1)
  (insert "--------- 8< --------- 8< --------- 8< --------- 8< --------- 8< ---------")
  (forward-line 1))

;;}}}
;;{{{ Snipping text

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
;;{{{ Information about me

(defun as-insert-homepage-url ()
  "Inserts Adam's homepage URL"
  (interactive)
  (insert "http://adamspiers.org/"))

(defun as-insert-old-homepage-url ()
  "Inserts Adam's old homepage URL"
  (interactive)
  (insert "http://www.new.ox.ac.uk/~adam/"))

(defun as-insert-tigerpig-url ()
  "Inserts the tigerpig.org URL"
  (interactive)
  (insert "http://tigerpig.org/"))

(defun as-insert-email-address (&optional prefix)
  "Inserts Adam's e-mail address."
  (interactive)
  (let ((p (point)))
    (if prefix (insert prefix))
    (insert "@adamspiers.org")
    ;; If no prefix provided, leave point where we can type it
    ;; straight away.
    (or prefix (goto-char p))))

(defvar as-work-email-address "aspiers@lehman.com"
  "Adam's work email address.")

(defun as-insert-work-email-address ()
  "Inserts Adam's e-mail address"
  (interactive)
  (insert as-work-email-address))

(defun as-insert-name ()
  "Inserts Adam's name"
  (interactive)
  (insert "Adam Spiers"))

(defun as-insert-name-and-email (&optional email)
  "Inserts Adam's name and e-mail address"
  (interactive "MPrefix for '@adamspiers.org' ? ")
  (as-insert-name)
  (insert " <")
  (cond (email (insert email "@adamspiers.org"))
        (t (as-insert-email-address)))
  (insert ">"))

(defun as-insert-name-and-work-email ()
  "Inserts Adam's name and work e-mail address"
  (interactive)
  (as-insert-name)
  (insert " <" as-work-email-address ">"))

(defun as-insert-snail-mail () 
  "Inserts Adam's snail mail address"
  (interactive)
  (insert "70 Ocean Wharf
60 Westferry Road
London
E14 8JS
"))

;;}}}
