;; Adam's auto-text utilities

;; Should be autoloaded by as-init.el

;;{{{ Date/time stamps

;; Functions I used to use for this:
;;
;; (current-time-string)
;; (shell-command-to-string "date")

(defun as-insert-date-and-time ()
  "Inserts the current date and time into the current buffer at the
point."
  (interactive)
  (insert (format-time-string "%a %b %e %H:%M:%S %Z %Y")))

(defun as-insert-log-timestamp ()
  "Inserts the current date, time and username into the current buffer
at the point."
  (interactive)
  (as-insert-date-and-time)
  (insert " Adam Spiers <>")
  (backward-char 1))

(defun as-insert-log-datestamp ()
  "Inserts the current date and username into the current buffer at
the point."
  (interactive)
  (insert (format-time-string "%a %b %e %Y")
          " Adam Spiers <>")
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
  "Inserts Adam's e-mail address"
  (interactive)
  (if prefix (insert prefix))
  (insert "@adamspiers.org"))

(defun as-insert-work-email-address ()
  "Inserts Adam's e-mail address"
  (interactive)
  (insert "aspiers@lehman.com"))

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

;;}}}
