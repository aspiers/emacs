;; Adam's auto-text utilities

;; Should be autoloaded by as-init.el

;;{{{ as-insert-date-and-time

(defun as-insert-date-and-time
  ()
  "Inserts the current date and time into the current buffer at the point."
  (interactive)
  (insert
   (shell-command-to-string "date")
   )
  (backward-delete-char 1)
;;(insert (current-time-string))
)

;;}}}
;;{{{ as-insert-log-timestamp

(defun as-insert-log-timestamp
  ()
  "Inserts the current date, time and username into the current buffer at the point."
  (interactive)
  (insert
   (shell-command-to-string "date")
   )
  (backward-delete-char 1)
  (insert
;;   (current-time-string)
   " "
   (user-login-name))
)

;;}}}

;;{{{ Signatures

(defun as-insert-japh-sig ()
  "Inserts Adam's cool JAPH .sig"
  (interactive)
  (insert-file "~/.sig/perl/japh_indirect"))

;;}}}
;;{{{ Scissors

(defun as-insert-scissors ()
  "Inserts a cut-here-with-scissors"
  (interactive)
  (open-line 1)
  (insert "--------- 8< --------- 8< --------- 8< --------- 8< --------- 8< ---------")
  (forward-line 1))

;;}}}
;;{{{ Home pages

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

;;}}}
;;{{{ e-mail address

(defun as-insert-email-address ()
  "Inserts Adam's e-mail address"
  (interactive)
  (insert "adam@spiers.net"))

;;}}}
;;{{{ Name (how lazy am I?)

(defun as-insert-name ()
  "Inserts Adam's name"
  (interactive)
  (insert "Adam Spiers"))

;;}}}
;;{{{ Me (name & e-mail)

(defun as-insert-name-and-email ()
  "Inserts Adam's name and e-mail address"
  (interactive)
  (as-insert-name)
  (insert " <")
  (as-insert-email-address)
  (insert ">"))

;;}}}
