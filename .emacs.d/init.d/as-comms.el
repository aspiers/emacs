;; Interaction / communication with other people.
;; See as-mail.el for mail-specific setup.

(defun as-setup-mode-for-discussion ()
  "Sets up a text mode in the way Adam likes for discussion with
other people."

   ;; Nicer version of fill-paragraph
   (local-set-key [(control meta q)] 'fill-common-prefix-region)

   ;; Treat single quoted ("> > ") lines the same as multiple
   ;; w.r.t. filling.
   (setq adaptive-fill-first-line-regexp adaptive-fill-regexp)

   (setq comment-start "> "))

;;{{{ ispell

(bind-key "C-$"   'ispell-complete-word)
(bind-key "C-M-$" 'ispell-buffer)

;;}}}

(provide 'as-comms)
