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

(bind-key "C-$"   'ispell-complete-word)
(bind-key "C-M-$" 'ispell-buffer)

;; Telegram client for emacs!
;; https://zevlg.github.io/telega.el/
;; (use-package telega)

(defun as-noop ()
  "Do nothing."
  (interactive))

;; Protect a key binding for push-to-talk
(bind-key "s-'" 'as-noop)

(provide 'as-comms)
