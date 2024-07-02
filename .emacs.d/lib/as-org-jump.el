;; See as-jump.el / as-package-loading.el for explanation of usage
(use-feature as-jump
  :after which-key
  :config

  (define-find-file-in-dir-function as-find-personal-note
    "~/org/notes" "Find note: ")

  (defun as-find-personal-todo ()
    (interactive)
    (find-file as-personal-todo))

  (defun as-find-personal-diary ()
    (interactive)
    (find-file "~/org/diary.org"))

  (require 'org-jump-olp)

  (defun as-org-jump-olp-and-next (file olp)
    (interactive)
    (org-jump-olp file olp)
    (org-next-visible-heading 1)
    ;; use org speed keys
    (beginning-of-line))

  (defun as-find-tool-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("computer and technology"
                                "toolchain software")))

  (defun as-find-emacs-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("computer and technology"
                                "toolchain software"
                                "emacs")))

  (defun as-find-email-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("computer and technology"
                                "email")))

  (defun as-find-GTD-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo '("GTD")))

  (defun as-find-orgmode-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("GTD" "orgmode")))

  (defun as-find-OW-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("community" "OW2000")))

  (defun as-find-CO2ken-todos ()
    (interactive)
    (as-org-jump-olp-and-next as-personal-todo
                              '("community" "green" "CO2ken")))

  (bind-keys :map as-jump-map
             ("t" "personal TODO" . as-find-personal-todo)
             ("l" "tool TODOs" . as-find-tool-todos)
             ("e" "email TODOs" . as-find-email-todos)
             ("E" "emacs TODOs" . as-find-emacs-todos)
             ("o" "orgmode TODOs" . as-find-orgmode-todos)
             ("O" "OW2000 TODOs" . as-find-OW-todos)
             ("c" "CO2ken TODOs" . as-find-CO2ken-todos)
             ("d" "personal diary" . as-find-personal-diary)
             ("G" "GTD TODOs" . as-find-GTD-todos)
             ("n" "personal note" . as-find-personal-note)))

(with-packages (key-chord as-jump)
  :chords (("ZN" . as-find-personal-note)))

(defun as-org-clock-in-switch-to-state (current-state)
  "Function for use with `org-clock-in-switch-to-state'."
  (if (equal current-state "NEXT") "STARTED" current-state))

(defun as-org-jump-clock-or-agenda ()
  "Jump to an active clock or to the agenda buffer if no clock is active."
  (interactive)
  (if (marker-buffer org-clock-marker)
      (org-clock-goto)
    (as-org-switch-to-agenda-buffer)))

(provide 'as-org-jump)
