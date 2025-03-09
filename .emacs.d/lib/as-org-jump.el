;; See as-jump.el / as-package-loading.el for explanation of usage

(defvar as-org-jump-targets
    `(("personal TODO" (,as-personal-todo))  ; Just opens the file
      ("tool TODOs" (,as-personal-todo "computer and technology" "toolchain software"))
      ("emacs TODOs" (,as-personal-todo "computer and technology" "toolchain software" "emacs"))
      ("email TODOs" (,as-personal-todo "computer and technology" "email"))
      ("GTD TODOs" (,as-personal-todo "GTD"))
      ("orgmode TODOs" (,as-personal-todo "GTD" "orgmode"))
      ("OW2000 TODOs" (,as-personal-todo "community" "OW2000"))
      ("CO2ken TODOs" (,as-personal-todo "community" "CO2ken"))
      ("Kernel TODOs" (,as-personal-todo "career" "Kernel")))
    "Alist mapping human-readable descriptions to (file outline-path...) lists.
The file can be a string or symbol (variable name), followed by optional outline path elements.")

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

  (defun as-jump-target (desc)
    "Jump to a target specified by DESC in `as-org-jump-targets'."
    (interactive
     (list (completing-read "Jump to: "
                           (mapcar #'car as-org-jump-targets))))
    (let* ((target (assoc desc as-org-jump-targets))
           (file-and-path (cadr target))  ; Get the first element of the cdr
           (file (if (listp file-and-path)
                    (car file-and-path)
                  file-and-path))
           (olp (if (listp file-and-path)
                   (cdr file-and-path)
                 nil))
           (resolved-file (if (symbolp file)
                            (symbol-value file)
                          file)))
      (if olp
          (as-org-jump-olp-and-next resolved-file olp)
        (find-file resolved-file))))

  (defun as-org-jump-olp-and-next (file olp)
    (interactive)
    (org-jump-olp file olp)
    (org-next-visible-heading 1)
    ;; use org speed keys
    (beginning-of-line))

  (defmacro as-define-jump-command (key desc)
    "Define a jump command and its binding spec for `as-jump-map'.
KEY is the key sequence in `as-jump-map'.
DESC is the human-readable description used for docstring, which-key, and alist lookup."
    (let ((func-name (intern (format "as-jump-to-%s"
                                   (replace-regexp-in-string "[^a-zA-Z0-9]+" "-"
                                                             (downcase desc))))))
      `(progn
         (defun ,func-name ()
           ,(format "Jump to %s using `as-jump-target'." desc)
           (interactive)
           (as-jump-target ,desc))
         (bind-keys :map as-jump-map
                    (,key ,desc . ,func-name)))))

  ;; Define all the jump commands
  (as-define-jump-command "t" "personal TODO")
  (as-define-jump-command "l" "tool TODOs")
  (as-define-jump-command "E" "emacs TODOs")
  (as-define-jump-command "o" "orgmode TODOs")
  (as-define-jump-command "O" "OW2000 TODOs")
  (as-define-jump-command "c" "CO2ken TODOs")
  (as-define-jump-command "G" "GTD TODOs")
  (as-define-jump-command "k" "Kernel TODOs")

  ;; Add the manual bindings
  (bind-keys :map as-jump-map
            ("d" "personal diary" . as-find-personal-diary)
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
