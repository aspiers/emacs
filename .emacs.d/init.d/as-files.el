;; File handling

(req-package tar-mode
  :mode ("\\.dump$" . tar-mode))

(req-package auto-compression-mode
  :ensure nil
  :defer t
  :config
  (defun lac () "Load auto-compression-mode."
    (interactive)
    (auto-compression-mode 1)))

(req-package recentf
  :if window-system
  :config
  (recentf-mode t))

(bind-key "C-c +"   'make-directory)
(bind-key "C-c R"   'as-rename-current-buffer-file)
(bind-key "C-c k"   'delete-file)
(bind-key "C-c K"   'as-destroy-buffer-delete-file)
(bind-key "C-x M-f" 'find-library)

(req-package ffap
  :bind
  (("C-M-'"   . find-file-at-point)
   ("C-x 4 '" . ffap-other-window)
   ("C-x 5 '" . ffap-other-frame)))

(use-package find-file-in-dir
  :ensure nil
  :config
  (define-find-file-in-dir-function as-find-from-home "~/")
  (define-find-file-in-dir-function as-find-from-root "/sudo:root@localhost:/")

  :bind (("C-~"     . as-find-from-home)
         ("C-c j h" . as-find-from-home)
         ("C-c j /" . as-find-from-root)))

(defun as-find-personal-todo ()
  (interactive)
  (find-file "~/org/TODO.org"))
(bind-key "C-c j t" 'as-find-personal-todo)
(bind-key "C-\""    'as-find-personal-todo)
(defun as-find-personal-diary ()
  (interactive)
  (find-file "~/org/diary.org"))
(bind-key "C-c j d" 'as-find-personal-diary)

(use-package find-file-in-dir
 :ensure nil
  :config
  (define-find-file-in-dir-function as-find-personal-note
    "~/org/notes" "Find note: ")

  :bind (("C-c j n" . as-find-personal-note)))

(req-package as-find-file-matching-regexp-hook
  :ensure nil)

(require 'as-vars)
;; This is right for SUSE, at least, assuming that the
;; emacs-debugsource package is installed.
(setq find-function-C-source-directory
      (format "/usr/src/debug/emacs-%s/src" emacs-version-number))

(provide 'as-files)
