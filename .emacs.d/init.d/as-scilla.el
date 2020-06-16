(defvar scilla-root (concat (getenv "HOME") "/blockchain/scilla/scilla")
  "Directory where scilla repository is checked out.")

(req-package scilla-mode
  :require flycheck
  :if (file-directory-p scilla-root)
  :load-path (lambda () (concat scilla-root "/misc/emacs-mode")))

(provide 'as-scilla)
