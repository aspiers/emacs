;; http://emacsrocks.com/e07.html

(req-package key-chord
  :commands (key-chord-mode key-chord-define-global))

(req-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(provide 'as-key-chord)
