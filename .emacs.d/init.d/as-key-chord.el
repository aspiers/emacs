;; http://emacsrocks.com/e07.html

(req-package key-chord
  :commands (key-chord-mode key-chord-define-global)
  :config
  (key-chord-mode 1))

(req-package use-package-chords)

(provide 'as-key-chord)
