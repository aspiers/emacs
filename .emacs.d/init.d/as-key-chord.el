;; http://emacsrocks.com/e07.html

(use-package key-chord
  :commands (key-chord-mode key-chord-define-global))

(use-package use-package-chords
  ;; Ensure that this is available for other (use-package ... :chords ...)
  ;; directives:
  :demand t

  :config (key-chord-mode 1))

(provide 'as-key-chord)
