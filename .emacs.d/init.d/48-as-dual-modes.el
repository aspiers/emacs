(as-progress "Dual major/minor modes...")

;;{{{ outline-mode

(mapc (lambda (x)
        (add-hook x 'turn-on-auto-fill))
      '(outline-mode-hook outline-minor-mode-hook))

;;}}}

