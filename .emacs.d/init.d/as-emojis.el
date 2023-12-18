(require 'as-key-chord)

(key-chord-define-global "ZE" 'null)
(define-key key-translation-map (kbd "<key-chord> ZE") (kbd "C-x 8 e"))
(define-key key-translation-map (kbd "<key-chord> EZ") (kbd "C-x 8 e"))

(provide 'as-emojis)
