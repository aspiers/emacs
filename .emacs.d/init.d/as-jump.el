(use-package as-which-key
  :ensure nil
  :init
  ;; N.B. We cannot allow the definition of as-jump-map to defer,
  ;; otherwise if as-jump's recursive dependencies aren't *all* loaded
  ;; when doing (use-package as-jump) from elsewhere, as-jump-map won't
  ;; be defined and this :config section will error.  We could but don't
  ;; want to use a fugly work-around of repeating dependencies in the
  ;; calling package (e.g. as-files) which are already declared in the
  ;; right place.
  (defvar as-jump-map (make-sparse-keymap "Jump to"))

  :defer 0.1
  :config

  (defun switch-to-messages-buffer ()
    "Switches to the *Messages* buffer"
    (interactive)
    (switch-to-buffer (messages-buffer)))

  ;; Need to load which-key to ensure define-key is advised
  (require 'which-key)

  (define-key as-jump-map "M" '("Switch to *Messages*" . switch-to-messages-buffer))

  (add-to-list 'as-which-key-no-delay-prefixes
               "C-c j\\|<key-chord> \\(z j\\|j z\\)")
  (bind-key "C-c j" as-jump-map)
  (key-chord-define-global "zj" as-jump-map)

  ;; Apparently :chords is processed before :config, so as-jump-map
  ;; is not available.
  ;; :chords ("zj" . as-jump-map)
  )

(provide 'as-jump)
