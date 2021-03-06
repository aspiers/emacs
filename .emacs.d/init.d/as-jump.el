;; N.B. We cannot allow the definition of as-jump-map to defer,
;; otherwise if as-jump's recursive dependencies aren't *all* loaded
;; when doing (use-package as-jump) from elsewhere, as-jump-map won't
;; be defined and their :config sections will error.
;;
;; We could but don't want to use a fugly work-around of repeating
;; dependencies in the calling package (e.g. as-files) which are
;; already declared in the right place.
;;
;; See as-package-loading.el for an example of how to set up bindings
;; in the as-jump-map.

(defvar as-jump-map (make-sparse-keymap "Jump to")
  "Adam's prefix keymap for quickly jumping to stuff")

(defun switch-to-buffer-if-exists (buffer-or-name &optional description)
  "Switches to the given buffer if it exists; otherwise emits a warning."
  (interactive)
  (if (get-buffer buffer-or-name)
      (switch-to-buffer buffer-or-name)
    (error "No %s buffer to jump to"
           (or description buffer-or-name "<unknown>"))))

(use-feature as-which-key
  ;; Need to load which-key to ensure define-key is advised
  :after which-key

  :defer 0.1
  :config

  (defun switch-to-help-buffer ()
    "Switches to the *Help* buffer"
    (interactive)
    (switch-to-buffer-if-exists (help-buffer)))

  (defun switch-to-messages-buffer ()
    "Switches to the *Messages* buffer"
    (interactive)
    (switch-to-buffer-if-exists (messages-buffer)))

  (defun switch-to-warnings-buffer ()
    "Switches to the *Warnings* buffer"
    (interactive)
    (switch-to-buffer-if-exists "*Warnings*"))

  (defun switch-to-scratch-buffer ()
    "Switches to the *scratch* buffer"
    (interactive)
    (switch-to-buffer-if-exists "*scratch*"))

  (bind-keys :map as-jump-map
             ("H" "*Help* buffer" . switch-to-help-buffer)
             ("W" "*Warnings* buffer" . switch-to-warnings-buffer)
             ("x" "*scratch* buffer" . switch-to-scratch-buffer)
             ("M" "*Messages* buffer" . switch-to-messages-buffer))

  (add-to-list 'as-which-key-no-delay-prefixes
               "C-c j\\|<key-chord> \\(z j\\|j z\\)")
  (bind-key "C-c j" as-jump-map)
  (key-chord-define-global "zj" as-jump-map)

  ;; Apparently :chords is processed before :config, so as-jump-map
  ;; is not available.
  ;; :chords ("zj" . as-jump-map)
  )

(provide 'as-jump)
