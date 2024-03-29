;; Issues with ctrlf:
;;
;;  - can't disable wraparound
;;    https://github.com/radian-software/ctrlf/issues/68
;;
;;  - minibuffer overview only via M-s o => ctrlf-occur
;;    which has no native integration
;;    https://github.com/radian-software/ctrlf/issues/69
;;
;;  - doesn't reveal org structure correctly
;;    https://github.com/radian-software/ctrlf/issues/118
;;    Workaround requires switching back to slower older `overlays'
;;    option for org-fold-core-style.
;;
;; (use-package ctrlf
;;   :defer 2
;;   :config
;;   (ctrlf-mode +1)
;;
;;   ;; https://github.com/raxod502/ctrlf/issues/65
;;   (defun ctrlf-yank-word-or-char ()
;;     (interactive)
;;     (let ((input (field-string (point-max))) yank)
;;       (when (or ctrlf--match-bounds (= (length input) 0))
;;         (with-current-buffer (window-buffer (minibuffer-selected-window))
;;           (setq yank (buffer-substring-no-properties
;;                       (or (and ctrlf--match-bounds
;;                                (cdr ctrlf--match-bounds))
;;                           ctrlf--current-starting-point)
;;                       (progn (forward-word) (point)))))
;;         (goto-char (field-end (point-max)))
;;         (insert yank))))
;;
;;   :bind (:map ctrlf-minibuffer-mode-map
;;               ("<down>" . 'ctrlf-forward-literal)
;;               ("<up>" . 'ctrlf-backward-literal)
;;               ("M-j" . 'ctrlf-yank-word-or-char)))

;; Loaded by counsel
(use-package swiper
  :demand t
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward))

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  )

;; C-o in ivy mode (e.g. in swiper) gives this super helpful hydra
(use-package ivy-hydra
  :after swiper)

(use-package rg
  :chords ("zr" . rg)
  :chords ("ZR" . rg-dwim)
  :config
  (rg-enable-default-bindings)
  (rg-define-toggle "-A 3" "A")
  (rg-define-toggle "-B 3" "B")
  (rg-define-toggle "-C 3" "C"))

;; These are used by projectile:
(use-package ripgrep
  :after projectile)
(use-package ag
  :after projectile)

(provide 'as-search)
