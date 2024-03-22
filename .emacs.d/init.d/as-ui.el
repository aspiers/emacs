;; see also as-smart-mode-line.el, as-selection.el

;; Native replacement for fci-mode from emacs 27.1
(use-feature display-fill-column-indicator
  :config
  (set-face-attribute 'fill-column-indicator nil :weight 'thin)
  (dolist (hook '(prog-mode-hook indented-text-mode))
    (add-hook hook (lambda () (display-fill-column-indicator-mode +1)))))

(use-package beacon
  :defer 5
  :config (beacon-mode 1))

(use-package flycheck :diminish)
(use-package hideshow-org)

(use-package insert-char-preview
    :commands insert-char-preview
    :bind ("C-x 8 RET" . insert-char-preview))

;; Nice top line of buffer showing context
(use-package topsy
  ;; emacs-lsp is better, so use that for now for other modes.
  :hook (emacs-lisp-mode . topsy-mode))

(use-package prism
  :straight (:host github :repo "alphapapa/prism.el")
  ;; :hook ((emacs-lisp-mode . prism-mode)
  ;;        (python-mode . prism-mode))
  )

;; (use-package transient-posframe
;;   :straight (:host github :repo "yanghaoxie/transient-posframe")
;;   :config
;;   (transient-posframe-mode))

;; Shamelessly copied from
;; https://github.com/KaratasFurkan/.emacs.d/tree/emacs-29#pixel-scroll

;; scroll less than default
(defvar fk/default-scroll-lines 15)

(use-feature pixel-scroll
  :if (fboundp 'pixel-scroll-precision-mode)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-interpolation-factor 1.0)
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  ;; (([remap scroll-up-command]   . fk/pixel-scroll-up-command)
  ;;  ([remap scroll-down-command] . fk/pixel-scroll-down-command)
  ;;  ([remap recenter-top-bottom] . fk/pixel-recenter-top-bottom))
  :init
  (pixel-scroll-precision-mode 1)
  :hook
  (dashboard-after-initialize . pixel-scroll-precision-mode)
  :config
  (defun fk/pixel-scroll-up-command ()
    "Similar to `scroll-up-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (- (* fk/default-scroll-lines (line-pixel-height)))))

  (defun fk/pixel-scroll-down-command ()
    "Similar to `scroll-down-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (* fk/default-scroll-lines (line-pixel-height))))

  (defun fk/pixel-recenter-top-bottom ()
    "Similar to `recenter-top-bottom' but with pixel scrolling."
    (interactive)
    (let* ((current-row (cdr (nth 6 (posn-at-point))))
           (target-row (save-window-excursion
                         (recenter-top-bottom)
                         (cdr (nth 6 (posn-at-point)))))
           (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
      (pixel-scroll-precision-interpolate distance-in-pixels))))

(provide 'as-ui)
