;; See also as-point-motion.el and as-ui.el

;; Scrolling
(defvar smooth-scrolling-repo-dir
  (concat edotdir "/.GIT/adamspiers.org/smooth-scrolling")
  "Directory where smooth-scrolling repository is checked out.")

;; (if (file-directory-p smooth-scrolling-repo-dir)
;;     (use-package smooth-scrolling
;;       :defer 3
;;       :load-path smooth-scrolling-repo-dir)
;;   (warn "Use mrco smooth-scrolling to check out the repository."))

(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")

  ;; :config
  ;; (good-scroll-mode 1)
  ;;
  ;; :bind (([next] . good-scroll-up-full-screen)
  ;;        ([prior] . good-scroll-down-full-screen))
  )

(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 2)

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

(provide 'as-scrolling)
