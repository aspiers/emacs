;;; outshine-mode

(require 'as-mode-lighters)

;; N.B. as per https://github.com/alphapapa/outshine/issues/11 if you
;; want to change outline-minor-mode-prefix, it has to be done via
;; customize-variable.
(use-package outshine
  :commands outshine-mode
  :diminish outshine-mode
  :hook (emacs-lisp-mode . outshine-mode)
  :bind (:map outshine-mode-map
              ("M-*" . outshine-navi)))

;;; folding-mode

(use-package folding
  :commands (folding-mode
             folding-mode-find-file
             folding-mode-add-find-file-hook
             folding-set-marks
             folding-add-to-marks-list
             fold-show)
  :defer 20
  :config
  (folding-add-to-marks-list 'latex-mode "%{{{ " "%}}}")
  (folding-add-to-marks-list 'Fundamental-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'shellscript-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'shell-script-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'Shellscript-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'Shell-script-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'makefile-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'makefile-gmake-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'sh-mode "\# {{{ " "\# }}}")
  (folding-add-to-marks-list 'tex-mode "% {{{ " "% }}}")
  (folding-add-to-marks-list 'ml-mode "\(* {{{ " "\(* }}} ")
  (folding-add-to-marks-list 'sawfish-mode ";; {{{ " ";; }}}")
  (folding-add-to-marks-list 'lilypond-mode "% {{{ " "% }}}")
  (folding-add-to-marks-list 'LilyPond-mode "% {{{ " "% }}}")

  (setq folding-default-keys-function 'folding-bind-backward-compatible-keys)

  (add-hook 'folding-mode-hook
            (lambda ()
              ;; Quick navigation
              (local-set-key [(control meta <)] 'folding-shift-out)
              (local-set-key [(control meta >)] 'folding-shift-in)
              (local-set-key [(shift left)] 'folding-shift-out)
              (local-set-key [(shift right)] 'folding-shift-in)
              ))

  (defun as-folding-previous-visible-heading ()
    "Wrapper around `folding-previous-visible-heading' which ensures a
consistent landing spot."
    (interactive)
    (and (folding-previous-visible-heading)
         (folding-mark-look-at 'mark)))

  (defun as-folding-next-visible-heading ()
    "Wrapper around `folding-next-visible-heading' which ensures a
consistent landing spot."
    (interactive)
    (and (folding-next-visible-heading)
         (folding-mark-look-at 'mark)))

  (add-hook 'folding-mode-hook
            (lambda ()
              (local-set-key [(control up  )] 'as-folding-previous-visible-heading)
              (local-set-key [(control down)] 'as-folding-next-visible-heading)

              ;; FIXME: not implemented yet
              ;;             (local-set-key [(control shift up)]   'folding-backward-current-level)
              ;;             (local-set-key [(control shift down)] 'folding-forward-current-level)
              ))

  (defun fm () "Loads folding-mode." (interactive) (folding-mode)))

;;; Use outshine instead of allout

;;; outline-minor-mode

(use-package outline
  :diminish outline-minor-mode
  :hook (outline-minor-mode . turn-on-auto-fill))

;;; Key bindings

;; I do these here rather than in per-mode folds to get consistency
;; across modes.

;; I need the following (in general, the entries within each section
;; are ordered in decreasing frequency of use):

;;;; for changing visibility:

;; These need to be easily repeatable, should be a chord:
;;
;;   C-  left         decrease depth of subheadings in current heading
;;   C-S-left         hide subtree
;;   C-  right        increase depth of subheadings in current heading
;;   C-S-right        show all subheadings
;; M-C-S-right        show whole subtree
;;
;; These don't need to be easily repeatable, so a key sequence is ok.
;;
;;     C-c C-o        show entire tree
;;     C-c C-w        hide entire tree

;;;;; as-folding-{hide,show}-current

;; (eval-when-compile (require 'folding))
(eval-and-compile
  (dolist (fn
           '(folding-mark-look-at
             folding-hide-current-entry
             folding-mark-look-at-top-mark-p
             folding-show-current-entry
             folding-get-mode-marks
             folding-shift-in
             folding-narrow-to-region
             folding-show-all
             folding-point-folded-p
             folding-open-buffer
             folding-use-overlays-p
             ))
    (autoload fn "folding")))

(defun as-folding-hide-current ()
  "Hides the current fold, ensuring a consistent landing spot."
  (interactive)
  (if (stringp (car (folding-get-mode-marks)))
      (if (eq (folding-mark-look-at) 0)
          (message "Fold already closed")
        (folding-hide-current-entry)
        (folding-mark-look-at 'mark))
    (message "`folding-get-mode-marks' didn't return valid top mark; check `folding-top-mark' and `folding-mode-marks-alist'.")))

(defun as-folding-show-current ()
  "Shows the current fold, ensuring a consistent landing spot."
  (interactive)
  (if (stringp (car (folding-get-mode-marks)))
      (if (folding-mark-look-at-top-mark-p)
          (and (folding-show-current-entry)
               ;; ensure consistent landing spot
               (folding-mark-look-at 'mark))
        (message "Not on top fold mark"))
    (message "`folding-get-mode-marks' didn't return valid top mark; check `folding-top-mark' and `folding-mode-marks-alist'.")))

(autoload 'folding-find-folding-mark "folding")
(autoload 'folding-hide-current-subtree "folding")
(autoload 'folding-show-current-subtree "folding")
(defun as-folding-hide-subtree ()
  "Hides the current subtree, ensuring a consistent landing spot."
  (interactive)
  (if (stringp (car (folding-get-mode-marks)))
      (if (eq (folding-mark-look-at) 0)
          (message "Fold already closed")
        (save-excursion
          (goto-char (folding-find-folding-mark))
          (folding-hide-current-subtree))
        (folding-mark-look-at 'mark))
    (message "`folding-get-mode-marks' didn't return valid top mark; check `folding-top-mark' and `folding-mode-marks-alist'.")))

(defun as-folding-show-subtree ()
  "Shows the current subtree, ensuring a consistent landing spot."
  (interactive)
  (if (stringp (car (folding-get-mode-marks)))
      (if (folding-mark-look-at-top-mark-p)
          (and (folding-show-current-subtree)
               ;; ensure consistent landing spot
               (folding-mark-look-at 'mark))
        (message "Not on top fold mark"))
    (message "`folding-get-mode-marks' didn't return valid top mark; check `folding-top-mark' and `folding-mode-marks-alist'.")))

;;;;; as-show-current (outline-mode)

(eval-when (compile) (require 'outline))
(defun as-show-current ()
  "Shows the body and children of the current topic in `outline-mode'."
  (interactive)
  (show-entry)
  (show-children))

;;;;; folding-mode-hook bindings

;;(eval-when (compile) (require 'folding))

(add-hook 'folding-mode-hook
          (lambda ()
            (local-set-key [(control left )] 'as-folding-hide-current)
            (local-set-key [(control right)] 'as-folding-show-current)

            ;; TODO
;;             (local-set-key [(control left)]
;;             (local-set-key [(control right)]
            ;;   increase/decrease depth of subheadings in current heading
            ;; possible useful functions for this:
            ;;   folding-mark-look-at-top-mark-p: numberp folding-mark-look-at
            ;;   folding-mark-look-at-bottom-mark-p: symbolp folding-mark-look-at
            ;;   folding-region-open-close
            ;;   folding-region-has-folding-marks-p
            ;;   folding-show-current-subtree
            ;;   folding-find-folding-mark

            (local-set-key [(control shift left )] 'as-folding-hide-subtree)
            (local-set-key [(control shift right)] 'as-folding-show-subtree)))

;;;;; org-mode-hook bindings

(require 'as-org-mode)

(defun as-local-set-outline-expose-keys ()
  "Bind local outline expose keys the way Adam likes them."
  (local-set-key [(control left )]       'hide-subtree)
  (local-set-key [(control right)]       'as-show-current)
  (local-set-key [(control shift left)]  'hide-sublevels)
  (local-set-key [(control shift right)] 'show-subtree))

(add-hook 'org-mode-hook  'as-local-set-outline-expose-keys)
(add-hook 'muse-mode-hook 'as-local-set-outline-expose-keys)
(add-hook 'outline-mode-hook 'as-local-set-outline-expose-keys)

(defvar muse-mode-map)
(add-hook 'muse-mode-hook
          (lambda ()
            (define-key muse-mode-map "\C-c."        'org-time-stamp)
            (define-key muse-mode-map [(shift up)]   'org-timestamp-up)
            (define-key muse-mode-map [(shift down)] 'org-timestamp-down)
            ))

;;;; for navigation:

;; Need to be easily repeatable, should be a chord:
;;
;;    C-U       up level
;;    C-???     down level
;;    C-S-up    prev heading at current level
;;    C-S-down  next heading at current level
;;    C-up      prev heading
;;    C-down    next heading

(defun as-local-set-outline-nav-keys ()
  "Bind local outline navigation keys the way Adam likes them."
  (local-set-key [(control shift u   )] 'outline-up-heading)
  (local-set-key [(control up        )] 'outline-previous-visible-heading)
  (local-set-key [(control down      )] 'outline-next-visible-heading)
  (local-set-key [(control shift down)] 'outline-forward-same-level)
  (local-set-key [(control shift up  )] 'outline-backward-same-level))
(add-hook 'muse-mode-hook 'as-local-set-outline-nav-keys)
(add-hook 'outline-mode-hook 'as-local-set-outline-nav-keys)

(with-packages (org)
  :bind
  (:map org-mode-map
        ([(control shift u)] . outline-up-heading)
        ;; org-up-heading-safe is not interactive
        ([(control up)] . outline-previous-visible-heading)
        ([(control down)] . outline-next-visible-heading)
        ([(control shift up)] . org-backward-heading-same-level)
        ([(control shift down)] . org-forward-heading-same-level)))

;;;; for editing structure:

;;    (choose to be analogous to navigation except:
;;      - with extra meta, current item moves with point
;;      - with extra meta-shift, current subtree moves with point)
;;                move heading up before previous sibling
;;                move heading down after next sibling
;;                promote heading up a level
;;                demote heading down a level?

;;;; other misc:

;;                  mark subtree
;;

;;;; unassigned

;; in use by org-mode:
;;     S-{up,down}               org-shift{up,down}

;; (global-set-key [(     control        left )] 'as-controlleft)
;; (global-set-key [(     control        right)] 'as-controlright)
;; conflict with global bindings:
;;     C-{up,down}    {forward,backward}-paragraph - scrap for M-curlies
;;     C-{left,right} {forward,backward}-word - scrap for M-{S-,}{b,f}


;; (global-set-key [(     control shift  left )] 'as-controlshiftleft)
;; (global-set-key [(     control shift  right)] 'as-controlshiftright)
;; (global-set-key [(     control shift  up   )] 'as-controlshiftup)
;; (global-set-key [(     control shift  down )] 'as-controlshiftdown)

;; (global-set-key [(meta                left )] 'as-metaleft)
;; (global-set-key [(meta                right)] 'as-metaright)
;; (global-set-key [(meta                up   )] 'as-metaup)
;; (global-set-key [(meta                down )] 'as-metadown)
;; conflict with org-mode bindings:
;;       M-{left,right,up,down}  org-meta{left,down,up,right}

;; (global-set-key [(meta         shift  left )] 'as-metashiftleft)
;; (global-set-key [(meta         shift  right)] 'as-metashiftright)
;; (global-set-key [(meta         shift  up   )] 'as-metashiftup)
;; (global-set-key [(meta         shift  down )] 'as-metashiftdown)
;; conflict with org-mode bindings:
;;     S-M-{left,right,up,down}  org-shiftmeta{left,down,up,right}

;; (global-set-key [(meta control        left )] 'as-metacontrolleft)
;; (global-set-key [(meta control        right)] 'as-metacontrolright)
;; (global-set-key [(meta control        up   )] 'as-metacontrolup)
;; (global-set-key [(meta control        down )] 'as-metacontroldown)

;; (global-set-key [(meta control shift  left )] 'as-metacontrolshiftleft)
;; (global-set-key [(meta control shift  right)] 'as-metacontrolshiftright)
;; (global-set-key [(meta control shift  up   )] 'as-metacontrolshiftup)
;; (global-set-key [(meta control shift  down )] 'as-metacontrolshiftdown)

;;; provide

(provide 'as-outlining)
