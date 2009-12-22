(load-library "as-loaddefs")
(require 'as-progress)

;; For faster compilation
(eval-when-compile (require 'cl))

(as-progress "key bindings...")

;;{{{ Why is this a separate file?

;; The following bindings are grouped by their location in the
;; "Universal Keymap", so that I can coordinate bindings globally and
;; prevent conflicts.  Otherwise I would have preferred to group the
;; code logically, e.g. a section for each mode, and have it all
;; within as-init.el.
;;
;; Update: I think the above approach is flawed, since it's easy to
;; figure out what bindings are available without that grouping,
;; and grouping logically is easier to maintain.  Having said that,
;; a clear strategy on use of keymaps is useful.

;;}}}

;; Ben uses (define-key global-map ...) instead of (global-set-key ...)
;; The latter is a wrapper around the former, so is a bit safer.

;;{{{ Navigation/outlining

;; I do these here rather than in per-mode folds to get consistency
;; across modes.

;; I need the following (in general, the entries within each section
;; are ordered in decreasing frequency of use):

;;{{{ for changing visibility:

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


;;{{{ M-C-{<,>} for foldout zoom/exit subtree in outline major/minor modes

(autoload 'foldout-exit-fold    "foldout")
(autoload 'foldout-zoom-subtree "foldout")
(mapc (lambda (mode)
        (add-hook mode
                  (lambda ()
                    ;; Quick navigation
                    (local-set-key [(meta control <)] 'foldout-exit-fold)
                    (local-set-key [(meta control >)] 'foldout-zoom-subtree)
                    )))
      '(outline-mode-hook outline-minor-mode-hook))

;;}}}

;;{{{ as-folding-{hide,show}-current

(eval-and-compile
  (mapc (lambda (fn) (autoload fn "folding"))
        '(folding-mark-look-at
          folding-hide-current-entry
          folding-mark-look-at-top-mark-p
          folding-show-current-entry
          )))

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

;;}}}
;;{{{ as-allout-{show,hide}-current

(autoload 'allout-current-topic-collapsed-p "allout")
(autoload 'allout-hide-current-subtree "allout")
(autoload 'allout-show-children "allout")
(autoload 'allout-show-current-entry "allout")
(autoload 'allout-up-current-level "allout")

(defun as-allout-show-current ()
  "Shows the body and children of the current topic."
  (interactive)
  (allout-show-current-entry)
  (allout-show-children))

(defun as-allout-hide-current ()
  "Hides the current topic, or the containing topic if the current one
is already hidden."
  (interactive)
  (cond ((allout-current-topic-collapsed-p)
         (allout-up-current-level 1)
         (allout-hide-current-subtree))
        (t (allout-hide-current-subtree))))

(add-hook 'allout-mode-hook
          (lambda ()
            (local-set-key [(control left )] 'as-allout-hide-current)
            (local-set-key [(control right)] 'as-allout-show-current)
            (local-set-key [(control shift left)]  'allout-hide-current-subtree)
            (local-set-key [(control shift right)] 'allout-show-current-subtree)
            ))

;;}}}
;;{{{ as-show-current (outline-mode)

(eval-when (compile) (require 'outline))
(defun as-show-current ()
  "Shows the body and children of the current topic in `outline-mode'."
  (interactive)
  (show-entry)
  (show-children))

;;}}}

;;{{{ folding-mode-hook bindings

(mapc (lambda (fn) (autoload fn "folding"))
      '(folding-narrow-to-region
        folding-next-visible-heading
        folding-open-buffer
        folding-point-folded-p
        folding-previous-visible-heading
        folding-shift-in
        folding-show-all
        folding-use-overlays-p
        ))
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
            
            (local-set-key [(control shift left)] 'as-folding-hide-current)))

;;}}}
;;{{{ org-mode-hook bindings

(require 'org nil 'noerror)

(eval-when-compile
  ;; org-default-extensions defaults to (org-irc) which causes a
  ;; compile to require erc.el which is not in emacs 21.
  (if (or (not (boundp 'org-default-extensions))
          (memq 'org-irc 'org-default-extensions))
      (defvar org-default-extensions '(org-mouse))))

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

;;}}}

;;}}}
;;{{{ for navigation:

;; Need to be easily repeatable, should be a chord:
;;
;;    C-U       up level
;;    C-???     down level
;;    C-S-up    prev heading at current level
;;    C-S-down  next heading at current level
;;    C-up      prev heading
;;    C-down    next heading

(global-set-key [(shift meta f)] 'as-forward-word-start)
(global-set-key [(shift meta b)] 'as-backward-before-word)
(global-set-key [(control meta F)] 'as-forward-sexp-start)
(global-set-key [(control meta B)] 'as-backward-before-sexp)

;; Define fast scroll keys, may be overridden per mode.
(defun as-fast-up   () "Move up two lines."   (interactive) (forward-line -2))
(defun as-fast-down () "Move down two lines." (interactive) (forward-line  2))
(global-set-key [(shift down)] 'as-fast-down)
(global-set-key [(shift up)]   'as-fast-up)

;;{{{ as-folding-{previous,next}-visible-heading

(eval-when-compile (require 'folding))
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

;;}}}

(eval-when-compile (require 'folding))
(add-hook 'folding-mode-hook
          (lambda ()
            (local-set-key [(control up  )] 'as-folding-previous-visible-heading)
            (local-set-key [(control down)] 'as-folding-next-visible-heading)

            ;; FIXME: not implemented yet
;;             (local-set-key [(control shift up)]   'folding-backward-current-level)
;;             (local-set-key [(control shift down)] 'folding-forward-current-level)
            ))

(eval-when-compile (require 'allout))
(add-hook 'allout-mode-hook
          (lambda ()
            (local-set-key [(control U         )] 'allout-up-current-level)
            (local-set-key [(control up        )] 'allout-previous-visible-heading)
            (local-set-key [(control down      )] 'allout-next-visible-heading)
            (local-set-key [(control shift up  )] 'allout-backward-current-level)
            (local-set-key [(control shift down)] 'allout-forward-current-level)))

(defun as-local-set-outline-nav-keys ()
  "Bind local outline navigation keys the way Adam likes them."
  (local-set-key [(control U         )] 'outline-up-heading)
  (local-set-key [(control up        )] 'outline-previous-visible-heading)
  (local-set-key [(control down      )] 'outline-next-visible-heading)
  (local-set-key [(control shift down)] 'outline-forward-same-level)
  (local-set-key [(control shift up  )] 'outline-backward-same-level))
(add-hook 'muse-mode-hook 'as-local-set-outline-nav-keys)
(add-hook 'outline-mode-hook 'as-local-set-outline-nav-keys)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key [(control U         )] 'outline-up-heading)
                                                  ;; org-up-heading-safe is not interactive
            (local-set-key [(control up        )] 'outline-previous-visible-heading)
            (local-set-key [(control down      )] 'outline-next-visible-heading)
            (local-set-key [(control shift up  )] 'org-backward-same-level)
            (local-set-key [(control shift down)] 'org-forward-same-level)))

;;}}}
;;{{{ for editing structure:

;;    (choose to be analogous to navigation except:
;;      - with extra meta, current item moves with point
;;      - with extra meta-shift, current subtree moves with point)
;;                move heading up before previous sibling
;;                move heading down after next sibling
;;                promote heading up a level
;;                demote heading down a level?

;;}}}
;;{{{ other misc:

;;                  mark subtree
;;

;;}}}
;;{{{ unassigned

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

;;}}}

;;}}}
;;{{{ Rebinding for improvement - naughty but nice

;; Non-mode-specific rebindings here.  Mode-specific rebindings should
;; go in the mode-specific section of as-init.el.

;;{{{ _O_rganisation/productivity (M-o)

(global-set-key "\C-co" 'overwrite-mode)
(global-unset-key [(meta o)])
;(global-unset-key "\eo")
(global-set-key "\eoa" 'org-agenda)
(global-set-key "\eA"  'as-org-switch-to-agenda-buffer) ;; X11 only
(global-set-key "\eob" 'as-org-switch-to-agenda-buffer)
(global-set-key "\eoq" 'org-remember)
(global-set-key "\eo\eo" 'as-org-jump-clock-or-agenda)

(defun org-show-effort ()
  "Shows the effort of the entry at the current point."
  (interactive)
  (let ((effort (org-entry-get (point) org-effort-property)))
    (message (if effort (format "Effort is %s" effort)
               "No effort defined"))))

(add-hook
 'org-mode-hook
 (lambda ()
   ;; Zero effort is last (10th) element of global Effort_ALL property
   ;; so that we get zero effort when pressing '0' in the Effort column
   ;; in Column view, since this invokes `org-set-effort' with arg 0,
   ;; which stands for the 10th allowed value.
   (let ((effort-values
          (car
           (read-from-string
            (concat "("
                    (cdr (assoc "Effort_ALL" org-global-properties))
                    ")")))))
     (dotimes (effort-index 10)
       (let* ((effort (nth effort-index effort-values))
              (key-suffix (number-to-string
                           (if (= effort-index 9) 0 (1+ effort-index))))
              (fn-name (concat "org-set-effort-"
                               (number-to-string effort-index)))
              (fn (intern fn-name)))
         ;; (message "Binding M-o %s to %s which sets effort to %s"
         ;;          key-suffix fn-name effort)
         (fset fn `(lambda ()
                     ,(format "Sets effort to %s." effort)
                     (interactive)
                     (org-set-effort ,(1+ effort-index))))
         (local-set-key (concat "\eo" key-suffix) fn)
         (local-set-key "\eo\eo" 'org-show-effort))))))

(defun org-unset-effort ()
  "Unsets the Effort property for the current headline."
  (interactive)
  (org-delete-property org-effort-property))
(global-set-key "\eo " 'org-unset-effort)

;;}}}

(global-set-key [(meta "\\")]   'fixup-whitespace)
                                ;; was delete-horizontal-space
(global-set-key [(meta g)]      'goto-line)          ;; was set-face
(global-set-key [(control h) a] 'apropos)            ;; was apropos-command
(autoload 'as-transpose-lines
          "as-editing" "as-transpose-lines" t)
(global-set-key [(control x)(control t)] 'as-transpose-lines) ;; was transpose-lines

(autoload 'bn-kill-region-or-backword-word
          "as-editing" "bn-kill-region-or-backword-word" t)
(global-set-key [(control w)]  'bn-kill-region-or-backword-word)
                               ;; was kill-region
(autoload 'bn-kill-line-or-region-save
          "as-editing" "bn-kill-line-or-region-save" t)
(global-set-key [(meta w)]     'bn-kill-line-or-region-save)
                               ;; kill-ring-save

(global-set-key [(delete)]     'delete-char)        ;; to make sure
(global-set-key [(insert)]     'overwrite-mode)     ;; to make sure

;; emacs < 22 doesn't have x-clipboard-yank
(if (boundp 'x-clipboard-yank)
    (global-set-key [(shift insert)] 'x-clipboard-yank)
  (global-set-key [(shift insert)] 'clipboard-yank))

(global-set-key [(meta i)]     'indent-relative)    ;; was tab-to-tab-stop


;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.
(global-set-key [(control x)(control b)] 'buffer-menu)

;; But if bs-show is available, choose that cos it's much nicer.
(if (functionp 'bs-show)
     (global-set-key [(control x)(control b)]
                     (lambda (arg)
                       (interactive "P")
                       (bs-show arg)
                       (let ((inhibit-read-only t))
                         (save-excursion
                           (goto-char (point-max))
                           (insert "\n"))))))

(global-set-key [(meta tab)] 'hippie-expand) ;; was complete-symbol etc.
                                             ;; depending on mode

;; why is this necessary?
;;(global-set-key "\e\C-i"     'hippie-expand)

;;}}}
;;{{{ Additions (hope for no conflicts)

(global-set-key [(control N)] 'next-logical-line)
(global-set-key [(control P)] 'previous-logical-line)

(global-set-key [(meta E)]                'mark-end-of-sentence)

(autoload 'as-kill-word "as-editing" "as-kill-word" t)
(global-set-key [(meta D)]                'as-kill-word)

(autoload 'bn-zap-nearly-to-char "as-editing" "bn-zap-nearly-to-char" t)
(global-set-key [(meta Z)]                'bn-zap-nearly-to-char)

(global-set-key [(control meta K)]        'as-kill-sexp)

(global-set-key [(control x) K]           'as-destroy-buffer)
(global-set-key [(control x) (I)]         'insert-buffer)
(global-set-key [(control x) (control y)] 'vim-yy)
(global-set-key [(control x) (meta f)]
                (if (functionp 'find-library)
                    'find-library
                  'find-library-backport))
(autoload 'find-file-at-point "ffap" nil t)
(autoload 'ffap-other-window  "ffap" nil t)
(autoload 'ffap-other-frame   "ffap" nil t)
(global-set-key [(control meta ?')]       'find-file-at-point)
(global-set-key [(control x) ?4 ?']       'ffap-other-window)
(global-set-key [(control x) ?5 ?']       'ffap-other-frame)

(autoload 'bn-end-of-line-but-one "as-editing" "bn-end-of-line-but-one" t)
(global-set-key [(control E)]             'bn-end-of-line-but-one)
(global-set-key [(control ?')]            'speedbar-get-focus)
(global-set-key [(control !)]             'ido-switch-buffer)
(global-set-key [(control ,)]             'delete-other-windows)
(global-set-key [(control .)]             'delete-window)
(global-set-key [(control \;)]            'bury-buffer)
(global-set-key [(control tab)]           'other-window)
(global-set-key [(control tab)]           'other-window)

(global-set-key [(control meta y)]        'as-join-line-with-next)
(global-set-key [(control meta return)]   'repeat-complex-command)
(global-set-key [(control c) .]           'repeat)
(autoload 'bn-strip-parentheses "as-editing" "bn-strip-parentheses" t)
(global-set-key [(control meta \()]       'bn-strip-parentheses)
(global-set-key [(control c) tab]         'indent-region)


(global-set-key [(control meta ??)]       'bn-make-region-into-secondary)
(global-set-key [(control meta T)]        'bn-exchange-region-and-secondary)
(global-set-key [(control g)]             'bn-keyboard-quit)

(global-set-key [(control $)]             'ispell-complete-word)
(global-set-key [(control meta $)]        'ispell-buffer)

(global-set-key [(control c)(control x)(control j)] 'org-clock-goto)

;;}}}
;;{{{ FSF-compliant user bindings

;;{{{ C-c [a-z][A-z]

(fset 'as-find-personal-todo "\C-x\C-f~/roaming/TODO.org")
(global-set-key "\C-cjt"  'as-find-personal-todo)
(fset 'as-find-personal-diary "\C-x\C-f~/roaming/diary.org")
(global-set-key "\C-cjd"  'as-find-personal-diary)
;;(fset 'as-find-personal-note "\C-x\C-f~/roaming/notes/")

(eval-when-compile (require 'ido))
(defun as-find-personal-note ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/roaming/notes/" "Find note: "))
(global-set-key "\C-cjn"  'as-find-personal-note)

(fset 'as-find-work-todo "\C-x\C-f~/ifolder/TODO.org")
(global-set-key "\C-cjT"  'as-find-work-todo)

(defun as-find-from-home ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/" "Find file: "))
(global-set-key [(control ~)] 'as-find-from-home)
(global-set-key "\C-cjh"      'as-find-from-home)


(global-set-key "\C-cA"   'as-align-to-previous-line)
(global-set-key "\C-cb"   'bury-buffer)
(global-set-key "\C-cB"   'as-bounce-buffer)
;;{{{ PCL-_C_VS (C-c c)

(fset 'as-next-cvs-buffer "\C-xb*cvs*")
(global-set-key "\C-ccb"   'as-next-cvs-buffer)
(global-set-key "\C-cce"  'cvs-examine)
(global-set-key "\C-ccq"  'cvs-quickdir)
(global-set-key "\C-ccs"  'cvs-status)
(global-set-key "\C-ccu"  'cvs-update)

;;}}}
(global-set-key "\C-cd"   'as-duplicate-line)
(global-set-key "\C-cF"   'font-lock-fontify-buffer)
;;{{{ _I_nsert auto-text (C-c i)

(global-set-key "\C-cid" 'as-insert-date-and-time)
(global-set-key "\C-ciD" 'as-insert-date-interactive)
(global-set-key "\C-cie" 'as-insert-email-address)
(global-set-key "\C-cim" 'as-insert-local-mode)
(global-set-key "\C-ciw" 'as-insert-work-email-address)
(global-set-key "\C-ciW" 'as-insert-name-and-work-email)
(global-set-key "\C-cij" 'as-insert-japh-method-chain-sig)
(global-set-key "\C-ciJ" 'as-insert-japh-indirect-sig)
(global-set-key "\C-cil" 'as-insert-log-timestamp)
(global-set-key "\C-ciL" 'as-insert-log-datestamp)
(global-set-key "\C-ciN" 'as-insert-name-and-email)
(global-set-key "\C-cin" 'as-insert-name)
(global-set-key "\C-cis" 'as-insert-scissors)
(global-set-key "\C-ciS" 'as-snip-region)
(global-set-key "\C-cit" 'as-insert-time)

;;}}}
(global-set-key "\C-ck"   'delete-file)
(global-set-key "\C-cK"   'as-destroy-buffer-delete-file)
(global-set-key "\C-cl"   'align)                      ;; new in emacs 21
(autoload 'org-store-link "org" "org-store-link" t)
(global-set-key "\C-cL"   'org-store-link)
;; I reserve C-c m for mode-specific user bindings

(autoload 'as-mairix-yank-links "as-gtd" "as-mairix-yank-links" t)
(autoload 'as-mairix-view-link-at-point "as-gtd" "as-mairix-view-link-at-point" t)
(global-set-key [(control c) (M) (y)]         'as-mairix-yank-links)
(global-set-key [(control c) (M) (control y)] 'as-mairix-yank-links)
(global-set-key [(control c) (M) (return)]    'as-mairix-view-link-at-point)

(global-set-key "\C-cn"   'as-display-buffer-filename)
(global-set-key "\C-cp"   'as-copy-previous-line-suffix)
(global-set-key "\C-cP"   'as-align-to-previous-line)
;;{{{ remember (C-c q for _q_uick)

(autoload 'org-remember "org-remember" nil t)
(global-set-key "\C-cq" 'org-remember)

;;}}}
(global-set-key "\C-cr"   'revert-buffer)
(global-set-key "\C-cR"   'as-rename-current-buffer-file)
;;{{{ _T_oggles and settings (C-c t)

;;{{{ as-toggle-indent-tabs-mode

(defun as-toggle-indent-tabs-mode ()
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

;;}}}
(global-set-key "\C-ctb"   'as-toggle-indent-tabs-mode)

;;{{{ as-toggle-case-fold-search

(defun as-toggle-case-fold-search ()
  "Toggles the value of case-fold-search in the current buffer"
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "case-fold-search set to %s" case-fold-search))

;;}}}
(global-set-key "\C-ctc"   'as-toggle-case-fold-search)

;;{{{ as-toggle-debug-on-error

(defun as-toggle-debug-on-error ()
  "Toggles the value of debug-on-error in the current buffer"
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

;;}}}
(global-set-key "\C-cte"   'as-toggle-debug-on-error)

(global-set-key "\C-ctf"   'auto-fill-mode)

;;{{{ as-toggle-truncate-lines

(defun as-toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display)
  (message "truncate-lines set to %s" truncate-lines))

;;}}}
(global-set-key "\C-cts"   'as-toggle-truncate-lines) ;; mnemonic: less -S

;;{{{ as-set-tab-width

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

;;}}}
(global-set-key "\C-ctw"   'as-set-tab-width)

;;}}}

(autoload 'set-any-variable "set-any-var" "set-any-variable" t)
(global-set-key "\C-cv"   'set-any-variable)
(global-set-key "\C-c+"   'make-directory)
(autoload 'org-occur-in-agenda-files "org" nil t)
(global-set-key [(control c) (control \?)] 'org-occur-in-agenda-files)

;;}}}
;;{{{ Function keys f5--f9 (no modifiers)

(global-set-key [(f5)] 'as-duplicate-line)
(global-set-key [(f6)] 'as-bounce-buffer)
(global-set-key [(f7)] 'as-align-to-previous-line)
(global-set-key [(f8)] 'as-copy-previous-line-suffix)

;;}}}

;;}}}
;;{{{ Mouse

(and window-system (not (boundp 'running-xemacs))
     (global-set-key [(M-mouse-4)] 'raise-frame)
     (global-set-key [(M-mouse-5)] 'lower-frame))

;;}}}

(as-progress "key bindings...done")

(provide 'as-bindings)

