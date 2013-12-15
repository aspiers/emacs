(load-library "as-loaddefs")
(require 'as-progress)
;; For faster compilation
(eval-when-compile (require 'cl))

(eval-and-compile (as-loading-started))

;;{{{ Why is this a separate file?

;; The following bindings are grouped by their location in the
;; "Universal Keymap", so that I can coordinate bindings globally and
;; prevent conflicts.  Otherwise I would have preferred to group the
;; code logically, e.g. a section for each mode, and have it all
;; within the relevant files in ~/.emacs.d/init.d.
;;
;; Update: I think the above approach is flawed, since it's easy to
;; figure out what bindings are available without that grouping (using
;; <prefix> C-h), and grouping logically is easier to maintain.
;; Having said that, a clear strategy on use of keymaps is useful.

;;}}}

;; Ben uses (define-key global-map ...) instead of (global-set-key ...)
;; The latter is a wrapper around the former, so is a bit safer.

;;{{{ Rebinding for improvement - naughty but nice

;; Non-mode-specific rebindings here.  Mode-specific rebindings should
;; go within the relevant files in ~/.emacs.d/init.d.


(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(global-set-key [(control ?0)] 'ace-jump-mode)
(autoload 'idomenu "idomenu" nil t)
(global-set-key [(control ?1)] 'idomenu)

(global-set-key [(meta x)]   'smex)
(global-set-key [(meta X)]   'smex-major-mode-commands)

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

;; I don't visit the FAQ very often; find-function way more useful.
(global-set-key [(control h) (control f)] 'find-function)
(global-set-key [(control h) (control F)] 'view-emacs-FAQ)

;;}}}
;;{{{ Additions (hope for no conflicts)

(autoload 'key-chord-mode "key-chord")
(autoload 'key-chord-define-global "key-chord")
(key-chord-mode 1)
(key-chord-define-global "zf" 'iy-go-to-char)
(defvar projectile-prefix-map
  (lookup-key projectile-mode-map projectile-keymap-prefix)
  "The keymap which Projectile typically binds to C-c p.")
(global-set-key "\C-xp" projectile-prefix-map)
(key-chord-define-global "zp" projectile-prefix-map)
(key-chord-define-global "zm" 'projectile-commander)
(key-chord-define-global "pf" 'projectile-find-file)
(key-chord-define-global "pb" 'projectile-switch-to-buffer)

;; Not needed since guide-key/recursive-key-sequence-flag is set:
;; (setq guide-key/guide-key-sequence
;;       '("<key-chord> z p" "<key-chord> p z"))

(global-set-key [(control x) ?8 ?e]
                (lambda ()
                  (interactive)
                  (ucs-insert (cdr (assoc-string "EURO SIGN" (ucs-names))))))

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
(global-set-key [(control x) (meta f)]    'find-library)

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
(global-set-key [(control meta !)]        'idomenu)
(global-set-key [(control \,)]            'delete-other-windows)
(global-set-key [(control .)]             'delete-window)
(global-set-key [(control \;)]            'bury-buffer)
(global-set-key [(control =)]             'switch-to-prev-buffer)
(global-set-key [(control +)]             'switch-to-next-buffer)
(global-set-key [(control tab)]           'other-window)
(defun previous-window-interactive ()
  "Interactive wrapper around (other-window -1) for key binding purposes."
  (interactive)
  (other-window -1))
(global-set-key [(control shift tab)]     'previous-window-interactive)
(global-set-key [C-S-iso-lefttab]         'previous-window-interactive)

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

;; This one might get overridden by per-mode bindings:
(global-set-key [(control meta q)]        'fill-common-prefix-region)
;; but this one won't, so serves as a backup:
(global-set-key [(control c)(meta q)]     'fill-common-prefix-region)

(global-set-key [(control h) (control k)] 'find-function-on-key)

;;}}}
;;{{{ FSF-compliant user bindings

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

;;{{{ C-c [a-z][A-Z]

(global-set-key "\C-cA"   'as-align-to-previous-line)
(global-set-key "\C-cb"   'bury-buffer)
(global-set-key "\C-cB"   'as-bounce-buffer)
(global-set-key "\C-cc"   'org-capture)
(global-set-key "\C-cd"   'as-duplicate-line)
(global-set-key "\C-cF"   'font-lock-fontify-buffer)
;;{{{ C-c g for git operations

(global-set-key "\C-cgb"  'magit-run-git-gui-blame)
(global-set-key "\C-cgg"  'magit-run-git-gui)
(global-set-key "\C-cgk"  'magit-run-gitk)
(global-set-key "\C-cgs"  'magit-status)
(global-set-key [(control shift g)] 'magit-status)

(autoload 'ido-buffer-internal "ido")
(defvar ido-default-buffer-method)
(defun ido-switch-magit-buffer ()
  "Switch to a magit status buffer via `ido'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method
                       nil "magit status: " nil "*magit: "))
(global-set-key [(control meta g)] 'ido-switch-magit-buffer)

;;}}}
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
(global-set-key "\C-cir" 'as-insert-rpm-changelog-datestamp)
(global-set-key "\C-cis" 'as-insert-scissors)
(global-set-key "\C-ciS" 'as-snip-region)
(global-set-key "\C-cit" 'as-insert-time)

;;}}}
;;{{{ C-c j for quick jumping

(fset 'as-find-personal-todo "\C-x\C-f~/org/TODO.org")
(global-set-key "\C-cjt" 'as-find-personal-todo)
(global-set-key [(control \")] 'as-find-personal-todo)
(fset 'as-find-personal-diary "\C-x\C-f~/org/diary.org")
(global-set-key "\C-cjd" 'as-find-personal-diary)
;;(fset 'as-find-personal-note "\C-x\C-f~/org/notes/")

(eval-when-compile (require 'ido))

(autoload 'bundle-open "bundler" nil t)
(global-set-key "\C-cjb" 'bundle-open)

(defun as-find-CVS-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.CVS/" "Find CVS repo: "))
(global-set-key "\C-cjc"  'as-find-CVS-repo)

(defun as-find-my-mrconfig ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.config/mr/" "Find mr config: "))
(global-set-key "\C-cjm"  'as-find-my-mrconfig)

(defun as-find-my-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: "))
(global-set-key "\C-cjg"  'as-find-my-git-repo)

(defun as-find-upstream-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/3rd-party/" "Find 3rd-party git repo: "))
(global-set-key "\C-cj3"  'as-find-upstream-git-repo)
(global-set-key "\C-cjG"  'as-find-upstream-git-repo)

(defun as-find-stow-package ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.STOW/" "Find stow package: "))
(global-set-key "\C-cjs"  'as-find-stow-package)

(defun as-find-personal-note ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/org/notes/" "Find note: "))
(global-set-key "\C-cjn"  'as-find-personal-note)

(fset 'as-find-work-todo "\C-x\C-f~/SUSE/TODO.org")
(global-set-key "\C-cjT"  'as-find-work-todo)
(global-set-key [(control \%)] 'as-find-work-todo)

(defun as-find-from-home ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/" "Find file: "))
(global-set-key [(control ~)] 'as-find-from-home)
(global-set-key "\C-cjh"      'as-find-from-home)

;;}}}
(global-set-key "\C-ck"   'delete-file)
(global-set-key "\C-cK"   'as-destroy-buffer-delete-file)
(global-set-key "\C-cl"   'align)                      ;; new in emacs 21
(autoload 'org-store-link "org" "org-store-link" t)
(global-set-key "\C-cL"   'org-store-link)
;; I reserve C-c m for mode-specific user bindings
;;{{{ C-c M for mairix

(autoload 'as-mairix-yank-links "as-gtd" "as-mairix-yank-links" t)
(autoload 'as-mairix-view-link-at-point "as-gtd" "as-mairix-view-link-at-point" t)
(global-set-key [(control c) (M) (y)]         'as-mairix-yank-links)
(global-set-key [(control c) (M) (control y)] 'as-mairix-yank-links)
(global-set-key [(control c) (M) (return)]    'as-mairix-view-link-at-point)

;;}}}
(global-set-key "\C-cn"   'as-display-buffer-filename)
(global-set-key "\C-cp"   'as-copy-previous-line-suffix)
(global-set-key "\C-cP"   'as-align-to-previous-line)
;;{{{ org-capture (C-c q for _q_uick)

;; Try to use C-c c but keeping this for backwards compatability with
;; my brain.
(global-set-key "\C-cq" 'org-capture)

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

(global-set-key "\C-cwl"  'org2blog/wp-login)
(global-set-key "\C-cwn"  'org2blog/wp-new-entry)

(autoload 'set-any-variable "set-any-var" "set-any-variable" t)
(global-set-key "\C-cv"   'set-any-variable)
(global-set-key "\C-cV"   'customize-variable)
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

(and window-system
     (global-set-key [(M-mouse-4)] 'raise-frame)
     (global-set-key [(M-mouse-5)] 'lower-frame))

;;}}}

(as-progress "key bindings...done")

(provide 'as-bindings)

