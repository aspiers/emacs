(use-package selectrum
  :config
  (selectrum-mode +1)

  :bind (("C-c C-r" . selectrum-repeat)
         ;; :map selectrum-minibuffer-map
         ;; ("TAB" . selectrum-partial)
         ))

;; WIP - see https://github.com/raxod502/selectrum/issues/272
(defun selectrum-partial ()
    "Complete the minibuffer text as much as possible."
    (interactive)
    (setq as-selectrum--refined-candidates selectrum--refined-candidates)
    (let* ((matchstr (if minibuffer-completing-file-name
                         (or (file-name-nondirectory
                              (minibuffer-contents)) "")
                       (minibuffer-contents)))
           (parts (or (split-string matchstr " " t) (list "")))
           (tail (last parts))
           (postfix (car tail))
           (new (try-completion postfix
                                selectrum--refined-candidates)))
      (cond ((or (eq new t) (null new)) nil)
            ((string= new matchstr) nil)
            ((string= (car tail) (car (split-string new " " t))) nil)
            (new
             (delete-region (save-excursion
                              (search-backward matchstr nil t)
                              (point))
                            (point-max))
             (setcar tail new)
             (insert (mapconcat #'identity parts " "))
             t))))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (use-package flx)
;; (use-package flx-ido)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package ivy
  :after prescient

  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; https://github.com/abo-abo/swiper/issues/2620#issuecomment-645665878
  ;; suggests to use M-: (ivy-state-caller ivy-last) to figure out the keys
  ;; in this alist.
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (counsel-git-grep . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  (defun ivy-show-context ()
    "Output a messaging with some helpful information about how ivy
has built the current list of completion matches."
    (interactive)
    (let ((caller (ivy-state-caller ivy-last)))
      (message "\nivy :caller %s, :matcher %s\n:sort (or %s %s), :re-builder %s\n:collection %s"
               caller
               (ivy-state-matcher ivy-last)
               (ivy-state-sort ivy-last)
               (assoc caller ivy-sort-functions-alist)
               (ivy-state-re-builder ivy-last)
               (ivy-state-collection ivy-last))))

  ;; See https://github.com/abo-abo/swiper/issues/2628
  ;; ("Fallback in ivy-sort-functions-alist is ignored")
  (add-to-list 'ivy-sort-matches-functions-alist
               '(set-any-variable . ivy--prefix-sort)
               'append)
  (let ((existing (assoc 'ivy-switch-buffer
                         ivy-sort-matches-functions-alist)))
    (when existing
      (setq ivy-sort-matches-functions-alist
            (delete existing ivy-sort-matches-functions-alist))))

  (ivy--alist-set 'ivy-initial-inputs-alist 'org-refile "")
  (ivy--alist-set 'ivy-initial-inputs-alist 'org-agenda-refile "")
  (ivy--alist-set 'ivy-initial-inputs-alist 'org-capture-refile "")

  :bind
  (:map ivy-mode-map
        ("C-c M-c" . ivy-show-context))

  ;; https://github.com/abo-abo/swiper/wiki/FAQ#how-to-prevent-tab-from-selecting-the-only-candidate
  (:map ivy-minibuffer-map
        ("TAB" . ivy-partial)))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

;; C-o in ivy minibuffer is awesome!!
(use-package ivy-hydra)

(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(key-chord-define-global "xd" (lambda () (interactive) (dired ".")))

(use-package consult
  ;; Replace bindings. Lazily loaded due to use-package.
  :chords ((",." . consult-buffer)
           (",;" . consult-buffer-other-window)
           ("xf" . find-file))
  :bind (("C-c h" . consult-history)
         ("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ;; ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ;; ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ;; ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ;; ("M-g i" . consult-imenu)   ;; "M-s i" is a good alternative
         ;; ("M-g e" . consult-error)   ;; "M-s e" is a good alternative
         ("M-s m" . consult-multi-occur)

         ;; browse-kill-ring is an alternative option
         ("M-Y" . consult-yank-pop)

         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

;; Enable Consult-Selectrum integration.
;; This should be installed if Selectrum is used.
(use-package consult-selectrum
  :demand t)

;; Install the consult-flycheck command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))

;; Optionally enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package counsel
  :defer 2
  :config
  (require 'ivy)

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  ;; :chords ((",." . counsel-switch-buffer)
  ;;          (",;" . counsel-switch-buffer-other-window)
  ;;          ("xf" . counsel-find-file))
  :bind (
         ;; ("C-x 4 b" . counsel-switch-buffer-other-window)
         ;;("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("C-1" . counsel-ibuffer)
         ;; ("C-h a" . counsel-apropos)
         ("C-h b" . counsel-descbinds)
         ;; ("C-h f" . counsel-describe-function)
         ;; ("C-h v" . counsel-describe-variable)
         ;; ("C-h o" . counsel-describe-symbol)
         ;; ("C-x M-f" . counsel-find-library)
         ;; ("C-h S" . counsel-info-lookup-symbol)
         ;; ("<f2> u" . counsel-unicode-char)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ;; ("C-c k" . counsel-ag)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-S-o" . counsel-rhythmbox)
         ))

(provide 'as-selection)
