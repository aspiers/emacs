;; see also as-smart-mode-line.el

;;{{{ fill-column-indicator

(use-package fill-column-indicator
  :straight (fill-column-indicator
             :fork (:repo "aspiers/Fill-Column-Indicator"
                          :branch "wrap-window-edge"))
  :config
  (dolist (hook '(prog-mode-hook indented-text-mode))
    (add-hook hook 'fci-mode)))

;;}}}
;;{{{ no toolbar

;; This is best done with X resources, otherwise you get funny
;; frame-resizing problems.  See ~/.Xresources/emacs.rdb.

;; (and window-system
;;      (>= emacs-major-version 21)
;;      (tool-bar-mode -1))

;; (setq default-frame-alist
;;       '((tool-bar-lines . 0)))

;;}}}
;;{{{ ido - superior replacement for iswitchb

(req-package ido
  :commands (ido-mode)
  :config
  (ido-mode t))

;;}}}
;;{{{ parentheses

(require 'paren)

(req-package smartparens)

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;}}}

(use-package flx)
;; (use-package flx-ido)

(use-package ivy
  :config
  (require 'flx)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; https://github.com/abo-abo/swiper/issues/2620#issuecomment-645665878
  ;; suggests to use M-: (ivy-state-caller ivy-last) to figure out the keys
  ;; in this alist.
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-git-grep . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (add-to-list 'ivy-sort-matches-functions-alist
               '(set-any-variable . ivy--prefix-sort)
               'append)
  (ivy--alist-set 'ivy-initial-inputs-alist 'org-refile "")
  (ivy--alist-set 'ivy-initial-inputs-alist 'org-agenda-refile "")
  (ivy--alist-set 'ivy-initial-inputs-alist 'org-capture-refile ""))

;; C-o in ivy minibuffer is awesome!!
(use-package ivy-hydra)

(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :defer 2
  :config
  (require 'ivy)

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  :chords ((",." . counsel-switch-buffer)
           (",;" . counsel-switch-buffer-other-window)
           ("xf" . counsel-find-file))
  :bind (("C-x 4 b" . counsel-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-1" . counsel-ibuffer)
         ("C-h a" . counsel-apropos)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h o" . counsel-describe-symbol)
         ("C-x M-f" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ;; ("<f2> u" . counsel-unicode-char)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ;; ("C-c k" . counsel-ag)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-S-o" . counsel-rhythmbox)
         ))

(bind-key "C-'" 'speedbar-get-focus)

(use-package beacon
  :defer 5
  :config (beacon-mode 1))

(req-package flycheck)
(req-package hideshow-org)

(req-package ido-completing-read+)
(req-package idomenu) ;; http://emacsrocks.com/e10.html
;; ido-hacks
;; ido-everywhere
;; ido-ubiquitous now obsolete
(req-package ido-vertical-mode)

;; Not impressed with this.  No native projectile integration, and can't
;; only display open files.
;;
;; (use-package neotree
;;   :bind ("C-'" . neotree-toggle)
;;   :after (all-the-icons projectile)
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-smart-open t)
;;   ;; But I want this to be 'projectile-vc!
;;   (setq projectile-switch-project-action 'neotree-projectile-action))

(provide 'as-ui)
