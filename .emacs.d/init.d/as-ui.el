;; see also as-smart-mode-line.el

;;{{{ fill-column-indicator

(req-package fill-column-indicator
  :quelpa (fill-column-indicator
           :fetcher github
           :repo "aspiers/Fill-Column-Indicator"
           :branch "wrap-window-edge")
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

;;}}}

(use-package flx)

(use-package ivy
  :after flx
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-git-grep-function . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

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
  :after ivy
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  :chords ((",." . counsel-switch-buffer)
           (",;" . counsel-switch-buffer-other-window))
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

(req-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(bind-key "C-'" 'speedbar-get-focus)

(req-package beacon
  :config (beacon-mode 1))

(req-package flycheck)
(req-package flycheck-package)
(req-package flymake-css)
(req-package flymake-ruby)
(req-package flymake-sass)
(req-package flymake-shell)

(req-package hideshow-org)

(req-package ido-completing-read+)
(req-package idomenu) ;; http://emacsrocks.com/e10.html
;; ido-hacks
;; ido-everywhere
;; ido-ubiquitous now obsolete
(req-package ido-vertical-mode)


(provide 'as-ui)
