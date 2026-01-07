;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "M-L")

  :config
  ;; Disable binding which clashes with my multiple-cursors binding
  (define-key lsp-mode-map (kbd "C-S-SPC") nil)

  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (gc-cons-threshold 100000000)

  :config
  (setq read-process-output-max (* 1024 1024))

  ;; :hook ((python-mode . lsp)
  ;;        (js-mode . lsp)
  ;;        (typescript-mode . lsp)
  ;;        ;; if you want which-key integration
  ;;        (lsp-mode . lsp-enable-which-key-integration))

  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; Installation checklist
;; ----------------------
;;
;; - zypper install ShellCheck
;;
;; - pip3 install --user epc orjson sexpdata six pyright
;;
;; - $ESR/lsp-bridge/lsp_bridge.py exists
;;   (FIXME: melpa recipe doesn't achieve this yet)
;;
;; - (executable-find "python3") => Python where required modules
;;   are installed (check exec-path variable if not)
;;
;; - npm install -g {bash,typescript,yaml}-language-server typescript emmet-ls vscode-langservers-extracted
;;
;; - make sure lsp-bridge has patches for eslint multi-server
;;
;; - make sure lsp-bridge-multi-lang-server-mode-list has eslint entries
;;
;; - M-x lsp-bridge-restart-process after changing multi-server JSON files
;;
;; - (getenv "PATH") => includes path where npm modules are installed.
;;   If not:
;;
;;     (setenv "PATH" "...")
;;
;;   e.g.
;;
;;     (setenv "PATH" (concat "/home/adam/.nvm/versions/node/v16.18.0/bin:" (getenv "PATH")))
;;
;;   (or use ielm to interactively edit) and then
;;
;;     M-x lsp-bridge-restart-process
;;
;;   so that lsp_bridge.py can inherit the right PATH.
;;
;; - Check *lsp-bridge* buffer for errors.
;;   Set lsp-bridge-enable-log
;;
(use-package lsp-bridge
  ;; Check / reset recipe cache:
  ;;
  ;; (gethash "lsp-bridge" straight--recipe-cache)
  ;; (remhash "lsp-bridge" straight--recipe-cache)
  ;;
  ;; This won't work interactively; need to bypass the cache
  ;; to register the recipe the first time:
  ;; (straight-use-package 'lsp-bridge)
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))

  :after yasnippet
  :bind (:map lsp-bridge-mode-map
              ("M-." . lsp-bridge-find-def))

  :custom
  ;; Stop lsp-bridge interfering with perfectly good emacs backup
  ;; and lockfile mechanisms which I depend on (e.g. in git-safe-push-to-checkout).
  (lsp-bridge-disable-backup nil)
  (acm-backend-yas-match-by-trigger-keyword t)

  :config
  ;; Not good enough to use everywhere yet
  ;; (global-lsp-bridge-mode)
  )

(provide 'as-lsp)
