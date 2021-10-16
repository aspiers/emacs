;; Steve Yegge to the rescue
(use-package js2-mode
  :mode ("\\.js\\(\.erb\\)?$"))

;; Allow easy configuring of 3rd party repos for various indentation
;; strategies, via statements like:
;;
;;    (dir-locals-set-directory-class "/path/to/repo/" 'js-tab-8-indent)
(dir-locals-set-class-variables
 'js-indent-8-tabs
 '((nil . ((indent-tabs-mode . t)
           (js-indent-level . 8)
           (js2-basic-offset . 8)))))
(dir-locals-set-class-variables
 'js-indent-2-no-tabs
 '((nil . ((indent-tabs-mode . nil)
           (js-indent-level . 2)
           (js2-basic-offset . 2)))))

(use-package coffee-mode)
(use-package flymake-eslint)
(use-package flymake-jshint)
(use-package flymake-jslint)

;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

(use-package tree-sitter
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          ;; Leave prettier.el to handle this
;;          ;; (before-save . tide-format-before-save)
;;          ))

(defvar as-prettier-js-dir-locals-variables
  '((js-mode . ((eval . (prettier-mode t))))
    (typescript-mode . ((eval . (prettier-mode t)))))
  "Variables for use with `dir-locals-set-class-variables' to
enable prettier.el for Javascript files.")

(dir-locals-set-class-variables 'prettier-js
                                as-prettier-js-dir-locals-variables)

(provide 'as-javascript)
