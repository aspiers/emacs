;; Steve Yegge to the rescue
;; N.B. doesn't support jsx - use built-in js or other options for that
(use-package js2-mode
  :mode ("\\.[cm]?js\\(\.erb\\)?$"))

;; TODO: maybe try https://github.com/js-emacs/js2-refactor.el
;; although it wouldn't work with jsx or ts or tsx.

(use-package js
  :mode ("\\.jsx\\'"))

;; Allow easy configuring of 3rd party repos for various indentation
;; strategies, via statements like:
;;
;;    (dir-locals-set-directory-class "/path/to/repo/" 'js-tab-8-indent)
(dir-locals-set-class-variables
 'js-indent-8-tabs
 '((nil . ((indent-tabs-mode . t)
           (js-indent-level . 8)))))
(dir-locals-set-class-variables
 'js-indent-2-no-tabs
 '((nil . ((indent-tabs-mode . nil)
           (js-indent-level . 2)))))

(use-package coffee-mode)
(use-package flymake-eslint)
(use-package flymake-jshint)
(use-package flymake-jslint)

;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :hook ((typescript-mode . subword-mode)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . setup-tide-mode)
         ;; Leave prettier.el to handle this
         ;; (before-save . tide-format-before-save)
         )

  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)))

(defun as-setup-tide-mode-for-tsx ()
    (when (string-equal "tsx"
                        (file-name-extension buffer-file-name))
      (setup-tide-mode)))

(with-packages (tide web-mode)
  ;; :mode ("\\.tsx\\'" . web-mode)
  :hook ((web-mode . as-setup-tide-mode-for-tsx))

  :config
  (defun as-setup-tide-mode-for-tsx ()
    (when (string-equal "tsx"
                        (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(defvar as-prettier-js-dir-locals-variables
  '((js-mode . ((eval . (prettier-mode t))))
    (typescript-mode . ((eval . (prettier-mode t)))))
  "Variables for use with `dir-locals-set-class-variables' to
enable prettier.el for Javascript files.")

(dir-locals-set-class-variables 'prettier-js
                                as-prettier-js-dir-locals-variables)

(provide 'as-javascript)
