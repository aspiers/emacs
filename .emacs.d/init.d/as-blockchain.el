;; Lefteris FTW! https://github.com/ethereum/emacs-solidity/
(use-package solidity-mode
  :config
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :config
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-solium-checker-active t)
  ;; (setq solidity-flycheck-chaining-error-level ...)
  )

(with-packages (solidity-mode solidity-flycheck)
  :config
  (add-hook 'solidity-mode-hook 'flycheck-mode))

(use-package company-solidity)

(defvar as-prettier-solidity-dir-locals-variables
  '((solidity-mode . ((eval . (prettier-mode t)))))
  "Variables for use with `dir-locals-set-class-variables' to
enable prettier.el for Solidity files.")

;; Remember that setting this class may override other classes,
;; so .dir-locals.el might be a better option.
(dir-locals-set-class-variables 'prettier-solidity
                                as-prettier-solidity-dir-locals-variables)

;; Unmaintained: https://github.com/kootenpv/flymake-solidity/tree/master
;; (use-package flymake-solidity)

(provide 'as-blockchain)
