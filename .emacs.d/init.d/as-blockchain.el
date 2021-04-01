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

;; Unmaintained: https://github.com/kootenpv/flymake-solidity/tree/master
;; (use-package flymake-solidity)

(provide 'as-blockchain)
